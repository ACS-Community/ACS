/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */
package com.cosylab.acs.laser.dao;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.Iterator;

import cern.laser.business.cache.AlarmCache;
import cern.laser.business.cache.AlarmCacheException;
import cern.laser.business.cache.AlarmCacheListener;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmImpl;
import cern.laser.business.data.AlarmChange;
import cern.laser.business.data.CategoryActiveList;
import cern.laser.business.data.Category;

public class ACSAlarmCacheImpl implements AlarmCache 
{
	// The cache of the alarms
	//
	// The key is a string (the same string generated 
	// by Alarm.getTriplet().toIdentifier())
	// The value is an alarm (AlarmImpl)
	private HashMap<String,Alarm> alarms = new HashMap<String,Alarm>();
	
	// The object to access the database
	private AlarmDAO dao;
	
	// The listener for the changes in this cache
	private AlarmCacheListener listener;
	
	/**
	 * The HashMap with the active alarms per each category
	 * The map contains a CategoryActiveList per each category 
	 * (the key is the Integer identifying the category)
	 * 
	 * Policy: 
	 *   the CategoryActiveList for a category is created and inserted in the map 
	 *   when a new alarm arrives (remember that an alarm has a Set of Categories)
	 *   So it basically happens in the put and replace methods (based on the
	 *   status of the alarm itself)
	 */
	private HashMap<Integer,CategoryActiveList> activeLists = new HashMap<Integer,CategoryActiveList>();
	
	/**
	 * The constructor
	 * 
	 * @param alarmDAO The object to access the database
	 * @param alarmCacheListener The listener
	 */
	public ACSAlarmCacheImpl(AlarmDAO alarmDAO, AlarmCacheListener alarmCacheListener)
	{
		if (alarmDAO==null || alarmCacheListener==null) {
			throw new IllegalArgumentException("The AlarmDAO and the listener can't be null!");
		}
		alarms.clear(); // Redundant
		
		// Store the values in local variables
		dao=alarmDAO;
		
		listener=alarmCacheListener;
		
		// The alarms should be loaded in the cache in the constructor
		// but it is not workingbecause something in the DAO chain has
		// not yet been initialized
		//
		// All the alarms are load when the first request arrives 
		// in the getReference method
	}

	public void initializeAlarmCache(Map alarms, Map activeLists) {
		this.alarms.putAll(alarms);
		this.activeLists.putAll(activeLists);
	}

	public Alarm getCopy(String identifier) throws AlarmCacheException {
		// This method get the reference to the object first and then
		// create a copy to return to the caller
		
		// The Alarm to return a copy of
		Alarm retAl;
		
		retAl = getReference(identifier);
		
		return (Alarm)(((AlarmImpl)retAl).clone());
	}

	public Alarm getReference(String identifier) throws AlarmCacheException {
		// The Alarm to return a reference of
		Alarm retAl;
		
		// Check the integrity of internal data structs
		if (alarms==null || dao==null) {
			System.err.println("*** ACSAlarmCache internal data corrupted!");
			throw new AlarmCacheException("ACSAlarmCache internal data corrupted!");
		}
		
		if (!alarms.containsKey(identifier)) {
			// Get the alarm from the database
			try {
				retAl=(Alarm)dao.findAlarm(identifier);
			} catch (Exception e) {
				System.err.println("*** Exception reading from CDB "+e.getMessage());
				throw new AlarmCacheException(e.getMessage());
			}
			if (retAl==null) {
				System.err.println("*** Alarm not found in database");
				throw new AlarmCacheException("Alarm not found in database");
			} else {
				// Add the alarm to the cache
				alarms.put(identifier,retAl);
			}
		} else {
			// Get the alarm from the cache
			retAl=alarms.get(identifier);
		}
		if (retAl==null) {
			System.err.println("*** Invalid Alarm");
			throw new AlarmCacheException("Invalid Alarm");
		}
		
		return retAl;
	}

	public void replace(Alarm alarm) throws AlarmCacheException {
		if (alarm==null) {
			throw new IllegalArgumentException("Replacing with a null alarm is not allowed");
		}
		
		Alarm oldAl=alarms.put(alarm.getTriplet().toIdentifier(),alarm);
		sendMsgToListener(alarm,oldAl);
		updateCategoryActiveLists(alarm);
		//dumpAlarmsCache(false);
	}

	public void put(Alarm alarm) throws AlarmCacheException {
		if (alarm==null) {
			throw new IllegalArgumentException("Inserting a null alarm is not allowed");
		}
		
		Alarm oldAl=alarms.put(alarm.getTriplet().toIdentifier(),alarm);
		sendMsgToListener(alarm,oldAl);
		updateCategoryActiveLists(alarm);
		//dumpAlarmsCache(false);
	}

	public void invalidate(String identifier) throws AlarmCacheException {
		if (identifier==null) {
			throw new IllegalArgumentException("Invalidating a null key is not allowed");
		}
		
		if (!alarms.containsKey(identifier)) {
			throw new AlarmCacheException("The object with the given identifier does not exist");
		}
		alarms.remove(identifier);
		//dumpAlarmsCache(false);
	}

	public CategoryActiveList getActiveListReference(Integer identifier) throws AlarmCacheException {
		if (activeLists.containsKey(identifier)) {
			return activeLists.get(identifier);
		} else {
			CategoryActiveList catList = new CategoryActiveList(identifier);
			activeLists.put(identifier,catList);
			return catList;
		}
	}

	public void close() {
		alarms.clear();
	}

	public void removeActiveList(Integer identifier) throws AlarmCacheException {
		activeLists.remove(identifier);
	}
	
	/**
	 * Send the message to the listener
	 * 
	 * NOTE: If the previous alarm does not exist (for example if the alarm is new)
	 *       then we send as old alarm the actual alarm
	 *       If we send a null then cern.laser.business.pojo.AlarmPublisher:publish
	 *       returns an exception while executing
	 *       Status previous_alarm_status=previous.getStatus();
	 *       
	 *       This policy seems reasonable but could have an impact somewhere
	 * 
	 * @param actual The actual alarm
	 * @param old The previous alarm
	 */
	private void sendMsgToListener(Alarm actual, Alarm old) {
		if (old==null) {
			listener.onAlarmChange(new AlarmChange(actual,actual));
		} else {
			listener.onAlarmChange(new AlarmChange(actual,old));
		}
	}
	
	/**
	 * Print a copy of the alarms in the cache in the standard output.
	 * This method is added for debugging
	 * 
	 *  @param verbose If true the details of each alarm found in the
	 *                 cache is also displayed
	 */
	private void dumpAlarmsCache(boolean verbose) {
		if (alarms==null) {
			return;
		}
		if (alarms.size()==0) {
			return;
		} 
		
		// Get the keys
		Set<String> keys = alarms.keySet();
		Iterator<String> iter = keys.iterator();
		while ( iter.hasNext() ) {
			String key = iter.next();
			if (verbose) {
				Alarm al = alarms.get(key);
			}
		}
	}
	
	/**
	 * Return the CategoryActiveList for the specified categoryId.
	 * If the CategoryActiveList does not exist than a new one is created and
	 * inserted in the activeList and then returned to the caller
	 * 
	 * @param categoryId The categoryId identifying the CategoryActiveList
	 * @return The CategoryActiveList for the specified categoryId
	 */
	private CategoryActiveList getCategoryList(Integer categoryId) {
		if (activeLists.containsKey(categoryId)) {
			return activeLists.get(categoryId);
		} else {
			CategoryActiveList catList = new CategoryActiveList(categoryId);
			activeLists.put(categoryId,catList);
			return catList;
		}
	}
	
	/**
	 * Update the CategoryActiveList for the alarm: all the CategoryActiveLists
	 * of the categories of the alarm will be updated.
	 * The alarmId is inserted or removed in the CategoryActiveList depending
	 * of the status of the alarm
	 * 
	 * @param alarm The alarm whose id must be inserted or removed for the
	 *     	        CategoryActiveLists of its categories
	 */
	private void updateCategoryActiveLists(Alarm alarm) {
		if (alarm.getStatus()==null || alarm.getStatus().getActive()==null) {
			throw new IllegalArgumentException("Invalid alarm: status and/or status.active null");
		}
		boolean status = alarm.getStatus().getActive().booleanValue();
		String alarmId = alarm.getAlarmId();
		Iterator categoryIterator = alarm.getCategories().iterator();
		while (categoryIterator.hasNext()) {
			Integer categoryId = (Integer)((Category)categoryIterator.next()).getCategoryId();
			CategoryActiveList catList = null;
			try {
				catList = getActiveListReference(categoryId);
			} catch (AlarmCacheException ace) {
				// In this implementation no excaption can be thrown
				// by getActiveListReference
				// I write a message in case someone changes the things
				System.err.println("Exception "+ace.getMessage());
				ace.printStackTrace();
				continue;
			}
			if (status) {
				catList.addAlarm(alarmId);
			} else {
				catList.removeAlarm(alarmId);
			}
		}
	}

}
