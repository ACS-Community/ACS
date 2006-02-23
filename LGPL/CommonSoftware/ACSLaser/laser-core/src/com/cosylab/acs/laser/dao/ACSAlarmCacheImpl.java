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
	private HashMap alarms = new HashMap();
	
	// The object to access the database
	private AlarmDAO dao;
	
	// The listener for the changes in this cache
	private AlarmCacheListener listener;
	
	/**
	 * The HashMap with the acitive alarms per each category
	 * The map contains a CategoryActiveList per each category 
	 * (the key is the Integer identifying the category)
	 * 
	 * Policy: 
	 *   the CategoryActiveList for a cetegory is created and inserted in the map 
	 *   when a new alarm arrives (remember that an alarm has a Set of Categories)
	 *   So it basically happens in the put and replace methods (based on the
	 *   status of the alarm itself)
	 */
	private HashMap activeLists = new HashMap();
	
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
		//System.out.println("*** ACSAlarmCacheImpl::ACSAlarmCacheImpl");
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
	
	/**
	 * Loads all the alarms at startup
	 * This is the way the cache works in the Laser
	 * 
	 * It create a Map of the alarms by reading the CDB then call the 
	 * initializeAlarmCache
	 * 
	 */
	private void preloadAlarms() {
		if (dao==null) {
			throw new IllegalStateException("The DAO is null");
		}
		ACSAlarmDAOImpl daoImpl = (ACSAlarmDAOImpl)dao;
		String[] alarmIDS = daoImpl.getAllAlarmIDs();
		if (alarmIDS==null || alarmIDS.length==0) {
			// Force the DAO to load the alarms
			daoImpl.loadAlarms();
			alarmIDS = daoImpl.getAllAlarmIDs();
		}
		HashMap alarmsMap = new HashMap(alarmIDS.length);
		for (int t=0; t<alarmIDS.length; t++) {
			Alarm alarm;
			alarm = (Alarm)dao.findAlarm(alarmIDS[t]);
			alarmsMap.put(alarmIDS[t],alarm);
		}
		HashMap activeMap = new HashMap();
		initializeAlarmCache(alarmsMap,activeMap);
	}

	public void initializeAlarmCache(Map alarms, Map activeLists) {
		//System.out.println("*** ACSAlarmCacheImpl::initializeAlarmCache");
		this.alarms.putAll(alarms);
		this.activeLists.putAll(activeLists);
	}

	public Alarm getCopy(String identifier) throws AlarmCacheException {
		//System.out.println("*** ACSAlarmCacheImpl::getCopy("+identifier+")");
		// This method get the reference to the object first and then
		// create a copy to return to the caller
		
		// The Alarm to return a copy of
		Alarm retAl;
		
		retAl = getReference(identifier);
		
		return (Alarm)(((AlarmImpl)retAl).clone());
	}

	public Alarm getReference(String identifier) throws AlarmCacheException {
		System.out.println("*** ACSAlarmCacheImpl::getReference("+identifier+") "+alarms.size()+" items in cache");
		// The Alarm to return a reference of
		Alarm retAl;
		
		// Check the integrity of internal data structs
		if (alarms==null || dao==null) {
			System.err.println("*** ACSAlarmCache internal data corrupted!");
			throw new AlarmCacheException("ACSAlarmCache internal data corrupted!");
		}
		
		// Load all the alarms
		// The laser source loads the alarm during the startup and keep
		// all of them in the cache: we load them when we receive the first
		// request
		// The reason is that if I try to load the alarm in the
		// constructor then I have a failure because some object is not yet ready
		// (It seems that some objects wait for some external initialization)
		if (alarms.size()==0) {
			preloadAlarms();
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
				System.out.println("*** Added "+identifier+" to cache from CDB ("+alarms.size()+" items in cache)");
			}
		} else {
			// Get the alarm from the cache
			System.out.println("*** Alarm retrieved from the cache");
			retAl=(Alarm)alarms.get(identifier);
		}
		if (retAl==null) {
			System.err.println("*** Invalid Alarm");
			throw new AlarmCacheException("Invalid Alarm");
		}
		
		System.out.println("*** ACSAlarmCacheImpl::getReference returning alarm "+retAl.getTriplet().toString());
		return retAl;
	}

	public void replace(Alarm alarm) throws AlarmCacheException {
		System.out.println("*** ACSAlarmCacheImpl::replace");
		if (alarm==null) {
			throw new IllegalArgumentException("Replacing with a null alarm is not allowed");
		}
		
		Alarm oldAl=(Alarm)alarms.put(alarm.getTriplet().toIdentifier(),alarm);
		sendMsgToListener(alarm,oldAl);
		updateCategoryActiveLists(alarm);
		//dumpAlarmsCache(false);
	}

	public void put(Alarm alarm) throws AlarmCacheException {
		System.out.println("*** ACSAlarmCacheImpl::put");
		if (alarm==null) {
			throw new IllegalArgumentException("Inserting a null alarm is not allowed");
		}
		
		Alarm oldAl=(Alarm)alarms.put(alarm.getTriplet().toIdentifier(),alarm);
		sendMsgToListener(alarm,oldAl);
		updateCategoryActiveLists(alarm);
		//dumpAlarmsCache(false);
	}

	public void invalidate(String identifier) throws AlarmCacheException {
		System.out.println("*** ACSAlarmCacheImpl::invalidate");
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
		System.out.println("*** ACSAlarmCacheImpl::getActiveListReference("+identifier+")");
		if (activeLists.containsKey(identifier)) {
			return (CategoryActiveList)activeLists.get(identifier);
		} else {
			CategoryActiveList catList = new CategoryActiveList(identifier);
			activeLists.put(identifier,catList);
			return catList;
		}
	}

	public void close() {
		//System.out.println("*** ACSAlarmCacheImpl::close");
		alarms.clear();
	}

	public void removeActiveList(Integer identifier) throws AlarmCacheException {
		System.out.println("*** ACSAlarmCacheImpl::removeActiveList");
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
		//System.out.println("*** ACSAlarmCacheImpl::sendMsgToListener");
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
			System.out.println("** The alarm cache is null");
			return;
		}
		if (alarms.size()==0) {
			System.out.println("** The alarm cache is empty");
			return;
		} 
		System.out.println("** Items in cache: "+alarms.size());
		
		// Get the keys
		Set keys = alarms.keySet();
		Iterator iter = keys.iterator();
		while ( iter.hasNext() ) {
			String key = (String)iter.next();
			System.out.println("** Key = "+key.toString());
			if (verbose) {
				Alarm al = (Alarm)alarms.get(key);
				System.out.println("** Alarm "+al.toString());
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
			return (CategoryActiveList)activeLists.get(categoryId);
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
