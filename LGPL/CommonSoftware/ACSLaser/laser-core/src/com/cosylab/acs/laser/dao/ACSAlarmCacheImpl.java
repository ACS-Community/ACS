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

import java.sql.Timestamp;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;
import java.util.Properties;
import java.util.Set;
import java.util.Iterator;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;

import cern.laser.business.LaserObjectNotFoundException;
import cern.laser.business.cache.AlarmCache;
import cern.laser.business.cache.AlarmCacheException;
import cern.laser.business.cache.AlarmCacheListener;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmImpl;
import cern.laser.business.data.AlarmChange;
import cern.laser.business.data.CategoryActiveList;
import cern.laser.business.data.Category;
import cern.laser.business.data.Location;
import cern.laser.business.data.ResponsiblePerson;
import cern.laser.business.data.Source;
import cern.laser.business.data.StatusImpl;
import cern.laser.business.data.Triplet;
import cern.laser.business.definition.data.SourceDefinition;

import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;

/**
 * Implementation of the {@link AlarmCache} to use within ACS.
 * <P>
 * <b>Note on the locking mechanism.</b><BR>
 * Each method that changes or reads the cache is surrounded by lock/unlock
 * to ensure the mutual exclusion and therefore the integrity of the cache.
 * If a sequence of operations have to be performed in mutual exclusion then
 * the code must initially call <code>acquire()</code> and finally <code>release()</code>. 
 * The locking mechanism of each method works as follows:
 * <UL>
 * <LI>if the lock is free then it is acquired at the beginning and released before exiting
 * <LI>if the lock is not free then it is acquired only if owned by the same thread that is locking it 
 *     and released before exiting
 * </UL>
 * 
 * @author acaproni
 */
public class ACSAlarmCacheImpl implements AlarmCache 
{
	/**
	 * The empty String used for undefined String fields
	 */
	private static final String EMPTY_STRING="";
	
	/**
	 * The key of the property for undocumented alarms
	 */
	public static final String alarmServerPropkey="AlarmServerProp";
	
	/**
	 * The value of the property for undocumented alarms
	 */
	public static final String undocumentedAlarmProp="UnconfiguredAlarm";
	
	// The cache of the alarms
	//
	// The key is a string (the same string generated 
	// by Alarm.getTriplet().toIdentifier())
	// The value is an alarm (AlarmImpl)
	private Map<String,Alarm> alarms = Collections.synchronizedMap(new HashMap<String,Alarm>());
	
	// The object to access the database
	private AlarmDAO dao;
	
	// The listener for the changes in this cache
	private AlarmCacheListener listener;
	
	/**
	 * the lock used to ensure mutual exclusion when accessing the cache.
	 * 
	 * It is required instead of synchronized because sometimes the mutual exclusion
	 * spans between calls to a series of methods of the cache.  
	 */
	private final ReentrantLock lock=new ReentrantLock();
	
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
	 * The logger
	 */
	private final Logger logger;
	
	/**
	 * The category DAO
	 */
	private final ACSCategoryDAOImpl categoryDAO;
	
	/**
	 * The constructor
	 * 
	 * @param alarmDAO The object to access the database
	 * @param alarmCacheListener The listener
	 */
	public ACSAlarmCacheImpl(AlarmDAO alarmDAO, ACSCategoryDAOImpl categoryDAO, AlarmCacheListener alarmCacheListener,Logger logger)
	{
		if (alarmDAO==null || alarmCacheListener==null) {
			throw new IllegalArgumentException("The AlarmDAO and the listener can't be null!");
		}
		this.logger=logger;
		this.categoryDAO=categoryDAO;
		
		alarms.clear(); // Redundant
		
		// Store the values in local variables
		dao=alarmDAO;
		ACSAlarmDAOImpl t = (ACSAlarmDAOImpl)dao;
		t.setAlarmCache(this);
		listener=alarmCacheListener;
		
		// The alarms should be loaded in the cache in the constructor
		// but it is not working because something in the DAO chain has
		// not yet been initialized
		//
		// All the alarms are load when the first request arrives 
		// in the getReference method
	}

	public void initializeAlarmCache(Map alarms, Map activeLists) {
		lock.lock();
		try {
			this.alarms.putAll(alarms);
			this.activeLists.putAll(activeLists);
		} finally {
			lock.unlock();
		}
	}

	public Alarm getCopy(String identifier) throws AlarmCacheException {
		// This method get the reference to the object first and then
		// create a copy to return to the caller
		
		// The Alarm to return a copy of
		lock.lock();
		Alarm retAl;
		try {
			retAl = getReference(identifier);
			return (Alarm)(((AlarmImpl)retAl).clone());
		} finally {
			lock.unlock();
		}
	}

	/**
	 * Get a reference to the alarm with the passed identifier.
	 * The alarm is searched in the configuration database delegating
	 * to the {@link ACSAlarmDAOImpl}.
	 * <P>
	 * If the alarm is not found then an alarm is built to be sent to the client
	 * but it is marked with a special property to identify it as a misconfigured 
	 * alarm. In the case of the alarm panel, such alarms will be displayed in a 
	 * dedicated tab.
	 * 
	 * @param identifier The ID of the alarm to get from the cache
	 */
	public Alarm getReference(String identifier) throws AlarmCacheException {
		lock.lock();
		Alarm retAl;
		try {
			// The Alarm to return a reference of
			// Check the integrity of internal data structs
			if (alarms==null || dao==null) {
				System.err.println("*** ACSAlarmCache internal data corrupted!");
				throw new AlarmCacheException("ACSAlarmCache internal data corrupted!");
			}
			
			if (!alarms.containsKey(identifier)) {
				// Get the alarm from the database
				try {
					retAl=(Alarm)dao.findAlarm(identifier);
				} catch (LaserObjectNotFoundException lonfe) {
					// The alarm is not in the configuration database
					//
					// Built an alarm to be sent to the clients 
					// with an special property
					logger.finer(identifier+" is not in TM/CDB: building an unconfigured alarm");
					retAl=buildUnconfiguredAlarm(identifier);
				} catch (Throwable t) {
					System.err.println("*** Exception reading from CDB "+t.getMessage());
					throw new AlarmCacheException(t.getMessage());
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
		} finally {
			lock.unlock();
		}
	}
	
	/**
	 * Update an alarm in the cache without notifying the listener.
	 * 
	 * NOTE: this is used when alarms are generated on the fly (defaultFM) because in
	 * that case there i sno need to notify the listener
	 * 
	 * @param alarm
	 */
	public void update(Alarm alarm) {
		lock.lock();
		try {
			if (alarm==null) {
				throw new IllegalArgumentException("The alarm can't be null");
			}
			alarms.put(alarm.getAlarmId(), alarm);
			dao.updateAlarm(alarm);
		} finally {
			lock.unlock();
		}
	}

	public void replace(Alarm alarm) throws AlarmCacheException {
		lock.lock();
		try {
			if (alarm==null) {
				throw new IllegalArgumentException("Replacing with a null alarm is not allowed");
			}
			Alarm oldAl=alarms.put(alarm.getTriplet().toIdentifier(),alarm);
			dao.updateAlarm(alarm);
			sendMsgToListener(alarm,oldAl);
			updateCategoryActiveLists(alarm);
			//dumpAlarmsCache(false); 
		} finally {
			lock.unlock();
		}
	}

	public void put(Alarm alarm) throws AlarmCacheException {
		lock.lock();
		try {
			if (alarm==null) {
				throw new IllegalArgumentException("Inserting a null alarm is not allowed");
			}
			Alarm oldAl=alarms.put(alarm.getTriplet().toIdentifier(),alarm);
			dao.updateAlarm(alarm);
			updateCategoryActiveLists(alarm);
			sendMsgToListener(alarm,oldAl);
			//dumpAlarmsCache(false);
		} finally {
			lock.unlock();
		}
	}

	public void invalidate(String identifier) throws AlarmCacheException {
		lock.lock();
		try {
			if (identifier==null) {
				throw new IllegalArgumentException("Invalidating a null key is not allowed");
			}
			
			if (!alarms.containsKey(identifier)) {
				throw new AlarmCacheException("The object with the given identifier does not exist");
			}
			alarms.remove(identifier);
			//dumpAlarmsCache(false);
		} finally {
			lock.unlock();
		}
	}

	public CategoryActiveList getActiveListReference(Integer identifier) throws AlarmCacheException {
		lock.lock();
		try {
			if (activeLists.containsKey(identifier)) {
				return activeLists.get(identifier);
			} else {
				CategoryActiveList catList = new CategoryActiveList(identifier);
				activeLists.put(identifier,catList);
				return catList;
			}
		} finally {
			lock.unlock();
		}
	}

	public void close() {
		lock.lock();
		try {
			alarms.clear();
		} finally {
			lock.unlock();
		}
	}

	public void removeActiveList(Integer identifier) throws AlarmCacheException {
		lock.lock();
		activeLists.remove(identifier);
		lock.unlock();
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
		
		System.out.println("*** ACSAlarmCacheImpl dumping cache.....");
		// Get the keys
		Set<String> keys = alarms.keySet();
		Iterator<String> iter = keys.iterator();
		while ( iter.hasNext() ) {
			String key = iter.next();
			System.out.print("***\t"+key+" ");
			if (verbose) {
				Alarm al = alarms.get(key);
				System.out.println("active="+al.getStatus().getActive());
			} else {
				System.out.println();
			}
		}
		System.out.println("ACSAlarmCacheImpl dumping cache..... done");
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
				// In this implementation no exception can be thrown
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
	
	/**
	 * Acquire the lock for using the cache.
	 */
	public void acquire() {
		if (lock.isHeldByCurrentThread()) {
			System.out.println("===>>>>> The same thread tries to acquire the cache more then once!");
		}
		lock.lock();
	}
	
	/**
	 * Acquire the lock for using the cache.
	 */
	public void release() {
		lock.unlock();
	}
	
	/**
	 * Build a unconfigured alarm from the passed ID.
	 * 
	 * A unconfigured alarm has no data but the triplet generated
	 * from the identifier.
	 * I associate the lowest priority and a user property
	 * to be recognized by clients as being generated by the AS instead
	 * of being retrieved from the configuration database.
	 * <P>
	 * The property identifying this alarm as generated by the AS is:
	 * <Name="AlarmServerProp", Value="UnconfiguredAlarm">
	 * <P>
	 * The alarm is associated to the ROOT category.
	 * 
	 * @param alarmID The ID of the alarm
	 * @return The default alarm
	 */
	private Alarm buildUnconfiguredAlarm(String alarmID) {
		if (alarmID==null || alarmID.isEmpty()) {
			throw new IllegalArgumentException("Invalid null or empty alarm ID!");
		}
		String[] parts=alarmID.split(":");
		if (parts.length!=3) {
			throw new IllegalArgumentException("Invalid alarm ID: "+alarmID);
		}
		String FF=parts[0];
		String FM=parts[1];
		Integer FC;
		try {
			FC=Integer.valueOf(parts[2]);
		} catch (NumberFormatException nfe) {
			throw new IllegalArgumentException("Invalid FC in alarm ID: "+alarmID,nfe);
		}
		
		AlarmImpl alarm = new AlarmImpl(); 
		
		alarm.setMultiplicityChildrenIds(new HashSet());
		alarm.setMultiplicityParentIds(new HashSet());
		alarm.setNodeChildrenIds(new HashSet());
		alarm.setNodeParentIds(new HashSet());
		
		alarm.setAction(EMPTY_STRING);
		alarm.setTriplet(new Triplet(FF, FM, FC));
		alarm.setCategories(new HashSet<Category>());
		alarm.setCause(EMPTY_STRING);
		alarm.setConsequence(EMPTY_STRING);
		alarm.setProblemDescription(EMPTY_STRING);
		alarm.setHelpURL(null);
		
		alarm.setInstant(false);
		Location location = new Location("0",EMPTY_STRING,EMPTY_STRING,EMPTY_STRING,EMPTY_STRING);
		alarm.setLocation(location);
		alarm.setPiquetEmail(EMPTY_STRING);
		alarm.setPiquetGSM(EMPTY_STRING);
		alarm.setPriority(3);
		ResponsiblePerson responsible = new ResponsiblePerson(
				1,EMPTY_STRING,EMPTY_STRING,EMPTY_STRING,EMPTY_STRING,EMPTY_STRING);
		alarm.setResponsiblePerson(responsible);
		
		SourceDefinition srcDef = new SourceDefinition("ALARM_SYSTEM_SOURCES","SOURCE",EMPTY_STRING,15,1);
		Source src = new Source(srcDef,responsible);
		alarm.setSource(src);
		alarm.setIdentifier(alarm.getTriplet().toIdentifier());
		
		// Build the status in order to associate the property
		Properties userProps = new Properties();
		userProps.put(alarmServerPropkey, undocumentedAlarmProp);
		StatusImpl status = new StatusImpl(
				false,false,false, false, false, EMPTY_STRING, new Timestamp(0), new Timestamp(0), new Timestamp(0), userProps);
		alarm.setStatus(status);
		
		// Category association
		//
		// A category for this FF can be already set (this is the case when a new FC/FM is found
		// but the FF has already been associated to a category)
		// In practice a new category has to be defined only if the FF is unknown.
		Category[] categories = categoryDAO.findAllCategories();
		Set<Category> categoriesSet=new HashSet<Category>();
		for (Category cat: categories) {
			if (cat.containsAlarm(alarm)) {
				categoriesSet.add(cat);
			}
		}
		if (!categoriesSet.isEmpty()) {
			System.out.println("Alarm "+alarm.getAlarmId()+" already associated to categories");
		} else {
			System.out.println("Alarm "+alarm.getAlarmId()+" has no associated category");
			Category root=categoryDAO.findCategoryByPath("ROOT");
			categoriesSet.add(root);
			alarm.setCategories(categoriesSet);
			root.addAlarm(alarm);
		}
		return alarm;
	}

}
