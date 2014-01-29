/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration),
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
package alma.acs.alarmsystem.source;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import alma.acs.concurrent.ThreadLoopRunner;
import alma.acs.concurrent.ThreadLoopRunner.CancelableRunnable;
import alma.acs.concurrent.ThreadLoopRunner.ScheduleDelayMode;

/**
 * The map of active and inactive alarms.
 * <P>
 * Alarms that are not updated after {@link #ALARM_ACTIVITY_TIME} are removed
 * from the map so that they will be sent again to the alarm service.
 * <P>
 * The class is thread safe i.e. the methods can be called without any further locking.
 * <P>
 * <EM>Implementation note</EM>: 
 * All the methods add new entries or update existing pairs in the {@link #alarms}
 * map with the exception of the periodic thread {@link AlarmsMapRunnable#run()}
 * that remove elements that are in the map unchanged since more the {@link #ALARM_ACTIVITY_TIME}.
 * <P>
 * {@link AlarmsMap} does not need any locking because the map contains immutable
 * {@link AlarmInfo} objects and the map itself is implemented by a {@link ConcurrentHashMap}.
 * 
 * @author acaproni
 */
public class AlarmsMap {
	
	/**
	 * The data stored for each alarm in the map.
	 * <P>
	 * Objects of this class are immutable.
	 * 
	 * @author acaproni
	 */
	public static class AlarmInfo {
		
		/**
		 * The time of the last update of this alarm
		 */
		public final long time;
		
		/**
		 * The state active/terminate of the alarm
		 */
		public final boolean active;
		
		/**
		 * Constructor 
		 * 
		 * @param active The initial state active/terminate of the alarm
		 */
		public AlarmInfo(boolean active) {
			this.active=active;
			this.time=System.currentTimeMillis();
		}
		
	}
	
	/**
	 * The thread to delete the alarms older than the time interval.
	 * <P>
	 * This thread is scheduled by the ThreadLoopRunner.
	 * Note that this thread is the only one that removes entries from the map: 
	 * all other methods update or add entries.
	 * 
	 * @author acaproni
	 */
	public class AlarmsMapRunnable extends CancelableRunnable {
		/**
		 * Check the timestamps of the entries in the map and remove those that
		 * have not been changed since more then ALARM_ACTIVITY_TIME.
		 */
		public void run() {
			for (String key: alarms.keySet()) {
				if (shouldTerminate) {
					return;
				}
				AlarmInfo info = alarms.get(key);
				// Info can't be null because this is the only
				// thread removing items from the map
				// We check nevertheless to improve safety.
				if (info!=null) {
					if (System.currentTimeMillis()-ALARM_ACTIVITY_TIME*1000>info.time) {
						alarms.remove(key);
					}	
				}
			}
		}
	}
	
	/**
	 * The alarms that have no activity after the following time interval
	 * are removed from the map. 
	 * <P>
	 * In practice, it means that an alarm with the same state will be sent again
	 * if it arrives after <code>ALARM_ACTIVITY_TIME</code> seconds.
	 */
	public static final int ALARM_ACTIVITY_TIME=30;
	
	/**
	 * The map of alarms.
	 * <P>
	 * The key is the alarm ID i.e. FF:FM:FC.
	 * The value is an {@link AlarmInfo} with boolean set to <code>true</code> 
	 * if the alarm has been activated, <code>false</code> otherwise.
	 */
	private final Map<String, AlarmInfo> alarms = new ConcurrentHashMap<String, AlarmsMap.AlarmInfo>();
	
	/**
	 * The runner for scheduling the thread to delete old alarms
	 * from the map
	 */
	private final ThreadLoopRunner loopRunner;
	
	/**
	 * The logger
	 */
	private final Logger logger;
	
	/**
	 * Constructor
	 * 
	 * @param threadFactory The thread factory to schedule the timer loop
	 * @param logger The logger
	 */
	public AlarmsMap(ThreadFactory threadFactory, Logger logger) {
		this.logger=logger;
		loopRunner = new ThreadLoopRunner(new AlarmsMapRunnable(), ALARM_ACTIVITY_TIME, TimeUnit.SECONDS, threadFactory, logger, "alarm_timer");
	}
	
	/**
	 * Return the active alarms in the map
	 * 
	 * @return the active alarms in the map
	 */
	public Collection<String> getActiveAlarms() {
		Vector<String>ret = new Vector<String>();
		Set<String> keys=alarms.keySet();
		for (String key: keys) {
			if (alarms.get(key).active) {
				ret.add(key);
			}
		}
		return ret;
	}
	
	/**
	 * An alarms has been raised and must be added to the map.
	 * 
	 * @param alarmID The ID of the alarm
	 * @return <code>true</code> if the alarm with the give ID was already present 
	 * 		   in the list and it was active; <code>false</code> otherwise.
	 */
	public boolean raise(String alarmID) {
		AlarmInfo info=alarms.get(alarmID);
		boolean ret=(info!=null) && (info.active);
		alarms.put(alarmID, new AlarmInfo(true));
		return ret;
	}
	
	/**
	 * An alarm has been cleared and must be added to the map.
	 * 
	 * @param alarmID The ID of the alarm
	 * @return <code>true</code> if the alarm with the give ID was already present 
	 * 		   in the list and it was terminated; <code>false</code> otherwise.
	 */
	public boolean clear(String alarmID) {
		AlarmInfo info=alarms.get(alarmID);
		boolean ret=(info!=null) && (!info.active);
		alarms.put(alarmID, new AlarmInfo(false));
		return ret;
	}
	
	/**
	 * Start the thread to delete the oldest alarms
	 */
	public void start() {
		loopRunner.setDelayMode(ScheduleDelayMode.FIXED_DELAY);
		loopRunner.runLoop();
	}
	
	/**
	 * Shutdown the thread a frees the resources
	 */
	public void shutdown() {
		try {
			if (loopRunner.shutdown(1, TimeUnit.SECONDS)) {
				logger.finest("Thread shut down");
			} else {
				logger.warning("Failed to cleanly shut down the AlarmsMap thread");
			}
		} catch (InterruptedException ie) {
			logger.warning("AlarmsMap thread interrupted while shutting down.");
		}
		clear();
	}

	/**
	 * The size of the map
	 * 
	 * @return The size of the map
	 * 
	 * @see ConcurrentHashMap#size()
	 */
	public int size() {
		return alarms.size();
	}

	/**
	 * 
	 * @param key The key to look for in the map
	 * 
	 * @return <code>true</code> if the map contains an item with the given ket
	 */
	public boolean containsKey(Object key) {
		return alarms.containsKey(key);
	}

	/**
	 * Empty the map.
	 * <P>
	 * This method should not be used because there is no need to clear the content of the
	 * map during normal operations. 
	 * Especially it must not be invoked when the periodic thread {@link AlarmsMapRunnable#run()}
	 * is checking the timestamps of the entries because it can generate a NPE
	 */
	private void clear() {
		alarms.clear();
	}

	/**
	 * 
	 * @param key The key to look for
	 * @return The object with the given key
	 */
	public AlarmInfo get(Object key) {
		return alarms.get(key);
	}
	
	/**
	 * This method returns a set of the keys in the map at the moment when the 
	 * method has been called. It is meant to be used for testing purposes only.
	 * <BR>
	 * Getting elements from the returned set could lead to <code>null</code> values
	 * if ssuch a value has been removed by the thread.
	 * @return A set with the key in the map
	 */
	public Set<String> keySet() {
		return alarms.keySet();
	}
}
