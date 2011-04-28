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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Vector;

/**
 * The map of alarms
 * 
 * @author acaproni
 *
 */
public class AlarmsMap extends TimerTask {
	
	/**
	 * The data stored for each alarm in the map
	 * 
	 * @author acaproni
	 */
	public class AlarmInfo {
		
		/**
		 * The time of the last update of this alarm
		 */
		private long time;
		
		/**
		 * The state active/terminate of the alarm
		 */
		private boolean active;
		
		/**
		 * Constructor 
		 * 
		 * @param active The initial state active/terminate of the alarm
		 */
		public AlarmInfo(boolean active) {
			this.active=active;
			time=System.currentTimeMillis();
		}
		
		/**
		 * Update the state of the alarm 
		 * 
		 * @param active The new state of the alarm
		 * @return The previous state of the alarm
		 */
		public boolean update(boolean active) {
			boolean ret=this.active;
			this.active=active;
			time=System.currentTimeMillis();
			return ret;
		}
		
		/**
		 * 
		 * @return The current state of the alarm
		 */
		public boolean isActive() {
			return active;
		}
		
		/**
		 * 
		 * @return The time when the alarm has been updated the last time
		 */
		public long lastUpdateTime() {
			return time;
		}
	}
	
	/**
	 * The alarms that have no activity after the following time interval (msecs)
	 * are removed from the map. 
	 * <P>
	 * In practice, it means that an alarm with the same state will be sent again
	 * if it arrives after <code>ALARM_ACTIVITY_TIME</code> msecs.
	 */
	public static final int ALARM_ACTIVITY_TIME=30000;
	
	/**
	 * The timer to remove from the map the alarms older then {@link AlarmsMap#ALARM_ACTIVITY_TIME}
	 */
	private final Timer timer = new Timer("AlarmsMap.timer", true);
	
	/**
	 * The map of alarms.
	 * <P>
	 * The key is the alarm ID i.e. FF:FM:FC.
	 * The value is <code>true</code> if the alarm has been activated,
	 * <code>false</code> otherwise.
	 */
	private final Map<String, AlarmInfo>alarms = 
		Collections.synchronizedMap(new HashMap<String, AlarmInfo>());
	
	/**
	 * Constructor
	 */
	public AlarmsMap() {
		timer.schedule(this,ALARM_ACTIVITY_TIME, ALARM_ACTIVITY_TIME);
	}
	
	/**
	 * Return the active alarms in the map
	 * 
	 * @return the active alarms in the map
	 */
	public synchronized Collection<String> getActiveAlarms() {
		Vector<String>ret = new Vector<String>();
		Set<String> keys=alarms.keySet();
		for (String key: keys) {
			if (alarms.get(key).isActive()) {
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
	public synchronized boolean raise(String alarmID) {
		AlarmInfo info=alarms.get(alarmID);
		boolean ret=(info!=null) && (info.isActive());
		if (info==null) {
			info = new AlarmInfo(true);
			alarms.put(alarmID, info);
		} else {
			info.update(true);
		}
		return ret;
	}
	
	/**
	 * An alarms has been cleared and must be added to the map.
	 * 
	 * @param alarmID The ID of the alarm
	 * @return <code>true</code> if the alarm with the give ID was already present 
	 * 		   in the list and it was terminated; <code>false</code> otherwise.
	 */
	public synchronized boolean clear(String alarmID) {
		AlarmInfo info=alarms.get(alarmID);
		boolean ret=(info!=null) && (!info.isActive());
		if (info==null) {
			info = new AlarmInfo(false);
			alarms.put(alarmID, info);
		} else {
			info.update(false);
		}
		return ret;
	}
	
	public synchronized void close() {
		timer.cancel();
		alarms.clear();
	}

	/**
	 * Remove the alarms older then {@link AlarmsMap#ALARM_ACTIVITY_TIME}
	 */
	@Override
	public synchronized void run() {
		System.out.println("Timer iteration at "+System.currentTimeMillis()+" with size="+alarms.size());
		Thread t = new Thread(new Runnable() {
			@Override
			public void run() {
				synchronized(this) {
					Vector<String>toRemove=new Vector<String>();
					for (String key: alarms.keySet()) {
						AlarmInfo info = alarms.get(key);
						if (System.currentTimeMillis()-ALARM_ACTIVITY_TIME>info.lastUpdateTime()) {
							toRemove.add(key);
						}
					}
					int size=toRemove.size();
					for (String key: toRemove) {
						alarms.remove(key);
					}
					toRemove.clear();
					System.out.println("Removed "+size+" items; items in map="+alarms.size());
				}
			}
		},"AlarmsMap.run");
		t.setDaemon(true);
		t.start();
	}

	/**
	 * 
	 * @return The size of the map
	 */
	public synchronized int size() {
		return alarms.size();
	}

	/**
	 * 
	 * @param key The key to look for in the map
	 * 
	 * @return <code>true</code> if the map contains an item with the given ket
	 */
	public synchronized boolean containsKey(Object key) {
		return alarms.containsKey(key);
	}

	/**
	 * Empty the map
	 */
	public void clear() {
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
	
	
}
