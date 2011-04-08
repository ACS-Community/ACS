/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.table;

import java.util.Vector;

import cern.laser.client.data.Alarm;

import alma.alarmsystem.clients.CategoryClient;

/**
 * Extends <code>AlarmsContainer</code> for the reduced alarms.
 * 
 * @author acaproni
 *
 */
public class AlarmsReductionContainer extends AlarmsContainer {
	
	/**
	 * The index when the reduction rules are in place
	 * <P>
	 * Each item in the vector represents the ID of the entry 
	 * shown in a table row when the reduction rules are used.
	 */
	private final Vector<String> indexWithReduction = new Vector<String>();
	
	/**
	 * The <code>CategoryClient</code> to ask for parents/children
	 * while reducing alarms.
	 * <P>
	 * It can be <code>null</code> so needs to be checked before
	 * invoking methods.
	 */
	private CategoryClient categoryClient=null;

	/**
	 * Constructor 
	 * 
	 * @param max
	 * 
	 * @see {@link AlarmsContainer}
	 */
	public AlarmsReductionContainer(int max) {
		super(max);
	}
	
	/**
	 * Return the number of alarms in the container depending
	 * if the reduction rules are applied or not
	 * 
	 * @param <code>true</code> if the reduction rules are applied
	 * @return The number of alarms in the container
	 */
	public synchronized int size(boolean reduced) {
		if (!reduced) {
			return super.size();
		} else {
			return indexWithReduction.size();
		}
	}
	
	/**
	 * Add an entry (i.e a alarm) in the collection.
	 * <P>
	 * If there is no room available in the container,
	 * an exception is thrown: checking if there is enough room 
	 * must be done by the caller.
	 * <P>
	 * @param entry The not <code>null</code> entry to add
	 * @throw {@link AlarmContainerException} If the entry is already in the container
	 */
	public synchronized void add(AlarmTableEntry entry) throws AlarmContainerException {
		super.add(entry);
		addAlarm(entry);
	}
	
	/**
	 * Add an alarm to the reduction container
	 * 
	 * @param alarm The alarm to add to the container
	 */
	private void addAlarm(AlarmTableEntry alarm) {
		if (!alarm.getStatus().isReduced()) {
			indexWithReduction.add(alarm.getAlarmId());
		}
		hideReducedChildren(alarm);
	}
	
	/**
	 * Hide the active alarms of this entry.
	 * 
	 * @param entry The not <code>null</code> entry to hide active children
	 */
	private void hideReducedChildren(AlarmTableEntry entry) {
		if (entry==null) {
			throw new IllegalArgumentException("The entry can't be null");
		}
		if (categoryClient==null) {
			return;
		}
		// Check if the table contains children of this alarm that must be hidden
		// i.e. removed
		Alarm[] children=null;
		try {
			if (entry.isNodeParent()) {
					children = categoryClient.getChildren(entry.getAlarmId(), true);
				
			} else if (entry.isMultiplicityParent()) {
				children = categoryClient.getChildren(entry.getAlarmId(), false);
			}
		} catch (Throwable t) {
			System.err.println("Error getting children of "+entry.getAlarmId()+": "+t.getMessage());
			t.printStackTrace();
			children=null;
		}
		if (children!=null) {
			System.out.println("Childern of "+entry.getAlarmId());
			for (Alarm al: children) {
				System.out.println("\tchild "+al.getAlarmId());
				indexWithReduction.remove(al.getAlarmId());
			}
		}
	}
	
	/**
	 * Set the <code>CategoryClient</code>
	 * 
	 * @param client The <code>CategoryCLient</code>; it can be <code>null</code>.
	 */
	public void setCategoryClient(CategoryClient client) {
		this.categoryClient=client;
	}
	
	/**
	 * Return the entry in the given position
	 * 
	 * @param pos The position of the alarm in the container
	 * @param reduced <code>true</code> if the alarms in the table are reduced
	 * @return The <code>AlarmTableEntry<code> in the given position
	 */
	public synchronized AlarmTableEntry get(int pos, boolean reduced) {
		if (!reduced) {
			return super.get(pos);
		} 
		String ID = indexWithReduction.get(pos);
		AlarmTableEntry ret = get(ID);
		if (ret==null) {
			throw new IllegalStateException("Inconsistent state of reduced container");
		}
		return ret;
	}
	
	/**
	 * Remove all the elements in the container
	 */
	public synchronized void clear() {
		indexWithReduction.clear();
		super.clear();
	}

	/**
	 * Remove the entry for the passed alarm
	 * 
	 * @param alarm The alarm whose entry must be removed
	 * @throws AlarmContainerException If the alarm is not in the container
	 */
	public synchronized void remove(AlarmTableEntry alarm) throws AlarmContainerException {
		if (alarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		int pos = indexWithReduction.indexOf(alarm.getAlarmId());
		if (pos>=0) {
			indexWithReduction.remove(pos);
		}
		super.remove(alarm);
	}
	
	/**
	 * Remove the oldest entry in the container
	 * 
	 * @return The removed item
	 * @throws AlarmContainerException If the container is empty
	 */
	public synchronized AlarmTableEntry removeOldest() throws AlarmContainerException {
		AlarmTableEntry removedEntry = super.removeOldest();
		indexWithReduction.remove(removedEntry.getAlarmId());
		return removedEntry;
	}
	
	/**
	 * Replace the alarm in a row with passed one.
	 * <P>
	 * The entry to replace the alarm is given by the alarm ID of the parameter.
	 * 
	 * @param newAlarm The not null new alarm 
	 * @throws AlarmContainerException if the entry is not in the container
	 */
	public synchronized void replace(AlarmTableEntry newAlarm) throws AlarmContainerException {
		super.replace(newAlarm);
		int pos=indexWithReduction.indexOf(newAlarm.getAlarmId());
		if (pos>=0) {
			String key = indexWithReduction.remove(pos);
			indexWithReduction.insertElementAt(key, 0);
			if (newAlarm.getStatus().isActive()) {
				hideReducedChildren(newAlarm);
			} else {
				showActiveChildren(newAlarm,pos);
			}	
		} else {
			addAlarm(newAlarm);
		}
		
	}
	
	/**
	 * Show the active children of the passed alarms
	 * 
	 * @param alarm The alarm whose active children must be displayed
	 * @param pos The position in the table where the active children must be shown
	 */
	private void showActiveChildren(AlarmTableEntry alarm, int pos) {
		if (categoryClient==null) {
			return;
		}
		// If the alarm is not active then the active children must be
		// visible
		Alarm[] als=null;
		try {
			if (alarm.isMultiplicityParent()){
				als= categoryClient.getActiveChildren(alarm.getAlarmId(), false);
			} else if (alarm.isNodeParent()) {
				als= categoryClient.getActiveChildren(alarm.getAlarmId(), true);
			}
		} catch (Throwable t) {
			System.err.println("Error getting children of "+alarm.getAlarmId()+": "+t.getMessage());
			t.printStackTrace();
			als=null;
		}
		if (als!=null) {
			for (Alarm al: als) {
				AlarmTableEntry newEntry = get(al.getAlarmId());
				if (newEntry!=null) {
					if (indexWithReduction.indexOf(al.getAlarmId())<0) {
						indexWithReduction.add(al.getAlarmId());
					}
				}
			}
		}
	}

	/**
	 * @return the categoryClient
	 */
	public CategoryClient getCategoryClient() {
		return categoryClient;
	}
	
	/**
	 * Check if the container has alarm not yet acknowledged.
	 * <P>
	 * if there are active alarms to be acknowledged by the user, 
	 * this method returns the highest of their priorities.
	 * Note that for alarm system the highest priority is 0.
	 * 
	 * @return  -1 if there are not alarm to acknowledge;
	 * 			the highest priority of the alarm to acknowledge
	 * @see AlarmContainer#hasNotAckAlarms()
	 */
	public synchronized int hasNotAckAlarms(boolean reduced) {
		if (!reduced) {
			return super.hasNotAckAlarms();
		}
		int ret=Integer.MAX_VALUE;
		for (String id: indexWithReduction) {
			AlarmTableEntry entry = get(id);
			if (entry.getStatus().isActive() && entry.isNew() && entry.getPriority()<ret) {
				ret=entry.getPriority();
			}
			if (ret==0) {
				break;
			}
		}
		return (ret==Integer.MAX_VALUE)?-1:ret;
	}
}
