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

import java.util.Collections;
import java.util.List;
import java.util.Vector;

import javax.swing.SwingWorker;

import alma.acs.gui.util.threadsupport.EDTExecutor;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.clients.alarm.AlarmClientException;
import cern.laser.client.data.Alarm;

/**
 * Extends <code>AlarmsContainer</code> to cope with reduced alarms.
 * <P>
 * Methods of this class ensure that the model is changed from inside the swing EDT.
 * <BR>Methods that queries the model instead are not forced to be executed inside the EDT and
 * immediately return the requested value 
 * (i.e. the caller must ensure to query from inside the EDT).
 * 
 * <P>
 * Synchronization is done by thread confinement (inside the EDT).
 * Invocation of read only methods must be done inside the EDT.
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
	private final List<String> indexWithReduction = Collections.synchronizedList(new Vector<String>());
	
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
	public int size(boolean reduced) {
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
	public void add(final AlarmTableEntry entry) throws AlarmContainerException {
		super.add(entry);
		addAlarm(entry);
	}
	
	/**
	 * Add an alarm to the reduction container
	 * 
	 * @param alarm The alarm to add to the container
	 */
	private void addAlarm(final AlarmTableEntry alarm) {
		if (!alarm.getStatus().isReduced()) {
			EDTExecutor.instance().execute(new Runnable() {
				@Override
				public void run() {
					indexWithReduction.add(alarm.getAlarmId());
				}
			});
		}
		hideReducedChildren(alarm);
	}
	
	/**
	 * Hide the active alarms of this entry.
	 * 
	 * @param entry The not <code>null</code> entry to hide active children
	 */
	private void hideReducedChildren(final AlarmTableEntry entry) {
		if (entry==null) {
			throw new IllegalArgumentException("The entry can't be null");
		}
		if (categoryClient==null) {
			return;
		}
		
		new SwingWorker<Alarm[], Object>() {
			@Override
			protected Alarm[] doInBackground() throws Exception {
				// slow task
				return getReducedChildren(entry.getAlarmId(),entry.isNodeParent());
			}

			@Override
			protected void done() {
				// Update the model
				Alarm[] children=null;
				try {
					children=get();
				} catch (Throwable t) {
					System.out.println("Error getting the children of "+entry);
					t.printStackTrace();
					return;
				}
				if (children!=null) {
					for (Alarm al: children) {
						indexWithReduction.remove(al.getAlarmId());
					}
				}
				super.done();
			}
			
		}.execute();
	}
	
	/**
	 * Get the children of a reduction of the alarm with the passed ID.
	 * <P>
	 * This method asks the {@link CategoryClient} for the children of alarm i.e.
	 * it implies a CORBA call and must not be run into the swing EDT.
	 * 
	 * @param alarmID The not <code>null</code> and not empty ID of the alarm
	 * @param nodeReduction <code>true</code> if it is a node reduction
	 * @return The array of children of the alarm reduced by the alarm with the passed ID
	 * @throws AlarmClientException in case of error from the {@link CategoryClient} 
	 */
	private Alarm[] getReducedChildren(final String alarmID, final boolean nodeReduction) 
	throws AlarmClientException {
		if (alarmID==null || alarmID.isEmpty()) {
			throw new IllegalArgumentException("Invalid null or empty alarm ID");
		}
		return categoryClient.getChildren(alarmID, nodeReduction);
	}
	
	/**
	 * Set the <code>CategoryClient</code>
	 * 
	 * @param client The <code>CategoryCLient</code>; it can be <code>null</code>.
	 */
	public synchronized void setCategoryClient(CategoryClient client) {
		this.categoryClient=client;
	}
	
	/**
	 * Return the entry in the given position
	 * 
	 * @param pos The position of the alarm in the container
	 * @param reduced <code>true</code> if the alarms in the table are reduced
	 * @return The <code>AlarmTableEntry<code> in the given position
	 */
	public AlarmTableEntry get(int pos, boolean reduced) {
		if (!reduced) {
			return super.get(pos);
		} 
		String ID = indexWithReduction.get(pos);
		AlarmTableEntry ret = get(ID);
		if (ret==null) {
			throw new IllegalStateException("Inconsistent state of reduced container pos="+pos+", size="+indexWithReduction.size());
		}
		return ret;
	}
	
	/**
	 * Remove all the elements in the container
	 */
	public void clear() {
		EDTExecutor.instance().execute(new Runnable() {
			@Override
			public void run() {
				indexWithReduction.clear();
			}
		});
		super.clear();
	}

	/**
	 * Remove the entry for the passed alarm
	 * 
	 * @param alarm The alarm whose entry must be removed
	 * @throws AlarmContainerException If the alarm is not in the container
	 */
	public void remove(final AlarmTableEntry alarm) throws AlarmContainerException {
		if (alarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		EDTExecutor.instance().execute(new Runnable() {
			@Override
			public void run() {
				int pos = indexWithReduction.indexOf(alarm.getAlarmId());
				if (pos>=0) {
					indexWithReduction.remove(pos);
				}
			}
		});
		super.remove(alarm);
	}
	
	/**
	 * Remove the oldest entry in the container
	 * 
	 * @return The removed item
	 * @throws AlarmContainerException If the container is empty
	 */
	public AlarmTableEntry removeOldest() throws AlarmContainerException {
		final AlarmTableEntry removedEntry = super.removeOldest();
		if (removedEntry==null) {
			throw new IllegalStateException("The oldest alarm can't be null!");
		}
		try {
			EDTExecutor.instance().executeSync(new Runnable() {
				@Override
				public void run() {
					indexWithReduction.remove(removedEntry.getAlarmId());
				}
			});
		} catch (Throwable t) {
			throw new AlarmContainerException("Error removing oldest alarm from the alarm ("+removedEntry.getAlarmId()+") reduction container",t);
		}
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
	public void replace(AlarmTableEntry newAlarm) throws AlarmContainerException {
		super.replace(newAlarm);
		int pos=indexWithReduction.indexOf(newAlarm.getAlarmId());
		if (pos>=0) {
			String key = indexWithReduction.remove(pos);
			indexWithReduction.add(0,key);
			if (newAlarm.getStatus().isActive()) {
				hideReducedChildren(newAlarm);
			} else {
				showActiveChildren(newAlarm,pos);
			}	
		} else {
			// This should never happen but we don't want to lose alarms
			addAlarm(newAlarm);
		}
	}
	
	/**
	 * Show the active children of the passed alarms
	 * 
	 * @param alarm The alarm whose active children must be displayed
	 * @param pos The position in the table where the active children must be shown
	 */
	private void showActiveChildren(final AlarmTableEntry alarm, int pos) {
		if (categoryClient==null) {
			return;
		}
		
		new SwingWorker<Alarm[], Object>() {

			@Override
			protected Alarm[] doInBackground() throws Exception {
				Alarm[] ret= getActiveChildren(alarm.getAlarmId(), alarm.isNodeParent());
				return ret;
			}
			

			@Override
			protected void done() {
				Alarm[] als=null;
				try {
					als=get();
				} catch (Throwable t) {
					System.err.println("Error getting the active children list"+t.getMessage());
					t.printStackTrace();
					return;
				}
				if (als!=null) {
					for (Alarm al: als) {
						AlarmTableEntry newEntry = AlarmsReductionContainer.this.get(al.getAlarmId());
						if (newEntry!=null) {
							if (indexWithReduction.indexOf(al.getAlarmId())<0) {
								indexWithReduction.add(al.getAlarmId());
							}
						}
					}
				}
				super.done();
			}
			
		}.execute();
	}
	
	/**
	 * Get the list of active children of a reduction of the alarm with the passed ID.
	 * <P>
	 * This method asks the {@link CategoryClient} for the children of alarm i.e.
	 * it implies a CORBA call and must not be run into the swing EDT.
	 * 
	 * @param alarmID The not <code>null</code> and not empty ID of the alarm
	 * @param nodeReduction <code>true</code> if it is a node reduction
	 * @return The array of children of the alarm reduced by the alarm with the passed ID
	 * @throws AlarmClientException in case of error from the {@link CategoryClient} 
	 */
	private Alarm[] getActiveChildren(final String alarmID, final boolean nodeReduction) 
	throws AlarmClientException {
		if (alarmID==null || alarmID.isEmpty()) {
			throw new IllegalArgumentException("Invalid null or empty alarm ID");
		}
		return categoryClient.getActiveChildren(alarmID, nodeReduction);
	}

	/**
	 * @return the categoryClient
	 */
	public synchronized CategoryClient getCategoryClient() {
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
	public int hasNotAckAlarms(boolean reduced) throws AlarmContainerException {
		if (!reduced) {
			return super.hasNotAckAlarms();
		}
		int ret=Integer.MAX_VALUE;
		synchronized (indexWithReduction) {
			for (String id: indexWithReduction) {
				AlarmTableEntry entry = get(id);
				if (entry.getStatus().isActive() && entry.isNew() && entry.getPriority()<ret) {
					ret=entry.getPriority();
				}
				if (ret==0) {
					break;
				}
			}	
		}
		return (ret==Integer.MAX_VALUE)?-1:ret;
	}
}
