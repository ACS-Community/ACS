/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.alarmsystem.clients.alarm;

import java.util.Collection;
import java.util.Collections;
import java.util.Vector;

import alma.alarmsystem.clients.alarm.AlarmStatistics.AlarmStatField;
import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;

/**
 * The container of alarm category listenera.
 * <BR>
 * {@link AlarmListenersContainer} owns 2 lists of listeners: one for the alarms 
 * and another one for the statistics.
 * 
 * <P>When finished using this object, {@link #close()} must be called
 * to free the resources
 * 
 * @author acaproni
 * @since ACS-12.2
 */
public class AlarmListenersContainer {

	/**
	 * Objects of {@link AlarmListener} are immutable and composed 
	 * of a listener and a filter based on the triplet.
	 * <P>
	 * If the filter is <code>null</code> then no filtering is applied.
	 * 
	 * @author acaproni
	 * @since ACS-12.2
	 */
	public final class AlarmListener {
		/**
		 * The listener of alarms and exceptions
		 */
		public final AlarmSelectionListener listener;
		
		/**
		 * The filtering based on the triplets.
		 */
		public final AlarmFilter filter;
		
		/**
		 * Constructor
		 * 
		 * @param listener The not <code>null</code> listener of alarms and exceptions
		 * @param filter The filter for the alarms (if <code>null</code> no filters is applied)
		 */
		public AlarmListener(AlarmSelectionListener listener, AlarmFilter filter) {
			if (listener==null) {
				throw new IllegalArgumentException("The listener can't be null");
			}
			this.listener=listener;
			this.filter=filter;
		}
	}
	
	/**
	 * Signal that the object has been closed
	 */
	private volatile boolean closed=false;
	
	/**
	 * The container of alarms and exceptions listeners
	 */
	private final Collection<AlarmListener> alarmListeners = Collections.synchronizedCollection(new Vector<AlarmListener>());
	
	/**
	 * The container of astatistics listeners
	 */
	private final Collection<AlarmCategoryStatListener> statListeners = Collections.synchronizedCollection(new Vector<AlarmCategoryStatListener>());
	
	/**
	 * Add a listener of alarms
	 * 
	 * @param listener The listener of alarms and exceptions
	 * @return <code>true</code> if this collection changed as a result of the call
	 */
	public boolean addAlarmListener(AlarmListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		if (closed) {
			return false;
		}
		return alarmListeners.add(listener);
	}
	
	/**
	 * Add a new lister to be notified for alarms and exceptions
	 * 
	 * @param listener The not <code>null</code> listener of alarms and exceptions
	 * @param filter The filter for the alarms (if <code>null</code> no filters is applied)
	 * @return The {@link AlarmListener} stored in the container
	 *         or <code>null</code> if the lister has not been added to the container
	 */
	public AlarmListener addAlarmListener(AlarmSelectionListener listener, AlarmFilter filter) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		AlarmListener ret = new AlarmListener(listener,filter);
		if (this.addAlarmListener(ret)) {
			return ret;
		} else {
			return null;
		}
	}
	
	/**
	 * Add a new unfiltered lister to be notified for alarms and exceptions
	 * 
	 * @param listener The not <code>null</code> listener of alarms and exceptions
	 * @return The {@link AlarmListener} stored in the container
	 *         or <code>null</code> if the lister has not been added to the container
	 */
	public AlarmListener addAlarmListener(AlarmSelectionListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		return this.addAlarmListener(listener, null);
	}
	
	/**
	 * Remove a lister from the container
	 * 
	 * @param listenerToRemove The not <code>null</code> listener to remove from the container
	 * @return <code>true</cod>true if an element was removed as a result of this call
	 */
	public boolean removeAlarmListener(AlarmListener listenerToRemove) {
		if (listenerToRemove==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		return alarmListeners.remove(listenerToRemove);
	}
	
	/**
	 * Close the object an free the resources
	 */
	public void close() {
		closed=true;
		alarmListeners.clear();
		statListeners.clear();
	}
	
	/**
	 * Deliver the alarm to the listeners of the container
	 * whose filter matches with the triplet of the alarm. 
	 * 
	 * @param alarm The not <code>null</code> alarm to forward to the listeners
	 */
	public void dispatchAlarm(Alarm alarm) {
		if (alarm==null) {
			throw new IllegalArgumentException("A null alarm can't be delivered to listeners");
		}
		synchronized (alarmListeners) {
			for (AlarmListener l: alarmListeners) {
				if (l.filter==null) {
					// no filtering
					l.listener.onAlarm(alarm);
				} else {
					String ff=alarm.getTriplet().getFaultFamily();
					String fm=alarm.getTriplet().getFaultMember();
					Integer fc=alarm.getTriplet().getFaultCode();
					if (l.filter.matches(ff, fm, fc)) {
						l.listener.onAlarm(alarm);
					}
				}
				if (closed) {
					return;
				}
			}
		}
	}
	
	/**
	 * Deliver the exception to the listeners of the container
	 * whose filter matches with the triplet of the alarm. 
	 * 
	 * @param exception The not <code>null</code> exception to forward to the listeners
	 */
	public void dispatchException(LaserSelectionException exception) {
		if (exception==null) {
			throw new IllegalArgumentException("A null exception can't be delivered to listeners");
		}
		synchronized (alarmListeners) {
			for (AlarmListener l: alarmListeners) {
				l.listener.onException(exception);
				if (closed) {
					return;
				}
			}
		}
	}
	
	/**
	 * Add a listener to be notified about statistics
	 * 
	 * @param listener The not <code>null</code> listener to be notified
	 * @return <code>true</code> if this collection changed as a result of the call
	 */
	public boolean  addStatsListener(AlarmCategoryStatListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		if (closed) {
			return false;
		}
		return statListeners.add(listener);
	}
	
	/**
	 * Remove a lister of statistics from the container
	 * 
	 * @param listenerToRemove The not <code>null</code> listener to remove from the container
	 * @return <code>true</cod>true if an element was removed as a result of this call
	 */
	public boolean removeStatListener(AlarmCategoryStatListener listenerToRemove) {
		if (listenerToRemove==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		return statListeners.remove(listenerToRemove);
	}
	
	/**
	 * Deliver the statistics to the listeners.
	 * 
	 * @param exception The not <code>null</code> exception to forward to the listeners
	 */
	public void dispatchStatistics(AlarmStatistics stats) {
		if (stats==null) {
			throw new IllegalArgumentException("A null AlarmStatistics can't be delivered to listeners");
		}
		synchronized (statListeners) {
			for (AlarmCategoryStatListener l: statListeners) {
				l.activationAlarmsStats(
						stats.getStatValue(AlarmStatField.ACTIVE), 
						stats.getStatValue(AlarmStatField.PRI1), 
						stats.getStatValue(AlarmStatField.PRI2), 
						stats.getStatValue(AlarmStatField.PRI3), 
						stats.getStatValue(AlarmStatField.PRI1));
				l.reductionAlarmsStat(
						stats.getStatValue(AlarmStatField.REDUCED), 
						stats.getStatValue(AlarmStatField.MASKED), 
						stats.getStatValue(AlarmStatField.MULTIPLICITY_PARENT), 
						stats.getStatValue(AlarmStatField.NODE_PARENT), 
						stats.getStatValue(AlarmStatField.MULTIPLICITY_CHILD), 
						stats.getStatValue(AlarmStatField.NODE_CHILD));
				if (closed) {
					return;
				}
			}	
		}
	}
}
