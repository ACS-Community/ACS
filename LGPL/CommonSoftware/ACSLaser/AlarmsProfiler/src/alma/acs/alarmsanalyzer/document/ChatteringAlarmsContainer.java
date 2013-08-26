/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
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
package alma.acs.alarmsanalyzer.document;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.jface.viewers.TableViewer;

import alma.acs.alarmsanalyzer.engine.AlarmUtils;
import alma.acs.alarmsanalyzer.save.TableData;
import alma.alarmsystem.clients.source.SourceListener;

import cern.laser.source.alarmsysteminterface.FaultState;

/**
 * The container of chattering alarms i.e. alarms whose state changes at least 3 times per minute.
 * <P>
 * Each chattering alarms is an entry of the table. If the same alarm at a certain minute chatters
 * more then it happened previously, its number and dates are updated to show the peak.
 * <P>
 * While counting for chattering alarms, this containers remember the last registered state because if
 * an alarm is terminated three times but never activated this is not a chatter.
 * <P>
 * The number of activation/termination can instead be investigated in the MFA table.
 * <P>
 * This container has one list for the ChatteringAlrms (<code>chatteringAlarms</code>) to be sent to the table model, 
 * and another one (<code>tempAlarms</code>) where received alarms are updated every minute.
 * At the end of the time interval, this new list is flushed in the the list for the view.
 * 
 * @author acaproni
 *
 */
public class ChatteringAlarmsContainer extends DocumentBase implements SourceListener, Runnable {
	
	/**
	 * A chattering alarm.
	 * <P>
	 * {@link Comparable} orders items by number of activations.
	 * 
	 * @author acaproni
	 *
	 */
	public class ChatteringAlarm implements Comparable<ChatteringAlarm> {
		/**
		 * The ID of the alarm
		 */
		public final String ID;
		
		/**
		 * The number of ACTIVE alarms received in a minute
		 */
		protected int numActive=0;
		
		/**
		 * The number of Terminate alarms received in a minute
		 */
		protected int numTerminate=0;
		
		/**
		 * The time when the chattering event has been registered
		 */
		protected Timestamp timestamp;
		
		/**
		 * Build a ChatteringAlarm with no alarms.
		 * 
		 * @param id The ID of the alarm
		 * @param time The time when the chattering has been registered
		 */
		protected ChatteringAlarm(String id, Timestamp time) {
			this.ID=id;
			numActive=numTerminate=0;
			timestamp=time;
		}
		
		/**
		 * Constructor
		 * 
		 * @param id The ID of the alarm
		 * @param nAct Number of active alarms in a minute
		 * @param nTerm Number of terminate alarms in a minute
		 * @param time The time when the chattering has been registered
		 */
		public ChatteringAlarm(String id, int nAct, int nTerm, Timestamp time) {
			this.ID=id;
			numActive=nAct;
			numTerminate=nTerm;
			timestamp=time;
		}

		/**
		 * Getter 
		 */
		public int getNumActive() {
			return numActive;
		}

		/**
		 * Getter 
		 */
		public int getNumTerminate() {
			return numTerminate;
		}
		
		/**
		 * @return The total of alarms (active/inactive) received for this alarm 
		 */
		public int getTotAlarms() {
			return numActive+numTerminate;
		}
		
		/**
		 * Update the number of this alarm but only if
		 * the new ones are worst then previously registered.
		 * 
		 * @param nAct Number of active alarms in a minute
		 * @param nTerm Number of terminate alarms in a minute
		 * @param time The time when the chattering has been registered
		 */
		public void update(int nAct, int nTerm, Timestamp time) {
			int tot=nAct+nTerm;
			if (tot>getTotAlarms()) {
				numActive=nAct;
				numTerminate=nTerm;
				timestamp=time;
			}
		}
		
		/**
		 * Getter 
		 */
		public Timestamp getTimestamp() {
			return timestamp;
		}

		@Override
		public int compareTo(ChatteringAlarm o) {
			if (o==null) {
				throw new NullPointerException();
			}
			return Integer.valueOf(numActive).compareTo(o.getNumActive());
		}
	}
	
	/**
	 * The type of alarm recorded  every minute.
	 * the only difference between AlarmCounter and {@link ChatteringAlarm} is that we need
	 * to remember the last activation state in this context.
	 * 
	 * @author acaproni
	 *
	 */
	private class AlarmCounter extends ChatteringAlarm {
		/**
		 * Record the last received state for updating
		 */
		private String lastRecordedState;
		
		/**
		 * 
		 * @param id The ID of the alarm
		 * @param time The time when the chattering has been registered
		 * @param activation The state ACTIVE/Terminate of the last received FaultState
		 */
		public AlarmCounter(String id, Timestamp time, String state) {
			super(id,time);
			lastRecordedState="";
			updateCounter(state);
		}

		/**
		 * Update the counters of the chattering alarms
		 * 
		 * @param state The state ACTIVE/TERMINATE
		 */
		public void updateCounter(String state) {
			if (lastRecordedState.equals(state)) {
				return;
			}
			if (state.equals(FaultState.ACTIVE)) {
				numActive++;
				lastRecordedState=state;
			} else if (state.equals(FaultState.TERMINATE)) {
				numTerminate++;
				lastRecordedState=state;
			} else {
				System.out.println("Unknown state "+state);
			}
		}
	}

	/**
	 * The chattering alarms
	 */
	private ConcurrentHashMap<String, ChatteringAlarm> chatteringAlarms=new ConcurrentHashMap<String, ChatteringAlarmsContainer.ChatteringAlarm>();
	
	/**
	 * The alarms received every minute
	 */
	private ConcurrentHashMap<String, AlarmCounter> tempAlarms = new ConcurrentHashMap<String, ChatteringAlarmsContainer.AlarmCounter>();
	
	/**
	 * The number of changes of state to accept an alarm as chattering
	 */
	private static final int STATECHANGES=3;
	
	/**
	 * The singleton
	 */
	private static ChatteringAlarmsContainer singleton=null;
	
	public static ChatteringAlarmsContainer getInstance() {
		if (singleton==null) {
			singleton = new ChatteringAlarmsContainer();
		}
		return singleton;
	}
	
	/**
	 * Constructor
	 */
	private ChatteringAlarmsContainer() {
		super("Chattering alarms",
				new String[] {
				"Alarm ID",
				"# ACTIVE",
				"# TERMINATE",
				"# state changes",
				"Peak time"
		});
	}
	
	/**
	 * Ovveride to start the thread to refresh the vie
	 */
	@Override
	public void setTableViewer(TableViewer table) {
		super.setTableViewer(table);
		Thread t = new Thread(this,this.getClass().getName());
		t.setDaemon(true);
		t.start();
	}
	
	@Override
	public synchronized Collection<?> getNumbers() {
		return chatteringAlarms.values();
	}

	@Override
	public void run() {
		while (!shutdown) {
			for (int count=0; count<60 && !shutdown; count++) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e) {
					continue;
				}
			}
			if (shutdown) {
				return;
			}
			synchronized (this) {
				removeNoChattertingAlarms();
				flush();
				tempAlarms.clear();
			}
		}
	}

	/**
	 * Each received {@link FaultState} is stored in <code>tempAlarms</code>.
	 */
	@Override
	public synchronized void faultStateReceived(FaultState faultState) {
		String id=AlarmUtils.getID(faultState);
		// Alarm already in the temporary container
		if (tempAlarms.containsKey(id)) {
			AlarmCounter alarm= tempAlarms.get(id);
			alarm.updateCounter(faultState.getDescriptor());
			return;
		}
		// Add an entry in the temporary container
		AlarmCounter alarm = new AlarmCounter(id, faultState.getUserTimestamp(), faultState.getDescriptor());
		tempAlarms.put(id, alarm);
	}
	
	/**
	 * Remove form the <code>tempAlarms</code> all the alarms that did not chatter
	 * i.e. whose number of changes is less then <code>STATECHANGES</code>.
	 */
	private void removeNoChattertingAlarms() {
		Set<String> keys=tempAlarms.keySet();
		for (String key: keys) {
			AlarmCounter alarm=tempAlarms.get(key);
			if (alarm.getTotAlarms()<STATECHANGES) {
				tempAlarms.remove(key);
			}
		}
	}
	
	/**
	 * Flush the chattering alarms in the temporary list within the 
	 * list to send to the table, <code>chatteringAlarms</code>.
	 */
	private void flush() {
		Set<String> keys=tempAlarms.keySet();
		for (String key: keys) {
			AlarmCounter alarm=tempAlarms.get(key);
			ChatteringAlarm chattering = chatteringAlarms.get(key);
			if (chattering==null) {
				// new entry
				chatteringAlarms.put(key, alarm);
			} else {
				// Update an existing entry
				chattering.update(alarm.numActive, alarm.numTerminate, alarm.timestamp);
			}
		}
	}

	@Override
	public void sourceXMLMsgReceived(String asiMessage) {}
	
	@Override
	public void setTableContent(TableData tData) {
		Vector<ChatteringAlarm> vals = new Vector<ChatteringAlarm>(chatteringAlarms.values());
		Collections.sort(vals);
		for (ChatteringAlarm val: vals) {
			String[] row = new String[5];
			row[0]="="+val.ID+"=";
			row[1]=Integer.valueOf(val.getNumActive()).toString();
			row[2]=Integer.valueOf(val.getNumTerminate()).toString();
			row[3]=Integer.valueOf(val.getTotAlarms()).toString();
			row[4]=val.getTimestamp().toString();
			tData.addRowData(row);
		}
	}
	
}
