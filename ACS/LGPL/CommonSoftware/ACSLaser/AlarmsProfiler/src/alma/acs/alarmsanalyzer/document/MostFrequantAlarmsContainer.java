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
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import cern.laser.source.alarmsysteminterface.FaultState;
import alma.acs.alarmsanalyzer.engine.AlarmUtils;
import alma.acs.alarmsanalyzer.save.TableData;
import alma.alarmsystem.clients.source.SourceListener;

/**
 * The container of the Most frequent alarms. It is a singleton
 * <P>
 * For the purpose of this tool, it is enough to save the triplets
 * with additional informations but there is no need to save the entire
 * {@link FaultState}.
 * 
 * @author acaproni
 *
 */
public class MostFrequantAlarmsContainer extends DocumentBase implements SourceListener {
	
	/**
	 * An object storing statistics for each alarm.
	 * <P>
	 * The {@link Comparable} define the ordering depending on the 
	 * number of activations. 
	 * 
	 * @author acaproni
	 *
	 */
	public class AlarmActNumber implements Comparable<AlarmActNumber>{

		/**
		 * The ID of this alarm
		 */
		private final String alarmID;
		
		/**
		 * The number of ACTIVATE alarm
		 */
		private long numActivation=0;
		
		/**
		 * The date of the last activation of the alarm
		 */
		private Timestamp lastActivationTime=null;
		
		/**
		 * The number of TERMINATE alarm
		 */
		private long numTermination=0;
		
		/**
		 * The date of the last activation of the alarm
		 */
		private Timestamp lastTerminationTime=null;
		
		/**
		 * Canstructor
		 * 
		 * @param id The ID of the alarm
		 * @param active The status of the alarm
		 * @param time The time of the last recevied alarm
		 */
		public AlarmActNumber(String id, boolean active, Timestamp time) {
			this.alarmID=id;
			update(active,time);
		}
		
		/**
		 * Update the numbers of activation/deactivation of this alarm
		 * 
		 * @param active If <code>true</code> the alarm was active
		 * @param time The time of the last recevied alarm
		 */
		public void update(boolean active, Timestamp time) {
			if (active) {
				numActivation++;
				lastActivationTime=time;
			} else {
				numTermination++;
				lastTerminationTime=time;
			}
		}

		/**
		 * Getter
		 */
		public long getNumActivation() {
			return numActivation;
		}

		/**
		 * Getter
		 */
		public Timestamp getLastActivationTime() {
			return lastActivationTime;
		}

		/**
		 * Getter
		 */
		public long getNumTermination() {
			return numTermination;
		}

		/**
		 * Getter
		 */
		public Timestamp getLastTerminationTime() {
			return lastTerminationTime;
		}
		
		/**
		 * Getter
		 */
		public String getAlarmID() {
			return alarmID;
		}
		
		@Override
		public int compareTo(AlarmActNumber o) {
			if (o==null) {
				throw new NullPointerException();
			}
			return Long.valueOf(numActivation).compareTo(o.getNumActivation());
		}
	}
	
	/**
	 * The most frequent alarms i.e. the number of times each alarm has been
	 * activated.
	 * <P>
	 * The key is the ID if the alarm, the value is the number of times the alarm has 
	 * been activated.
	 */
	private final ConcurrentHashMap<String, AlarmActNumber> mostFrequentAlarms = new ConcurrentHashMap<String, AlarmActNumber>();
	
	/**
	 * The singleton
	 */
	private static MostFrequantAlarmsContainer singleton=null;
	
	public static MostFrequantAlarmsContainer getInstance() {
		if (singleton==null) {
			singleton = new MostFrequantAlarmsContainer();
		}
		return singleton;
	}
	
	/**
	 * Constructor
	 */
	private MostFrequantAlarmsContainer() {
		super("Most frequent alarms",
				new String[] {
				"Alarm ID",
				"# ACTIVE",
				"Activation time",
				"# TERMINATE",
				"Termination time"
		});
	}
	
	/**
	 * An alarm has been received from the source NC.
	 * 
	 * @param alarm The alarm received from a source
	 */
	@Override
	public void faultStateReceived(FaultState faultState) {
		if (faultState==null || shutdown) {
			return;
		}
		String ID=AlarmUtils.getID(faultState);
		
		AlarmActNumber numbers=mostFrequentAlarms.get(ID);
		boolean active=faultState.getDescriptor().equals(FaultState.ACTIVE);
		Timestamp timestamp=faultState.getUserTimestamp();
		if (numbers!=null) {
			numbers.update(active, timestamp);
		} else {
			numbers=new AlarmActNumber(ID,active, timestamp);
			mostFrequentAlarms.put(ID, numbers);
		}
	}

	/**
	 * @see SourceListener
	 */
	@Override
	public void sourceXMLMsgReceived(String asiMessage) {
	}
	
	/**
	 * 
	 * @return All the alarms in the container
	 */
	public Collection<AlarmActNumber> getNumbers() {
		return mostFrequentAlarms.values();
	}
	
	@Override
	public void setTableContent(TableData tData) {
		Collection<AlarmActNumber> vals = mostFrequentAlarms.values();
		Vector<AlarmActNumber> items = new Vector<MostFrequantAlarmsContainer.AlarmActNumber>(vals);
		Collections.sort(items);
		for (AlarmActNumber val: items) {
			if (val==null) {
				continue;
			}
			String[] row = new String[5];
			row[0]="="+val.getAlarmID()+"=";
			row[1]=Long.valueOf(val.getNumActivation()).toString();
			row[2]=(val.getLastActivationTime()!=null)?val.getLastActivationTime().toString():"";
			row[3]=Long.valueOf(val.getNumTermination()).toString();
			row[4]=(val.getLastTerminationTime()!=null)?val.getLastTerminationTime().toString():"";
			tData.addRowData(row);
		}
	}
}
