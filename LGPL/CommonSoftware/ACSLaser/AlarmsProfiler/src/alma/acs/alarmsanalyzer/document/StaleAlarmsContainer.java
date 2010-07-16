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
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.jface.viewers.TableViewer;

import alma.acs.alarmsanalyzer.engine.AlarmUtils;
import alma.alarmsystem.clients.source.SourceListener;
import cern.laser.source.alarmsysteminterface.FaultState;

/**
 * A class for stale alarms i.e. alarms activated and never deactivated.
 * <P>
 * Active alarms are stored in a map. When a <code>TERMINATE</code> alarms arrive and the alarm is in the map, it is removed.
 * If an alarm already present in the map is activated again, it is not modified because we are interested in the longest
 * time interval.
 * 
 * @author acaproni
 *
 */
public class StaleAlarmsContainer extends DocumentBase implements SourceListener {
	
	/**
	 * A stale alarm
	 * 
	 * @author acaproni
	 *
	 */
	public class StaleAlarm {
		/**
		 * The ID of the alarm
		 */
		public final String ID;
		
		/**
		 * The time when the alarm has been activated
		 */
		public final Timestamp activationTime;
		
		/**
		 * Constructor
		 * 
		 * @param id The ID of the alarm 
		 * @param time The time when the alarm has been activated
		 */
		public StaleAlarm(String id, Timestamp time) {
			this.ID=id;
			this.activationTime=time;
		}
	}
	
	/**
	 * The singleton
	 */
	private static StaleAlarmsContainer singleton=null;
	
	public static StaleAlarmsContainer getInstance() {
		if (singleton==null) {
			singleton = new StaleAlarmsContainer();
		}
		return singleton;
	}
	
	/**
	 * Constructor
	 */
	private StaleAlarmsContainer() {}
	
	/**
	 * The stale alarms in staleAlarms are active alarms.
	 * <P>
	 * When a <code>TERMINATE</code> alarms is received, and an active alarm is in this list then it must be removed.
	 * <P>
	 * The key is the ID of the alarm.
	 */
	private final ConcurrentHashMap<String, StaleAlarm> staleAlarms = new ConcurrentHashMap<String, StaleAlarm>();
	
	@Override
	public void faultStateReceived(FaultState faultState) {
		if (faultState==null || shutdown) {
			return;
		}
		String ID=AlarmUtils.getID(faultState);
		if (faultState.getDescriptor().equals(FaultState.ACTIVE)) {
			if (staleAlarms.containsKey(ID)) {
				return;
			}
			StaleAlarm alarm = new StaleAlarm(ID, faultState.getUserTimestamp());
			staleAlarms.put(ID, alarm);
		} else if (faultState.getDescriptor().equals(FaultState.TERMINATE)) {
			if (!staleAlarms.containsKey(ID)) {
				return;
			}
			staleAlarms.remove(ID);
		} else {
			System.out.println("Unknown "+ID+"state "+faultState.getDescriptor());
		}
	}

	@Override
	public void sourceXMLMsgReceived(String asiMessage) {}
	
	/**
	 * 
	 * @return All the alarms in the container
	 */
	public Collection<StaleAlarm> getNumbers() {
		return staleAlarms.values();
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
	
}
