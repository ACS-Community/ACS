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

import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;

import cern.laser.client.data.Alarm;

import alma.acs.alarmsanalyzer.engine.AlarmCategoryListener;

/**
 * The container for the suppressed (reduced) and annunciated (not reduced) alarms.
 * <P>
 * This container needs to be extend to reimplement getNumbers()
 * in order to return the suppressed or the annunciated alarms
 * 
 * @author acaproni
 *
 */
public abstract class SupAnnCommonContainer extends DocumentBase implements AlarmCategoryListener {
	
	/**
	 * The item to send to the table
	 * 
	 * @author acaproni
	 *
	 */
	public class ReductionValue {
		/**
		 * The ID of the alarm
		 */
		public final String ID;
		
		private int value=1;
		
		/**
		 * Canstructor 
		 * 
		 * @param id The ID of the alarm
		 */
		public ReductionValue(String id) {
			ID=id;
		}
		
		/**
		 * Increment the counter
		 */
		public void inc() {
			value++;
		}

		/**
		 * Getter
		 */
		public int getValue() {
			return value;
		}
	}
	
	/**
	 * Suppressed alarms
	 */
	protected final ConcurrentHashMap<String, ReductionValue> suppressed = new ConcurrentHashMap<String, SupAnnCommonContainer.ReductionValue>();
	
	/**
	 * Suppressed alarms
	 */
	protected final ConcurrentHashMap<String, ReductionValue> annunciated = new ConcurrentHashMap<String, SupAnnCommonContainer.ReductionValue>();
	
	
	
	/**
	 * Canstructor
	 */
	protected SupAnnCommonContainer() {	}
	
	@Override
	public void onAlarm(Alarm alarm) {
		if (alarm.getStatus().isReduced()) {
			update(alarm.getAlarmId(),suppressed);
		} else {
			update(alarm.getAlarmId(),annunciated);
		}
	}
	
	/**
	 * Update the count of the alarm in the passed map
	 * 
	 * @param ID The id of the alarm
	 * @param map The map
	 */
	private void update(String ID, ConcurrentHashMap<String, ReductionValue> map) {
		ReductionValue val = map.get(ID);
		if (val==null) {
			val=new ReductionValue(ID);
			map.put(ID, val);
		} else {
			val.inc();
		}
	}
	
	/**
	 * 
	 * @return The collection of annunciated alarms
	 */
	public Collection<?> getAnnunciated() {
		return annunciated.values();
	}
	
	/**
	 * 
	 * @return The collection of annunciated alarms
	 */
	public Collection<?> getSuppressed() {
		return suppressed.values();
	}
}
