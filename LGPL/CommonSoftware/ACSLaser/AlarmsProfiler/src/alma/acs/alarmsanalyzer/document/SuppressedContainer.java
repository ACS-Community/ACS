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
import java.util.Collections;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

import cern.laser.client.data.Alarm;

import alma.acs.alarmsanalyzer.engine.AlarmCategoryListener;
import alma.acs.alarmsanalyzer.save.TableData;

/**
 * The container for the ACTIVE suppressed (reduced) alarms.
 * 
 * @author acaproni
 *
 */
public class SuppressedContainer extends DocumentBase implements AlarmCategoryListener{
	/**
	 * The item to send to the table.
	 * <P>
	 * The same type is used by the {@link AnnunciatedContainer}
	 * <P>
	 * Items are ordered by the number of events
	 * 
	 * @author acaproni
	 *
	 */
	public static class ReductionValue implements Comparable<ReductionValue> {
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

		@Override
		public int compareTo(ReductionValue o) {
			if (o==null) {
				throw new NullPointerException();
			}
			return Integer.valueOf(value).compareTo(Integer.valueOf(o.getValue()));
		}
	}
	/**
	 * The singleton
	 */
	private static SuppressedContainer singleton=null;
	
	public static SuppressedContainer getInstance() {
		if (singleton==null) {
			singleton = new SuppressedContainer();
		}
		return singleton;
	}
	
	/**
	 * Suppressed alarms
	 */
	private final ConcurrentHashMap<String, ReductionValue> suppressed = new ConcurrentHashMap<String, ReductionValue>();
	
	/**
	 * Constructor
	 */
	private SuppressedContainer() {
		super("Reduced alarms",
				new String[] {
				"Entry",
				"Value"
		});
	}
	
	@Override
	public Collection<?> getNumbers() {
		return suppressed.values();
	}

	@Override
	public void onAlarm(Alarm alarm) {
		if (alarm.getStatus().isActive() && alarm.getStatus().isReduced()) {
			ReductionValue val = suppressed.get(alarm.getAlarmId());
			if (val==null) {
				val=new ReductionValue(alarm.getAlarmId());
				suppressed.put(alarm.getAlarmId(), val);
			} else {
				val.inc();
			}
		}
	}
	
	@Override
	public void setTableContent(TableData tData) {
		Vector<ReductionValue>vals = new Vector<SuppressedContainer.ReductionValue>(suppressed.values());
		Collections.sort(vals);
		for (ReductionValue val: vals) {
			String[] row = new String[2];
			row[0]="="+val.ID+"=";
			row[1]=Integer.valueOf(val.getValue()).toString();
			tData.addRowData(row);
		}
	}
}
