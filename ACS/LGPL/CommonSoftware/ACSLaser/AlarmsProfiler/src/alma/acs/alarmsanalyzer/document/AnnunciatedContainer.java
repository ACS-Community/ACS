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

import alma.acs.alarmsanalyzer.document.SuppressedContainer.ReductionValue;
import alma.acs.alarmsanalyzer.engine.AlarmCategoryListener;
import alma.acs.alarmsanalyzer.save.TableData;

/**
 * The container for the ACTIVE annunciated (not reduced) alarms.
 * 
 * @author acaproni
 *
 */
public class AnnunciatedContainer extends DocumentBase implements AlarmCategoryListener{
	
	/**
	 * Annunciated alarms
	 */
	private final ConcurrentHashMap<String, ReductionValue> annunciated = new ConcurrentHashMap<String, ReductionValue>();
	
	/**
	 * The singleton
	 */
	private static AnnunciatedContainer singleton=null;
	
	public static AnnunciatedContainer getInstance() {
		if (singleton==null) {
			singleton = new AnnunciatedContainer();
		}
		return singleton;
	}
	
	/**
	 * Constructor
	 */
	private AnnunciatedContainer() {
		super("Annunciated alarms",
				new String[] {
				"Entry",
				"Value"
		});
	}
	
	@Override
	public Collection<?> getNumbers() {
		return annunciated.values();
	}

	@Override
	public void onAlarm(Alarm alarm) {
		if (alarm.getStatus().isActive() && !alarm.getStatus().isReduced()) {
			ReductionValue val = annunciated.get(alarm.getAlarmId());
			if (val==null) {
				val=new ReductionValue(alarm.getAlarmId());
				annunciated.put(alarm.getAlarmId(), val);
			} else {
				val.inc();
			}
		}
	}

	@Override
	public void setTableContent(TableData tData) {
		Vector<ReductionValue>vals = new Vector<SuppressedContainer.ReductionValue>(annunciated.values());
		Collections.sort(vals);
		for (ReductionValue val: vals) {
			String[] row = new String[2];
			row[0]="="+val.ID+"=";
			row[1]=Integer.valueOf(val.getValue()).toString();
			tData.addRowData(row);
		}
	}
}
