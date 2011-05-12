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
import cern.laser.source.alarmsysteminterface.FaultState;

import alma.acs.alarmsanalyzer.engine.AlarmCategoryListener;
import alma.acs.alarmsanalyzer.save.TableData;
import alma.alarmsystem.clients.source.SourceListener;

/**
 * A collection of numbers for checking the quality of the alarm system
 * 
 * @author acaproni
 *
 */
public class StatisticsContainer extends DocumentBase implements SourceListener, AlarmCategoryListener {
	
	/**
	 * The items shown in the table
	 * 
	 * @author acaproni
	 *
	 */
	public class AlarmStat {
		
		/**
		 * The name show in the table
		 */
		public final String name;
		
		/**
		 * The value
		 */
		private int absolute=0;
		
		/**
		 * Constructor
		 * 
		 * @param txt The name
		 * @param type The type
		 */
		public AlarmStat(String txt) {
			name=txt;
		}

		/**
		 * 
		 * @return The value
		 */
		public int getValue() {
			return absolute;
		}
		
		/**
		 * Increment the absolute value
		 */
		public void inc() {
			absolute++;
		}
		
	}
	
	/**
	 * ID for the total number of source alarms 
	 */
	private final String totSrcAlarms="Source events";
	
	/**
	 * ID for the total number of ACTIVE source alarms 
	 */
	private final String totActiveSrcAlarms="ACTIVE source events";
	
	/**
	 * ID for the total number of TERMINATE source alarms 
	 */
	private final String totTerminateSrcAlarms="TERMINATE source events";
	
	/**
	 * ID for the total number of source alarms 
	 */
	private final String totCatAlarms="Annunciated alarms";
	
	/**
	 * ID for the total number of ACTIVE category alarms 
	 */
	private final String totActiveCatAlas="ACTIVE alarms";
	
	/**
	 * ID for the total number of TERMINATE category alarms 
	 */
	private final String totTerminateCatAlas="TERMINATE alarms";
	
	/**
	 * ID for the percentage of priority 0 events
	 */
	private final String pri0="Priority 0 (VERY HIGH)";
	
	/**
	 * ID for the percentage of priority 1 events
	 */
	private final String pri1="Priority 1 (HIGH)";
	
	/**
	 * ID for the percentage of priority 2 events
	 */
	private final String pri2="Priority 2 (Medium)";
	
	/**
	 * ID for the percentage of priority 3 events
	 */
	private final String pri3="Priority 3 (Low)";
	
	/**
	 * The number of suppressed (i.e. reduced) alarms
	 */
	private final String suppressedAlarms="Suppressed alarms";
	
	/**
	 * The number of NOT suppressed (i.e. reduced) alarms
	 */
	private final String annunciatedAlarms="Annunciated alarms";
	/**
	 * The map to store values into
	 */
	private final ConcurrentHashMap< String, AlarmStat> values= new ConcurrentHashMap<String, StatisticsContainer.AlarmStat>();
	
	/**
	 * The singleton
	 */
	private static StatisticsContainer singleton=null;
	
	public static StatisticsContainer getInstance() {
		if (singleton==null) {
			singleton = new StatisticsContainer();
		}
		return singleton;
	}
	
	private StatisticsContainer() {
		super("Statistics",
				new String[] {
				"Entry",
				"Value"
		});
		// Populate the map
		values.put(totSrcAlarms, new AlarmStat(totSrcAlarms));
		values.put(totActiveSrcAlarms, new AlarmStat(totActiveSrcAlarms));
		values.put(totTerminateSrcAlarms, new AlarmStat(totTerminateSrcAlarms));
		values.put(totCatAlarms, new AlarmStat(totCatAlarms));
		values.put(totActiveCatAlas, new AlarmStat(totActiveCatAlas));
		values.put(totTerminateCatAlas, new AlarmStat(totTerminateCatAlas));
		values.put(pri0, new AlarmStat(pri0));
		values.put(pri1, new AlarmStat(pri1));
		values.put(pri2, new AlarmStat(pri2));
		values.put(pri3, new AlarmStat(pri3));
		values.put(suppressedAlarms, new AlarmStat(suppressedAlarms));
		values.put(annunciatedAlarms, new AlarmStat(annunciatedAlarms));
		
	}

	@Override
	public Collection<?> getNumbers() {
		return values.values();
	}

	@Override
	public void onAlarm(Alarm alarm) {
		values.get(totCatAlarms).inc();
		if (alarm.getStatus().isActive()) {
			values.get(totActiveCatAlas).inc();
		} else {
			values.get(totTerminateCatAlas).inc();
		}
		switch (alarm.getPriority().intValue()) {
		case 0: values.get(pri0).inc(); break;
		case 1: values.get(pri1).inc(); break;
		case 2: values.get(pri2).inc(); break;
		case 3: values.get(pri3).inc(); break;
		}
		if (alarm.getStatus().isReduced()) {
			values.get(suppressedAlarms).inc();
		} else {
			values.get(annunciatedAlarms).inc();
		}
	}

	@Override
	public void faultStateReceived(FaultState faultState) {
		values.get(totSrcAlarms).inc();
		if (faultState.getDescriptor().equals(FaultState.ACTIVE)) {
			values.get(totActiveSrcAlarms).inc();
		}
		if (faultState.getDescriptor().equals(FaultState.TERMINATE)) {
			values.get(totTerminateSrcAlarms).inc();
		}
	}

	@Override
	public void sourceXMLMsgReceived(String asiMessage) {}
	
	@Override
	public void setTableContent(TableData tData) {
		String[] row = new String[2];
		row[0]="Source alarms";
		row[1]="";
		tData.addRowData(row);
		
		AlarmStat stat = values.get(totSrcAlarms);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		stat = values.get(totActiveSrcAlarms);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		stat = values.get(totTerminateSrcAlarms);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		row = new String[2];
		row[0]="Category alarms";
		row[1]="";
		tData.addRowData(row);
		
		stat = values.get(totCatAlarms);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		stat = values.get(totActiveCatAlas);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		stat = values.get(totTerminateCatAlas);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		stat = values.get(pri0);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		stat = values.get(pri1);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		stat = values.get(pri2);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		stat = values.get(pri3);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);

		stat = values.get(suppressedAlarms);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
		
		stat = values.get(annunciatedAlarms);
		row = new String[2];
		row[0]=stat.name;
		row[1]=Integer.valueOf(stat.getValue()).toString();
		tData.addRowData(row);
	}

}
