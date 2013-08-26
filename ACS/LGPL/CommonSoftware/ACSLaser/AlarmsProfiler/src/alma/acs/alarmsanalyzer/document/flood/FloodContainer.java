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
package alma.acs.alarmsanalyzer.document.flood;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Vector;

import org.eclipse.jface.viewers.TableViewer;

import cern.laser.client.data.Alarm;

import alma.acs.alarmsanalyzer.document.DocumentBase;
import alma.acs.alarmsanalyzer.document.SuppressedContainer.ReductionValue;
import alma.acs.alarmsanalyzer.engine.AlarmCategoryListener;
import alma.acs.alarmsanalyzer.save.TableData;

/**
 * Count the number of floods and generate the statistics
 * 
 * @author acaproni
 */
public class FloodContainer extends DocumentBase implements AlarmCategoryListener {
	
	public enum FloodItem {
		NUM_OF_FLOODS("Num. of floods",false,false),
		ACTUALLY_IN_FLOOD("Actually in flood", false,true),
		TOT_ALARMS("Tot. alarms in floods",false,false),
		HIGHEST_ALARMS("Highest num. of alarms in flood",false,false),
		AVG_ALARMS("Avg. alarms per flood",false,false),
		MEASURED_TIME("Monitoring time",true,false),
		FLOOD_TIME("Time of Alarm service in flood",true,false);
		
		/**
		 *  The name show in the first column
		 */
		public String description;
		
		/**
		 * <code>true</code> if number represents a time value
		 */
		private final boolean isTime;
		
		/**
		 * <code>true</code> if number represents a boolean value (0 means <code>false</code>,
		 * all other values mean <code>true</code>)
		 */
		private final boolean isBoolean;
		
		/**
		 * The value
		 */
		private Number value;
		
		/**
		 * The formatter of the times
		 */
		private static final SimpleDateFormat timeFormat = new SimpleDateFormat("HH:mm:ss.S");
		
		/**
		 * Constructor
		 * 
		 * @param name The description of the value
		 */
		private FloodItem(String name, boolean isTm, boolean isBool) {
			this.description=name;
			this.isTime=isTm;
			this.isBoolean=isBool;
			value=Integer.valueOf(0);
		}
		
		public void setNumber(Number newValue) {
			value=newValue;
		}
		
		/**
		 * Getter
		 */
		public Number getValue() {
			return value;
		}
		
		@Override
		public String toString() {
			if (isBoolean) {
				int val = value.intValue();
				if (val==0) {
					return "No";
				} else {
					return "Yes";
				}
			}
			if (isTime) {
				Date date = new Date(value.longValue());
				Calendar cal = Calendar.getInstance();
				cal.setTime(date);
				int day = cal.get(Calendar.DAY_OF_MONTH)-1;
				synchronized (timeFormat) {
					if (day>0) {
						return ""+day+" days, "+timeFormat.format(date);
					} else {
						return timeFormat.format(date);
					}
				}
			}
			if ((value instanceof Float) || (value instanceof Double)) {
				double d = value.doubleValue();
				return String.format("%.2f", d);
			}
			return value.toString();
		}
	};
	
	/**
	 * The singleton
	 */
	private static FloodContainer singleton=null;
	
	public static FloodContainer getInstance() {
		if (singleton==null) {
			singleton = new FloodContainer();
		}
		return singleton;
	}
	
	/**
	 * Constructor
	 */
	private FloodContainer() {
		super("Alarm floods statistics",
				new String[] {
				"Entry",
				"Value"
		});
		actualFlood=new AlarmFlood(this);
	}

	/**
	 * The floods registered since the system started
	 */
	private final Vector<AlarmFlood> floods = new Vector<AlarmFlood>();
	
	/**
	 * The start time of this container in mesec
	 */
	private final long startTime=System.currentTimeMillis();
	
	/**
	 * The actual counter of alarm floods
	 */
	private AlarmFlood actualFlood;
	
	/**
	 * 
	 * @return The number of alarms registered in all the floods
	 */
	public synchronized int getTotAlarmsInFloods() {
		int ret=0;
		for (AlarmFlood af: floods) {
			ret+=af.getAlarmsInFlood();
		}
		ret+=actualFlood.getAlarmsInFlood();
		return ret;
	}
	
	/**
	 * 
	 * @return The total time the alarm system was in flood in msec
	 */
	public synchronized long getTotTimeInFloods() {
		long ret=0;
		for (AlarmFlood af: floods) {
			ret+=af.lengthOfFlood();
		}
		if (actualFlood.getStartTimeOfFlood()>0) {
			ret+=actualFlood.lengthOfFlood();
		}
		return ret;
	}
	
	/**
	 * 
	 * @return The average number of alarms registered in all the floods
	 */
	public synchronized float getAvgAlarmsInFloods() {
		if (floods.size()==0) {
			return 0;
		}
		float ret=0;
		for (AlarmFlood af: floods) {
			ret+=af.getAlarmsInFlood();
		}
		return ret/(float)floods.size();
	}
	
	/**
	 * 
	 * @return The highest number of alarms registered between all the floods
	 */
	public synchronized int getHighestAlarmsCountInFloods() {
		int ret=0;
		for (AlarmFlood af: floods) {
			if (ret<af.getAlarmsInFlood()) {
				ret=af.getAlarmsInFlood();
			}
		}
		return ret;
	}
	
	/**
	 * @return The total number of floods
	 */
	public synchronized int getNumOfFloods() {
		return floods.size();
	}
	
	/**
	 * @return The values of the flood to be shown in the table
	 */
	@Override
	public synchronized Collection<FloodItem> getNumbers() {
		FloodItem.AVG_ALARMS.setNumber(Float.valueOf(getAvgAlarmsInFloods()));
		FloodItem.FLOOD_TIME.setNumber(Long.valueOf(getTotTimeInFloods()));
		FloodItem.HIGHEST_ALARMS.setNumber(Integer.valueOf(getHighestAlarmsCountInFloods()));
		FloodItem.MEASURED_TIME.setNumber(Long.valueOf(System.currentTimeMillis()-startTime));
		FloodItem.NUM_OF_FLOODS.setNumber(Integer.valueOf(getNumOfFloods()));
		FloodItem.TOT_ALARMS.setNumber(Integer.valueOf(getTotAlarmsInFloods()));
		FloodItem.ACTUALLY_IN_FLOOD.setNumber(Integer.valueOf(actualFlood.isFloodStarted()?1:0));
		Vector<FloodContainer.FloodItem> ret= new Vector<FloodContainer.FloodItem>(FloodItem.values().length);
		for (FloodItem fi: FloodItem.values()) {
			ret.add(fi);
		}
		return ret;
	}

	@Override
	public synchronized void shutdownContainer() {
		super.shutdownContainer();
		actualFlood.stop();
	}

	@Override
	public void setTableViewer(TableViewer table) {
		super.setTableViewer(table);
	}
	
	public synchronized void doneFlood() {
		floods.add(actualFlood);
		actualFlood=new AlarmFlood(this);
		System.out.println("Refreshing table");
		System.out.println("doneFlood done");
	}

	@Override
	public synchronized void onAlarm(Alarm alarm) {
		actualFlood.onAlarm(alarm);
	}
	
	@Override
	public void setTableContent(TableData tData) {
		Collection<FloodItem> vals=getNumbers();
		for (FloodItem val: vals) {
			String[] row = new String[2];
			row[0]=val.description;
			row[1]=val.toString();
			tData.addRowData(row);
		}
	}
}
