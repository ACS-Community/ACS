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

import java.util.Collection;
import java.util.Vector;

import org.eclipse.jface.viewers.TableViewer;

import cern.laser.client.data.Alarm;

import alma.acs.alarmsanalyzer.document.DocumentBase;
import alma.acs.alarmsanalyzer.engine.AlarmCategoryListener;

/**
 * Count the number of floods and generate the statistics
 * 
 * @author acaproni
 */
public class FloodContainer extends DocumentBase implements AlarmCategoryListener {
	
	public enum FloodItem {
		NUM_OF_FLOODS("Num. of floods"),
		TOT_ALARMS("Tot. alarms in floods"),
		HIGHEST_ALARMS("Highest num. of alarms in flood"),
		AVG_ALARMS("Avg. alarms per flood"),
		MEASURED_TIME("Alarm service time"),
		FLOOD_TIME("Time of Alarm service in flood");
		
		/**
		 *  The name show in the first column
		 */
		public String description;
		
		/**
		 * The value
		 */
		private Number value;
		
		/**
		 * Constructor
		 * 
		 * @param name The description of the value
		 */
		private FloodItem(String name) {
			this.description=name;
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
		return ret;
	}
	
	/**
	 * 
	 * @return The average number of alarms registered in all the floods
	 */
	public synchronized float getAvgAlarmsInFloods() {
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
		Vector<FloodContainer.FloodItem> ret= new Vector<FloodContainer.FloodItem>();
		ret.copyInto(FloodItem.values());
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
		refresh();
	}

	@Override
	public synchronized void onAlarm(Alarm alarm) {
		actualFlood.onAlarm(alarm);
	}
}
