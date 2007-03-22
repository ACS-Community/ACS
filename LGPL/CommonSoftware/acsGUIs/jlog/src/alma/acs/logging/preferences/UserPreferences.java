/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.preferences;

/**
 * The preferences 
 * 
 * @author acaproni
 *
 */
public class UserPreferences {
	
	/**
	 * A class containing the options for the time
	 * Each option is a couple <label, value> where
	 *   - value is the value of the option
	 *   - label is a label to show to the user for that option
	 */ 
	public static class TimeOption {
		private String label;
		private int value;
		
		/**
		 * Constructor
		 * 
		 * @param lbl The label to show in the combo box
		 * @param val The number of minutes
		 */
		public TimeOption(String lbl, int val) {
			label=lbl;
			value=val;
		}
		
		public String toString() {
			return label;
		}
		
		/**
		 * @return The number of minutes of the timeframe
		 *
		 */
		public int getTimeFrame() {
			return value;
		}
		
		public boolean equals(int min) {
			return value==min;
		}
	}
	
	/**
	 * A class containing the options for the number of logs
	 * Each option is a couple <label, value> where
	 *   - value is the value of the option
	 *   - label is a label to show to the user for that option
	 */ 
	public static class NumberOption {
		private String label;
		private int value;
		
		/**
		 * Constructor 
		 * 
		 * @param lbl The label to show
		 * @param val The number of logs
		 */
		public NumberOption(String lbl, int val) {
			label=lbl;
			value=val;
		}
		
		public String toString() {
			return label;
		}
		
		public int getNumOfLogs() {
			return value;
		}
		
		public boolean equals(int val) {
			return value==val;
		}
	}
	
	/**
	 * The possible option for the time frame
	 */
	public final static TimeOption[] timeOptions = new TimeOption[] {
		new TimeOption("Unlimited",0),
		new TimeOption("1h",60),
		new TimeOption("3h",180), // Default
		new TimeOption("5h",300),
		new TimeOption("12h",720),
		new TimeOption("1d",1440)
	};
	
	/**
	 * The possible option for the max number of logs
	 */
	public final static NumberOption[] maxLogNumOptions = new NumberOption[] {
		new NumberOption("Unlimited",0),
		new NumberOption("100K",100000), // Default
		new NumberOption("200K",200000), 
		new NumberOption("300K",300000),
		new NumberOption("400K",400000)
	};
	
	// The time frame (in minutes)
	private int timeFrame;
	
	// The max number of logs
	private int maxNumOfLogs;
	
	/**
	 * Empty constructor
	 * 
	 * All the values are set to the default value
	 *
	 */
	public UserPreferences() {
		timeFrame = timeOptions[0].getTimeFrame();
		maxNumOfLogs = maxLogNumOptions[1].getNumOfLogs();
	}
	
	/**
	 * Builds an object with the given values
	 * 
	 * @param time The time frame (in minutes)
	 * @param maxLogs The max number of logs
	 */
	public UserPreferences(int time, int maxLogs) {
		setTimeFrame(time);
		setMaxLogs(maxLogs);
	}
	
	/**
	 * Set the time frame to the given value
	 * (it checks if the value is valid i.e. it is in the timeOptions)
	 * 
	 * @param time The new time frame (in minutes)
	 * @throws IllegalArgumentException If the time frame is invalid
	 */
	public void setTimeFrame(int time) throws IllegalArgumentException {
		for (int t=0; t<timeOptions.length; t++) {
			if (timeOptions[t].equals(time)) {
				timeFrame=time;
				return;
			}
		}
		throw new IllegalArgumentException("Invalid time frame: "+time);
	}
	
	/**
	 * Set the max number of logs
	 * (it checks if the value is valid i.e. it is in the numberOptions)
	 * 
	 * @param maxLogs The max number of logs 
	 * @throws IllegalArgumentException If the number of logs is illegal
	 */
	public void setMaxLogs(int maxLogs) throws IllegalArgumentException {
		for (int t=0; t<maxLogNumOptions.length; t++) {
			if (maxLogNumOptions[t].equals(maxLogs)) {
				maxNumOfLogs=maxLogs;
				return;
			}
		}
		throw new IllegalArgumentException("Invalid number of logs: "+maxLogs);
	}
	
	/**
	 * @return The max number of logs
	 *
	 */
	public int getMaxNumOfLogs() {
		return maxNumOfLogs;
	}
	
	/**
	 * 
	 * @return The time frame length in minutes
	 */
	public int getMinuteTimeFrame() {
		return timeFrame;
	}
	
	/**
	 * 
	 * @return The length of the time frame in milliseconds
	 */
	public long getMillisecondsTimeFrame() {
		return timeFrame*60*1000; 
	}
		
}
