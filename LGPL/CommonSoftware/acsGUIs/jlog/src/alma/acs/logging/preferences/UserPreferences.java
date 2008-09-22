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
public class UserPreferences implements Cloneable {
	
	/** 
	 * The time frame (in minutes)
	 */
	private int timeFrame=0;
	
	/** 
	 * The max number of logs
	 */
	private int maxNumOfLogs=0;
	
	/**
	 * Max number of logs per second read from the NC.
	 * 
	 * @see ACSLogRetrieval}
	 */
	private int maxInputRate=Integer.MAX_VALUE;
	
	/**
	 * Max number of logs per second that the engine sends to the table.
	 * 
	 * @see ACSLogRetrieval}
	 */
	private int maxOutputRate=Integer.MAX_VALUE;
	
	/**
	 * The threshold to activate dynamic discard level
	 */
	private int dynThreshold=Integer.MAX_VALUE;
	
	/**
	 * The damping factor for dynamic discard level
	 */
	private int dynDamping=0;
	
	/**
	 * The time (seconds) for dynamic discard level
	 */
	private int dynTime=1;
	
	/**
	 * Builds an object with the given values
	 * 
	 * @param time The time frame (in minutes)
	 * @param maxLogs The max number of logs
	 */
	public UserPreferences(int time, int maxLogs, int inRate, int outRate) {
		setTimeFrame(time);
		setMaxLogs(maxLogs);
		setMaxInputRate(inRate);
		setMaxOutputRate(outRate);
	}
	
	/**
	 * Set the time frame to the given value
	 * 
	 * @param time The new time frame (in minutes)
	 */
	public void setTimeFrame(int time) {
		timeFrame=time;
	}
	
	/**
	 * Set the max number of logs
	 * 
	 * @param maxLogs The max number of logs 
	 */
	public void setMaxLogs(int maxLogs) throws IllegalArgumentException {
		maxNumOfLogs=maxLogs;
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

	public int getMaxInputRate() {
		return maxInputRate;
	}

	public void setMaxInputRate(int maxInputRate) {
		this.maxInputRate = maxInputRate;
	}

	public int getMaxOutputRate() {
		return maxOutputRate;
	}

	public void setMaxOutputRate(int maxOutputRate) {
		this.maxOutputRate = maxOutputRate;
	}
	
	/**
	 * Return a copy of this object
	 */
	public UserPreferences clone() throws CloneNotSupportedException {
		return (UserPreferences)super.clone();
	}

	public int getDynThreshold() {
		return dynThreshold;
	}

	public void setDynThreshold(int dynThreshold) {
		this.dynThreshold = dynThreshold;
	}

	public int getDynDamping() {
		return dynDamping;
	}

	public void setDynDamping(int dynDamping) {
		this.dynDamping = dynDamping;
	}

	public int getDynTime() {
		return dynTime;
	}

	public void setDynTime(int dynTime) {
		this.dynTime = dynTime;
	}

	public int getTimeFrame() {
		return timeFrame;
	}

	public void setMaxNumOfLogs(int maxNumOfLogs) {
		this.maxNumOfLogs = maxNumOfLogs;
	}	
}
