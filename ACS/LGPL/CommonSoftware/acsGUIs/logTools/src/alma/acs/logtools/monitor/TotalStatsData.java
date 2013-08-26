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
package alma.acs.logtools.monitor;

import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * A class to ensure mutual exclusion while updating/reading values
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public class TotalStatsData {
	/**
	 * The total number of logs received  since the begging of the execution
	 * <BR>
	 * One entry for each log type.
	 */
	protected final long[] totalLogs = new long[LogTypeHelper.values().length];
	
	/**
	 * Total number of logs (of any type) received since the beginning
	 * of the execution
	 */
	protected volatile long numOfLogs=0;
	
	/**
	 * The distribution of each log type since the begging of the execution
	 * <BR>
	 * One entry for each log type.
	 */
	protected final float[] logsTypeDistribution= new float[LogTypeHelper.values().length];
	
	/**
	 * The longest XML log received since the begging of the execution
	 */
	protected volatile int longestLogSize=0;
	
	/**
	 * The shortest XML log received since the begging of the execution
	 */
	protected volatile int shortestLogSize=0;
	
	/**
	 * Number of errors parsing logs since the begging of the execution
	 */
	protected volatile int errors=0;
	
	/**
	 * Update the sizes
	 * 
	 * @param min Shortest size
	 * @param max Longest size
	 */
	public synchronized void updateSizes(int min,int max) {
		if (min>=0 && min!=Integer.MAX_VALUE) {
			shortestLogSize=min;
		}
		if (max>=0) {
			longestLogSize=max;
		}
	}
	
	/**
	 * Update the total num of errors
	 * 
	 * @param errs Total num. of errors
	 */
	public synchronized void updateErrors(int errs) {
		this.errors=errs;
	}
	
	/**
	 * Update all the total numbers
	 * 
	 * @param data Total numbers
	 */
	public synchronized void updateAll(TotalStatsData data) {
		updateErrors(data.getErrors());
		updateSizes(data.getShortestLogSize(), data.getLongestLogSize());
		updateTotalLogs(data.getTotalLogs());
	}
	
	/**
	 * Update the total number of logs, the distribution and so on
	 */
	public synchronized void updateTotalLogs(long[] nums) {
		numOfLogs=0;
		for (int t=0; t<nums.length; t++) {
			totalLogs[t]=nums[t];
			numOfLogs+=nums[t];
		}
		for (int t=0; t<nums.length; t++) {
			logsTypeDistribution[t]=(float)nums[t]/(float)numOfLogs;
		}
	}

	/**
	 * Getter
	 */
	public synchronized long[] getTotalLogs() {
		return totalLogs;
	}

	/**
	 * Getter
	 */
	public synchronized long getNumOfLogs() {
		return numOfLogs;
	}

	/**
	 * Getter
	 */
	public synchronized float[] getLogsTypeDistribution() {
		return logsTypeDistribution;
	}


	/**
	 * Getter
	 */
	public synchronized int getLongestLogSize() {
		return longestLogSize;
	}

	/**
	 * Getter
	 */
	public synchronized int getShortestLogSize() {
		return shortestLogSize;
	}

	/**
	 * Getter
	 */
	public synchronized int getErrors() {
		return errors;
	}
}
