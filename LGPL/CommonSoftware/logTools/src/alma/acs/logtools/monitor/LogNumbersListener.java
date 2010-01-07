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
 * The listener of the number of logs received.
 * <P>
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public interface LogNumbersListener {
	
	/**
	 * Notify the listener about the number of each log type received in 
	 * {@link LogDetailsDispatcher#NUMBER_LISTENERS_INTERVAL}
	 * seconds.
	 * <P>
	 * The array contains one integer for each log type as defined
	 * in {@link LogTypeHelper}.
	 * <P>
	 * This callback is executed approximately every 
	 * <code>NUMBER_LISTENERS_INTERVAL</code> seconds.
	 * 
	 * @param nums The number of logs
	 * @param msec The interval of time (seconds)
	 */
	public void recvLogs(int[] nums, int secs);
	
	/**
	 * Notify the listener about the total number of logs 
	 * read since the application started.
	 * 
	 * @param d The total number of logs read since the application started
	 */
	public void totalData(TotalStatsData d);
}
