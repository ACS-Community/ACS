/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

package alma.alarmsystem.statistics;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;

/**
 * A class to calculate various statistics 
 * on the alarms processed by the alarm server.
 * <P>
 * The statistics are
 * <UL>
 * 	<LI>logged at definite time intervals to avoid flooding the logging systems
 * 	<LI>saved on a file
 * <P>
 * <code>StatsCalculator</code> logs a minimal set of statistics on the logging system
 * but the full statistics are saved on the file.
 * <P>
 * Statistics are logged every time interval whose default is {@value #defaultTimeInterval} minutes.
 * The time interval can be customized by setting the {@value #TimeIntervalProperty} java property.
 * A time interval of 0 minutes disable the logging of statistics.
 * <P>
 * Life cycle: 
 * {@link #start()} must be called to start gathering statistics and {@link #shutdown()} must be
 * executed when finished.
 * 
 * @author  acaproni
 * @since   ACS 2015.2
 */
public class StatsCalculator implements Runnable {

	/**
	 * The name of the property to customize the time interval (in minutes) to log
	 * statistics.
	 * 
	 */
	private static final String TimeIntervalProperty="alma.acs.alarmsystem.statistics.timeinterval";
	
	/**
	 * The default time interval is {@value #defaultTimeInterval} minutes
	 */
	private static final int defaultTimeInterval=10;
	
	/**
	 * Statistics are logged every time interval
	 */
	public static final int timeInterval=Integer.getInteger(TimeIntervalProperty, defaultTimeInterval);
	
	/**
	 * The executor to schedule the writing of statistics at every time interval.
	 */
	private final ScheduledExecutorService executor = Executors.newScheduledThreadPool(1);
	
	/**
	 * The number of activations received in the last time interval.
	 * <P>
	 * These are the active alarms received from sources.
	 * 
	 */
	private final AtomicLong alarmsActivationForTimeInterval = new AtomicLong();
	
	/**
	 * The number of terminations received in the last time interval.
	 * <P>
	 * These are the terminate alarms received from sources.
	 */
	private final AtomicLong alarmsTerminationForTimeInterval = new AtomicLong();
	
	/**
	 * The logger
	 */
	private final Logger logger;
	
	/**
	 * Constructor
	 * 
	 * @param logger The logger
	 */
	public StatsCalculator(Logger logger) {
		if (logger==null) {
			throw new IllegalArgumentException("The logger can't be null!");
		}
		this.logger=logger;
	}
	
	/**
	 * An alarm with the passed ID and activation state 
	 * has been received from a source.
	 * 
	 * @param alarmID The ID of the received alarm
	 * @param status The status of the alarm (<code>true</code> means active)
	 */
	public void alarmReceivedFromSource(String alarmID,boolean status) {
		if (status) {
			alarmsActivationForTimeInterval.incrementAndGet();
		} else {
			alarmsTerminationForTimeInterval.incrementAndGet();
		}
	}
	
	/**
	 * Start to gather statistics and spawn the time task to blish them.
	 */
	public void start() {
		// Start the scheduled task
		if (timeInterval>0) {
			executor.scheduleWithFixedDelay(this, timeInterval, timeInterval, TimeUnit.MINUTES);
			Object[] params={"Time interval",Integer.valueOf(timeInterval)};
			logger.log(AcsLogLevel.INFO,"Gathering of statistics enabled",params);
		} else {
			logger.log(AcsLogLevel.INFO,"Gathering of statistics disabled (time interval<=0)");
		}
		
	}
	
	/**
	 * This method must be called when no more statistics must be collected and 
	 * published.
	 * It stops the timer task and frees all the resources.
	 */
	public void shutdown() {
		// Stop the timer task
		if (timeInterval>0) {
			logger.log(AcsLogLevel.INFO,"Shutting down stats calucaltion");
			executor.shutdown();
		}
	}
			
	
	/**
	 * The scheduler invokes this method at fixed time intervals to generate and publish statistics. 
	 * It also reset the variable to calculate the correct statistics at the next iteration.
	 */
	@Override
	public void run() {
	}
}
