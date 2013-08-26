/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.logging;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigSubscriber;

/**
 * Process level throttle for logs.
 * @author hsommer
 * @since ACS 9.0 (see http://jira.alma.cl/browse/COMP-4541)
 */
class LogThrottle 
{
	/**
	 * Separate throttle for local (stdout) logs
	 */
	private final LogStreamThrottle localLogThrottle;

	/**
	 * Separate throttle for remote (Log service) logs
	 */
	private final LogStreamThrottle remoteLogThrottle;

	private final ThrottleCallback throttleCallback;

	private final AtomicBoolean localLogsSuppressed = new AtomicBoolean(false);
	private final AtomicBoolean remoteLogsSuppressed = new AtomicBoolean(false);
	
	/**
	 * Constructor that takes the LogConfig object to read the throttle setting.
	 * @param logConfig
	 */
	LogThrottle(LogConfig logConfig, ThrottleCallback throttleCallback) {
		localLogThrottle = new LogStreamThrottle(logConfig);
		remoteLogThrottle = new LogStreamThrottle(logConfig);
		this.throttleCallback = throttleCallback;
	}

	/**
	 * @return true if the log should pass, false if it should be suppressed due to throttling.
	 */
	boolean checkPublishLogRecordLocal() {
		boolean thisLogSuppressed = !localLogThrottle.checkPublishLogRecord();
		boolean lastLocalLogSuppressed = localLogsSuppressed.getAndSet(thisLogSuppressed);

		// callbacks
		if (thisLogSuppressed) {
			throttleCallback.suppressedLog(false);
		} 
		else if (lastLocalLogSuppressed && !remoteLogsSuppressed.get()) {
			// this local log (and also remote logs) are allowed, but previous local log failed
			throttleCallback.clearedLogSuppression();
		}
		
		return !thisLogSuppressed;
	}
	
	
	/**
	 * @return true if the log should pass, false if it should be suppressed due to throttling.
	 */
	boolean checkPublishLogRecordRemote() {
		boolean thisLogSuppressed = !remoteLogThrottle.checkPublishLogRecord();
		boolean lastRemoteLogSuppressed = remoteLogsSuppressed.getAndSet(thisLogSuppressed);
		
		// callbacks
		if (thisLogSuppressed) {
			throttleCallback.suppressedLog(true);
		} 
		else if (lastRemoteLogSuppressed && !localLogsSuppressed.get()) {
			// this remote log (and also local logs) are allowed, but previous remote log failed
			throttleCallback.clearedLogSuppression();
		}
		
		return !thisLogSuppressed;
	}
	

	/**
	 * The actual throttle functionality is kept in this class, which is the same
	 * for the local and remote logging streams.
	 */
	private static class LogStreamThrottle implements LogConfigSubscriber {
		/**
		 * The configured maximum logs per time interval. 
		 * For the time being we use this single value for both local and remote logging.
		 */
		private int maxLogsPerInterval;

		/**
		 * Hardcoded to 1000 ms, thus matching the CDB configuration with attribute maxLogsPerSecond"
		 */
		private final long intervalLengthMillis = 1000;
		
		/**
		 * Time in milliseconds when the current time interval for checking the number of logs has started.
		 */
		private long intervalBeginMillis;

		/**
		 * Number of logs since {@link #intervalBeginMillis}.
		 */
		private final AtomicInteger logCounter = new AtomicInteger(0);
		
		/**
		 * Constructor that takes the LogConfig object, from which it gets {@link #maxLogsPerInterval},
		 * also for updates in the future.
		 * @param logConfig
		 */
		public LogStreamThrottle(LogConfig logConfig) {
			intervalBeginMillis = System.currentTimeMillis();
			maxLogsPerInterval = -1;
			
			if (logConfig != null) {
				configureLogging(logConfig);
				logConfig.addSubscriber(this); // passing "this" should only be done when this object is fully constructed.
			} else {
				throw new NullPointerException("LogConfig must not be null");
			}
		}
		
		@Override
		public void configureLogging(LogConfig logConfig) {
			maxLogsPerInterval  = logConfig.getMaxLogsPerSecond();
		}

		
		/**
		 * Checks whether the log throttle allows logging a record. 
		 * No exception or other action beyond the returned boolean.
		 * @return true if a record can be logged, false otherwise.
		 */
		boolean checkPublishLogRecord() {
	
			if (maxLogsPerInterval < 0) {
				// no throttle configured
				return true;
			}
			
			long time = System.currentTimeMillis();
			
			if (time > intervalBeginMillis + intervalLengthMillis) {
				// starting new time interval
				synchronized (logCounter) {
					// We only care to keep intervalBeginMillis and logCounter consistent. 
					// It could happen that another thread resets them again right afterwards, which is OK compared to more locking 
					intervalBeginMillis = time;
					logCounter.set(0);
				}
				// whether we can log now that we are in a new time interval is still to be checked below, 
				// to cover cases like maxLogsPerInterval=0, or a really tough race among loggers
			}
			// Allow logging if we've had less than the max number of logs in the current interval
			return logCounter.getAndIncrement() < maxLogsPerInterval;
		}
	}
	
	
	/**
	 * Callback class that allows clients to be notified of log throttle action,
	 * for example to raise and clear alarms.
	 */
	static interface ThrottleCallback {
		/**
		 * Notification for every log that was suppressed by the throttle,
		 * counting local and remote publishing of the same log twice (if both are enabled and suppressed).
		 * <p>
		 * May be called concurrently.
		 * @param remoteLog  true if the suppressed log was a remote log, and false if it was a local (stdout) log.
		 */
		public void suppressedLog(boolean remoteLog);
		
		/**
		 * Notification that log suppression is over, for both remote and local logs.
		 * <p>
		 * May be called concurrently.
		 */
		public void clearedLogSuppression();
	}
}
