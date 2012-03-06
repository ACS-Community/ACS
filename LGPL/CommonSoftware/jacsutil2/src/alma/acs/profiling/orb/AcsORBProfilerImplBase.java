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
package alma.acs.profiling.orb;

import java.util.HashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;

import org.jacorb.orb.acs.AcsORBProfiler;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.RepeatGuard;
import alma.acs.util.IsoDateFormat;

/**
 * Profiler implementation that can be used as a base class (or directly) for ORB profiling
 * of containers, manager, OMC etc.
 * <p>
 * If registered with the ORB, this class will collect ORB callbacks and log statistics every 10 seconds,
 * see {@link #checkAndLogStatus()}.
 * <p>
 * To get logs for every ORB callback (only available for stdout), use {@link #DEBUG_CONFIG_PROPERTYNAME}.
 * 
 * @author msekoran (first impl as cdb_rdb :: ORBRequestTimer), hsommer
 */
public class AcsORBProfilerImplBase implements AcsORBProfiler
{
	/**
	 * This property can optionally be set to one or more (comma and/or space separated) names of 
	 * callback methods as defined in {@link AcsORBProfiler}, whose invocations will then be printed to stdout
	 * in addition to the normal profiling summary statistics that gets logged.
	 * <p>
	 * Example: <code>-Dalma.acs.profiling.orb.debugLogs='requestFinished,undeliveredRequest requestQueueSizeChanged'"</code>
	 */
	public static final String DEBUG_CONFIG_PROPERTYNAME = "alma.acs.profiling.orb.debugLogs";
	
	/**
	 * Logger passed in the c'tor. 
	 */
	protected final AcsLogger logger;
	
	/**
	 * Repeat guard wrapped around {@link #logger}, to control the number of ORB status messages logged.
	 * @see #checkAndLogStatus()
	 */
	private final RepeatGuard orbStatusLogRepeatGuard;
	
	/**
	 * Percentage of busy threads in the connection thread pool.
	 */
	private volatile int connectionPoolUsePercent;
	
	/**
	 * Number of undelivered requests since the last ORB profiler status log.
	 * @see #undeliveredRequest(int, String, String, boolean)
	 */
	private final AtomicInteger undeliveredRequests = new AtomicInteger(0);
	
	/**
	 * Maximum request queue usage in percent, for the queue owned by the POA given in {@link #requestQueueMaxUsePOA}.
	 * <p>
	 * Note about usage of AtomicInteger: What we really would like to use is an AtomicReference class
	 * whose value is a custom class that bundles together an "int requestQueueMaxUsePercent"
	 * and "String requestQueueMaxUsePOA" while offering a method "compareGreaterEqualsAndSet"
	 * that can swap a new "usepercent / poa" pair if the new integer is greater or equal than the old one. 
	 * In this way we could get rid of the synchronized blocks in methods requestQueueSizeChanged and logStatus.
	 * Since such an "enhanced AtomicReference" is not available and I don't dare to write one, we do use
	 * synchronized blocks, and "abuse" this AtomicInteger as a monitor which otherwise would have to be a separate field.
	 */
	private final AtomicInteger requestQueueMaxUsePercent = new AtomicInteger(0);

	/**
	 * Name of the POA who got the longest request queue (see {@link #requestQueueMaxUsePercent}) 
	 * since the last ORB profiler status log.
	 */
	private volatile String requestQueueMaxUsePOA = "---";
	
	/**
	 * Used to trace a request from {@link #requestStarted(int, String, String)} 
	 * to {@link #requestFinished(int, String, String)}.
	 */
	private final HashMap<ThreadRequestId, Long> requestTimeMap = new HashMap<ThreadRequestId,Long>();

	/**
	 * Can be set via {@link #DEBUG_CONFIG_PROPERTYNAME}.
	 * We convert to explicit booleans instead of string-based lookup to improve performance.
	 */
	protected boolean debugConnectionThreadPoolSizeChanged = false;
	protected boolean debugUndeliveredRequest = false;
	protected boolean debugRequestQueueSizeChanged = false;
	protected boolean debugThreadPoolSizeChanged = false;
	protected boolean debugRequestStarted = false;
	protected boolean debugRequestFinished = false;

	
	public AcsORBProfilerImplBase(AcsLogger logger) {
		this.logger = logger;
		orbStatusLogRepeatGuard = new RepeatGuard(30, TimeUnit.SECONDS, -1);
		String debugConfig = System.getProperty(DEBUG_CONFIG_PROPERTYNAME);
		if (debugConfig != null) {
			String[] debugMethodNames = debugConfig.split("[ ,]+");
			for (int i = 0; i < debugMethodNames.length; i++) {
				if (debugMethodNames[i].equals("connectionThreadPoolSizeChanged")) {
					debugConnectionThreadPoolSizeChanged = true;
				}
				else if (debugMethodNames[i].equals("undeliveredRequest")) {
					debugUndeliveredRequest = true;
				}
				else if (debugMethodNames[i].equals("requestQueueSizeChanged")) {
					debugRequestQueueSizeChanged = true;
				}
				else if (debugMethodNames[i].equals("threadPoolSizeChanged")) {
					debugThreadPoolSizeChanged = true;
				}
				else if (debugMethodNames[i].equals("requestStarted")) {
					debugRequestStarted = true;
				}
				else if (debugMethodNames[i].equals("requestFinished")) {
					debugRequestFinished = true;
				}
			}
		}
	}
	
	@Override
	public void connectionThreadPoolSizeChanged(int idleThreads, int totalThreads, int maxThreads) {
		if (debugConnectionThreadPoolSizeChanged) {
			System.out.println(IsoDateFormat.formatCurrentDate() + " connectionThreadPoolSizeChanged: idleThreads=" + idleThreads + ", totalThreads=" + totalThreads + ", maxThreads=" + maxThreads);
		}
		
		connectionPoolUsePercent = (int)(((totalThreads-idleThreads)/(double)maxThreads)*100);
		checkAndLogStatus();
	}
	
	@Override
	public void undeliveredRequest(int messageSize, String poaName, String operation, boolean causedByQueueFull) {
		if (debugUndeliveredRequest) {
			System.out.println(IsoDateFormat.formatCurrentDate() + " undeliveredRequest: messageSize=" + messageSize + ", poaName=" + poaName + ", operation=" + operation);
		}
		undeliveredRequests.incrementAndGet();
		checkAndLogStatus();
	}

	/**
	 * Only records the maximum queue length and the POA name owning that queue.
	 * For more information, we could record the queue lengths for all POAs, but then have to watch out that 
	 * the backing map etc structure does not overflow when different POAs come and go.
	 * @see org.jacorb.orb.acs.AcsORBProfiler#requestQueueSizeChanged(int, java.lang.String, int, int)
	 */
	@Override
	public void requestQueueSizeChanged(int requestId, String poaName, int queueSize, int maxQueueLength) {
		if (debugRequestQueueSizeChanged) {
			System.out.println(IsoDateFormat.formatCurrentDate() + " requestQueueSizeChanged: requestId=" + requestId + ", poaName=" + poaName + ", queueSize=" + queueSize + ", maxQueueLength=" + maxQueueLength);
		}
		int requestQueueUsePercent = (int)((queueSize/(double)maxQueueLength)*100);
		synchronized (requestQueueMaxUsePercent) { // cannot compare >= and include requestQueueMaxUsePOA with just AtomicInteger methods.
			if (requestQueueUsePercent >= requestQueueMaxUsePercent.get()) {
				requestQueueMaxUsePercent.set(requestQueueUsePercent);
				requestQueueMaxUsePOA = poaName;
			}
		}
		checkAndLogStatus();
	}

	@Override
	public void threadPoolSizeChanged(String poaName, int idleThreads, int totalThreads, int maxThreads) {
		if (debugThreadPoolSizeChanged) {
			System.out.println(IsoDateFormat.formatCurrentDate() + " threadPoolSizeChanged: poaName=" + poaName + ", idleThreads=" + idleThreads + ", totalThreads=" + totalThreads + ", maxThreads=" + maxThreads);
		}
		checkAndLogStatus();
	}

	/**
	 * Uses requestId and threadId to trace a call from requestStarted to requestFinished.
	 * <p>
	 * Note that multiple clients may generate the same requestId concurrently, 
	 * and that at least for JacORB a single client may generate the same requestId concurrently for different ClientConnections.
	 * <p>
	 * TODO: Wouldn't using only the thread ID be good enough?
	 */
	static final class ThreadRequestId {
		private long threadId;
		private int requestId;
		public ThreadRequestId(long threadId, int requestId) {
			this.threadId = threadId;
			this.requestId = requestId;
		}
		@Override
		public int hashCode() {
			return (int)threadId * 911 + requestId;
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			ThreadRequestId other = (ThreadRequestId) obj;
			if (requestId != other.requestId)
				return false;
			if (threadId != other.threadId)
				return false;
			return true;
		}
	}
	
	@Override
	public void requestStarted(int requestId, String poaName, String operation) {
		long threadId = Thread.currentThread().getId();
		if (debugRequestStarted) {
			System.out.println(IsoDateFormat.formatCurrentDate() + " requestStarted(" + requestId + ", " + poaName + ", " + operation + ", " + threadId + ")");
		}
		synchronized (requestTimeMap) {
			requestTimeMap.put(new ThreadRequestId(threadId, requestId), System.currentTimeMillis());
		}
	}

	@Override
	public void requestFinished(int requestId, String poaName, String operation) {
		synchronized (requestTimeMap) {
			long threadId = Thread.currentThread().getId();
			Long startTime = requestTimeMap.remove(new ThreadRequestId(threadId, requestId));
			if (startTime != null)
			{
				long timeSpent = System.currentTimeMillis() - startTime.longValue();
				if (debugRequestFinished) {
					System.out.println(IsoDateFormat.formatCurrentDate() + " requestFinished(" + requestId + ", " + poaName + ", " + operation + ", " + threadId + ") in " + timeSpent + " ms");
				}
			}
			else
			{
				if (debugRequestFinished) {
					System.out.println(IsoDateFormat.formatCurrentDate() + " requestFinished(" + requestId + ", " + poaName + ", " + operation + ", " + threadId + ")");
				}
			}
		}
	}
	
	/**
	 * Logs the ORB status and resets {@link #undeliveredRequests}, {@link #requestQueueMaxUsePercent}.
	 * We use repeat guard {@link #orbStatusLogRepeatGuard} so that at most one status message 
	 * gets logged per configured time interval, while nothing is logged if the ORB does not get called.
	 * <p>
	 * Subclasses that want to modify the log message should override {@link #logStatus(String)}.
	 */
	private void checkAndLogStatus() {
		if (orbStatusLogRepeatGuard.checkAndIncrement()) {
			String msg = null;
			int snapshotConnectionPoolUsePercent = -1;
			int snapshotUndeliveredRequests = -1;
			int snapshotRequestQueueMaxUsePercent = -1;
			String snapshotRequestQueueMaxUsePOA = null;
			synchronized (requestQueueMaxUsePercent) {
				snapshotConnectionPoolUsePercent = connectionPoolUsePercent ;
				snapshotUndeliveredRequests = undeliveredRequests.getAndSet(0);
				snapshotRequestQueueMaxUsePercent = requestQueueMaxUsePercent.getAndSet(0);
				snapshotRequestQueueMaxUsePOA = requestQueueMaxUsePOA;
				requestQueueMaxUsePOA = "---";
			}
			msg = "ORB status: connectionThreadsUsed=" + snapshotConnectionPoolUsePercent +
			"%, lost calls=" + snapshotUndeliveredRequests + 
			", requestQueueMaxUsePercent=" + snapshotRequestQueueMaxUsePercent + "% (in POA '" + snapshotRequestQueueMaxUsePOA + "').";
			
			logStatus(msg, Level.INFO, snapshotConnectionPoolUsePercent, 
						snapshotUndeliveredRequests, snapshotRequestQueueMaxUsePercent, snapshotRequestQueueMaxUsePOA);
		}
	}
	
	/**
	 * This method is broken out from {@link #checkAndLogStatus()} so that subclasses can change or suppress the log message,
	 * without having to worry about log repeat guard or synchronization.
	 * 
	 * @param defaultLogMessage A default log message, that can be used or replaced by another message.
	 * @param defaultLogLevel A suggested log level.
	 * @param connectionPoolUsePercent See {@link #connectionPoolUsePercent}. Can be used to build a custom message.
	 * @param undeliveredRequests See {@link #undeliveredRequests}. Can be used to build a custom message.
	 * @param requestQueueMaxUsePercent See {@link #requestQueueMaxUsePercent}. Can be used to build a custom message.
	 * @param requestQueueMaxUsePOA See {@link #requestQueueMaxUsePOA}. Can be used to build a custom message.
	 */
	protected void logStatus(String defaultLogMessage, Level defaultLogLevel, int connectionPoolUsePercent,
			int undeliveredRequests, int requestQueueMaxUsePercent, String requestQueueMaxUsePOA) {
		logger.log(defaultLogLevel, defaultLogMessage);
	}

}
