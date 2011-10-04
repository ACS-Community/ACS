package alma.acs.profiling.orb;

import java.util.HashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.jacorb.orb.acs.AcsORBProfiler;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.RepeatGuard;
import alma.acs.util.IsoDateFormat;

/**
 * Profiler implementation that can be used as a base class (or directly) for ORB profiling
 * of containers, manager, OMC etc.
 * <p>
 * If registered with the ORB, this class will collect ORB callbacks and log statistics every 10 seconds,
 * see {@link #logStatus()}.
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
	private final AcsLogger logger;
	
	/**
	 * Repeat guard logger wrapped around {@link #logger}, to control the number of ORB status messages logged.
	 * @see #logStatus()
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
	 * Maximum request queue usage in percent, for the queue owned by the POA given in  
	 */
	private final AtomicInteger requestQueueMaxUsePercent = new AtomicInteger(0);

	/**
	 * Name of the POA who got the longest request queue (see {@link #requestQueueMaxUsePercent}) 
	 * since the last ORB profiler status log.
	 */
	private volatile String requestQueueMaxUsePOA;
	
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
		orbStatusLogRepeatGuard = new RepeatGuard(10, TimeUnit.SECONDS, -1);
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
		logStatus();
	}
	
	@Override
	public void undeliveredRequest(int messageSize, String poaName, String operation, boolean causedByQueueFull) {
		if (debugUndeliveredRequest) {
			System.out.println(IsoDateFormat.formatCurrentDate() + " undeliveredRequest: messageSize=" + messageSize + ", poaName=" + poaName + ", operation=" + operation);
		}
		undeliveredRequests.incrementAndGet();
		logStatus();
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
		logStatus();
	}

	@Override
	public void threadPoolSizeChanged(String poaName, int idleThreads, int totalThreads, int maxThreads) {
		if (debugThreadPoolSizeChanged) {
			System.out.println(IsoDateFormat.formatCurrentDate() + " threadPoolSizeChanged: poaName=" + poaName + ", idleThreads=" + idleThreads + ", totalThreads=" + totalThreads + ", maxThreads=" + maxThreads);
		}
		logStatus();
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
	 */
	private void logStatus() {
		if (orbStatusLogRepeatGuard.checkAndIncrement()) {
			String msg = null;
			synchronized (requestQueueMaxUsePercent) {
				msg = "ORB status: connectionThreadsUsed=" + connectionPoolUsePercent +
					"%, lost calls=" + undeliveredRequests.getAndSet(0) + 
					", requestQueueMaxUsePercent=" + requestQueueMaxUsePercent.getAndSet(0) + "% (in POA '" + requestQueueMaxUsePOA + "').";
			}
			logger.info(msg);
		}
	}

}
