/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.logging;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import alma.acs.concurrent.DaemonThreadFactory;

/**
 * Queue for <code>LogRecord</code>s which takes care of dispatching them to a remote log service, 
 * using the one provided in {@link #setRemoteLogDispatcher(RemoteLogDispatcher)}. 
 * <p>
 * Technically this class is not a singleton, but it is foreseen to be used as a single instance.
 * It is thread-safe, so multiple log handlers can submit records.
 * <p>
 * All log records to be sent remotely to the central log service must be submitted
 * to the {@link #log(LogRecord)} method.
 * If the remote log service is not available (e.g. during startup, or later temporarily due to network problems),
 * the log records will be cached. 
 * The cache size is given by {@link #MAX_QUEUE_SIZE}.
 * If the cache is more than 70% full, the <code>log</code> method will only accept records with level <code>INFO</code> or higher.
 * If the cache is full, no records are accepted. The idea is to not jeopardize the running system, but rather stop remote logging.
 * <p>
 * The queue sorts log records by their log levels and thus dispatches the most important records first,
 * using {@link LogRecordComparator}. <br>
 * 
 * @author hsommer
 * created Apr 19, 2005 1:48:27 PM
 */
public class DispatchingLogQueue {

    private volatile PriorityBlockingQueue<LogRecord> queue;
    
    // 
    private final ReentrantLock flushLock;
    
    private final ScheduledThreadPoolExecutor executor;
    private ScheduledFuture<?> flushScheduleFuture;
    
    // system time in millisec when last flush finished (either scheduled or explicit flush)
    private long lastFlushFinished;
    
    private int currentFlushPeriod;
    
    private boolean outOfCapacity;    
    private int preOverflowFlushPeriod;

    // log dispatcher: initially null until set when remote log service is available
    private RemoteLogDispatcher remoteLogDispatcher;
    
    
    /** Underlying PriorityBlockingQueue is unbounded, but we don't want logging to cause memory problems
     * while the remote logger is unavailable and all logs must be cached. Thus a maximum queue size. 
     * The chosen value must be larger than {@link RemoteLogDispatcher#getBufferSize()} */
    private int maxQueueSize;

    private final boolean DEBUG = Boolean.getBoolean("alma.acs.logging.verbose");
    
    /**
     * If true, them the queue does not drop any log records, but rather blocks loggers in {@link #log(LogRecord)} 
     * until their request can be processed.
     * Default is <code>false</code>, because application performance is more important than logging.
     * For performance testing etc, setting this property may be useful though.
     */
    private static boolean LOSSLESS = Boolean.getBoolean("alma.acs.logging.lossless");
    
    
    DispatchingLogQueue() {
        outOfCapacity = false;
        preOverflowFlushPeriod = 0;
        currentFlushPeriod = 0;
        setMaxQueueSize(1000);
        flushLock = new ReentrantLock();
        queue = new PriorityBlockingQueue<LogRecord>(100, new LogRecordComparator());
        executor = new ScheduledThreadPoolExecutor(1, new DaemonThreadFactory("LogDispatcher"));
        executor.setContinueExistingPeriodicTasksAfterShutdownPolicy(false);
    }
    
    int getMaxQueueSize() {
    	return maxQueueSize;
    }
    
    /**
     * Sets the maximum size of the log queue. Logs will be dropped when the queue is full.
     * The default is 1000, but gets overwritten by the value of <code>&lt;LoggingConfig MaxLogQueueSize/&gt;</code> in the CDB.
     */
    synchronized void setMaxQueueSize(int maxQueueSize) {
    	this.maxQueueSize = maxQueueSize;
    }

    /**
     * Adds a <code>LogRecord</code> to the internal queue, so that it gets scheduled for logging.
     * If a high-level log record should be sent out immediately, the caller of this method should 
     * subsequently call {@link #flush()}, as the log method itself does not trigger a flush based on levels.
     * <p>
     * Threading note: it seems ok to make this method "synchronized". This avoids problems with stale queue size, 
     * even though the damage would be small since the treatment of queue size is somewhat arbitrary anyway.
     * Synchronization should not block callers long at all because flushing is done in a separate thread
     * and only gets triggered here.
     * <p>
     * TODO: there is currently a minor problem with queue overflow: log records get drained from the queue,
     * and when they can't be sent off to the log service, they are resubmitted to the queue.
     * It could happen that in the meantime, some other record with INFO or higher level has been added to the queue,
     * and that resubmitting some of the even more important records may fail.
     * The solution would be to replace the 70%-filter rule with a running priority filter: any incoming record
     * can kick out a less important record if the queue is full. 
     * 
     * @param logRecord  to be logged
     * @return true if logRecord was added to the queue for logging. False if queue was too full for this record.
     */
    synchronized boolean log(LogRecord logRecord) {
        int oldSize = queue.size();
        // if queue is full, then drop the record
        if (oldSize >= maxQueueSize) {
        	
        	
            // first time overflow? Then start periodic flushing attempts to drain the queue once the central logger comes up again
            if (!outOfCapacity) {
                preOverflowFlushPeriod = currentFlushPeriod;
                setPeriodicFlushing(10000);
            }
            outOfCapacity = true;
            
        	if (LOSSLESS) {
        		do {
					try {
						Thread.sleep(1000);
					} catch (InterruptedException ex) {
						// nada
					}
				} while (queue.size() >= maxQueueSize);
        	}
        	else {
        		if (DEBUG || oldSize == maxQueueSize) {
                    System.err.println("log queue overflow: log record with message '" + logRecord.getMessage() + 
                    "' and possibly future log records will not be sent to the remote logging service.");
        		}
                return false;
        	}
        }

        // queue was full before, but now is better again
        if (outOfCapacity) {
            outOfCapacity = false;
            setPeriodicFlushing(preOverflowFlushPeriod);
            preOverflowFlushPeriod = 0;
        }
        
        // drop less important messages if queue space gets scarce
        if (!LOSSLESS) {
	        final int filterThreshold = maxQueueSize * 7 / 10;
	
	        if (oldSize >= filterThreshold && logRecord.getLevel().intValue() < Level.INFO.intValue()) {
	            if (DEBUG) {
	                System.err.println("looming log queue overflow (" + (oldSize+1) + "/" + maxQueueSize + 
	                                    "): low-level log record with message '" + logRecord.getMessage() +
	                                    "' will not be sent to the remote logging service.");
	            }
	            return false;
	        }
        }
        
        queue.put(logRecord);
        
        if (DEBUG) {
            System.out.println("DispatchingLogQueue#log called with record msg = " + logRecord.getMessage());
        }
        
        flushIfEnoughRecords(true);
                
        return true;
    }

    /////////////////////////////////////////////////////////////
    // external flush methods 
    /////////////////////////////////////////////////////////////
    
    /**
     * Flushes all log records if the remote log service has been made available before.
     * Returns only when the log queue contains no more records, so execution time may be long.
     * Should better not be called from the main thread.
     */
    void flushAllAndWait() {
        if (DEBUG) {
            System.out.println("DispatchingLogQueue#flushAllAndWait() called");
        }
        if (!hasRemoteDispatcher()) {
            if (DEBUG) {
                System.err.println("ignoring call to DispatchingLogQueue#flushAllAndWait because the remote log service has not been supplied.");
            }
            return;
        }
        boolean flushSuccess = true; // not yet used
        
        while (realQueueSize() > 0) {
            Future<Boolean> future = flush();
            try {
                // the future.get call blocks until the flush thread has completed
                flushSuccess = future.get().booleanValue();
            } catch (Exception e) {
                flushSuccess = false;
                if (DEBUG) {
                    System.err.println("flushAll: exception occurred while waiting for flush thread. Will try again. " + e.toString());
                }
                try {
                    Thread.sleep(100);
                } catch (Exception e2) {
                    // ignore
                }                
            } finally {
                if (DEBUG) {
                    System.out.println("DispatchingLogQueue#flushAllAndWait() called flush(), success = " + flushSuccess);
                }
            }
        }
    }

    
    /**
     * Tries to send log records to the remote log service, but at most {@link RemoteLogDispatcher#getBufferSize()}.
     * If sending fails, the log records remain in the queue (actually they may be first taken out and then get re-submitted).
     * <p>
     * This method returns immediately, since flushing is done in a separate thread.
     * The returned future object can be used to wait for termination of the log flush and get the result, or to cancel the flush. 
     * The result is a <code>Boolean</code> which is true if all or at least 1 log record could be taken off the log queue.
     */
    Future<Boolean> flush() {
        if (DEBUG) {
            System.out.println("DispatchingLogQueue#flush() called in thread " + Thread.currentThread().getName());
        }
        // run in dispatch thread  
        Callable<Boolean> cmd = new Callable<Boolean>() {
            public Boolean call() throws Exception {
                return new Boolean(flush(false));
            }
        };
        Future<Boolean> future = executor.schedule(cmd, 0, TimeUnit.NANOSECONDS);
        if (DEBUG) {
            System.out.println("Pending log flushes: " + pendingFlushes());
        }
        return future;
    }

    
    /////////////////////////////////////////////////////////////
    // internal flush methods 
    /////////////////////////////////////////////////////////////
    
    /**
     * Flushes if we have enough records in the queue to fill up the entire buffer for sending them remotely, 
     * and if the remote log service can be assumed to be available.
     * <p>
     * Since flushing happens in a separate thread with a certain time lag, this method can avoid creating too many flush requests
     * by only generating them if the number of queued log records is <em>a multiple of</em> the send buffer
     * as opposed to <em>larger than</em> the buffer. This is controlled by the <code>conservative</code> flag.
     * <p>
     * Note that the most important records are sent first because the queue sorts by log level, and that a successful 
     * flush() will call this method again to submit another flush; 
     * thus even a large queue will be drained fairly fast if the remote log service works,
     * even in the absence of periodic flushing.
     * @param conservative
     */
    private void flushIfEnoughRecords(boolean conservative) {
        int numRecords = queue.size();
        if (DEBUG) {
            System.out.println("flushIfEnoughRecords(" + conservative + "): current queue size is " + numRecords + ", remote dispatcher available: " + hasRemoteDispatcher());
        }
        if (hasRemoteDispatcher() && numRecords > 0) {
            if ( (numRecords % remoteLogDispatcher.getBufferSize() == 0) || 
                 (!conservative && (numRecords >= remoteLogDispatcher.getBufferSize())) ) {
                flush();      
            }
        }        
    }
    
    
    /**
     * Internal flush method which covers straight calls to flush as well as scheduled calls.
     * <p>
     * Threading note: this method is thread safe because competing threads are blocked on a flush lock
     * (and additionally on <code>queue.drainTo(..)</code>),
     * so that the second thread may unexpectedly not find any log records and thus will return immediately.
     *  
     * @param isScheduled true if this method is called by a timer, as opposed to some more direct thread. Used only for debugging. 
     * @return Success indicator: true if all or at least some log records were permanently taken off the queue by this flush request.
     */
    private boolean flush(boolean isScheduled) {
        if (DEBUG) {
            System.out.println("DispatchingLogQueue#flush(isScheduled=" + isScheduled + ") called in thread " + Thread.currentThread().getName());
        }
        
        boolean flushedSomeRecords = false;

        if (!hasRemoteDispatcher()) {
            System.err.println("failed to flush logging queue because remote logging service has not been made available.");
        }
        else {
            flushLock.lock();
            try {

                //List<LogRecord> logRecordList = new ArrayList<LogRecord>();
                // queue.drainTo(logRecordList, remoteLogDispatcher.getBufferSize());

                // queue.drainTo is removing the log records from the queue, which is bad
                // Instead, we should only pick them (but not remove them), and then, if they
                // are well sent, we remove them from the queue

                // Solution: toArray(), and then take the getBufferSize()'th/queue.size()'th first ones
                int bufferSize = remoteLogDispatcher.getBufferSize();
                int queueSize  = queue.size();
                LogRecord[] allRecords = queue.toArray(new LogRecord[queueSize]);

                final LogRecord[] logRecords;
                if( queueSize > bufferSize ) {
                	logRecords = new LogRecord[bufferSize];
                	System.arraycopy(allRecords, 0, logRecords, 0, bufferSize);
                }
                else {
                	logRecords = new LogRecord[queueSize];
                	System.arraycopy(allRecords, 0, logRecords, 0, queueSize);
                }

                if (logRecords.length != 0 ) {
//                    final LogRecord[] logRecords = logRecordList.toArray(new LogRecord[logRecordList.size()]);
                    
                    flushedSomeRecords = flushLogRecords(logRecords);
                    // if successful, try to schedule another flush to drain the queue further (if it's large enough)
                    if (flushedSomeRecords) {
                        if (DEBUG) {
                            System.out.println("flushing was successful, will try again if there are enough records queued.");
                        }
                        flushIfEnoughRecords(false);
                    }
                }
                else {
                    if (DEBUG){
                        System.err.println("flush(isScheduled=" + isScheduled + "): no log records found!");
                    }
                }
            } finally {
                flushLock.unlock();
            }        
        }
        
        return flushedSomeRecords;
    }

    
    /**
	 * Extracted from {@link #flush(boolean)} to improve readability of the code.
	 * 
	 * @param logRecords
	 *            the records to be sent to the remote logger.
	 * @return true if all or at least some log records were permanently taken off the queue.
	 */
	private boolean flushLogRecords(final LogRecord[] logRecords) {
		if (DEBUG) {
			System.out.println("DispatchingLogQueue#flushLogRecords called in thread "
					+ Thread.currentThread().getName());
		}
		boolean flushedSomeRecords = true;

		RemoteLogDispatcher.FailedLogRecords failures = remoteLogDispatcher.sendLogRecords(logRecords);

		List<LogRecord> allFailures = new ArrayList<LogRecord>();
		if (failures.hasSendFailures()) {
			List<LogRecord> sendFailures = failures.getSendFailures();

			allFailures.addAll(sendFailures);
			// Since now they are not removed anymore from the queue before trying to send them,
			// we should instead NOT remove these from the queue
			// we'll try to send them some other time
//			queue.addAll(sendFailures);

			flushedSomeRecords = (sendFailures.size() < logRecords.length);
			if (DEBUG) {
				System.err.println("flushLogRecords: had to add back " + sendFailures.size()
						+ " send-failed log records to the queue.");
			}
		}
		if (failures.hasSerializationFailures()) {
			for (LogRecord logRecord : failures.getSerializationFailures()) {
				System.err.println("Failed to translate log record for sending remotely. Log message = " + logRecord.getMessage());
			}
			// these records are not quite satisfactorily flushed, but gone from the queue, which matters here
			flushedSomeRecords = true;
		}

		// Remove successfully sent records from the queue
		for(LogRecord e: logRecords) {
			if( !allFailures.contains(e) )
				queue.remove(e);
		}

		lastFlushFinished = System.currentTimeMillis();
		return flushedSomeRecords;
	}

    
    /////////////////////////////////////////////////////////////
    // status info 
    /////////////////////////////////////////////////////////////
    
    boolean hasRemoteDispatcher() {
        return (remoteLogDispatcher != null);
    }
    
    /**
     * Returns the number of currently queued log messages.
     * This method is intended only for testing and monitoring of the logging system.
     */
    int recordQueueSize() {
        return queue.size();
    }
    
    
    /**
     * Returns the number of currently waiting flush requests.
     * This method is intended only for monitoring the logging system.
     * Periodic flushing enabled by {@link #setPeriodicFlushing(int)} is not counted as a waiting flush request. 
     * <p>
     * Note that not all of these requested flushes necessarily result in a flushing of log records,
     * because some of them may find an empty log record queue when they get executed, and thus end w/o effect.   
     */
    int pendingFlushes() {
        BlockingQueue<Runnable> flushQueue = executor.getQueue();
        int size = flushQueue.size();
        // periodic flushing shows up as one entry in the executor queue 
        if (flushesPeriodically()) {
            size--;
        }
        return size;
    }
    
    /**
     * Waits if necessary until a flushing thread has finished, and then returns the real number of currently queued log records.
     * <p>
     * Note that during flushing, the records are first taken out of the queue, but then get resubmitted if sending
     * them to the central log service failed.
     * Therefore, calling <code>queue.size()</code> w/o flush synchronization may yield too low a value.
     */
    int realQueueSize() {
        flushLock.lock();
        try {
            return queue.size();
        } finally {
            flushLock.unlock();
        }        
    }

    
    boolean flushesPeriodically() {
        return (flushScheduleFuture != null);        
    }

    
    /////////////////////////////////////////////////////////////
    // adding features
    /////////////////////////////////////////////////////////////
    
    /**
     * Triggers periodic calls to {@link #flush(boolean)}, 
     * or terminates such automatic flushing if <code>periodMillisec == 0</code>.
     * <p>
     * All control over periodic log flushing is confined in this method.
     * <p> 
     * The call returns without further action if flushing is already enabled 
     * with the same period as <code>periodMillisec</code>.
     * 
     * @param periodMillisec the delay between end of last scheduled flush() and the next scheduled flush().
     */
    void setPeriodicFlushing(final int periodMillisec) {
        if (!hasRemoteDispatcher()) {
            System.err.println("DispatchingLogQueue#setPeriodicFlushing is ignored until setRemoteLogDispatcher() has been called!");
            return;
        }        
        
        // Only re-set the flushing if the value has changed, because the operation is expensive (stop/start).
        if (currentFlushPeriod == periodMillisec && flushesPeriodically()) {
        	return;
        }
        
        // store period 
        currentFlushPeriod = periodMillisec;

        if (flushesPeriodically()) {
            // we already have something scheduled periodically
            flushScheduleFuture.cancel(false);
            flushScheduleFuture = null;
            
            if (DEBUG && periodMillisec == 0) {
                System.out.println("Stopping periodic log flushing.");
            }
        }
        
        if (periodMillisec > 0) {
            Runnable cmd = new Runnable() {
                public void run() {
                    if (queue.size() > 0) {
                        // skip if last non-scheduled flush was too recent
                        if (lastFlushFinished <= System.currentTimeMillis() - periodMillisec) {
                            try {
                                flush(true);
                            } catch (Throwable thr) {
                                System.err.println("Scheduled flushing of log buffer failed: " + thr.getMessage());
                                // we swallow the error because otherwise future executions would be suppressed
                            }
                        }
                        else {
                            if (DEBUG) {
                                System.err.println("Skipping a scheduled log flush because of another recent flush.");
                            }
                        }
                    }
                    else {
                        if (DEBUG) {
                            System.err.println("Skipping a scheduled log flush because log queue is empty.");
                        }
                    }
                }            
            };
            flushScheduleFuture = executor.scheduleWithFixedDelay(cmd, periodMillisec, periodMillisec, TimeUnit.MILLISECONDS);
        }
    }
    
    
    /**
     * Sets the remote log dispatcher. Should be called once the remote log service is available.
     * Calling this method will not flush the log queue (need to call {@link #flushAllAndWait()} separately),
     * nor will it automatically trigger periodic flushes (call {@link #setPeriodicFlushing(int)} for this).  
     *   
     * @param remoteLogDispatcher The remoteLogDispatcher to set.
     */
    void setRemoteLogDispatcher(RemoteLogDispatcher remoteLogDispatcher) {
        this.remoteLogDispatcher = remoteLogDispatcher;
    }

    
    void shutDown() {
        if (DEBUG) {
            System.out.println("DispatchingLogQueue#shutDown called");
        }
        if (!executor.isShutdown()) {
            executor.shutdown();
        }
    }
}


