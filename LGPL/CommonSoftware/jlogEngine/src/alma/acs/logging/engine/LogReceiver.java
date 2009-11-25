/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acs.logging.engine;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.DelayQueue;
import java.util.concurrent.Delayed;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.omg.CORBA.ORB;

import com.cosylab.logging.engine.ACS.ACSLogConnectionListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.LCEngine;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

import si.ijs.maci.Manager;


/**
 * Receives log messages from the ACS Log service 
 * and sorts them by timestamp, even if the log messages arrive
 * in different order within a certain time window.
 * <p>
 * For a client of this class to consume these log messages, 
 * method {@link #getLogQueue()} provides a {@link java.util.concurrent.BlockingQueue}
 * from which the client can extract the messages.
 * <p>
 * Before log messages can be received, {@link #initialize(ORB, Manager)} must be called.
 * To disconnect from the the log stream, call {@link #stop()}.
 * <p>
 * As an alternative to <code>getLogQueue</code> and <code>stop</code>, 
 * this class also offers the method pair {@link #startCaptureLogs(PrintWriter)} 
 * and {@link #stopCaptureLogs()}. These methods can be used to directly 
 * print the received log messages to some writer, sparing the effort of listening on the LogQueue.
 * Method {@link #stopCaptureLogs()} must be called in that case to stop capturing logs. 
 *   
 * @author hsommer
 */
public class LogReceiver {


	private boolean verbose = false;

	protected LCEngine lct;
	protected MyRemoteResponseCallback rrc;
	
	// the queue into which all log records are stored
	private DelayQueue<DelayedLogEntry> logDelayQueue;
	
	private ArrayList<String> statusReports;
	
	private volatile boolean listenForLogs;
	private volatile long sortingDelayMillis = 20000;

	
	public LogReceiver() {
		super();
	}

	public boolean isVerbose() {
		return verbose;
	}
	public void setVerbose(boolean verbose) {
		this.verbose = verbose;
	}
	
    /**
     * Initializes the connection to the logging channel, which leads
     * to log entries getting written to the sorting queue.
     * To reuse an existing ORB and manager reference, use {@link #initialize(ORB, Manager)} instead of this method.
     * <p>
     * This method attempts to wait for successful initialization for up to 10 seconds.
     * If initialization did not happen within this time, <code>false</code> is returned, otherwise true.
     * <p>
     * If you call this method, make sure to subsequently call
     * {@link #getLogQueue()} and drain the queue at your own responsibility,
     * or to call {@link #startCaptureLogs(PrintWriter)} (or {@link #startCaptureLogs(PrintWriter, ThreadFactory)})
     * which will drain the queue automatically.
     * 
     * @return true if initialization was successful within at most 10 seconds 
     */
    public boolean initialize() {
        return initialize(null, null, 10);
    }
    
    /**
     * Variant of {@link #initialize()} which takes an existing ORB and manager reference.
     * <p>
     * This method attempts to wait for successful initialization for up to <code>timeoutSeconds</code> seconds.
     * If initialization did not happen within this time, <code>false</code> is returned, otherwise true.
     * 
     * @param theORB  the fully functional ORB object to reuse, or <code>null</code> if an ORB should be created 
     * @param manager  reference to the acs manager, or <code>null</code> if this reference should be created
     * @param timeoutSeconds  timeout for awaiting the successful initialization. 
     * @return true if initialization was successful within at most <code>timeoutSeconds</code> seconds. 
     */
	public boolean initialize(ORB theORB, Manager manager, int timeoutSeconds) {
		boolean ret = false;
		if (verbose) {
			System.out.println("Attempting to connect to Log channel...");
		}

		logDelayQueue = new DelayQueue<DelayedLogEntry>();
		statusReports = new ArrayList<String>();
		
		rrc = new MyRemoteResponseCallback(logDelayQueue, statusReports);
        rrc.setVerbose(verbose);
        rrc.setDelayMillis(sortingDelayMillis);

        lct = new LCEngine();
        lct.addLogConnectionListener(rrc);
        lct.addLogListener(rrc);

		lct.setAccessType("ACS");
		lct.connect(theORB, manager);
        
        try {
            ret = rrc.awaitConnection(timeoutSeconds, TimeUnit.SECONDS);
        } catch (InterruptedException ex) {
            ex.printStackTrace();
        }
        return ret;
	}
	
    public boolean isInitialized() {
        return lct.isConnected();
    }
    
    /**
     * Sets the delay that a log record's timestamp must have with respect to the current system time
     * in order to be allowed to leave the sorting queue.
     * @param newDelayMillis the delay time in milliseconds.
     */
    public void setDelayMillis(long newDelayMillis) {
        this.sortingDelayMillis = newDelayMillis;
        if (rrc != null) {
            rrc.setDelayMillis(newDelayMillis);
        }
    }

    public long getDelayMillis() {
        return this.sortingDelayMillis;
    }

	/**
	 * Gets access to the log record queue, from which the time-sorted log records should be fetched.
	 * <p>
	 * The queue elements are of type {@link DelayedLogEntry},
	 * from which the log record can be extracted
	 * using the method {@link DelayedLogEntry#getLogEntry()}.
	 * @return
	 */
	public BlockingQueue<DelayedLogEntry> getLogQueue() {
		return logDelayQueue;
	}
	
	/**
	 * Gets the status reports. Currently only a forceful disconnect would be shown,
	 * the rest is ignored.
	 * TODO: separate good and bad status reports
	 * @return
	 */
	public String[] getStatusReports() {
		return statusReports.toArray(new String[statusReports.size()]); 
	}

	/**
	 * Disconnects from the logging channel.
	 * A special "queue poison" version of DelayedLogEntry will be added to the queue with zero delay,
	 * so that all clients who fetch log recored of the queue obtained from {@link #getLogQueue()} 
	 * know that the show is over.
	 * @see DelayedLogEntry#isQueuePoison()
	 */
	public void stop() {
		if (verbose) {
			System.out.println("Attempting to destroy LogConnect...");
		}
		lct.close(true);
		listenForLogs = false;
		logDelayQueue.offer(DelayedLogEntry.createQueuePoison(0));
	}
		
	
	
	/**
	 * Callback class that receives log data from {@link LCEngine}. 
	 */
	static class MyRemoteResponseCallback implements ACSRemoteLogListener, ACSLogConnectionListener {
		private boolean verbose = false;
		
		private final ArrayList<String> statusReports;
		private final DelayQueue<DelayedLogEntry> logDelayQueue;
        private long delayMillis = 20000;
        
        private boolean isConnected = false;
        private CountDownLatch connectSync;
		
		MyRemoteResponseCallback(DelayQueue<DelayedLogEntry> logDelayQueue, ArrayList<String> statusReports) {
			this.logDelayQueue = logDelayQueue;
			this.statusReports = statusReports;
		}
		
        boolean awaitConnection(long timeout, TimeUnit unit) throws InterruptedException {
            synchronized(this) {
                if (isConnected) {
                    return true;
                }
                if (connectSync == null) {
                    connectSync = new CountDownLatch(1);
                }
            }
            return connectSync.await(timeout, unit);
        }
        
        public void acsLogConnConnecting() {
            if (verbose) {
                System.out.println("LogReceiver#acsLogConnConnecting()");             
            }
        }
        public synchronized void acsLogConnEstablished() {
            isConnected = true;
            if (connectSync != null) {
                connectSync.countDown();
                connectSync = null;
            }
            if (verbose) {
                System.out.println("LogReceiver#acsLogConnEstablished()");             
            }
        }
		public void acsLogConnDisconnected() {
            isConnected = false;
            if (verbose) {
                System.out.println("LogReceiver#acsLogConnDisconnected()");             
            }
		}
        public synchronized void acsLogConnLost() {
            isConnected = false;
            if (verbose) {
                System.out.println("LogReceiver#acsLogConnLost()");             
            }
            // the connection loss was unexpected, so we notify the queue consumer
    		logDelayQueue.offer(DelayedLogEntry.createQueuePoison(delayMillis));
        }

		public void reportStatus(String status) {
			if (verbose) {
				System.out.println("LogReceiver status report: " + status);				
			}
//			statusReports.add(status);
		}

		public void logEntryReceived(ILogEntry logEntry) {
			if (verbose) {
				System.out.println("*** received ILogEntry");
			}
			DelayedLogEntry delayedLogEntry = new DelayedLogEntry(logEntry, delayMillis);
			// add the record to the queue
			logDelayQueue.offer(delayedLogEntry);
		}
		
		public void acsLogConnSuspended() {
			
		}
		
		public void acsLogsDelay() {
			
		}

		boolean isVerbose() {
			return verbose;
		}

		void setVerbose(boolean verbose) {
			this.verbose = verbose;
		}
        
        /**
         * Sets the delay for log entries in the queue.
         * @param newDelayMillis
         * @see DelayedLogEntry#DelayedLogEntry(ILogEntry, long)
         */
        void setDelayMillis(long newDelayMillis) {
            delayMillis = newDelayMillis;
        }

		
// TODO: these are normal status messages which should be ignored,
// while other messages should raise an error. Currently all messages are ignored.		
//		Connecting to ACS remote access...
//		Initializing CORBA...
//		CORBA initialized.
//		Resolving corbaloc::acc:3000/Manager manager reference...
//		Manager reference resolved.
//		Resolving Naming Service...
//		Naming Service resolved.
//		Resolving channel "LoggingChannel" from Notify Service...
//		Channel "LoggingChannel" resolved.
//		Creating Consumer Admin...
//		Consumer Admin created.
//		Initializing Structured Push Consumer...
//		Structured Push Consumer initialized.
//		Connected to ACS remote access.

	}	

	
	
	/**
	 * Wraps an {@link ILogEntry} for storage in a {@link java.util.concurrent.DelayQueue}.
	 * <p>
	 * The <code>delayTimeMillis</code> parameter in the constructor sets the buffer time during which log entries
	 * are not yet available for the consumer, so that late arriving records get a chance
	 * to be sorted in according to timestamp. <br>
	 * As of ACS 7.0.1 the issue of timestamps that lie in the future (e.g. logs from a different machine with unsync'd time)
	 * is addressed in the way that "future" log records will become available to the consumer before
	 * the local system time has reached the timestamp.  
	 * 
	 * @author hsommer
	 */
	public static class DelayedLogEntry implements Delayed {

		/** delay for sorting by timestamp */
		private long delayTimeMillis;
		private boolean isQueuePoison = false;
		
		private static final AtomicInteger logRecordCounter = new AtomicInteger();

		private int logRecordIndex;
		private ILogEntry logEntry;
		private long triggerTimeMillis;

		DelayedLogEntry(ILogEntry logEntry, long delayTimeMillis) {
			logRecordIndex = logRecordCounter.incrementAndGet();
			this.logEntry = logEntry;
            this.delayTimeMillis = delayTimeMillis;
			Date logDate = new Date((Long)logEntry.getField(LogField.TIMESTAMP));
			// if log record has a time stamp in the future (according to local machine time), then we clip it to the current time
			long adjustedLogTimestamp = Math.min(System.currentTimeMillis(), logDate.getTime());
			triggerTimeMillis = adjustedLogTimestamp + delayTimeMillis;
		}

		/**
		 * Ctor used for special queue poison instance
		 * @param delayTimeMillis
		 */
		private DelayedLogEntry(long delayTimeMillis) {
			logRecordIndex = logRecordCounter.incrementAndGet();
            this.delayTimeMillis = delayTimeMillis;
			triggerTimeMillis = System.currentTimeMillis() + delayTimeMillis;
		}
		
		public static DelayedLogEntry createQueuePoison(long delayTimeMillis) {
			DelayedLogEntry dle = new DelayedLogEntry(delayTimeMillis);
			dle.isQueuePoison = true;
			return dle;
		}
		
		/**
		 * True if this entry designates the end of the queue.
		 * According to {@link BlockingQueue}, this element is called the "poison".
		 * @return true if this is the end-of-queue marker.
		 * @see #createQueuePoison(long)
		 */
		public boolean isQueuePoison() {
			return isQueuePoison;
		}
		
		/**
		 * Returns the <code>ILogEntry</code> class that was wrapped for sorting inside the queue.
		 * That class represents the log record as it was received from the logging service.
		 * <b>Beware that the log level you get from this ILogEntry is not an ACS log level, but
		 *    comes from some level representation internally used by the jlog application whose code got reused here!
		 *    To get an ACS level, you must convert it using {@link LogTypeHelper#getAcsCoreLevel(Integer)}</b>
		 * @deprecated use {@link #getLogRecord()} to avoid dealing with jlog-internal log level (=severity) number ranges.
		 */
		public ILogEntry getLogEntry() {
			return logEntry;
		}

		/**
		 * Returns the log record that was wrapped for sorting inside the queue. 
		 * The returned object represents the log record as it was received from the logging service.
		 */
		public ReceivedLogRecord getLogRecord() {
			return new ReceivedLogRecord(logEntry);
		}
		
		/**
		 * This method is used by the queue to determine whether the log record may 
		 * leave the queue already.
		 */
		public long getDelay(TimeUnit unit) {
			long delay = triggerTimeMillis - System.currentTimeMillis();
			return unit.convert(delay, TimeUnit.MILLISECONDS);
		}
		
		/**
		 * This method is used by the queue for sorting.
		 */
		public int compareTo(Delayed other) {
			DelayedLogEntry otherDelayedLogEntry = (DelayedLogEntry) other;
			long i = triggerTimeMillis;
			long j = (otherDelayedLogEntry).triggerTimeMillis;
			int returnValue;
			if (i < j) {
				returnValue = -1;
			} else if (i > j) {
				returnValue = 1;
			} else {
				// if timestamps are equal we sort by arrival order
				if (this.logRecordIndex < otherDelayedLogEntry.logRecordIndex) {
					returnValue = -1;
				} else if (this.logRecordIndex > otherDelayedLogEntry.logRecordIndex) {
					returnValue = 1;
				} else {
					// this should never happen
					returnValue = 0;
				}
			}
			return returnValue;
		}

		/**
		 * Equals method, just to be consistent with <code>compareTo</code>.
		 */
		public boolean equals(Object other) {
			if (other==null) {
				return false;
			}
			if (other instanceof LogReceiver) {
				return false;
			}
			DelayedLogEntry otherDelayedLogEntry = (DelayedLogEntry) other;
			return ( otherDelayedLogEntry.triggerTimeMillis == triggerTimeMillis &&
					otherDelayedLogEntry.logRecordIndex == logRecordIndex );
		}

		long getDelayTimeMillis() {
			return delayTimeMillis;
		}
	}
	
	/**
	 * A jlog-independent representation of the log record we received
	 */
	public static class ReceivedLogRecord {
		private ILogEntry jlogRecord;
				
		public ReceivedLogRecord(ILogEntry jlogRecord) {
			this.jlogRecord = jlogRecord;
		}
		
		public Date getTimestamp() {
			return new Date((Long)jlogRecord.getField(LogField.TIMESTAMP));
		}
		
		public LogTypeHelper getLevel() {
			return (LogTypeHelper)jlogRecord.getField(LogField.ENTRYTYPE);
		}
		
		public String getSourceObject() {
			return (String)jlogRecord.getField(LogField.SOURCEOBJECT);
		}

		public String getFile() {
			return (String)jlogRecord.getField(LogField.FILE);
		}

		public int getLine() {
			return ((Integer)jlogRecord.getField(LogField.LINE)).intValue();
		}

		public String getRoutine() {
			return (String)jlogRecord.getField(LogField.ROUTINE);
		}

		public String getHost() {
			return (String)jlogRecord.getField(LogField.HOST);
		}

		public String getProcess() {
			return (String)jlogRecord.getField(LogField.PROCESS);
		}

		public String getLogID() {
			return (String)jlogRecord.getField(LogField.LOGID);
		}

		public String getThread() {
			return (String)jlogRecord.getField(LogField.THREAD);
		}

		public String getMessage() {
			return (String)jlogRecord.getField(LogField.LOGMESSAGE);
		}

		// @TODO add more getter methods if required...
//		CONTEXT("Context",String.class,"Context"),
//		PRIORITY("Priority",Integer.class,"Priority"),
//		URI("URI",String.class,"URI"),
//		STACKID("Stack ID",String.class,"StackId"),
//		STACKLEVEL("Stack Level",Integer.class,"StackLevel"),
//	    AUDIENCE("Audience",String.class, "Audience"),
//	    ARRAY("Array",String.class,"Array"),
//	    ANTENNA("Antenna",String.class,"Antenna");		
	}

    /**
     * Convenience method to capture logs directly into a PrintWriter.
     * Method {@link #initialize(ORB, Manager)} must be called as a precondition.
     * Method {@link #stopCaptureLogs()} must be called to stop writing logs to <code>logWriter</code>.
     * <p>
     *  
     * @param logWriter 
     * @throws IOException
     */
    public void startCaptureLogs(final PrintWriter logWriter) throws IOException {
        startCaptureLogs(logWriter, (ThreadFactory) null);
    }
    
	/**
     * Variant of {@link #startCaptureLogs(PrintWriter)} which takes an optional ThreadFactory
     * which will be used to create the thread that reads out the log queue.
     * This method could be used if <code>LogReceiver</code> is run inside a container 
     * or as part of a ComponentClientTestCase.
	 */
	public void startCaptureLogs(final PrintWriter logWriter, ThreadFactory threadFactory) throws IOException {
        if (!isInitialized()) {
            throw new IllegalStateException("First call LogReceiver#initialize(ORB, Manager), then startCaptureLogs(PrintWriter)");
        }
		if (listenForLogs) {
            if (verbose) {
                System.out.println("Ignoring call to 'startCaptureLogs' while already capturing logs.");
            }
			return;
		}
		
		Runnable logRetriever = new Runnable() {

            public void run() {
//            	System.out.println("logRetriever.run called...");
				try {
					BlockingQueue<DelayedLogEntry> logQueue = getLogQueue();
					
					listenForLogs = true;
					// loop until "queue poison" entry is found
					while (true) {						
						try {
							// extract logs from queue
							DelayedLogEntry delayedLogEntry = logQueue.take();
							if (delayedLogEntry.isQueuePoison()) {
								if (verbose) {
									System.out.println("got queue poison, will terminate method 'startCaptureLogs'.");
								}
								break;
							}
							else {
								ILogEntry logEntry = delayedLogEntry.getLogEntry();
								String xmlLog = logEntry.toXMLString();
//								System.out.println("Yeah, got a log record: " + xmlLog);
								logWriter.println(xmlLog);
							}
						} catch (Exception e) {
							e.printStackTrace();
						}
					}
				} catch (Throwable thr) {
					System.out.println("Log receiver got exception " + thr);
					logWriter.println("Log receiver failed with exception " + thr.toString());
				} finally {
					logWriter.close();
                    // we only call stop now, so that late arriving records could still be added 
                    // to the queue during the whole waiting time.
					stop();					
				}
			}
		};
        Thread t = null;
        if (threadFactory != null) {
            t = threadFactory.newThread(logRetriever);
        }
        else {
            t = new Thread(logRetriever);
            t.setDaemon(true);
        }        
		t.start();
	}
	
	/**
     * Stops capturing logs into the PrintWriter that was provided to {@link #startCaptureLogs(PrintWriter)}
     * or any of the related methods.
     * Even though the call to this method returns immediately, all log records that are  
     * currently residing inside the queue will still be written out, waiting the due time to allow sorting them.
	 */
	public void stopCaptureLogs() {
		DelayedLogEntry queuePoison = DelayedLogEntry.createQueuePoison(getDelayMillis());
		logDelayQueue.offer(queuePoison); // this queue poison will be fetched off the queue by the thread started in 'startCaptureLogs'
	}	
}

