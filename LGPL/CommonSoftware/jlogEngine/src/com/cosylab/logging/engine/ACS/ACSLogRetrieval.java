/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
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

/** 
 * @author  acaproni   
 * @version $Id: ACSLogRetrieval.java,v 1.42 2010/06/30 15:12:06 acaproni Exp $
 * @since    
 */

package com.cosylab.logging.engine.ACS;

import java.util.Timer;
import java.util.TimerTask;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.logging.engine.utils.IResourceChecker;
import alma.acs.logging.engine.utils.ResourceChecker;

import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.LogMatcher;
import com.cosylab.logging.engine.cache.EngineCache;
import com.cosylab.logging.engine.cache.ILogQueueFileHandler;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * <code>ACSLogRetrieval</code> stores the XML string (or a String in case of binary logs) 
 * representing logs on a file when the engine is not able to follow the flow 
 * of the incoming logs 
 * The strings are stored on disk and the logs published to
 * the listeners when there is enough CPU available.
 * <P> 
 * <code>ACSLogRetrieval</code> allows to set the rate (i.e. number of logs per second) for the logs 
 * to receive and push in cache. 
 * All the logs received after this limit has been reached are discarded regardless of their content.
 * This option <i>must be used very carefully</i> because can cause loss of logs.
 * <P>
 * It also allows to set the rate i.e. number of logs per second) for the logs read from the cache and published to listener.
 * When the limit has been reached, no logs are pushed anymore out of the cache until the current second has elapsed.
 * This limitation <I>must be used very carefully</I> because it can cause a uncontrolled growth of the cache that could lead
 * to an out of memory.
 * <BR>
 * By default, the input and output rates are unlimited (<code>Integer.MAX_VALUE</code>)
 * <P>
 * The available memory is checked every second. 
 * To avoid out of memory the user can enable the dynamic discarding by giving a threshold (in bytes).
 * If a threshold is defined, the thread that checks the amount of available memory increases the discard level.
 * To avoid oscillations, the user can define a a damping factor: the discard level is decreased when the
 * amount of available memory is greater the the threshold plus the damping.
 * 
 * 
 * @see ACSRemoteLogListener
 * @see ACSRemoteRawLogListener
 * @see ACSLogConnectionListener
 */
public class ACSLogRetrieval extends LogMatcher implements Runnable {
	
	public class RateUpdater extends TimerTask {
		
		/**
		 * The thread
		 * 
		 */
		public void run() {
			inputRate=receivedCounter;
			receivedCounter=0;
			outputRate=readCounter;
			readCounter=0;
			
			// Monitor memory usage for dynamic filtering
			
			
			// Check the amount of free memory if the dynamic discard level is enabled
			if (threshold<Integer.MAX_VALUE) {
				checkMemory(resourceChecker.getTotalFreeMemory());
			}
		}
	}
	
	// If the number of logs in endingPositions queue if greater 
	// then this number, we assume that there is a delay between the logs
	// received and those shown in the GUI
	// The thread will publish this situation to the listeners
	private static final int DELAY_NUMBER=1000;
	
	// The object to dispatch messages to the listeners
	private ACSListenersDispatcher listenersDispatcher = null;
	
	// If it is true, the thread will not publish logs 
	// to the listeners
	private volatile boolean paused=false;
	
	// Signal the thread to terminate
	private volatile boolean terminateThread=false;
	
	// Remember if the object is closed to avoid adding new logs
	private volatile boolean closed=false;
	
	/**
	 * The parser
	 */
	private ACSLogParser parser=null;
	
	/**
	 * <code>true</code> if the binary format is in use, <code>false</code> otherwise
	 */
	private final boolean binaryFormat;

	/**
	 * The cache
	 */
	private final EngineCache cache;
	
	/**
	 * The thread sending logs to the listeners
	 */
	private Thread thread;
	
	/**
	 * The timer to update the input and output rates
	 */
	private Timer timerThread;
	
	/**
	 * The max input rate (i.e. the max number of strings to push
	 * in the cache per second.
	 * <P>
	 * If the number of logs received in the current second is greater then this number, 
	 * then the strings are discarded.
	 * <P>
	 * <B>Note</B>: <code>maxinputRate</code> should be used carefully because
	 * 		it causes the loss of information
	 */
	private int maxInputRate=Integer.MAX_VALUE;
	
	/**
	 * The number of logs per second pushed in the cache.
	 * <P>
	 * This is changed by the thread every second to be
	 * equal to <code>receivedCounter</code> 
	 */
	private volatile int inputRate=0;
	
	/**
	 * Counts the number of logs received every second
	 */
	private volatile int receivedCounter; 
	
	/**
	 * The max output rate (i.e. the max number of strings to read from
	 * in the cache per second).
	 * <P>
	 * If the number of logs received in the current second is greater then this number, 
	 * then the strings are removed from the cache but discarded without being sent to
	 * the listener
	 * <P>
	 * <B>Note</B>: <code>maxOutputRate</code> should be used carefully because
	 * 		it causes the loss of information
	 */
	private int maxOutputRate=Integer.MAX_VALUE;
	
	/**
	 * The number of logs per second popped from the cache
	 * <P>
	 * This is changed by the thread every second to be
	 * equal to <code>readCounter</code> 
	 */
	private volatile int outputRate=0;
	
	/**
	 * Counts the number of logs popped every second
	 */
	private volatile int readCounter; 
	
	
	/**
	 * The threshold, in bytes, to dynamically increase the discard level.
	 * <P>
	 * The discard level is increased of one step whenever the available 
	 * memory for the application is greater then this number.
	 */
	private int threshold;
	
	/**
	 * <code>damping</code> is used to avoid oscillations in the discard level.
	 * <P>
	 * When the available memory is below the <code>threshold</code>, the
	 * discard level is immediately increased.
	 * To decrease the discard level instead the free memory must be
	 * greater then <code>threshold+damping</code>.
	 * <P>
	 * <code>damping</code> must be greater or equal to <code>0</code>.
	 */
	private int damping;
	
	/**
	 * The time (in seconds) between two adjustments of the dynamic discard level.
	 * 
	 */
	private int dynamicDiscardingTime=10;
	
	/**
	 * The time when the dynamic discard level has been updated the last time
	 * (msec)
	 */
	private long lastDiscardingUpdateTime;
	
	/**
	 * The discard level set by the user
	 */
	private LogTypeHelper userDiscardLevel=null;
	
	/**
	 * <code>resourceChecker</code> gets the details of the memory allocation
	 */
	private IResourceChecker resourceChecker = new ResourceChecker();
	
	/**
	 * Constructor
	 * 
	 * @param listenersDispatcher The object to send messages to the listeners
	 *                            Can't be null
	 * @param binFormat true if the lags are binary, 
	 *                  false if XML format is used 
	 */
	public ACSLogRetrieval(
			ACSListenersDispatcher listenersDispatcher,
			boolean binFormat) {
		if (listenersDispatcher==null) {
			throw new IllegalArgumentException("The ACSListenersDispatcher can't be null");
		}
		this.binaryFormat=binFormat;
		cache=new EngineCache(binaryFormat);
		this.listenersDispatcher=listenersDispatcher;
		initialize();
	}
	
	/**
	 * Constructor
	 * 
	 * @param listenersDispatcher The object to send messages to the listeners
	 *                            Can't be null
	 * @param binFormat true if the lags are binary, 
	 *                  false if XML format is used
	 * @param The handler for the files of the cache 
	 */
	public ACSLogRetrieval(
			ACSListenersDispatcher listenersDispatcher,
			boolean binFormat,
			ILogQueueFileHandler fileHandler) {
		this.binaryFormat=binFormat;
		cache=new EngineCache(fileHandler, binaryFormat);
		this.listenersDispatcher=listenersDispatcher;
		initialize();
	}
	
	/**
	 * Constructor
	 * 
	 * @param listenersDispatcher The object to send messages to the listeners
	 *                            Can't be null
	 * @param binFormat true if the lags are binary, 
	 *                  false if XML format is used
	 * @param filters The filters to apply to incoming logs
	 *                If <code>null</code> or empty no filters are applied 
	 */
	public ACSLogRetrieval(
			ACSListenersDispatcher listenersDispatcher,
			boolean binFormat,
			FiltersVector filters) {
		this(listenersDispatcher,binFormat);
		setFilters(filters);
	}
	
	/**
	 * A construct that allow passing a {@link ResourceChecker}.
	 * <P>
	 * This is mostly intended for testing purposes to simulate
	 * overloading or memory consumption.
	 * 
	 * @param listenersDispatcher The object to send messages to the listeners
	 *                            Can't be null
	 * @param binFormat true if the lags are binary, 
	 *                  false if XML format is used
	 * @param filters The filters to apply to incoming logs
	 *                If <code>null</code> or empty no filters are applied
	 * @param roCecher
	 */
	public ACSLogRetrieval(
			ACSListenersDispatcher listenersDispatcher,
			boolean binFormat,
			FiltersVector filters,
			IResourceChecker resCecker) {
		this(listenersDispatcher,binFormat,filters);
		if (resCecker==null) {
			throw new IllegalArgumentException("resChecker can't be null");
		}
		resourceChecker=resCecker;
	}
	
	/**
	 * Init the file and the parser
	 *
	 */
	private void initialize() {
		if (!binaryFormat) {
			try {
				parser = ACSLogParserFactory.getParser();
			} catch (Exception pce) {
				System.err.println("Error getting the parser: "+pce.getMessage());
				
				parser=null;
			}
		}
		thread = new Thread(this,"ACSLogRetrieval");
		thread.setDaemon(true);
		thread.start();
		
		timerThread = new Timer("ACSLogretrieval.RateUpdater",true);
		timerThread.schedule(new RateUpdater(), 1000, 1000);
	}
	
	/**
	 * Add a log in the file
	 * 
	 * @param XMLLogStr The XML string of the new log to add
	 */
	public void addLog(String XMLLogStr) {
		if (closed) {
			return;
		}
		if (++receivedCounter>maxInputRate) {
			// The number of logs to push in the cache exceeded the max 
			// allowable rate ==> this entry is discarded!
			return;
		}
		try {
			cache.push(XMLLogStr);
		} catch (Exception e) {
			System.err.println("Log los while inserting in cache: "+XMLLogStr);
			System.err.println("Reason: "+e.getMessage());
			listenersDispatcher.publishError("Log los while inserting in cache: "+e.getMessage());
			e.printStackTrace();
		}
	}
	
	/**
	 * The thread to read and notify the logs read from the file to the listeners
	 */
	public void run() {
		// delay is used to remember if there is a delay between the logs received
		// and those shown in the GUI.
		// We assume that such delay exists if the number of logs in queue is 
		// bigger then DELAY_NUMBER
		boolean delay=false;
		while (!terminateThread) {
			//	Check if a delay has to be notified to the listeners
			if (cache.size()>ACSLogRetrieval.DELAY_NUMBER) {
				if (!delay) {
					delay=true;
					listenersDispatcher.publishDiscarding();
				}
			} else if (delay) {
				delay=false;
				listenersDispatcher.publishConnected(true);
			}
			// Do not flush the logs if the application is paused
			if (paused) {
				try {
					Thread.sleep(250);
				} catch(InterruptedException e) {}
				continue;
			}
			if (readCounter>maxOutputRate) {
				// The number of logs read from the cache exceeded the max 
				// allowable rate ==> wait for some time until
				// readConter is cleared
				try {
					Thread.sleep(50);
				} catch(InterruptedException e) {}
				continue;
			}
			String tempStr = null;
			try {
				tempStr=cache.pop();
			} catch (InterruptedException ie) {
				continue;
			} catch (Throwable t) {
				System.err.println("Exception from cache.pop: "+t.getMessage());
				t.printStackTrace();
				continue;
			}
			if (tempStr==null) {
				// Timeout
				try {
					Thread.sleep(250);
				} catch (InterruptedException ie) {}
				continue;
			}
			if (tempStr.length()>0) {
				ILogEntry log;
				if (!binaryFormat) {
					try {
						log = parser.parse(tempStr);
					} catch (Throwable t) {
						listenersDispatcher.publishError(tempStr);
						listenersDispatcher.publishReport(tempStr);
						System.err.println("Exception parsing a log: "+t.getMessage());
						t.printStackTrace(System.err);
						continue;
					}
					publishLog(tempStr, log);
				} else {
					String xmlStr=null;
					try {
						log=CacheUtils.fromCacheString(tempStr);
						xmlStr=log.toXMLString();
					} catch (Throwable t) {
						listenersDispatcher.publishError(tempStr);
						listenersDispatcher.publishReport(tempStr);
						System.err.println("Exception parsing a log: "+t.getMessage());
						t.printStackTrace(System.err);
						continue;
					}
					readCounter++;
					publishLog(xmlStr, log);
				}
			}
		}
	}
	
	/**
	 * Send the logs to the listeners.
	 * <P>
	 * The filters are applied before sending the log to the listener.
	 * <P>
	 * XML logs are not filtered.
	 * <P>
	 * 
	 * @param xmlLog The XML (RAW) log
	 * @param log The log
	 */
	private void publishLog(String xmlLog, ILogEntry log) {
		if (xmlLog!=null) {
			try {
				listenersDispatcher.publishRawLog(xmlLog);
			} catch (Throwable t) {
				System.err.println("Error publishing XML log ["+xmlLog+"]: log lost!");
				t.printStackTrace(System.err);
				try {
					listenersDispatcher.publishReport(xmlLog);
				} catch (Throwable t2) {}
			}
		}
		if (log!=null && match(log)) {
			try {
				listenersDispatcher.publishLog(log);
			} catch (Throwable t) {
				System.err.println("Error publishing log ["+log.toString()+"]: log lost!");
				t.printStackTrace(System.err);
				try {
					listenersDispatcher.publishReport(log.toString());
				} catch (Throwable t2) {}
			}
		}
	}
	
	
	
	/**
	 * Pause/unpause the thread that publishes logs
	 * 
	 * @param pause
	 */
	public void pause(boolean pause) {
		paused=pause;
	}
	
	/**
	 * Close the threads and free all the resources
	 * 
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		closed=true;
		terminateThread=true;
		if (timerThread!=null) {
			timerThread.cancel();
		}
		if (sync) {
			try {
				thread.join();
			} catch (InterruptedException ie) {}
		}
		cache.close(sync);
	}
	
	/**
	 * Check if there are logs to be published in the cache. 
	 * 
	 * @return true if there are logs to be processed in the file
	 */
	public boolean hasPendingEntries() {
		return cache.size()!=0;
	}
	
	/**
	 * Return the number of entries in the cache
	 * 
	 * @return the number of entries in the cache
	 */
	public int size() {
		return cache.size();
	}
	
	/**
	 * Return the number of strings received in the last second.
	 * Note that the number of received strings can be greater then the max number of
	 * input strings.
	 * 
	 * @return The number of strings received in the last second
	 */
	public int getInputRate() {
		return inputRate;
	}

	/**
	 * 
	 * Return the number of strings read from the cache in the last second limited.
	 * It can never be greater then the max rate.
	 * 
	 * @return The number of strings read from the cache in 
	 * 			the last second
	 */
	public int getOutputRate() {
		return outputRate;
	}

	/**
	 * 
	 * @return The max number of strings pushed in the cache per second
	 */
	public int getMaxInputRate() {
		return maxInputRate;
	}

	/**
	 * Set the max number of logs to process per second.
	 * <P>
	 * All the logs in a second read after this number has been 
	 * reached are discarded i.e. never pushed in the cache.
	 * 
	 * @param maxInRate The max number of logs per second to push in cache.
	 * 					<code>Integer.MAX_VALUE</code> means unlimited
	 */
	public void setMaxInputRate(int maxInRate) {
		if (maxInRate<=0) {
			throw new IllegalArgumentException("The input rate must be greater then 0");
		}
		this.maxInputRate = maxInRate;
	}

	/**
	 * 
	 * @return The maximum number of logs published per second
	 */
	public int getMaxOutputRate() {
		return maxOutputRate;
	}

	/**
	 * Set the max number of logs to read from the cache per second.
	 * <P>
	 * All the logs in a second read after this number has been 
	 * reached are discarded i.e. never published to listeners.
	 * 
	 * @param maxOutRate The max number of logs per second to read from cache.
	 * 					<code>Integer.MAX_VALUE</code> means unlimited
	 */
	public void setMaxOutputRate(int maxOutRate) {
		if (maxOutRate<=0) {
			throw new IllegalArgumentException("The input rate must be greater then 0");
		}
		this.maxOutputRate = maxOutRate;
	}
	
	/**
	 * Enable dynamic discarding of incoming logs.
     *
	 * @param threashold The discard level is increased when the available
	 * 					memory for the application is less then the <code>threshold</code>
	 * 					(in bytes).
	 * 					<code>Integer.MAX_VALUE</code> disables this feature.
	 * @param damping The damping factor is used to avoid oscillations
	 * 					The discard level is decreased when the free memory is
	 * 					is greater then the <code>threshold</code> plus the <code>dumping</code>.
	 * @param interval The time (in seconds) between two adjustments of the 
	 * 					dynamic discard level.  
	 * 					<code>interval</code> defaults to <code>10</code>.
	 * 
	 * @see setDiscardLevel(LogTypeHelper newLevel)
	 */
	public void enableDynamicDiscarding(int threshold, int damping, int interval) {
		if (threshold<=1024) {
			throw new IllegalArgumentException("Threshold must be greater the 1024");
		}
		if (damping<0) {
			throw new IllegalArgumentException("Damping factor can't be negative");
		}
		if (interval<1) {
			throw new IllegalArgumentException("Damping factor must begreater then 1");
		}
		this.threshold=threshold;
		this.damping=damping;
		dynamicDiscardingTime=interval;
	}
	
	/**
	 * Set the discard level
	 * <P>
	 * <B>Note</B>: if dynamic filtering by memory is enabled then the discard
	 * 			level effectively used can be greater then this value.
	 * 
	 * @param newLevel The new discard level (<code>null</code> means 
	 * 					no discard level).
	 *  
	 * @see enableDynamicDiscarding(int threashold)
	 * @see getDiscardLevel()
	 */
	public void setDiscardLevel(LogTypeHelper newLevel) {
		userDiscardLevel=newLevel;
		actualDiscardLevel=newLevel;
	}
	
	/**
	 * Return the discard level set by the user
	 * 
	 * @return the discardLevel
	 * 
	 * @see etDiscardLevel(LogTypeHelper newLevel)
	 */
	public LogTypeHelper getDiscardLevel() {
		return userDiscardLevel;
	}
	
	/**
	 * Check the available memory against the threshold and the damping factor
	 * to increase/decrease the discard level.
	 * 
	 * @param freeMem The amount of available memory to the application
	 * 					in bytes.
	 */
	private void checkMemory(long freeMem) {
		if (
				threshold==Integer.MAX_VALUE || 
				System.currentTimeMillis()<lastDiscardingUpdateTime+1000*dynamicDiscardingTime) {
			// Dynamic adjustment disabled or delayed
			return;
		}
		// First check if the discard level must be increased
		if (freeMem<threshold) {
			if (actualDiscardLevel==LogTypeHelper.values()[LogTypeHelper.values().length-1]) {
				lastDiscardingUpdateTime=System.currentTimeMillis();
				return;
			}
			if (actualDiscardLevel==null) {
				actualDiscardLevel=LogTypeHelper.TRACE;
			} else {
				actualDiscardLevel=LogTypeHelper.values()[actualDiscardLevel.ordinal()+1];
			}
			lastDiscardingUpdateTime=System.currentTimeMillis();
			return;
		}
		// Then check if the discard level can be decreased
		if (freeMem>threshold+damping) {
			if (actualDiscardLevel==userDiscardLevel) {
				// The discard level never goes below the user selected value
				lastDiscardingUpdateTime=System.currentTimeMillis();
				return;
			}
			if (actualDiscardLevel==LogTypeHelper.values()[0]) {
				actualDiscardLevel=null;
			} else {
				actualDiscardLevel=LogTypeHelper.values()[actualDiscardLevel.ordinal()-1];
			}
			lastDiscardingUpdateTime=System.currentTimeMillis();
		}
	}	
}
