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

import java.util.Vector;

import com.cosylab.logging.engine.ACS.ACSLogConnectionListener;
import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.ACS.LCEngine;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * LogDetailsDispatcher reads logs from the logging NC and dispatch them 
 * to the registered listeners. 
 * <P>
 * There are 2 types of listeners to perform different kind of statistics:
 * <UL>
 * 	<LI>number listeners: they are informed about the number of logs received 
 * 						 for each log type in the 
 * 						 {@link LogDetailsDispatcher#NUMBER_LISTENERS_INTERVAL}
 * 						 time interval
 *  <LI>log listeners: they are informed for the type each received log // TODO
 * </UL>
 *
 * @author acaproni
 * @since ACS 8.1.0
 */
public class LogDetailsDispatcher implements 
ACSRemoteErrorListener,
ACSRemoteLogListener,
ACSRemoteRawLogListener,
ACSLogConnectionListener, 
Runnable {

	/**
	 * The interval of time (seconds) to notify listener of numbers
	 * 
	 * @see LogNumbersListener
	 */
	public static final int NUMBER_LISTENERS_INTERVAL=10;
	
	/**
	 * The number of each received log in the <code>NUMBER_LISTENERS_INTERVAL</code>
	 * time interval.
	 * <BR>
	 * One entry for each log type.
	 */
	private final int[] receivedLogNums = new int[LogTypeHelper.values().length];
	
	/**
	 * The total number of logs received  since the begging of the execution
	 * <BR>
	 * One entry for each log type.
	 */
	private final long[] totalLogs = new long[LogTypeHelper.values().length];
	
	/**
	 * The longest XML log received since the begging of the execution
	 */
	private int longestLogSize=Integer.MIN_VALUE;
	
	/**
	 * The shortest XML log received since the begging of the execution
	 */
	private int shortestLogSize=Integer.MAX_VALUE;
	
	/**
	 * Number of errors parsing logs since the begging of the execution
	 */
	private int errors=0;
	
	/**
	 * The total numbers read since when the application started
	 */
	private final TotalStatsData totData=new TotalStatsData();
	
	/**
	 * The engine to receive logs from the logging NC
	 */
	private final LCEngine engine;
	
	/**
	 * The listeners of numbers
	 */
	private final Vector<LogNumbersListener> numListeners= new Vector<LogNumbersListener>();
	
	/**
	 * The thread to notify listeners about statistics
	 */
	private final Thread notifierThread;
	
	/**
	 * Signal the thread to terminate
	 */
	private boolean terminateThread=false;
	
	/**
	 * Constructor
	 */
	public LogDetailsDispatcher() {
		// Start the thread
		notifierThread=new Thread(this);
		notifierThread.setDaemon(true);
		notifierThread.setName(this.getClass().getName());
		notifierThread.start();
		
		// Create and start the engine
		engine = new LCEngine();
		engine.addLogConnectionListener(this);
		engine.addLogErrorListener(this);
		engine.addLogListener(this);
		engine.addRawLogListener(this);
		engine.connect();
	}

	@Override
	public void errorReceived(String xml) {
		errors++;
		totData.updateErrors(errors);
	}

	@Override
	public void xmlEntryReceived(String xmlLogString) {
		if (xmlLogString==null || xmlLogString.length()==0) {
			return;
		}
		int sz=xmlLogString.length();
		boolean changed=false;
		if (sz<shortestLogSize) {
			shortestLogSize=sz;
			changed=true;
		}
		if (sz>longestLogSize) {
			longestLogSize=sz;
			changed=true;
		}
		if (changed) {
			totData.updateSizes(shortestLogSize, longestLogSize);
		}
	}

	@Override
	public void acsLogConnConnecting() {}

	@Override
	public void acsLogConnDisconnected() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void acsLogConnEstablished() {}

	@Override
	public void acsLogConnLost() {
		// TODO Auto-generated method stub
	}

	@Override
	public void acsLogConnSuspended() {}

	@Override
	public void acsLogsDelay() {}

	@Override
	public void reportStatus(String status) {}

	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		System.out.println("received "+logEntry.getType());
		synchronized (totalLogs) {
			totalLogs[logEntry.getType().ordinal()]++;
			totData.updateTotalLogs(totalLogs);
		}
		synchronized (receivedLogNums) {
			receivedLogNums[logEntry.getType().ordinal()]++;
		}
	}
	
	/**
	 * Add a {@link LogNumbersListener} listener to be notified about incoming
	 * logs statistics
	 * 
	 * @param listener The listener to add
	 */
	public void addNumsListener(LogNumbersListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		synchronized (numListeners) {
			if (!numListeners.contains(listener)) {
				numListeners.add(listener);
			}
		}
	}
	
	/**
	 * Remove a {@link LogNumbersListener} listener.
	 * 
	 * @param listener The listener to remove
	 */
	public void removeNumsListener(LogNumbersListener listener) {
		if (listener==null) {
			return;
		}
		synchronized (numListeners) {
			numListeners.remove(listener);
		}
	}

	/**
	 * The thread notifying listeners about statistics
	 */
	@Override
	public void run() {
		while (!terminateThread) {
			try {
				Thread.sleep(NUMBER_LISTENERS_INTERVAL*1000);
			} catch (InterruptedException ie) {
				continue;
			}
			synchronized (numListeners) {
				synchronized(receivedLogNums) {
					for (LogNumbersListener listener:numListeners) {
						listener.recvLogs(receivedLogNums, NUMBER_LISTENERS_INTERVAL);

					}
					for (int t=0; t<receivedLogNums.length; t++) {
						receivedLogNums[t]=0;
					}
				}
			}
			synchronized (numListeners) {
				synchronized(totData) {
					for (LogNumbersListener listener:numListeners) {
						listener.totalData(totData);
					}
				}
			}
		}
	}
	
	/**
	 * Close the engine and free the resources
	 */
	public void close() {
		terminateThread=true;
		notifierThread.interrupt();
		engine.close(true);
	}
}
