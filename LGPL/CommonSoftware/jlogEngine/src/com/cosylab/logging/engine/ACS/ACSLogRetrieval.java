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
 * @version $Id: ACSLogRetrieval.java,v 1.28 2008/04/14 16:22:56 acaproni Exp $
 * @since    
 */

package com.cosylab.logging.engine.ACS;

import javax.xml.parsers.ParserConfigurationException;

import com.cosylab.logging.client.cache.CacheUtils;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;

/**
 * ACSLogRetireval stores the XML string (or a String in case of binary logs) 
 * representing logs on a file when the engine is not able to follow the flow 
 * of the incoming logs 
 * The strings are stored on disk and the logs published to
 * the listeners when there is enough CPU available.
 * 
 * It is possible to define the audience for objects of this class.
 * A special set of filters is applied for each audience.
 * The audience filtering applies only to regular log listeners (i.e. not for XML listeners).
 * 
 * In addition, <code>ACSLogretrieval</code> applies a custom set of  filters
 * before sending logs to the listeners.
 * The filters apply only to regular log listeners (i.e. not for XML listeners).
 * Custom filters are applied after audience filters.
 * 
 * @see ACSRemoteLogListener
 * @see ACSRemoteRawLogListener
 * @see ACSLogConnectionListener
 */
public class ACSLogRetrieval extends Thread {
	
	// If the number of logs in endingPositions queue if greater 
	// then this number, we ssume that there is a delay between the logs
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
	 * Set the audience.
	 * 
	 * Only the logs for the defined audience will be forwarded to the listeners.
	 * 
	 * @see <code>LCEngine.setFilters()</code>
	 */
	private EngineAudienceHelper audience=EngineAudienceHelper.NO_AUDIENCE; 
	
	/**
	 * The filters to apply before publishing logs to the listeners.
	 * The filters are not applied to XML listeners.
	 * These filters are applied after the audience.
	 */
	private FiltersVector filters=null;
	
	// The parser
	private ACSLogParser parser=null;
	
	// true if the binary format is in use, false otherwise
	private boolean binaryFormat;
	
	private EngineCache cache = new EngineCache();
		
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
		super("ACSLogRetrieval");
		if (listenersDispatcher==null) {
			throw new IllegalArgumentException("The ACSListenersDispatcher can't be null");
		}
		this.listenersDispatcher=listenersDispatcher;
		this.binaryFormat=binFormat;
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
		this.filters=filters;
	}
	
	/**
	 * Init the file and the parser
	 *
	 */
	private void initialize() {
		if (!binaryFormat) {
			try {
				parser = new ACSLogParserDOM();
			} catch (ParserConfigurationException pce) {
				parser=null;
			}
		}
		this.setName("ACSLogRetrieval");
		this.start();
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
			String tempStr = null;
			try {
				tempStr=cache.pop(250);
			} catch (InterruptedException ie) {
				continue;
			} catch (Exception e) {
				System.err.println("Exception from cache.pop: "+e.getMessage());
				e.printStackTrace();
				continue;
			}
			if (tempStr==null) {
				// Timeout
				continue;
			}
			if (tempStr.length()>0) {
				ILogEntry log;
				if (!binaryFormat) {
					try {
						log = parser.parse(tempStr);
					} catch (Exception e) {
						listenersDispatcher.publishError(tempStr);
						listenersDispatcher.publishReport(tempStr);
						System.err.println("Exception parsing a log: "+e.getMessage());
						e.printStackTrace(System.err);
						continue;
					}
					publishLog(tempStr, log);
				} else {
					String xmlStr=null;
					try {
						log=CacheUtils.fromCacheString(tempStr);
						xmlStr=log.toXMLString();
					} catch (Exception e) {
						listenersDispatcher.publishError(tempStr);
						listenersDispatcher.publishReport(tempStr);
						System.err.println("Exception parsing a log: "+e.getMessage());
						e.printStackTrace(System.err);
						continue;
					}
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
			listenersDispatcher.publishRawLog(xmlLog);
		}
		if (log==null) {
			return;
		}
		// Check the log against the audience
		if (!checkAudience(log)) {
			return;
		}
		// Check the log against custom filters
		if (filters==null || filters.applyFilters(log)) {
			listenersDispatcher.publishLog(log);
		} 
	}
	
	/**
	 * Check if a log matches with the audience
	 * 
	 * @param log The not <code>null</code> log to check
	 * @return <code>true</code> if the log matches with the defined 
	 *                           audience
	 */
	private boolean checkAudience(ILogEntry log) {
		if (log==null) {
			throw new IllegalArgumentException("The log can't be null");
		}
		switch (audience) {
		case OPERATOR: {
			if (log.getType().ordinal()>=LogTypeHelper.WARNING.ordinal()) {
				return true;
			}
			String logAudience = (String)log.getField(Field.AUDIENCE);
			return EngineAudienceHelper.OPERATOR.val.equalsIgnoreCase(logAudience);
		}
		default: {
			return true;
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
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		closed=true;
		terminateThread=true;
		if (sync) {
			while (isAlive()) {
				try {
					Thread.sleep(250);
				} catch (InterruptedException ie) {
					continue;
				}
			}
		}
		cache.close();
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
	 * Set the filters to apply to incoming logs before sending to
	 * the listeners
	 * 
	 * @param filters The filters to apply
	 *                If <code>null</code> or empty the filtering is disabled
	 */
	public void setFilters(FiltersVector filters) {
		this.filters=filters;
	}
	
	/**
	 * Set the audience
	 * 
	 * @param newAudience The new audience as defined in log_audience IDL module
	 * @see <code>LCEngine.setFilters()</code>
	 */
	public void setAudience(EngineAudienceHelper newAudience) {
		if (newAudience==null) {
			throw new IllegalArgumentException("The audience can't be null");
		}
		audience=newAudience;
	}

	/**
	 * @return the audience
	 */
	public EngineAudienceHelper getAudience() {
		return audience;
	}
	
	/**
	 * Return the number of entries in the cache
	 * 
	 * @return the number of entries in the cache
	 */
	public int size() {
		return cache.size();
	}
	
}
