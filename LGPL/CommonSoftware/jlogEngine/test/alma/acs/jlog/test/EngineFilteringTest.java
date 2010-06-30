/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
package alma.acs.jlog.test;

import java.util.Collection;
import java.util.Vector;
import java.util.logging.Level;

import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.ACS.ACSLogConnectionListener;
import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.ACS.LCEngine;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.config.LogConfig;

/**
 * A class testing the filtering of logs in the engine.
 * 
 * @author acaproni
 *
 */
public class EngineFilteringTest 
	extends 
		ComponentClientTestCase  
	implements 
		ACSLogConnectionListener, 
		ACSRemoteLogListener, 
		ACSRemoteRawLogListener,
		ACSRemoteErrorListener {
	
	// The engine to get logs from the logging service
	private LCEngine engine;
	
	// The received logs
	private Vector<ILogEntry> receivedLogs;
	
	// The number of XML logs received
	private int xmlLogs=0;
	
	// The number of INFO's in the XML
	private int xmlInfos=0;
	
	// The timeout (in secs) to decide when all the logs has been received
	private static final int TIMEOUT=60;
	
	// The number of logs generated for testing
	private static final int NUMBER_OF_LOGS =100;
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public EngineFilteringTest() throws Exception {
		super("EngineFilteringTest");
	}

	/**
	 * Setup the environment by creating the engine with a null set of filters.
	 * The engine is disconnected because some of the tests do not need the
	 * connection alive.
	 * 
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		
		receivedLogs= new Vector<ILogEntry>();
		assertNotNull(receivedLogs);
		receivedLogs.clear();
		xmlLogs=0;
		
		xmlInfos=0;
		
		assertNotNull(m_logger);
		AcsLogger acsLogger = (AcsLogger)m_logger;
		LogConfig config = new LogConfig();
		acsLogger.configureLogging(config);
		
		
		
		engine = new LCEngine();
		assertNotNull(engine);
		engine.addLogErrorListener(this);
		engine.addLogConnectionListener(this);
		engine.addLogListener(this);
		engine.addRawLogListener(this);
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		engine.disconnect();
		super.tearDown();
	}

	/* (non-Javadoc)
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener#acsLogConnConnecting()
	 */
	@Override
	public void acsLogConnConnecting() {}

	/**
	 * Disconnected
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener#acsLogConnDisconnected()
	 */
	@Override
	public void acsLogConnDisconnected() {
		System.out.println("Disconnected");
		
	}

	/**
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener#acsLogConnEstablished()
	 */
	@Override
	public void acsLogConnEstablished() {}

	/**
	 * Connection lost: tat will show this situation
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener#acsLogConnLost()
	 */
	@Override
	public void acsLogConnLost() {
		System.out.println("Connection lost!");		
	}

	/**
	 * tat will show this situation
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener#acsLogConnSuspended()
	 */
	@Override
	public void acsLogConnSuspended() {
		System.out.println("Connection suspended");
		
	}

	/* (non-Javadoc)
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener#acsLogsDelay()
	 */
	@Override
	public void acsLogsDelay() {}

	/* (non-Javadoc)
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener#reportStatus(java.lang.String)
	 */
	@Override
	public void reportStatus(String status) {}

	/** 
	 * Executed whenever a new log is received.
	 * Filtering works here.
	 * 
	 * The received log is added to the vector
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#logEntryReceived(com.cosylab.logging.engine.log.ILogEntry)
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		synchronized (receivedLogs) {
			receivedLogs.add(logEntry);
		}
	}

	/**
	 * XML entries are not filtered!
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener#xmlEntryReceived(java.lang.String)
	 */
	@Override
	public void xmlEntryReceived(String xmlLogString) {
		if (xmlLogString.contains("testFiltering")) {
			
			xmlLogs++;
			if (xmlLogString.contains("Info") && !xmlLogString.contains("Manager")) {
				xmlInfos++;
			}
		}
	}

	/**
	 * Method executed in case of error: print a message in the stdout, 
	 * tat will show this error
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteErrorListener#errorReceived(java.lang.String)
	 */
	@Override
	public void errorReceived(String xml) {
		System.out.println("ERROR: "+xml);
	}
	
	/**
	 * Test the adding and clearing filters
	 * 
	 * @throws Exception
	 */
	public void testAddFilter() throws Exception {
		// No filters defined: getFilters() return null
		assertNull(engine.getFilters());
		
		// Create a filter
		Filter f = new Filter(LogField.ENTRYTYPE,false,LogTypeHelper.INFO.ordinal(),false);
		assertNotNull(f);
		
		// Add the filter
		engine.addFilter(f);
		assertNotNull(engine.getFilters());
		assertEquals("Sizes differ", 1, engine.getFilters().size());
		// The filters has been added as active
		assertTrue(engine.getFilters().hasActiveFilters());
		
		// Create and add another filter
		Filter f2 = new Filter(LogField.ENTRYTYPE,false,LogTypeHelper.DEBUG.ordinal(),false);
		assertNotNull(f2);
		assertEquals("Sizes differ", 1, engine.getFilters().size());
		
		// Clear the filters
		engine.clearFilters();
		assertNull(engine.getFilters());
	}
	
	/**
	 * Test adding a FiltersVector
	 */
	public void testAddFiltersVector() throws Exception {
		// No filters defined: getFilters() return null
		assertNull(engine.getFilters());
		
		// Create a filter
		Filter f = new Filter(LogField.ENTRYTYPE,false,LogTypeHelper.INFO.ordinal(),false);
		assertNotNull(f);
		
		// Create and add another filter
		Filter f2 = new Filter(LogField.ENTRYTYPE,false,LogTypeHelper.DEBUG.ordinal(),false);
		assertNotNull(f2);
		
		// Setup the filters vector
		FiltersVector filters = new FiltersVector();
		assertNotNull(filters);
		filters.addFilter(f, true);
		filters.addFilter(f2, true);
		
		// Set the filters
		engine.setFilters(filters, false);
		
		assertNotNull(engine.getFilters());
		assertEquals(filters.size(), engine.getFilters().size());
		
		// Append the filters
		engine.setFilters(filters, true);
		
		assertNotNull(engine.getFilters());
		assertEquals(filters.size()*2, engine.getFilters().size());
	}
	
	/**
	 * Set a filter and checks if the logs received are those passing a filters.
	 * <P>
	 * In this case we do not need to check the correctness of the logs received 
	 * in the listeners but only if they are received or not because it means that
	 * the engine is using the filters.
	 * <P>
	 * For this example, the test defines 1 filter based on the type of the logs.
	 * <P>
	 * The correctness of the filtering is also tested in another test because the engine 
	 * uses the same filters used by the table.
	 * 
	 * @throws Exception
	 */
	public void testFiltering() throws Exception {
		// Randomly generate the logs
		Collection<ILogEntry> logs = CacheUtils.generateLogs(NUMBER_OF_LOGS);
		assertNotNull(logs);
		assertEquals(NUMBER_OF_LOGS, logs.size());
		
		Collection<ILogEntry> flushLogs = CacheUtils.generateLogsType(100, LogTypeHelper.NOTICE);
		
		// Create a filter for the type INFO
		Filter f = new Filter(LogField.ENTRYTYPE,false,LogTypeHelper.INFO.ordinal(),false);
		assertNotNull(f);
		
		// And a filter for the source name
		Filter nameFilter = new Filter(LogField.ROUTINE,false,getName(),false);
		assertNotNull(nameFilter);
		
		// No filters exists in the engine
		assertNull(engine.getFilters());
		// Add the filters
		engine.addFilter(f);
		engine.addFilter(nameFilter);
		assertNotNull(engine.getFilters());
		assertEquals("Size differ", 2, engine.getFilters().size());
		
		// connect the engine
		engine.connect();
		// wait until the engine is connected
		int iterCount=0;
		while (iterCount<TIMEOUT && !engine.isConnected()) {
			iterCount++;
			try {
				Thread.sleep(1000);
			} catch (Exception e) {}
		}
		assertTrue(engine.isConnected());
		
		// publish all the logs and count the number of INFO logs
		int infos=0;
		for (ILogEntry log: logs) {
			AcsLogLevel level=AcsLogLevel.fromAcsCoreLevel(log.getType().acsCoreLevel);
			m_logger.log(level,(String)log.getField(LogField.LOGMESSAGE));
			if (log.getType()==LogTypeHelper.INFO) {
				infos++;
			}
		}
		
		// publish some logs to flush the cache
		//
		// For a client it is not possible to configure the cache of logs through the CDB
		// and without sending some more logs the test sometimes fails because the logs are
		// cached.
		// Is it possible to change the parameters of the logging programmatically?
		// If it is possible then it is better this second option then sending logs..
		try {
			Thread.sleep(1000);
		} catch (Exception e) {}
		System.out.println("Flushing");
		for (ILogEntry log: flushLogs) {
			AcsLogLevel level=AcsLogLevel.fromAcsCoreLevel(log.getType().acsCoreLevel);
			m_logger.log(level,(String)log.getField(LogField.LOGMESSAGE));
		}
		
		// wait till all the logs are received
		//
		// It additionally waits for TIMEOUT seconds after the last log has been 
		// received to be sure there are no further logs delayed because of caching
		// or similar problems
		int elapsed =0; // The number of seconds after the last log has been received
		int count=0; // The number of logs at the previous iteration
		while (elapsed<TIMEOUT) {
			synchronized (receivedLogs) {
				if (receivedLogs.size()!=count) {
					count= receivedLogs.size();
					elapsed=0;
					continue;
				}
			}
			// No new logs received
			try {
				Thread.sleep(1000);
			} catch (Exception e) {}
			elapsed++;
		}
		
		// If all the logs has been received then xmlLogs must be greater then NUMBER_OF_LOGS
		// No t equal because the xml logs are not filtered and there can be logs generated
		// outside of this object
		assertTrue(xmlLogs>=NUMBER_OF_LOGS);
		
		// Check if the number of received logs is as expected
		assertEquals(infos,receivedLogs.size());
		
	}
}
