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

import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.Vector;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.util.IsoDateFormat;

import com.cosylab.logging.engine.ACS.ACSListenersDispatcher;
import com.cosylab.logging.engine.ACS.ACSLogRetrieval;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.audience.Audience;
import com.cosylab.logging.engine.audience.OperatorAudience;
import com.cosylab.logging.engine.audience.Audience.AudienceInfo;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;

import junit.framework.TestCase;

/**
 * A class testing the filtering by mode performed by 
 * <code>ACSLogretrieval</code>.
 * The mode is defined in <code>log_audience</code> ILD module.
 * 
 * The test is done by checking what logs <code>ACSLogRetrieval</code> discards
 * i.e. the <code>LCEngine</code> is not used here as it is stressed by
 * other tests.
 * This class is the listener for the log and the XMLs that <code>ACSLogRetrieval</code>
 * sends to the listeners throw the <code>ACSListenersDispatcher</code>.
 * 
 * @author acaproni
 *
 */
public class EngineAudienceTest extends TestCase implements  ACSRemoteLogListener, ACSRemoteRawLogListener {

	/**
	 * The timeout (secs) to wait before giving up waiting for logs
	 */
	private final int TIMEOUT = 60;
	
	// The header and footer to create log to fill the caches
	public static final String logHeaderStr = " TimeStamp=\"";
	public static final String logBodyStr = "\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\" Audience=\"";
	public static final String logEndOfBodyStr="\"><![CDATA[";
	public static final String logFooterStr = "]]></";
	
	/** 
	 * The object whose filtering capabilities we want to test
	 */
	private ACSLogRetrieval logRetieval=null;
	
	/**
	 * / The dispatcher used by <code>ACSLogRetrieval</code> to send logs to the
	 * registered listeners.
	 */
	private ACSListenersDispatcher dispatcher=null;
	
	// The number of logs received in the callbacks
	private int numOfReceivedLogs;
	private int numOfReceivedXMLLogs;
	
	// The audience under testing 
	// At the present only OPERATOR is implemented but the test has to check
	// also the NO_AUDIENCE case
	private Audience audience;
	
	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		numOfReceivedLogs=0;
		numOfReceivedXMLLogs=0;
		dispatcher = new ACSListenersDispatcher();
		assertNotNull(dispatcher);
		logRetieval = new ACSLogRetrieval(dispatcher,false);
		assertNotNull(logRetieval);
		logRetieval.setFilters(null);
		dispatcher.addLogListener(this);
		dispatcher.addRawLogListener(this);
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		logRetieval.close(true);
		dispatcher=null;
		logRetieval=null;
		super.tearDown();
	}
	
	/**
	 * Test the setting of the mode
	 */
	public void testSetMode() throws Exception {
		// Set valid modes
		logRetieval.setAudience(AudienceInfo.OPERATOR.getAudience());
		assertEquals(AudienceInfo.OPERATOR.getAudience(), logRetieval.getAudience());
		logRetieval.setAudience(AudienceInfo.SCILOG.getAudience());
		assertEquals(AudienceInfo.SCILOG.getAudience(), logRetieval.getAudience());
		logRetieval.setAudience(AudienceInfo.ENGINEER.getAudience());
		assertEquals(AudienceInfo.ENGINEER.getAudience(), logRetieval.getAudience());
		
		// Setting a null value should throw an exception
		try {
			logRetieval.setAudience(null);
		} catch (Exception e) {
			// OK
			return;
		}
		throw new Exception("Null shuold not be accepted by ACSLogRetrieval.setAudience()");
	}
	
	/**
	 * Test the filtering for OPERATOR.
	 * 
	 * The logs that should not be filtered are those that:
	 *  - have a level of WARINING or greater
	 *  - have the audience set to OPERATOR 
	 *  
	 *  The test is done in 2 times:
	 *   1. a collection of logs with no audience is tested
	 *   2. a collection of logs with different values for audience 
	 *      is tested
	 *  
	 * @throws Exception
	 */
	public void testOperatorModeFiltering() throws Exception {
		audience=AudienceInfo.OPERATOR.getAudience();
		logRetieval.setAudience(audience);
		//
		// Step 1: generate a collection of logs with no audience
		//
		
		// Generate 1000 log of each type
		Collection<ILogEntry> logs = new Vector<ILogEntry>();
		for (LogTypeHelper logType: LogTypeHelper.values()) {
			Collection<ILogEntry> tempLogs = CacheUtils.generateLogsType(1000, logType);
			logs.addAll(tempLogs);
		}
		assertEquals(1000*LogTypeHelper.values().length, logs.size());
		for (ILogEntry log: logs) {
			logRetieval.addLog(log.toXMLString());
		}
		// Wait until logRetrieval publish all the logs
		assertFalse("Timeout waiting for logs", waitForLogs());
		// Check the received logs
		assertEquals(logs.size(), numOfReceivedXMLLogs);
		assertEquals(1000*5,numOfReceivedLogs);
		
		// 
		// Step 2: test a collection of logs with the audience
		//
		ACSLogParser parser = ACSLogParserFactory.getParser();
		assertNotNull(parser);
		SimpleDateFormat df = new IsoDateFormat();
		assertNotNull(df);
		
		// Generate 2 logs for each type.
		// only one of them with audience OPERATOR
		logs.clear();
		for (LogTypeHelper logType: LogTypeHelper.values()) {
			Date dt = new Date(System.currentTimeMillis());
			StringBuffer dateSB = new StringBuffer();
			FieldPosition pos = new FieldPosition(0);
			df.format(dt,dateSB,pos);
	
			StringBuilder logOperatorStr = new StringBuilder("<");
			StringBuilder logNoOperatorStr = new StringBuilder();
			
			// HSO 2009-11 No idea why this level translation is needed, 
			// but anyway I add the new level DELOUSE because it is similar to TRACE.
			// (see COMP-3749 : JDK levels FINER and FINEST were previously mapped to TRACE, while now FINER maps to DELOUSE).
			if (logType==LogTypeHelper.TRACE || logType==LogTypeHelper.DELOUSE) {
				logType=LogTypeHelper.INFO;
			}
			logOperatorStr.append(logType.logEntryType);
			logOperatorStr.append(logHeaderStr);
			logOperatorStr.append(dateSB.toString());
			logOperatorStr.append(logBodyStr);
			
			// Insert the audience
			logNoOperatorStr = new StringBuilder(logOperatorStr);
			logOperatorStr.append(alma.log_audience.OPERATOR.value);
			logNoOperatorStr.append(alma.log_audience.DEVELOPER.value);
			
			logOperatorStr.append(logEndOfBodyStr);
			logNoOperatorStr.append(logEndOfBodyStr);
			logOperatorStr.append("LOG Txt>");
			logNoOperatorStr.append("LOG Txt>");
			logOperatorStr.append(logFooterStr);
			logNoOperatorStr.append(logFooterStr);
			logOperatorStr.append(logType.logEntryType);
			logNoOperatorStr.append(logType.logEntryType);
			logOperatorStr.append('>');
			logNoOperatorStr.append('>');
			ILogEntry logOp = parser.parse(logOperatorStr.toString());
			logs.add(logOp);
			ILogEntry logNoOp = parser.parse(logNoOperatorStr.toString());
			logs.add(logNoOp);
		}
		assertEquals(2*LogTypeHelper.values().length, logs.size());
		
		numOfReceivedLogs=0;
		numOfReceivedXMLLogs=0;
		
		for (ILogEntry log: logs) {
			logRetieval.addLog(log.toXMLString());
		}
		assertFalse("Timeout waiting for logs", waitForLogs());
		
		assertEquals(logs.size(), numOfReceivedXMLLogs);
		assertEquals(10, numOfReceivedLogs);
	}
	
	
	/**
	 * Test the case of ENGINEER i.e. no filtering
	 * 
	 * @throws Exception
	 */
	public void testNoAudienceModeFiltering() throws Exception {
		audience=AudienceInfo.ENGINEER.getAudience();
		logRetieval.setAudience(audience);
		Collection<ILogEntry> logs = CacheUtils.generateLogs(10000);
		for (ILogEntry log: logs) {
			logRetieval.addLog(log.toXMLString());
		}
		assertEquals(10000, logs.size());
		assertFalse("Timeout waiting for logs", waitForLogs());
		// All logs received or timeout
		assertEquals(numOfReceivedXMLLogs, logs.size());
		assertEquals(numOfReceivedLogs, logs.size());
	}
	
	/**
	 * Wait until the logRetrieval publishes all the logs
	 * 
	 * @return true in case of timeout
	 */
	private boolean waitForLogs() {
		int timeout=0;
		int logsInQueue=0;
		while (timeout<TIMEOUT && logRetieval.hasPendingEntries()) {
			try {
				Thread.sleep(250);
			} catch (InterruptedException ie) {
				continue;
			}
			if (logRetieval.size()!=logsInQueue) {
				logsInQueue=logRetieval.size();
			} else {
				timeout++;
			}
		}
		// We need to wait some more time because there is a critical race
		// between the time the entry is removed from the EmgineCache
		// and the time it is sent to listeners.
		//
		// @see EngineCache.size()
		try {
			Thread.sleep(1500);
		} catch (InterruptedException ie) { }
		return timeout>=TIMEOUT;
	}
	
	/**
	 * The counter <code>numOfReceivedXMLLogs</code> is incremented for each received log
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener#xmlEntryReceived(java.lang.String)
	 */
	@Override
	public void xmlEntryReceived(String xmlLogString) {
		// Filtering does not apply to XML
		numOfReceivedXMLLogs++;
	}

	/** 
	 * 
	 * The counter <code>numOfReceivedLogs</code> is incremented for each received log.
	 * The method write a message in the stdout if the received log does not match
	 * with the <code>audience</code>
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#logEntryReceived(com.cosylab.logging.engine.log.ILogEntry)
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		numOfReceivedLogs++;
		if (audience instanceof OperatorAudience) {
			
			if (logEntry.getType().ordinal()>=LogTypeHelper.WARNING.ordinal()) {
				return;
			}
			String logStrAudience = (String)logEntry.getField(LogField.AUDIENCE);
			String logAudience=alma.log_audience.OPERATOR.value;
			if (!logStrAudience.equals(logAudience)) {
				System.out.println("This log did not match for OPERATOR audience: "+logEntry.toString());
			}
		}
	}
}
