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
import java.util.Date;
import java.util.Vector;

import junit.framework.TestCase;

import com.cosylab.logging.engine.ACS.ACSListenersDispatcher;
import com.cosylab.logging.engine.ACS.ACSLogRetrieval;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;
import com.cosylab.logging.engine.log.LogField;

import alma.ACSLoggingLog.LogBinaryRecord;
import alma.ACSLoggingLog.NameValue;
import alma.acs.logging.engine.utils.IResourceChecker;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.StopWatch;

/**
 * A class to test the dispatching of logs in XML and binary format.
 * It tests the flow of the logs between they are added in the ACSLogRetrieval 
 * till the moment they are published to the listeners.
 * 
 * @author acaproni
 *
 */
public class LogDispatcherTest extends TestCase {

	/**
	 * This class is used to check the dynamic change of the discard 
	 * level depending on the amount of free memory
	 * 
	 * @author acaproni
	 *
	 */
	public class Checker implements IResourceChecker {
		
		public volatile long freemMem=Long.MAX_VALUE;

		@Override
		public long getTotalFreeMemory() {
			return freemMem;
		}
		
	}
	
	/**
	 * The listener for logs.
	 * <P>
	 * Whenever a log is received a number is increased to know how much logs
	 * have been received.
	 * 
	 * @author acaproni
	 *
	 */
	public class LogsRecv implements ACSRemoteLogListener, ACSRemoteRawLogListener {
		/**
		 * The method receiving logs
		 * 
		 * @param logEntry
		 * @see com.cosylab.logging.engine.ACS.ACSListenersDispatcher
		 */
		public void logEntryReceived(ILogEntry logEntry) {
			logsReceived++;
		}
		
		/**
		 * The method receiving RAW (XML) logs
		 * 
		 * @param logEntry
		 * @see com.cosylab.logging.engine.ACS.ACSListenersDispatcher
		 */
		public void xmlEntryReceived(String xmlLogString) {
			rawLogsReceived++;
		}
	}

	private final int LOGS_NUMBER=50000;
	
	// The object receiving logs
	private LogsRecv logRecv = null;
	
	// The logs to add to the retrieval and the get from the listener
	private Collection<ILogEntry> logs=null;
	
	// The timer for logs
	private StopWatch stopWatch = new StopWatch();
	
	private int logsReceived=0;
	private int rawLogsReceived=0;
	
	/**
	 * Constructor
	 *
	 */
	public LogDispatcherTest() {
		super("LogDispatcherTest");
	}
	
	/**
	 * Constructor
	 *
	 */
	public LogDispatcherTest(String str) {
		super(str);
	}
	
	/**
	 * @see junit.framework.TestCase
	 */
	protected void setUp() throws Exception {
		super.setUp();
		// Create a collection of logs to dispatch
		logs = CacheUtils.generateLogs(LOGS_NUMBER);
		assertEquals("Lengths differ",logs.size(),LOGS_NUMBER);
		logRecv = new LogsRecv();
		assertNotNull("Null object to receive logs",logRecv);
		logsReceived=0;
		rawLogsReceived=0;
	}
	
	/**
	 * @see junit.framework.TestCase
	 */
	protected void tearDown() throws Exception {
		assertNotNull("The collection of logs is null!",logs);
		logs.clear();
		logs=null;
		logRecv = null;
		super.tearDown();
	}
	
	/**
	 * Test the dispatching of XML logs
	 * 
	 * @throws Exception
	 */
	public void testXMLDispatch() throws Exception {
		// Add the name of the test in the additional data
		for (ILogEntry log: logs) {
			log.addData("TestType", "XML");
		}
		ACSListenersDispatcher listenerDispatcher = new ACSListenersDispatcher();
		assertNotNull("Null listener dispatcher!",listenerDispatcher);
		ACSLogRetrieval logDispatcher = new ACSLogRetrieval(listenerDispatcher,false);
		assertNotNull("Null log dispatcher!",logDispatcher);
		listenerDispatcher.addLogListener(logRecv);
		listenerDispatcher.addRawLogListener(logRecv);
		stopWatch.reset();
		for (ILogEntry log: logs) {
			logDispatcher.addLog(log.toXMLString());
		}
		waitProcessingComplete(logDispatcher,300);
		long xmlTime=stopWatch.getLapTimeMillis()/1000;
		//String str ="Time to publish "+LOGS_NUMBER+" XML logs: "+xmlTime + " seconds.\n";
		//System.out.print(str);
		assertEquals("XML case: logs sent and logs received differ", LOGS_NUMBER, logsReceived);
		assertEquals("XML case: logs sent and raw logs received differ", LOGS_NUMBER, rawLogsReceived);
		logDispatcher.close(true);
	}
	
	/**
	 * Test the dispatching of binary logs
	 * 
	 * @throws Exception
	 */
	public void testBinaryDispatch() throws Exception {
		for (ILogEntry log: logs) {
			log.addData("TestType", "Binary");
		}
		ACSListenersDispatcher listenerDispatcher = new ACSListenersDispatcher();
		assertNotNull("Null listener dispatcher!",listenerDispatcher);
		ACSLogRetrieval logDispatcher = new ACSLogRetrieval(listenerDispatcher,true);
		assertNotNull("Null log dispatcher!",logDispatcher);
		listenerDispatcher.addLogListener(logRecv);
		listenerDispatcher.addRawLogListener(logRecv);
		
		stopWatch.reset();
		for (ILogEntry log: logs) {
			logDispatcher.addLog(this.toCacheString(log));
		}
		waitProcessingComplete(logDispatcher,300);
		long binTime=stopWatch.getLapTimeMillis()/1000;
		//String str="Time to publish "+LOGS_NUMBER+" binary logs: "+binTime + " seconds.\n";
		//System.out.print(str);
		assertEquals("Binary case: logs sent and logs received differ", LOGS_NUMBER, logsReceived);
		assertEquals("Binary case: logs sent and raw logs received differ", LOGS_NUMBER, rawLogsReceived);
		logDispatcher.close(true);
	}
		
	/**
	 * Wait until all the logs are processed or throws an exception
	 * if the passed timeout elapsed.
	 * 
	 * @param processor The {@link ACSLogRetrieval}
	 * @param timeout The timeout (sec) to wait for the entries to be consumed
	 * @throws Exception If a timeout occurs
	 */
	private void waitProcessingComplete(ACSLogRetrieval processor, final int timeout) throws Exception {
		if (timeout<=0) {
			throw new IllegalArgumentException("The timeout must be greater then 0");
		}
		long endTime = System.currentTimeMillis()+timeout*1000;
		while (processor.hasPendingEntries()) {
			if (System.currentTimeMillis()>endTime) {
				throw new Exception("A timeout occurredTimeout!");
			}
			try {
				Thread.sleep(50);
			} catch (Exception e) {
				continue;
			}
		}
	}
	
	/**
	 * Build the string for the cache out of an ILogEntry for the case of
	 * binary logs
	 * 
	 * @param log The log to get the cache string in binary format
	 * @return The cache string (binary format)
	 */
	private synchronized String toCacheString(ILogEntry log) throws Exception {
		LogBinaryRecord logBin=convertLogToBinary(log);
		String str=null;
		try {
			str=com.cosylab.logging.engine.ACS.CacheUtils.toCacheString(logBin);
		} catch (Throwable t) {
			t.printStackTrace(System.err);
			System.err.println("Log with error "+logBin.toString());
		}
		return str;
		
	}
	
	/**
	 * Convert a ILogEntry into a binary log
	 * 
	 * @param The log to convert
	 * @return The binary log
	 * 
	 */
	private LogBinaryRecord convertLogToBinary(ILogEntry log) throws Exception {
		LogBinaryRecord logBin = new LogBinaryRecord();
		logBin.Audience=(String)log.getField(LogField.AUDIENCE);
		logBin.File=(String)log.getField(LogField.FILE);
		logBin.Host=(String)log.getField(LogField.HOST);
		Integer line =(Integer)log.getField(LogField.LINE);
		if (line!=null) {
			logBin.Line=line;
		} else {
			logBin.Line=0;
		}
		logBin.LogContext=(String)log.getField(LogField.CONTEXT);
		logBin.LogId=(String)log.getField(LogField.LOGID);
		logBin.MsgData=(String)log.getField(LogField.LOGMESSAGE);
		Integer priority=(Integer)log.getField(LogField.PRIORITY);
		if (priority!=null) {
			logBin.Priority=priority;
		} else {
			logBin.Priority=0;
		}
		logBin.Process=(String)log.getField(LogField.PROCESS);
		logBin.Routine=(String)log.getField(LogField.ROUTINE);
		logBin.SourceObject=(String)log.getField(LogField.SOURCEOBJECT);
		logBin.StackId=(String)log.getField(LogField.STACKID);
		Integer stackL=(Integer)log.getField(LogField.STACKLEVEL);
		if (stackL!=null) {
			logBin.StackLevel=stackL;
		} else {
			logBin.StackLevel=0;
		}
		logBin.Thread=(String)log.getField(LogField.THREAD);
		final Date date = new Date((Long)log.getField(LogField.TIMESTAMP));
		logBin.TimeStamp=IsoDateFormat.formatDate(date);
		logBin.type=(short)log.getType().ordinal();
		logBin.Uri=(String)log.getField(LogField.URI);
		if (log.hasDatas()) {
			Vector<AdditionalData> data=log.getAdditionalData();
			logBin.log_data=new NameValue[data.size()];
			for (int t=0; t<data.size(); t++) {
				AdditionalData d=data.get(t);
				logBin.log_data[t]=new NameValue(d.name,d.value);
			}
		}
		return logBin;
	}
	
	/**
	 * Test setting and getting of the discard level.
	 * 
	 * @throws Exception
	 */
	public void testGetSetDiscardLevel() throws Exception {
		ACSListenersDispatcher listenerDispatcher = new ACSListenersDispatcher();
		assertNotNull("Null listener dispatcher!",listenerDispatcher);
		ACSLogRetrieval logDispatcher = new ACSLogRetrieval(listenerDispatcher,false);
		assertNotNull("Null log dispatcher!",logDispatcher);
		
		logDispatcher.setDiscardLevel(null);
		assertNull(logDispatcher.getDiscardLevel());
		
		for (LogTypeHelper logType: LogTypeHelper.values()) {
			logDispatcher.setDiscardLevel(logType);
			assertEquals(logType, logDispatcher.getDiscardLevel());
		}
		logDispatcher.close(true);
	}
	
	/**
	 * Test the changing of the discard level by simulating
	 * a low memory situation 
	 * @throws Exception
	 */
	public void testDynamicDiscardLevel() throws Exception {
		Checker checker = new Checker();
		assertNotNull(checker);
		ACSListenersDispatcher listenerDispatcher = new ACSListenersDispatcher();
		assertNotNull("Null listener dispatcher!",listenerDispatcher);
		ACSLogRetrieval logDispatcher = new ACSLogRetrieval(listenerDispatcher,false,null,checker);
		assertNotNull("Null log dispatcher!",logDispatcher);
		
		// Check increasing of the discard level
		logDispatcher.setDiscardLevel(null);
		checker.freemMem=Long.MAX_VALUE;
		// Changes of the discard level should happens every 2 seconds
		logDispatcher.enableDynamicDiscarding(8192, 1024, 2);
		// When the available memory is greater then the threshold,
		// the discard level does not change
		try {
			Thread.sleep(5);
		} catch (InterruptedException ie) {}
		assertNull(logDispatcher.getActualDiscardLevel());
		assertNull(logDispatcher.getDiscardLevel());
		
		// The index of the next log type
		int type=0;
		
		// Time to wait for the next change before giving up
		int timeout=10;
		
		// start dynamic changes of discard level
		checker.freemMem=4096; 
		while (logDispatcher.getActualDiscardLevel()!=LogTypeHelper.values()[LogTypeHelper.values().length-1]) {
			long to = System.currentTimeMillis()+1000*timeout;
			boolean found=false;
			while (!found && System.currentTimeMillis()<to) {
				found=logDispatcher.getActualDiscardLevel()==LogTypeHelper.values()[type];
				Thread.yield();
			}
			assertTrue("timeout waiting for level "+LogTypeHelper.values()[type],found);
			// Original discard level does not change
			assertNull(logDispatcher.getDiscardLevel());
			type++;
		}
		// Now the level is the topmost and it should not change...
		try {
			Thread.sleep(10000);
		} catch (InterruptedException ie) {}
		assertEquals(LogTypeHelper.values()[LogTypeHelper.values().length-1], logDispatcher.getActualDiscardLevel());
		assertNull(logDispatcher.getDiscardLevel());
		
		// Now check the decreasing
		
		// First set the free memory to a value between the threshold and the damping
		checker.freemMem=8192+512;
		// The level does not change
		try {
			Thread.sleep(10000);
		} catch (InterruptedException ie) {}
		assertEquals(LogTypeHelper.values()[LogTypeHelper.values().length-1], logDispatcher.getActualDiscardLevel());
		assertNull(logDispatcher.getDiscardLevel());
		
		// Increase the free memory ===> the level start decreasing
		type=LogTypeHelper.values()[LogTypeHelper.values().length-1].ordinal();
		checker.freemMem=2*8192;
		while (logDispatcher.getActualDiscardLevel()!=null) {
			long to = System.currentTimeMillis()+1000*timeout;
			boolean found=false;
			while (!found && System.currentTimeMillis()<to) {
				if (type>=0) {
					found=logDispatcher.getActualDiscardLevel()==LogTypeHelper.values()[type];
				} else {
					found=logDispatcher.getActualDiscardLevel()==null;
				}
				Thread.yield();
			}
			if (type>=0) {
				assertTrue("timeout waiting for level "+LogTypeHelper.values()[type],found);
			}
			// Original discard level does not change
			assertNull(logDispatcher.getDiscardLevel());
			type--;
		}
		assertNull(logDispatcher.getActualDiscardLevel());
		logDispatcher.close(true);
	}
}
