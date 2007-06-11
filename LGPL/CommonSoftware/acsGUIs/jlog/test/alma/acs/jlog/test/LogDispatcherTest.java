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

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.Random;
import java.util.Vector;

import junit.framework.TestCase;

import alma.ACSLoggingLog.LogBinaryRecord;
import alma.ACSLoggingLog.LogType;
import alma.ACSLoggingLog.NameValue;
import alma.acs.util.StopWatch;

import com.cosylab.logging.engine.ACS.ACSListenersDispatcher;
import com.cosylab.logging.engine.ACS.ACSLogRetrieval;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;

/**
 * A class to the the dispatching of logs in XML and binary format.
 * It test the path of the logs betwween they are added in the ACSLogRetrieval 
 * till the moment they are published to the listeners.
 * 
 * @author acaproni
 *
 */
public class LogDispatcherTest extends TestCase {
	
	public class LogsRecv implements ACSRemoteLogListener, ACSRemoteRawLogListener {
		/**
		 * The method receiving logs
		 * 
		 * @param logEntry
		 * @see com.cosylab.logging.engine.ACS.ACSListenersDispatcher
		 */
		public void logEntryReceived(ILogEntry logEntry) {
			/*System.out.print("Log message: "+logEntry.getField(ILogEntry.FIELD_LOGMESSAGE)+", ");
			if (logEntry.hasDatas()) {
				Vector<AdditionalData>  data = logEntry.getAdditionalData();
				for (AdditionalData d: data) {
					System.out.println("[Name="+d.getName()+", Value="+d.getValue()+"]");
				}
			} else {
				System.out.println("No additional data");
			}*/
			logsReceived++;
		}
		
		/**
		 * The method receiving RAW (XML) logs
		 * 
		 * @param logEntry
		 * @see com.cosylab.logging.engine.ACS.ACSListenersDispatcher
		 */
		public void xmlEntryReceived(String xmlLogString) {
			//System.out.println("RAW: "+xmlLogString);
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
		super.tearDown();
		assertNotNull("The collection of logs is null!",logs);
		logs.clear();
		logs=null;
		logRecv = null;
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
		waitProcessingComplete(logDispatcher);
		long xmlTime=stopWatch.getLapTimeMillis()/1000;
		//String str ="Time to publish "+LOGS_NUMBER+" XML logs: "+xmlTime + " seconds.\n";
		//System.out.print(str);
		assertEquals("XML case: logs sent and logs received differ", LOGS_NUMBER, logsReceived);
		assertEquals("XML case: logs sent and raw logs received differ", LOGS_NUMBER, rawLogsReceived);
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
		waitProcessingComplete(logDispatcher);
		long binTime=stopWatch.getLapTimeMillis()/1000;
		//String str="Time to publish "+LOGS_NUMBER+" binary logs: "+binTime + " seconds.\n";
		//System.out.print(str);
		assertEquals("Binary case: logs sent and logs received differ", LOGS_NUMBER, logsReceived);
		assertEquals("Binary case: logs sent and raw logs received differ", LOGS_NUMBER, rawLogsReceived);
	}
		
	/**
	 * Wait until all the logs are processed
	 * 
	 * @param processor
	 */
	private void waitProcessingComplete(ACSLogRetrieval processor) {
		while (processor.hasPendingEntries()) {
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
	private synchronized String toCacheString(ILogEntry log) {
		LogBinaryRecord logBin=convertLogToBinary(log);
		String str=null;
		try {
			str=com.cosylab.logging.client.cache.CacheUtils.toCacheString(logBin);
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
	private LogBinaryRecord convertLogToBinary(ILogEntry log) {
		LogBinaryRecord logBin = new LogBinaryRecord();
		logBin.Audience=(String)log.getField(ILogEntry.FIELD_AUDIENCE);
		logBin.File=(String)log.getField(ILogEntry.FIELD_FILE);
		logBin.Host=(String)log.getField(ILogEntry.FIELD_HOST);
		Integer line =(Integer)log.getField(ILogEntry.FIELD_LINE);
		if (line!=null) {
			logBin.Line=line;
		} else {
			logBin.Line=0;
		}
		logBin.LogContext=(String)log.getField(ILogEntry.FIELD_CONTEXT);
		logBin.LogId=(String)log.getField(ILogEntry.FIELD_LOGID);
		logBin.MsgData=(String)log.getField(ILogEntry.FIELD_LOGMESSAGE);
		Integer priority=(Integer)log.getField(ILogEntry.FIELD_PRIORITY);
		if (priority!=null) {
			logBin.Priority=priority;
		} else {
			logBin.Priority=0;
		}
		logBin.Process=(String)log.getField(ILogEntry.FIELD_PROCESS);
		logBin.Routine=(String)log.getField(ILogEntry.FIELD_ROUTINE);
		logBin.SourceObject=(String)log.getField(ILogEntry.FIELD_SOURCEOBJECT);
		logBin.StackId=(String)log.getField(ILogEntry.FIELD_STACKID);
		Integer stackL=(Integer)log.getField(ILogEntry.FIELD_STACKLEVEL);
		if (stackL!=null) {
			logBin.StackLevel=stackL;
		} else {
			logBin.StackLevel=0;
		}
		logBin.Thread=(String)log.getField(ILogEntry.FIELD_THREAD);
		Date date = (Date)log.getField(ILogEntry.FIELD_TIMESTAMP);
		logBin.TimeStamp=com.cosylab.logging.client.cache.CacheUtils.dateFormat.format(date);
		logBin.type=LogType.from_int(log.getType().intValue());
		logBin.Uri=(String)log.getField(ILogEntry.FIELD_URI);
		if (log.hasDatas()) {
			Vector<AdditionalData> data=log.getAdditionalData();
			logBin.log_data=new NameValue[data.size()];
			for (int t=0; t<data.size(); t++) {
				AdditionalData d=data.get(t);
				logBin.log_data[t]=new NameValue(d.getName(),d.getValue());
			}
		}
		return logBin;
	}
}
