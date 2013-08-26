/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2011, All rights reserved
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
import java.util.Calendar;
import java.util.Date;
import java.util.Vector;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.logging.table.reduction.LogProcessor;
import alma.acs.util.IsoDateFormat;

/**
 * A class to test the LogProcessor.
 * <P>
 * It writes strings on the stdout and rely on tat for verifying the
 * correctness of the test.
 * 
 * @author acaproni
 *
 */
public class LogProcessorTest {
	/**
	 *  The processor to test
	 */
	private final LogProcessor processor;
	
	/**
	 * The vector of logs to apply the reduction rules
	 */
	private static final Vector<ILogEntry> logs = new Vector<ILogEntry>(); 
	
	/**
	 * Constructor
	 */
	public LogProcessorTest() {
		processor = new LogProcessor();
	}
	
	public void test() {
		createLogsVector();
		System.out.println("Logs in vector BEFORE reductions");
		dumpLogs();
		processor.reduce(logs);
		System.out.println("Logs in vector AFTER reductions");
		dumpLogs();
	}
	
	private ILogEntry createLog(String logMsg) throws Exception {
		ACSLogParser parser= ACSLogParserFactory.getParser();
		
		long now = Calendar.getInstance().getTimeInMillis();
		SimpleDateFormat df = new IsoDateFormat();
		
		Date dt = new Date(now);
		StringBuffer dateSB = new StringBuffer();
		FieldPosition pos = new FieldPosition(0);
		df.format(dt,dateSB,pos);
			
		StringBuilder logStr = new StringBuilder("<Info TimeStamp=\"");
		logStr.append(dateSB.toString());
		logStr.append("\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[");
		logStr.append(logMsg);
		logStr.append("]]></Info>");
		ILogEntry newLog = parser.parse(logStr.toString());
		return newLog;
	}
	
	private void createLogsVector() {
		// A normal log
		ILogEntry log=null;
		try {
			log = createLog("Test log that will not be reduced");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		logs.add(log);
		
		// Create a log that matches
		try {
			log = createLog("Antenna PM02 in position");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		logs.add(log);
		
		// Create a log that does not match
		// It has an antenna but in a wrong position
		try {
			log = createLog("Antenna in position PM02");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		logs.add(log);
		
		// Create another log that does not match
		// No antenna
		try {
			log = createLog("Antenna in position");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		logs.add(log);
		
		// Create another log that matches
		try {
			log = createLog("Antenna DA41 in position");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		logs.add(log);
		
		// Create another log that matches
		try {
			log = createLog("Antenna DV07 in position");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		logs.add(log);
		
		// Create another log that does not match
		try {
			log = createLog("DV15 antenna in position");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		logs.add(log);
		
		System.out.println("Logs in Vector: "+logs.size());
	}
	
	/**
	 * Print the logs in the vector
	 */
	private void dumpLogs() {
		for (ILogEntry log: logs) {
			System.out.println(log.getField(LogField.LOGMESSAGE).toString());
		}
	}
	
	public static void main(String[] args) {
		System.out.println("test started");
		LogProcessorTest test = new LogProcessorTest();
		test.test();
	}
}
