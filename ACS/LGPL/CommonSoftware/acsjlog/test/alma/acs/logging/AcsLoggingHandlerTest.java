/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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

import java.util.logging.Level;
import java.util.logging.LogRecord;

import junit.framework.TestCase;

import alma.acs.logging.config.LogConfig;
import alma.acs.testsupport.LogRecordCollectingLogger;

/**
 */
public class AcsLoggingHandlerTest extends TestCase
{
	private AcsLoggingHandler loggingHandler;
	private TestLogQueue logQueue;
	
	protected void setUp() {
		logQueue = new TestLogQueue();
		LogConfig logConfig = new LogConfig();
		loggingHandler = new AcsLoggingHandler(logQueue, logConfig, "dummyLoggerNameForHandlerLevelConfig", null);
	}
	
	/**
	 * Tests if logs with the silly log level <code>Level.OFF</code> are successfully ignored.
	 * Note that Level.OFF is only meant to be used as an argument to the setLevel() method, 
	 * but not for the log methods. Yet the JDK processes logs with Level.OFF, which ACS does not.
	 */
	public void testOffLevel() {
		LogRecordCollectingLogger colLogger = LogRecordCollectingLogger.getCollectingLogger("colLogger");
		
		// first an INFO log to warm up
		colLogger.info("test message INFO");
		LogRecord testRecord = colLogger.getCollectedLogRecords()[0];
		assertEquals(Level.INFO.intValue(), testRecord.getLevel().intValue());		
		loggingHandler.publish(testRecord);
		assertEquals(1, logQueue.logRecords.size());

		colLogger.clearLogRecords();
		logQueue.reset();

		// now comes the silly OFF log
		// the JDK logger and handler are stupid enough to process this log at highest possible level!
		colLogger.log(Level.OFF, "test message OFF");
		testRecord = colLogger.getCollectedLogRecords()[0];
		assertEquals(Level.OFF.intValue(), testRecord.getLevel().intValue());
		// but our publish method is supposed to catch this...
		loggingHandler.publish(testRecord);
		assertEquals(0, logQueue.logRecords.size());
	}
	
}