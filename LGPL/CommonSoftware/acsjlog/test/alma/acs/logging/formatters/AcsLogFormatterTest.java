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
package alma.acs.logging.formatters;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import junit.framework.TestCase;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.LogParameterUtil;
import alma.acs.testsupport.LogRecordCollectingLogger;

/**
 * Note that <code>LogRecord</code>s are usually constructed inside of the log methods of class <code>Logger</code>;
 * if we create them with <code>new</code> in the test routines, the class name and method name can't be figured out
 * by <code>LogRecord#inferCaller</code> -- this failure has nothing to do with the normal limitations of this "best effort" feature.
 */
public class AcsLogFormatterTest extends TestCase
{
	private AcsXMLLogFormatter acsLogFormatter;
	private LogRecordCollectingLogger collectingLogger; 

	protected void setUp() throws Exception {
		acsLogFormatter = new AcsXMLLogFormatter();
		collectingLogger = LogRecordCollectingLogger.getCollectingLogger("testLogger");
	}


	/**
	 * Method testFormat.
	 */
	public void testFormat()
	{ 
	    String msg = "my <<< georgious >>> \\log message";
		LogRecord logRecord = new LogRecord(Level.INFO, msg);
		String logXML = acsLogFormatter.format(logRecord);
		//String expected = "<Info TimeStamp="2004-11-26T15:45:25.519" Host="GA004797" Thread="10" LogId="0">INFO</Info>";
		System.out.println(logXML);
		assertTrue(logXML.startsWith("<Info TimeStamp=\""));
		assertTrue(logXML.indexOf(" Host=\"") > 0);
		assertTrue(logXML.indexOf(" Thread=\"") > 0);
		assertTrue(logXML.endsWith("><![CDATA[" + msg + "]]></Info>"));		
	}

	
	/**
	 * Assures that an exception which gets passed to the log method will show up as a <code>&lt;Data&gt;</code> element in the XML. 
	 */
	public void testLogException()
	{
	    String msg = "A darn NPE occured, see stack trace";
		LogRecord logRecord = new LogRecord(Level.WARNING, msg);
		logRecord.setThrown(new NullPointerException("test NPE"));
		String logXML = acsLogFormatter.format(logRecord);
//		System.out.println(logXML);
		assertTrue(logXML.indexOf("<Data Name=\"LoggedException\"><![CDATA[java.lang.NullPointerException: test NPE") > 0);
	}

	
	/**
	 * Method testGetLevel. Gets the string representing a level using getLogLevelDescription 
	 * and compares it to the string representing the formatted log record.
	 */
	public void testGetLevel()
	{ 
		LogRecord logRecord = new LogRecord(Level.INFO, "INFO");
		Level level = logRecord.getLevel();
		String expected = AcsLogLevel.getNativeLevel(level).getEntryName();
		String act = acsLogFormatter.format(logRecord);
		int timestamp = act.indexOf(" TimeStamp");
		String actual = act.substring(1, timestamp);
		assertEquals(expected, actual);
	}

	public void testLoggedParameters() {
		// a stupid empty parameter that should show up as "N/A" to work around a xerces parser bug
		LogRecord record = new LogRecord(AcsLogLevel.INFO, "INFO message");
		System.out.println(acsLogFormatter.format(record));
		record = new LogRecord(AcsLogLevel.DEBUG, "DEBUG message");
		record.setParameters(new Object[] {""}); 		
		String logXML = acsLogFormatter.format(record);
		assertTrue(logXML.endsWith("<![CDATA[DEBUG message]]><Data Name=\"LoggedParameter\"><![CDATA[N/A]]></Data></Debug>"));

		// one string parameter
		String param = "My string log parameter";
		collectingLogger.clearLogRecords();
		collectingLogger.log(Level.INFO, "Log with one string parameter.", param);
		LogRecord oneStringParamRecord = collectingLogger.getCollectedLogRecords()[0];
//		System.out.println(acsLogFormatter.format(oneStringParamRecord));
		logXML = acsLogFormatter.format(oneStringParamRecord);
		assertTrue(logXML.endsWith("<Data Name=\"LoggedParameter\"><![CDATA[" + param + "]]></Data></Info>"));
		
		// todo: multiple parameters as Object[]
		
		// properties map parameter
		Map<String, String> myNamedValues = new HashMap<String, String>();
		myNamedValues.put("aKey", "aValue");
		collectingLogger.clearLogRecords();
		collectingLogger.log(Level.INFO, "Log with name/value pair.", myNamedValues);
		LogRecord nameValueParamRecord = collectingLogger.getCollectedLogRecords()[0];
//		System.out.println(acsLogFormatter.format(nameValueParamRecord));
		logXML = acsLogFormatter.format(nameValueParamRecord);
		assertTrue(logXML.endsWith("<Data Name=\"aKey\"><![CDATA[aValue]]></Data></Info>"));
	}
	
	
	
	public void testFormatter()
	{
		LogRecord record = new LogRecord(AcsLogLevel.INFO, "INFO message");
		System.out.println(acsLogFormatter.format(record));

		// test additional properties
		Map<String, Object> map = LogParameterUtil.createPropertiesMap();
		map.put("Line", new Long(1208));
		map.put("StackId", "Stack ID");
		map.put("StackLevel", new Long(4));
		map.put("Priority", new Long(11));
		map.put("Context", "Testing...");
		map.put("ThreadName", "MyThread");
		map.put("Uri", "abeans-CDBDAL://csl01:5001/alma/gizmo/current/units?get");
		Map<String, String> nv = new HashMap<String, String>();
		nv.put("name1", "value1");
		nv.put("name2", "value2");
		
		// TODO: put asserts on these otherwise useless ancient test pieces below 
		
		record = new LogRecord(AcsLogLevel.ERROR, "ERROR message");
		record.setParameters(new Object[] { map, nv });
		System.out.println(acsLogFormatter.format(record));

		record = new LogRecord(AcsLogLevel.EMERGENCY, "EMERGENCY message");
		System.out.println(acsLogFormatter.format(record));

		Exception ex = new IllegalArgumentException("source of all");
		record = new LogRecord(AcsLogLevel.ERROR, "ERROR message");
		System.out.println(acsLogFormatter.format(record));
	}

}