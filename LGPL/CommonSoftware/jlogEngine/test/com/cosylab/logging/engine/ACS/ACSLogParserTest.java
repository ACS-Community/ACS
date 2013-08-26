/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package com.cosylab.logging.engine.ACS;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import org.xml.sax.helpers.ParserFactory;

import junit.framework.TestCase;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.logging.engine.parser.ACSLogParserFactory.ParserTypes;
import alma.acs.util.IsoDateFormat;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;
import com.cosylab.logging.engine.log.LogField;

/**
 * The class to test parsing.
 * <P>
 * The tests iterate over all possible parsers.
 * 
 * @author acaproni
 *
 */
public class ACSLogParserTest extends TestCase {

	private ACSLogParser parser;

	private final String xmlLogInfo1 = 
		"<Info TimeStamp=\"2006-03-28T00:26:29.238\" File=\"maciContainerImpl.cpp\" Line=\"1454\" " + 
		"Routine=\"maci::ContainerImpl::initThread\" Host=\"gas\" Process=\"maciContainer\" Thread=\"ARCHIVE_BULKSENDER::actionThread\" " + 
		"Context=\"CTXT\" SourceObject=\"ARCHIVE_BULKSENDER::source\" Audience=\"Operator\" Array=\"AnArray\" Antenna=\"ThisIsTheAntenna\""+
		" StackLevel=\"10\" StackId=\"TheStackID\" Priority=\"3\">" + 
		"<![CDATA[Thread name: 'ARCHIVE_BULKSENDER::actionThread']]>" +
		"</Info>";
	
	private final String xmlLogInfo2 = 
		"<Info TimeStamp=\"2006-03-28T00:26:29.239\" File=\"maciContainerImpl.cpp\" Line=\"1454\" " + 
		"Routine=\"maci::ContainerImpl::initThread\" Host=\"gas\" Process=\"maciContainer\" Thread=\"ARCHIVE_BULKSENDER::monitorThread\" " + 
		"Context=\"\" SourceObject=\"ARCHIVE_BULKSENDER::monitorThread\">" + 
		"<![CDATA[Thread name: 'ARCHIVE_BULKSENDER::monitorThread']]>" + 
		"</Info>";
	
	private final String xmlLogWarningWithException = 
		"<Warning TimeStamp=\"2006-03-28T00:26:46.149\" " + 
		"File=\"alma.acs.container.ContainerSealant\" Line=\"184\" Routine=\"invoke\" Host=\"gas\" " + 
		"Process=\"LoggerName: alma.acs.container.ARCHIVE/ACC/javaContainer\" SourceObject=\"ARCHIVE/ACC/javaContainer\" " + 
		"Thread=\"RequestProcessor-15\" StackId=\"unknown\" StackLevel=\"0\" LogId=\"298\">" + 
		"<![CDATA[checked exception was thrown in functional method 'ARCHIVE_CONNECTION/alma.xmlstore.Operational#retrieve':]]>" +
		"<Data Name=\"LoggedException\"><![CDATA[alma.xmlstore.OperationalPackage.NotFound: uid://X00000000000028aa/X00000002" + "\n" +
		"	at alma.archive.components.OperationalImpl.retrieve(OperationalImpl.java:450)" + "\n" +
		"	at alma.archive.components.OperationalImpl.retrieve(OperationalImpl.java:480)" + "\n" +
		"	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)" + "\n" +
		"	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)" + "\n" +
		"	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)" + "\n" +
		"	at java.lang.reflect.Method.invoke(Method.java:585)" + "\n" +
		"	at alma.acs.container.ContainerSealant.invoke(ContainerSealant.java:155)" + "\n" +
		"	at $Proxy3.retrieve(Unknown Source)" + "\n" +
		"	at alma.xmlstore.OperationalPOATie.retrieve(OperationalPOATie.java:103)" + "\n" +
		"	at alma.xmlstore.OperationalPOA._invoke(OperationalPOA.java:316)" + "\n" +
		"	at org.jacorb.poa.RequestProcessor.invokeOperation(Unknown Source)" + "\n" +
		"	at org.jacorb.poa.RequestProcessor.process(Unknown Source)" + "\n" +	
		"	at org.jacorb.poa.RequestProcessor.run(Unknown Source)" + "\n" +
		"]]></Data><Data Name=\"Pippo\"><![CDATA[Pluto]]></Data></Warning>";
	
	/**
	 * An array of special ogs i.e. logs that for one reason or another presented some problem while parsing.
	 */
	private final String[] specialLogs = {
		// There was an error while parsing logs where the <DATA> appears before the body of the message
		"<Info TimeStamp=\"2006-03-28T00:26:29.239\">" + 
		"<Data Name=\"Pippo\"><![CDATA[Pluto]]></Data>" +
		"<Data Name=\"2ndName\"><![CDATA[2ndVal]]></Data>" +
		"<![CDATA[Thread name: 'ARCHIVE_BULKSENDER::monitorThread']]>" + 
		"</Info>"
	};

	/**
	 * Parses one log record from XML and verifies a few fields, 
	 * including the exception details that are attached as additional data.
	 * 
	 * @throws Exception
	 */
	public void testParseLogRecord() throws Exception {
		// Cycle through all available parsers
		for (ParserTypes type: ParserTypes.values()) {
			System.out.println("testParseLogRecord: Testing parser "+type);
			parser = ACSLogParserFactory.getParser(type);
			assertNotNull(parser);
			assertEquals(type, ACSLogParserFactory.getParserType(parser));
			
			ILogEntry log = parser.parse(xmlLogWarningWithException);
			
			// verify some fields
			assertEquals("wrong typename string", "Warning", ((LogTypeHelper)log.getField(LogField.ENTRYTYPE)).logEntryType);
			assertEquals("wrong type code", LogTypeHelper.WARNING, ((LogTypeHelper)log.getField(LogField.ENTRYTYPE)));
			
			Vector<ILogEntry.AdditionalData> additionalData = log.getAdditionalData();
			assertFalse("There should have been 2 pieces of additional data!", additionalData == null || additionalData.size() != 2);
			
			// Check the first data
			ILogEntry.AdditionalData myData = additionalData.get(0);
			assertEquals("LoggedException", myData.name);
			assertTrue(myData.value.startsWith("alma.xmlstore.OperationalPackage.NotFound: uid://X00000000000028aa/X00000002"));
			// Check the second data
			ILogEntry.AdditionalData d = additionalData.get(1);
			assertEquals("Pippo", d.name);
			assertEquals("Pluto", d.value);
		}
	}
	
	/**
	 * Check if the fields are read as expected
	 * 
	 * @throws Exception
	 */
	public void testFields() throws Exception {
		// Cycle through all available parsers
		for (ParserTypes type: ParserTypes.values()) {
			System.out.println("testFields: Testing parser "+type);
			parser = ACSLogParserFactory.getParser(type);
			assertNotNull(parser);
			assertEquals(type, ACSLogParserFactory.getParserType(parser));
			
			ILogEntry log = parser.parse(xmlLogInfo1);
			assertNotNull(log);
			
			// Check the date
			Long logDate = (Long)log.getField(LogField.TIMESTAMP);
			assertNotNull(logDate);
			Long xmlDate=null;
			SimpleDateFormat dateFormat = new IsoDateFormat();
			Date date = dateFormat.parse("2006-03-28T00:26:29.238");
			xmlDate = Long.valueOf(date.getTime());
			assertEquals("The dates differ", xmlDate,logDate);
			
			// Check the log type
			assertEquals(LogTypeHelper.INFO,(LogTypeHelper)log.getField(LogField.ENTRYTYPE));
			
			// Check the file
			assertEquals("maciContainerImpl.cpp", log.getField(LogField.FILE));
			
			// Check the line
			assertEquals(Integer.valueOf(1454), log.getField(LogField.LINE));
			
			// Check the routine
			assertEquals("maci::ContainerImpl::initThread",log.getField(LogField.ROUTINE));
			
			// Check the host
			assertEquals("gas",log.getField(LogField.HOST));
			
			// Check the process
			assertEquals("maciContainer",log.getField(LogField.PROCESS));
			
			// Check the thread
			assertEquals("ARCHIVE_BULKSENDER::actionThread",log.getField(LogField.THREAD));
			
			// Check the Antenna
			assertEquals("CTXT", log.getField(LogField.CONTEXT));
			
			// Check the source object
			assertEquals("ARCHIVE_BULKSENDER::source",log.getField(LogField.SOURCEOBJECT));
			
			// Check the stack level
			assertEquals(10,log.getField(LogField.STACKLEVEL));
			
			// Check the stack level
			assertEquals(3,log.getField(LogField.PRIORITY));
			
			// Check the stack level
			assertEquals("TheStackID",log.getField(LogField.STACKID));
			
			// Check the Audience
			assertEquals("Operator",log.getField(LogField.AUDIENCE));
			
			// Check the Array
			assertEquals("AnArray", log.getField(LogField.ARRAY));
			
			// Check the Antenna
			assertEquals("ThisIsTheAntenna", log.getField(LogField.ANTENNA));
		}
	}
	
	/**
	 * Call the parser several times
	 * 
	 * @throws Exception
	 */
	public void testMultipleParse() throws Exception {
		// Cycle through all available parsers
		for (ParserTypes type: ParserTypes.values()) {
			System.out.println("testMultipleParse: Testing parser "+type);
			parser = ACSLogParserFactory.getParser(type);
			assertNotNull(parser);
			assertEquals(type, ACSLogParserFactory.getParserType(parser));
			
			String[] logs = new String[3*1000];
			for (int t=0; t<logs.length; t+=3) {
				logs[t]=xmlLogInfo1;
				logs[t+1]=xmlLogInfo2;
				logs[t+2]=xmlLogWarningWithException;
			}
			for (String xmlLog: logs) {
				ILogEntry log = parser.parse(xmlLog);
				assertNotNull(log);
			}
		}
	}
	
	/**
	 * Test special logs i.e. logs that sometime have returned errors while parsing.
	 * 
	 * @see <code>specialLogs</code>
	 * @throws Exception
	 */
	public void testSpecialLogs() throws Exception {
		// Cycle through all available parsers
		for (ParserTypes type: ParserTypes.values()) {
			System.out.println("testSpecialLogs: Testing parser "+type);
			parser = ACSLogParserFactory.getParser(type);
			assertNotNull(parser);
			assertEquals(type, ACSLogParserFactory.getParserType(parser));
			
			for (String xmlLog: specialLogs) {
				ILogEntry log = parser.parse(xmlLog);
				Vector <AdditionalData> data = log.getAdditionalData();
				if (data!=null) {
					for (int t=0; t<data.size(); t++) {
						AdditionalData d = data.get(t);
						System.out.println("Data:  name="+d.name+", value="+d.value);
					}
				} else {
					System.out.println("The log has no additional data entries");
				}
				System.out.println("Body: "+log.getField(LogField.LOGMESSAGE));
			}
		}
	}
}
