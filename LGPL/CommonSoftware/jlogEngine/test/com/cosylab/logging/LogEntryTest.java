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
package com.cosylab.logging;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.xml.sax.InputSource;
import org.w3c.dom.*;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;

import com.cosylab.logging.engine.log.LogEntryXML;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

import java.util.Vector;

/**
 * LogEntryTest tests whether the starting tag is the same as expected. 
 * It tests the output of the constructors for a complete log entry and 
 * the one for a log entry derived from a node.
 * 
 * Creation date: (11/15/2001 10:58:03 AM)
 * @author: 
 */
public class LogEntryTest extends junit.framework.TestCase
{

	Document doc = null;
	ILogEntry log = null;
	Node node = null;
	DocumentBuilderFactory factory = null;

	java.util.Vector<TestEntry> testEntries = new java.util.Vector<TestEntry>();

	ACSLogParser logparser = null;

	class TestEntry
	{
		String logType;
		String log;
		TestEntry(String logtype, String l)
		{
			logType = logtype;
			log = l;
		}
	}

	/**
	 * LogEntryTest constructor comment.
	 * @param name java.lang.String
	 * 
	 */
	public LogEntryTest(String name)
	{
		super(name);
	}
	
	protected void setUp() throws Exception
	{

		logparser = ACSLogParserFactory.getParser();
		log =
			logparser.parse(
				"<Trace TimeStamp=\"2002-11-07T15:13:00.012\" File=\"maciHeartbeatController.cpp\" Line=\"64\"><Data Name=\"DataName\">first data</Data></Trace>");

		//test for testLogEntry1
		testEntries.add(
			new TestEntry(
				"Trace",
				"<Trace TimeStamp=\"2002-11-07T15:13:00.012\" File=\"maciHeartbeatController.cpp\" Line=\"64\"><Data Name=\"DataName\">first data</Data></Trace>"));
	}

	public void testLogEntryConst() throws Exception
	{ //public LogEntryXML(Node log) throws DOMException

		String xmlString = log.toXMLString();
		ILogEntry log1 = logparser.parse(xmlString);

		String actual = log.toString();
		String expected = log1.toString();
		
		assertEquals("The two logs are not equal.", expected, actual);

	}

	public void testGetDatas()
	{ //public NodeList getDatas()

		Vector<ILogEntry.AdditionalData> datas = log.getAdditionalData();
		int actualLength = datas.size();
		int expectedLength = 1;

		assertEquals(
			"The two logs are not equal.",
			expectedLength,
			actualLength);

		String dataName = datas.get(0).name;
		String dataVal =  datas.get(0).value;
		
		assertEquals("Data name differs", "DataName",dataName);
		assertEquals("Data value differs", "first data",dataVal);
	}

	public void testGetEntryTypeAsString()
	{ //public String getEntryTypeAsString()  

		String actualEntryType = ((LogTypeHelper)log.getField(LogField.ENTRYTYPE)).logEntryType;
		String expectedEntryType = "Trace";
		assertEquals(
			"The two logs are not equal.",
			expectedEntryType,
			actualEntryType);
	}

	public void testGetEntryTypeDescription()
	{ //public final static String getEntryTypeDescription(int index) 

		String actual = null;
		for (int j = 0; j < LogTypeHelper.values().length; j++)
		{
			String actualEntryTypeDesc = LogTypeHelper.values()[j].logEntryType;
			// can be anything: trace, debug, info
			if (actualEntryTypeDesc
				.equalsIgnoreCase(((LogTypeHelper)log.getField(LogField.ENTRYTYPE)).logEntryType));
				actual = actualEntryTypeDesc; // is the one that matches
				break;
		}
		String expected = ((LogTypeHelper)log.getField(LogField.ENTRYTYPE)).logEntryType;
		assertEquals("The two logs are not equal.", expected, actual);
	}

	public void testGetField()
	{ //Object getField(int fieldIndex)

		Object actualField = null;
		String curFieldDesc = null;

		Object expectedField = "maciHeartbeatController.cpp";

		LogField fileField=LogField.FILE;
		actualField = log.getField(LogField.FILE);
		
		assertEquals("The two logs are not equal.", expectedField, actualField);
	}

	public void testGetFieldClass(String currentFieldDesc)
	{ //Class getFieldClass(int fieldIndex)

		Class actualFieldClass = null;
		Class curFieldClass = null;
		String curFieldDesc = null;

		currentFieldDesc = "File";
		Class expectedFieldClass = String.class;

		for (LogField f: LogField.values())
		{
			curFieldDesc = f.getName();
			// can be anything: timstamp, entrytype, field
			if (curFieldDesc.equals(currentFieldDesc))
				curFieldClass = f.getClass();
			// gets the class of the "File" which is String
		}
		assertEquals(
			"The two logs are not equal.",
			expectedFieldClass,
			actualFieldClass);
	}

	public void testIsValidLogEntryType()
	{ //boolean isValidLogEntryType(int index)
		boolean expected = true;
		boolean actual = false;
		for (int j = 0; j < 9; j++)
		{
			actual = LogEntryXML.isValidLogEntryType(j);
		}
		assertEquals("The two logs are not equal.", expected, actual);

	}

	public void testGetFieldDesc()
	{ //String getFieldDescription(int fieldIndex) 
		String actualFieldDesc = null;
		Object fieldContent = null;
		String f = null;
		String expectedFieldDesc = "File";
		String expectedField = "maciHeartbeatController.cpp";

		for (LogField field: LogField.values())
		{
			fieldContent = log.getField(field); // all the fields 		
			f = "" + fieldContent;

			if (f.equalsIgnoreCase(expectedField))
			{ // maciHeartbeatController.cpp
				actualFieldDesc = field.getName();
			}
		}
		assertEquals(
			"The two logs are not equal.",
			expectedFieldDesc,
			actualFieldDesc);
	}

	public void testLogEntry1()
	{
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

		try
		{
			for (TestEntry entry: testEntries)
			{
				DocumentBuilder db = factory.newDocumentBuilder();

				doc =
					db.parse(
						new InputSource(
							new java.io.StringReader(entry.log)));

				Node root = doc.getFirstChild();
				LogEntryXML le = new LogEntryXML(root);
				String act = le.getEntryTypeAsString();
				String exp = entry.logType;

				// Checks whether starting and supposedly starting tags match.
				assertEquals(
					"The starting tag is different from expected.",
					exp,
					act);

			}
		}
		catch (Exception e)
		{
			System.out.println("Exception: " + e);
		}
	}
	
	/**
	 * This test tries to parse 2 logs with empty Data.
	 * The former has a <Data .../> tag
	 * The latter has a <Data ...></Data> tag
	 * Both of the test should fail
	 * 
	 * @throws Exception
	 */
	public void testEmptyData() throws Exception {
		String xml1="<Trace TimeStamp=\"2002-11-07T15:13:00.012\" File=\"maciHeartbeatController.cpp\" Line=\"64\"><Data Name=\"DataName\"/></Trace>";
		String xml2="<Trace TimeStamp=\"2002-11-07T15:13:00.012\" File=\"maciHeartbeatController.cpp\" Line=\"64\"><Data Name=\"DataName\"></Data></Trace>";
		ILogEntry log1 = null;
		try {
			logparser.parse(xml1);
		} catch (Exception e) {
			log1=null;
		}
		assertNull("The parser parsed a null Data: <Data.../>",log1);
		ILogEntry log2 = null;
		try {
			logparser.parse(xml2);
		} catch (Exception e) {
			log2=null;
		}
		assertNull("The parser parsed a null Data: <Data...></Data>",log2);
	}
}
