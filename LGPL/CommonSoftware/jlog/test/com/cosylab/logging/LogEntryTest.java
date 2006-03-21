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
import com.cosylab.logging.engine.log.LogEntryXML;
import com.cosylab.logging.engine.log.LogTypeHelper;
//import com.cosylab.logging.LogTypeHelper;

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
	LogEntryXML log = null;
	Node node = null;
	DocumentBuilderFactory factory = null;

	java.util.Vector testEntries = new java.util.Vector();

	com.cosylab.logging.engine.ACS.ACSLogParser logparser = null;

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

		logparser = new com.cosylab.logging.engine.ACS.ACSLogParser();
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

		Node node = log.getNode();
		LogEntryXML log1 = new LogEntryXML(node);

		Node node1 = log1.getNode();
		LogEntryXML log2 = new LogEntryXML(node1);

		String actual = log.toString();
		String expected = log1.toString();

		assertEquals("The two logs are not equal.", expected, actual);

	}

	public void testGetNode()
	{ //public Node getNode()

		Node node = log.getNode();

		LogEntryXML log1 = new LogEntryXML(node);
		Node node1 = log1.getNode();

		String actual = node.toString();
		String expected = node1.toString();

		assertEquals("The two logs are not equal.", expected, actual);
	}

	public void testGetDatas()
	{ //public NodeList getDatas()

		NodeList nl = log.getDatas();
		int actualLength = nl.getLength();
		int expectedLength = 1;

		assertEquals(
			"The two logs are not equal.",
			expectedLength,
			actualLength);

		String nodelist = "" + nl;
		String nodes = "";
		if (actualLength > 0)
		{
			for (int i = 0; i < nl.getLength(); i++)
			{
				Node n = nl.item(i);
				nodes = nodes + "[" + n.toString() + "]";
			}
		}

		assertEquals("The two logs are not equal.", nodelist, nodes);
	}

	public void testGetEntryTypeAsString()
	{ //public String getEntryTypeAsString()  

		String actualEntryType = log.getEntryTypeAsString();
		String expectedEntryType = "Trace";
		assertEquals(
			"The two logs are not equal.",
			expectedEntryType,
			actualEntryType);
	}

	public void testGetEntryTypeDescription()
	{ //public final static String getEntryTypeDescription(int index) 

		String actual = null;
		for (int j = 0; j < 9; j++)
		{
			String actualEntryTypeDesc = LogTypeHelper.getLogTypeDescription(j);
			// can be anything: trace, debug, info
			if (actualEntryTypeDesc
				.equalsIgnoreCase(log.getEntryTypeAsString()))
				actual = actualEntryTypeDesc; // is the one that matches
		}
		String expected = log.getEntryTypeAsString(); // "Trace" (Trace)
		assertEquals("The two logs are not equal.", expected, actual);
	}

	public void testGetField()
	{ //Object getField(int fieldIndex)

		Object actualField = null;
		String curFieldDesc = null;

		Object expectedField = "maciHeartbeatController.cpp";

		String act = log.getEntryTypeAsString(); // Trace

		for (int j = 0; j < 15; j++)
		{
			curFieldDesc = LogEntryXML.getFieldDescription(j); // File
			if (curFieldDesc.equalsIgnoreCase("File"))
			{
				actualField = log.getField(j);
			}
		}
		assertEquals("The two logs are not equal.", expectedField, actualField);
	}

	public void testGetFieldClass(String currentFieldDesc)
	{ //Class getFieldClass(int fieldIndex)

		Class actualFieldClass = null;
		Class curFieldClass = null;
		String curFieldDesc = null;

		currentFieldDesc = "File";
		Class expectedFieldClass = String.class;

		for (int j = 0; j < 15; j++)
		{
			curFieldDesc = LogEntryXML.getFieldDescription(j);
			// can be anything: timstamp, entrytype, field
			if (curFieldDesc.equals(currentFieldDesc))
				curFieldClass = LogEntryXML.getFieldClass(j);
			// gets the class of the "File" which is String
		}
		assertEquals(
			"The two logs are not equal.",
			expectedFieldClass,
			actualFieldClass);
	}

	public void testIsLogEntryMessageSimple()
	{ //void isLogEntryMessageSimple() throws DOMException
		boolean actual = log.isLogEntryMessageSimple();
		boolean expected = false;
		assertEquals("The two logs are not equal.", expected, actual);
	}

	public void testIsValidFieldIndex()
	{ //boolean isValidFieldIndex(int fieldIndex)
		boolean expected = true;
		boolean actual = false;
		for (int j = 0; j < 15; j++)
		{
			actual = LogEntryXML.isValidFieldIndex(j);
		}
		assertEquals("The two logs are not equal.", expected, actual);
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

	public void testToString()
	{ //String toString()
		String expected = log.toString();
		String actual = null;

		StringBuffer sb = new StringBuffer("--- LogEntryXML ---\n");
		for (int j = 0; j < 15; j++)
		{
			if (log.getField(j) != null)
			{
				if (j == LogEntryXML.FIELD_ENTRYTYPE)
					sb.append(
                            LogEntryXML.getFieldDescription(j)
							+ ": "
							+ log.getEntryTypeAsString()
							+ "\n");
				else
					sb.append(
                            LogEntryXML.getFieldDescription(j)
							+ ": "
							+ log.getField(j)
							+ "\n");

			}

		}

		NodeList nl = log.getDatas();
		int actualLength = nl.getLength();
		if (actualLength != 0)
			sb.append("Datas: " + log.getDatas() + "\n");
		actual = sb.toString();
		assertEquals("The two logs are not equal.", expected, actual);
	}

	public void testGetFieldDesc()
	{ //String getFieldDescription(int fieldIndex) 
		String actualFieldDesc = null;
		Object field = null;
		String f = null;
		String expectedFieldDesc = "File";
		String expectedField = "maciHeartbeatController.cpp";

		for (int j = 0; j < 15; j++)
		{
			field = log.getField(j); // all the fields 		
			f = "" + field;

			if (f.equalsIgnoreCase(expectedField))
			{ // maciHeartbeatController.cpp
				actualFieldDesc = LogEntryXML.getFieldDescription(j);
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
			for (int i = 0; i < testEntries.size(); i++)
			{
				TestEntry testEntry = (TestEntry) testEntries.get(i);
				DocumentBuilder db = factory.newDocumentBuilder();

				doc =
					db.parse(
						new InputSource(
							new java.io.StringReader(testEntry.log)));

				Node root = doc.getFirstChild();
				LogEntryXML le = new LogEntryXML(root);
				String act = le.getEntryTypeAsString();
				String exp = testEntry.logType;

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
}
