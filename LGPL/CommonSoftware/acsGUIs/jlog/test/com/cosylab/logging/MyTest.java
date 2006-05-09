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

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import com.cosylab.logging.LogTableDataModel;

/**
 * MyTest tests saving/loading a file, clearing as well as reading a file. Each time a method is 
 * called setUp and tearDown are run.
 * 
 * @author
 */
public class MyTest extends junit.framework.TestCase
{

	LogTableDataModel ltdm = null;
	FileReader reader, reader1, reader2 = null;
	java.io.BufferedReader br, br1, br2 = null;
	String curDir = null;
	String s = null;

	private FileWriter fw = null;

	public MyTest(String name)
	{
		super(name);
	}

	protected void setUp()
	{
		ltdm = new LogTableDataModel();
		curDir = System.getProperty("user.dir");
		save();
	}

	protected void tearDown()
	{
		ltdm.clearAll();
		ltdm = null;
		curDir = null;
		reader = null;
		reader1 = null;
		br = null;
		br1 = null;
	}

	public void testFileRead() throws java.io.FileNotFoundException
	{
		reader = new FileReader(curDir + "/LogExample-complete.xml");
		try
		{

			char act = (char) reader.read();

			assertEquals("File not found.", '<', act);

		}
		catch (Exception e)
		{
			System.out.println("testFileRead() exception: " + e);
		}
	}

	/**
	 * Compares two objects that stand for the beginning xml tag.
	 */

	public void testFileXml() throws Exception
	{
		reader = new FileReader(curDir + "/LogExample-complete.xml");

		try
		{
			char beg[] = new char[5];

			reader.read(beg);
			reader.close();

			String exp = "<?xml";
			Object expected = (Object) exp;

			String act = new String(beg);
			Object actual = (Object) act;

			assertEquals("Not complete Xml file.", exp, act);

		}
		catch (Exception e)
		{
			System.out.println("testFileXml() exception: " + e);
		}
	}

	/**
	 * Checks whether the display has been cleared from the logs.
	 */

	public void testClearAll() throws Exception
	{
		try
		{
			ltdm.clearAll();
			int act = ltdm.getRowCount();
			int exp = 0;
			assertEquals("Not cleared.", exp, act);

		}
		catch (Exception e)
		{
			System.out.println("testClearAll() exception: " + e);
		}
	}

	/**
	 * Creates a file.
	 */
	public void save()
	{
		String contents =
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
				+ "<Log>"
				+ "<Header Name=\"NameForXmlDocument\" Type=\"LOGFILE\" />"
				+ "<Debug TimeStamp=\"2002-11-07T15:13:20.651\" File=\"acsexmplDoorImpl.cpp\" Line=\"203\" Routine=\"Checking thread status: suspendCount = \" Host=\"te1\" Process=\"maciActivate\" Thread=\"doorControl\" Context=\"\">0</Debug>"
				+ "</Log>";
		// check whethether "logs" exists under "test"		
		try
		{
			boolean success = (new File(curDir)).mkdir();
			if (!success)
			{
				success = (new File(curDir)).mkdirs();
			}
		}
		catch (Exception e)
		{
			System.out.println("save(): exception occured while creating a directory logs under test: " + e);
		}

		String file = curDir + "/LogExample-complete.xml";
		try
		{
			FileWriter fileWriter = new FileWriter(file);
			fileWriter.write(contents);
			fileWriter.close();
		}
		catch (IOException e)
		{
			System.out.println("save() exception: " + e);
		}

	}

	public void testSaveFile() throws Exception
	{
		String filename = curDir + "/LogExample-complete.xml";
		String filename1 = curDir + "/LogExample-complete-saved.xml";

		// Tests saving a file using JFileChooser.
		try
		{
			reader = new FileReader(filename);
			java.io.BufferedReader br = new java.io.BufferedReader(reader);
			StringBuffer sb = new StringBuffer("");
			String line;
			while ((line = br.readLine()) != null)
			{
				sb.append(line);
			}
			String exp = sb.toString();
			br.close();
			reader.close();

			fw = new java.io.FileWriter(filename1);
			fw.write(exp);
			fw.close();

			reader1 = new FileReader(filename1);
			java.io.BufferedReader br1 = new java.io.BufferedReader(reader1);
			StringBuffer sb1 = new StringBuffer("");
			String line1;
			while ((line1 = br1.readLine()) != null)
			{
				sb1.append(line1);
			}
			String act = sb1.toString();
			br1.close();
			reader1.close();

			// Compare expected and actual  
			assertEquals("Saved not equal.", exp, act);

		}
		catch (Exception e)
		{
			System.out.println("testSaveFile()  exception: " + e);
		}
	}

	/**
	 * Tests loading from a file.
	 */

	public void testLoadFromFile() throws Exception
	{
		String filename = curDir + "/LogExample-complete.xml";
		String filename1 = curDir + "/LogExample-complete-saved1.xml";
		String filename2 = curDir + "/LogExample-complete-saved2.xml";

		try
		{
			reader = new FileReader(filename);
			br = new java.io.BufferedReader(reader);
			StringBuffer sb = new StringBuffer("");
			String line;
			while ((line = br.readLine()) != null)
			{
				sb.append(line + "\n");
			}
			String act = sb.toString();
			br.close();

			reader.close();

			fw = new java.io.FileWriter(filename1);
			fw.write(act);
			fw.close();

			// Reads a file into a string act
			reader1 = new FileReader(filename1);
			br1 = new java.io.BufferedReader(reader1);
			StringBuffer sb1 = new StringBuffer("");
			String line1;
			while ((line1 = br1.readLine()) != null)
			{
				sb1.append(line1 + "\n");
			}
			String exp = sb1.toString();
			// System.out.print("exp: "+exp);
			br1.close();
			reader1.close();

			fw = new java.io.FileWriter(filename2);
			fw.write(act);
			fw.close();

			reader2 = new FileReader(filename2);
			br2 = new java.io.BufferedReader(reader2);
			StringBuffer sb2 = new StringBuffer("");
			String line2;
			while ((line2 = br2.readLine()) != null)
			{
				sb2.append(line2 + "\n");
			}
			String exp1 = sb2.toString();

			// Compares expected and equal	
			assertEquals("Load not equal.", exp, exp1);

		}
		catch (Exception e)
		{
			System.out.println("testLoadFromFile() exception: " + e.toString());
		}
	}
}
