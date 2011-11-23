/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
package alma.acs.jlog.test.zoom;

import java.io.File;
import java.util.Date;

import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;

import alma.acs.logging.archive.zoom.FileHelper;
import alma.acs.logging.engine.io.IOPorgressListener;
import alma.acs.util.IsoDateFormat;

import junit.framework.TestCase;

public class FileHelperTest extends TestCase implements ACSRemoteLogListener, ACSRemoteErrorListener, IOPorgressListener {
	
	/**
	 * The xml file used for testing
	 */
	private static final String xmlFileName="logOutput2008-09-19T11_21_49.212--2008-09-19T11_21_49.670.xml";
	
	/**
	 * Total number of logs in the XML <code>xmlFileName</code>
	 */
	private static final int totalLogsInFile = 202;
	
	
	/**
	 * The file to read for testing
	 */
	private File testFile;
	
	/**
	 * The <code>FileHelper</code> to test.
	 */
	private FileHelper fileHelper;
	
	/**
	 * Number of logs read
	 * 
	 * @see logsRead(int numOfLogs)
	 */
	private int numOfLogsRead;
	
	public FileHelperTest() {
		super(FileHelperTest.class.getName());
	}

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		String userDir = System.getProperty("user.dir");
		assertNotNull(userDir);
		assertTrue(userDir.length()>0); // Not empty
		String fileFolder= userDir+"/zoom";
		
		testFile = new File(fileFolder+"/"+xmlFileName);
		assertNotNull(testFile);
		assertTrue(testFile.canRead());
	}

	/**
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		// TODO Auto-generated method stub
		super.tearDown();
	}

	/**
	 * Print a message in the standard output: the error will be
	 * visible thanks to tat.
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteErrorListener#errorReceived(java.lang.String)
	 */
	@Override
	public void errorReceived(String xml) {
		System.out.println("Error parsing "+xml);
		
	}

	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#logEntryReceived(com.cosylab.logging.engine.log.ILogEntry)
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		numOfLogsRead++;
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesRead(long)
	 */
	@Override
	public void bytesRead(long bytes) {}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesWritten(long)
	 */
	@Override
	public void bytesWritten(long bytes) {}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsRead(int)
	 */
	@Override
	public void logsRead(int numOfLogs) {}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsWritten(int)
	 */
	@Override
	public void logsWritten(int numOfLogs) {}

	/**
	 * Test loading with no times and no levels restrictions
	 */
	public void testLoadAll() throws Exception {
		fileHelper = new FileHelper(
				testFile,
				0L,
				System.currentTimeMillis(), 
				LogTypeHelper.values()[0],
				LogTypeHelper.values()[LogTypeHelper.values().length-1]);
		
		numOfLogsRead=0;
		assertTrue(fileHelper.loadLogs(this, this, this));
		assertEquals(totalLogsInFile, numOfLogsRead);
	}
	
	/**
	 * Test the loading of logs between a defined range of levels
	 */
	public void testLoadLevelRange() throws Exception {
		fileHelper = new FileHelper(
				testFile,
				0L,
				System.currentTimeMillis(), 
				LogTypeHelper.TRACE,
				LogTypeHelper.INFO);
		
		numOfLogsRead=0;
		assertTrue(fileHelper.loadLogs(this, this, this));
		assertEquals(68, numOfLogsRead);
	}
	
	/**
	 * Test the loading of logs between a defined time interval
	 */
	public void testLoadTimelRange() throws Exception {
		IsoDateFormat dateFormat = new IsoDateFormat();
		Date startDate = dateFormat.parse("2008-09-19T11:21:49.500");
		Date endDate = dateFormat.parse("2008-09-19T11:21:49.600");
		fileHelper = new FileHelper(
				testFile,
				startDate.getTime(),
				endDate.getTime(), 
				LogTypeHelper.values()[0],
				LogTypeHelper.values()[LogTypeHelper.values().length-1]);
		
		numOfLogsRead=0;
		assertTrue(fileHelper.loadLogs(this, this, this));
		assertEquals(45, numOfLogsRead);
	}
	
	/**
	 * Test the loading of logs between a defined time interval
	 * and a definit log level interval
	 */
	public void testLoadFilterAll() throws Exception {
		IsoDateFormat dateFormat = new IsoDateFormat();
		Date startDate = dateFormat.parse("2008-09-19T11:21:49.500");
		Date endDate = dateFormat.parse("2008-09-19T11:21:49.600");
		fileHelper = new FileHelper(
				testFile,
				startDate.getTime(),
				endDate.getTime(), 
				LogTypeHelper.DEBUG,
				LogTypeHelper.NOTICE);
		
		numOfLogsRead=0;
		assertTrue(fileHelper.loadLogs(this, this, this));
		assertEquals(15, numOfLogsRead);
	}
}
