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
import java.util.Collection;
import java.util.Properties;
import java.util.Vector;

import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.ILogEntry;

import junit.framework.TestCase;
import alma.acs.logging.engine.io.IOHelper;
import alma.acs.logging.engine.io.IOPorgressListener;

/**
 * A class testing the load and save facilities
 * <P>
 * As a general rule, the class generates some logs with the help of <code>CacheUtile</code>,
 * then save and load the logs. 
 * Finally it checks if the logs written and those read are equals. 
 * 
 * @author acaproni
 *
 */
public class LoadSaveTest extends TestCase implements IOPorgressListener, ACSRemoteLogListener, ACSRemoteErrorListener {
	
	// The number of bytes read and written
	private volatile long bytesRead, bytesWritten;
	
	/**
	 * The logs to read and write
	 */
	private Collection<ILogEntry> logs;
	
	/**
	 * The logs read form a file
	 * <P>
	 * This collection is filled by <code>logEntryReceived()</code>.
	 */
	private Vector<ILogEntry> logsRead;
	
	/**
	 * The number of logs in <code>logs</code> to load/save
	 */
	private static final int NUMBER_OF_LOGS=1500;
	
	/**
	 * The directory where the logs are read and written.
	 * <P>
	 * It comes form the <code>user.dir</code> property. 
	 */
	private String folder;
	private static final String USER_DIR = "user.dir";
	
	/**
	 * The name of the file used for testing
	 */
	private String fileName;
	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesRead(long)
	 */
	@Override
	public void bytesRead(long bytes) {
		bytesRead=bytes;
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesWritten(long)
	 */
	@Override
	public void bytesWritten(long bytes) {
		bytesWritten+=bytes;
	}

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		bytesRead=0;
		bytesWritten=0;
		logs=CacheUtils.generateLogs(NUMBER_OF_LOGS);
		assertEquals(NUMBER_OF_LOGS, logs.size());
		// Read the folder name and generate the file name
		Properties props = System.getProperties();
		assertNotNull(props);
		folder = props.getProperty(USER_DIR);
		assertNotNull(folder);
		File folderFile = new File(folder);
		String folderPath = folderFile.getAbsolutePath();
		fileName = folderPath+"/logs.xml";
		// Check if folder is  a writable directory
		assertTrue(folderFile.isDirectory());
		assertTrue(folderFile.canWrite());
		
		logsRead = new Vector<ILogEntry>();
	}

	/**
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
		logs.clear();
		logs=null;
		logsRead.clear();
		logsRead=null;
	}
	
	/**
	 * Load and save a collection of logs
	 * <P>
	 * The save is performed by passing the name of the file
	 *  
	 * @throws Exception
	 */
	public void testSaveLoad() throws Exception {
		IOHelper ioHelper = new IOHelper();
		assertNotNull(ioHelper);
		
		long assumedLen=0;
		for (ILogEntry log: logs) {
			char[] chars = (log.toXMLString()+"\n").toCharArray();
			assumedLen+=chars.length;
		}
		
		// Save the logs on file
		ioHelper.saveLogs(fileName, logs, this, false);
		
		// Read the logs
		ioHelper.loadLogs(fileName, this, this, this);
	
		// Compare the 2 collections
		assertEquals(logs.size(), logsRead.size());
		
		int t=0;
		for (ILogEntry log: logs) {
			String logXML = log.toXMLString();
			String logReadXML = logsRead.get(t++).toXMLString();
			assertEquals(logXML, logReadXML);
		}
	}

	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteErrorListener#errorReceived(java.lang.String)
	 */
	@Override
	public void errorReceived(String xml) {
		System.err.println("ERROR: "+xml);
	}

	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#logEntryReceived(com.cosylab.logging.engine.log.ILogEntry)
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		logsRead.add(logEntry);
	}
	
}
