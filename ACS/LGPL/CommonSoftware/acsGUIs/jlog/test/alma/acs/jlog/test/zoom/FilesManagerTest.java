/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.jlog.test.zoom;

import java.io.File;
import java.util.Date;

import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;

import alma.acs.logging.archive.zoom.FilesManager;
import alma.acs.logging.archive.zoom.ZoomManager;
import alma.acs.logging.archive.zoom.ZoomProgressListener;
import alma.acs.util.IsoDateFormat;
import junit.framework.TestCase;

public class FilesManagerTest extends TestCase 
implements ACSRemoteLogListener, 
ACSRemoteErrorListener,
ZoomProgressListener {
	
	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#logEntryReceived(com.cosylab.logging.engine.log.ILogEntry)
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		logsRead++;
	}

	/** 
	 * The date format
	 */
	private IsoDateFormat dateFormat = new IsoDateFormat();
	
	/**
	 * The object to test
	 */
	private FilesManager manager;
	
	/**
	 * The number of logs received
	 */
	private int logsRead=0;
	
	/**
	 * The number of files that the zoom engine will read
	 */
	private int filesToRead;
	
	public FilesManagerTest() {
		super(FilesManagerTest.class.getName());
	}

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		String folder = System.getProperty(ZoomManager.FILES_LOCATION_PROPERTY_NAME);
		manager = new FilesManager(folder);
		assertNotNull(manager);
		assertNotNull(dateFormat);
		filesToRead=0;
		logsRead=0;
	}

	/**
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * Check if the right folder is used for getting XML files
	 * 
	 * @throws Exception
	 */
	public void testFolderName() throws Exception {
		String userDir = System.getProperty("user.dir");
		assertNotNull(userDir);
		assertEquals(userDir+File.separator+"zoom",manager.filesFolder);
	}
	
	/**
	 * Test the selection of files with a given interval
	 * 
	 * @throws Exception
	 */
	public void testGetFiles() throws Exception {
		Date startDate;
		Date endDate;
		// Test with an interval BEFORE the files in the folder
		startDate = dateFormat.parse("2008-09-18T11:21:50.115");
		endDate = dateFormat.parse("2008-09-18T11:30:50.115");
		File[] files = manager.getFileList(startDate.getTime(), endDate.getTime());
		assertNotNull(files);
		assertEquals(0, files.length);
		
		// Test with an interval AFTER the files in the folder
		startDate = dateFormat.parse("2008-09-19T11:30:50.115");
		endDate = dateFormat.parse("2008-09-19T11:35:50.115");
		files = manager.getFileList(startDate.getTime(), endDate.getTime());
		assertNotNull(files);
		assertEquals(0, files.length);
		
		// Test with start before the first date of the file and the end in the middle
		endDate = dateFormat.parse("2008-09-19T11:21:50.500");
		files = manager.getFileList(0, endDate.getTime());
		assertNotNull(files);
		assertEquals(3, files.length);
		
		// Test with start after the first date of the file and the end after the last file end
		startDate = dateFormat.parse("2008-09-19T11:21:50.500");
		files = manager.getFileList(endDate.getTime(),System.currentTimeMillis());
		assertNotNull(files);
		assertEquals(2, files.length);
		
		// In the middle
		startDate = dateFormat.parse("2008-09-19T11:21:50.000");
		endDate = dateFormat.parse("2008-09-19T11:21:51.500");
		assertNotNull(files);
		assertEquals(2, files.length);
		
		// Test with the start=0 and end current time (==> select all files)
		files = manager.getFileList(0, System.currentTimeMillis());
		assertNotNull(files);
		assertEquals(4, files.length);
	}
	
	/**
	 * Test isOperational()
	 * 
	 * @throws Exception
	 */
	public void testIsOperational() throws Exception {
		assertTrue(manager.isOperational());
	}

	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteErrorListener#errorReceived(java.lang.String)
	 */
	@Override
	public void errorReceived(String xml) {
		System.out.println("Error parsing: "+xml);		
	}
	
	/**
	 * Test the getting of logs with no constraints so the engine should return
	 * all the logs of all the files in the folder
	 * <P>
	 * We do not need to test other cases because the generation of the list
	 * of the files to read is tested in testGetFiles for all possible cases.
	 * <BR>
	 * This method checks if all the logs read from all the files are sent to
	 * the listener. 
	 */
	public void testGetAllLogs() throws Exception {
		
		boolean ret=manager.getLogs(
				"2008-09-01T11:21:50.000", 
				"2008-09-25T11:21:50.000", 
				this, 
				LogTypeHelper.values()[0], 
				LogTypeHelper.values()[LogTypeHelper.values().length-1], 
				this, 
				this);
		assertTrue(ret);
		assertEquals(202+221+185+44,logsRead);
		assertEquals(4,filesToRead);
	}

	/**
	 * @see alma.acs.logging.archive.zoom.ZoomProgressListener#zoomReadingFile(int)
	 */
	@Override
	public void zoomReadingFile(int num) {}

	/**
	 * @see alma.acs.logging.archive.zoom.ZoomProgressListener#zoomTotalFileToRead(int)
	 */
	@Override
	public void zoomTotalFileToRead(int num) {
		filesToRead=num;
		
	}
}
