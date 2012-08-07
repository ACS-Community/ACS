/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
package alma.acs.jlog.test;

import java.io.File;
import java.io.RandomAccessFile;

import junit.framework.TestCase;

import com.cosylab.logging.engine.LogEngineException;
import com.cosylab.logging.engine.cache.CacheEntry;
import com.cosylab.logging.engine.cache.CacheFile;
import com.cosylab.logging.engine.cache.LogQueueFileHandlerImpl;

/**
 * Test the {@link CacheFile}.
 *   
 * @author  acaproni
 * 
 * @version $Id: CacheFileTest.java,v 1.1 2012/08/07 08:46:39 acaproni Exp $
 * @since ACS 10.2    
 */
public class CacheFileTest  extends TestCase {
	
	/**
	 * The file to test. It is removed when the test terminates.
	 */
	private File file;
	
	/**
	 * The random file
	 */
	private RandomAccessFile raFile;
	
	/**
	 * The object to test
	 */
	private CacheFile cacheFile;
	
	/**
	 * Each {@link CacheFile} has a key i.e. a unique identifier used to join
	 * a {@link CacheEntry} to a file.
	 */
	private final int key=Integer.valueOf(0);
	
	/**
	 * The name of the file
	 */
	private String fileName;
	
	/**
	 * <code>fileHandler</code> is used to create a new file. 
	 * It is also in charge of deleting the file when the test terminates.
	 */
	private final LogQueueFileHandlerImpl fileHandler = new LogQueueFileHandlerImpl();
	
	/**
	 * A log for testing
	 */
	private final String log1 = "<Debug TimeStamp=\"2012-08-04T23:53:10.760\" File=\"org.jacorb.orb.dsi.ServerRequest\" Line=\"330\" Routine=\"reply\" Host=\"gas01\" Process=\"CONTROL/ACC/javaContainer\" SourceObject=\"jacorb@CONTROL/ACC/javaContainer\" Thread=\"RequestProcessor-2593\" LogId=\"27932171\"><![CDATA[ServerRequest: reply to getAtmosphericConditions]]></Debug>";
	
	/**
	 * A log for testing
	 */
	private final String log2 = "<Debug TimeStamp=\"2012-08-04T23:55:44.658\" File=\"alma.acs.jlog.test.CacheFileTest\" Routine=\"setUp\" Host=\"alma22\" Process=\"CONTROL/ACC/javaContainer\"><![CDATA[ServerRequest: reply to getAtmosphericConditions]]></Debug>";
	
	/**
	 * A log for testing
	 */
	private final String log3 = "<Debug TimeStamp=\"2012-08-05T12:10:00.124\" File=\"org.jacorb.orb.dsi.ServerRequest\" Line=\"1258\" Routine=\"reply\" Host=\"gas01\" Process=\"CONTROL/ACC/javaContainer\" SourceObject=\"jacorb@CONTROL/ACC/javaContainer\" Thread=\"RequestProcessor-2593\" LogId=\"27932171\"><![CDATA[ServerRequest: reply to getAtmosphericConditions]]></Debug>";
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		file = fileHandler.getNewFile();
		assertNotNull("Error getting a new file",file);
		assertTrue("Unreadable file",file.canRead());
		assertTrue("Unwritable file",file.canWrite());
		fileName=file.getAbsolutePath();
		assertNotNull("Invalid null file name!", fileName);
		assertFalse("Invalid empty file name",fileName.isEmpty());
		raFile = new RandomAccessFile(file,"rw");
		assertNotNull("Error building the RandomAccessFile",raFile);
		
		cacheFile=new CacheFile(file.getName(), key, raFile, file, false);
	}

	@Override
	protected void tearDown() throws Exception {
		cacheFile.close();
		file=null;
		raFile=null;
		
		super.tearDown();
	}
	
	/**
	 * When the cache file contains no logs, the oldest and newst dates
	 * are <code>null</code>.
	 * 
	 * @throws Exception
	 */
	public void testintialDates() throws Exception {
		System.out.println("testintialDates");
		assertNull("Initial min date schould be null", cacheFile.minDate());
		assertNull("Initial max date schould be null", cacheFile.maxDate());
	}
	
	/**
	 * Add a malfolrmed log in the cache and expect an exception.
	 * 
	 * @throws Exception
	 */
	public void testLogWithMalformedDate() throws Exception {
		System.out.println("testLogWithMalformedDate");
		
		try {
			CacheEntry entry = cacheFile.writeOnFile("WrongLog", key);
			throw new Exception("This line should be unreachable!!! Log with wrong date has been accepetd by the cache");
		} catch (LogEngineException lee) {
			// This is expected!!!
		}
		// min date and max date must be both null
		assertNull("Initial min date schould be null", cacheFile.minDate());
		assertNull("Initial max date schould be null", cacheFile.maxDate());
	}
	
	/**
	 * Add a log in the cache and checks if the dates of the newest and oldest logs
	 * are the same and correct.
	 * 
	 * @throws Exception
	 */
	public void testDatesWithOneLog() throws Exception {
		System.out.println("testDatesWithOneLog");
		
		CacheEntry entry = cacheFile.writeOnFile(log1, key);
		String minDate = cacheFile.minDate();
		assertNotNull("Invalid null youngest date", minDate);
		String maxDate = cacheFile.maxDate();
		assertNotNull("Invalid null oldest date", maxDate);
		
		assertEquals("Oldest and younget dates differ",minDate, maxDate);
		assertEquals("2012-08-04T23:53:10.760", minDate);
	}
	
	/**
	 * Add few logs in cache and checks the correctness of the newest and oldest logs dates.
	 * 
	 * @throws Exception
	 */
	public void testDatesSeveralLogs() throws Exception {
		System.out.println("testDatesSeveralLogs");
		
		CacheEntry entry1 = cacheFile.writeOnFile(log1, key);
		CacheEntry entry2 = cacheFile.writeOnFile(log2, key);
		CacheEntry entry3 = cacheFile.writeOnFile(log3, key);
		String minDate = cacheFile.minDate();
		assertNotNull("Invalid null youngest date", minDate);
		String maxDate = cacheFile.maxDate();
		assertNotNull("Invalid null oldest date", maxDate);
		
		assertEquals("2012-08-04T23:53:10.760", minDate);
		assertEquals("2012-08-05T12:10:00.124", maxDate);
	}
	
	/**
	 * Add few logs in cache and checks the correctness of the newest and oldest logs dates.
	 * 
	 * @throws Exception
	 */
	public void testReadWrite() throws Exception {
		System.out.println("testReadWrite");
		
		CacheEntry entry1 = cacheFile.writeOnFile(log1, key);
		CacheEntry entry2 = cacheFile.writeOnFile(log2, key);
		CacheEntry entry3 = cacheFile.writeOnFile(log3, key);
		
		String log1readFromCache = cacheFile.readFromFile(entry1);
		assertNotNull(log1readFromCache);
		assertFalse(log1readFromCache.isEmpty());
		assertEquals(log1, log1readFromCache);
		String log2readFromCache = cacheFile.readFromFile(entry2);
		assertNotNull(log2readFromCache);
		assertFalse(log2readFromCache.isEmpty());
		assertEquals(log2, log2readFromCache);
		String log3readFromCache = cacheFile.readFromFile(entry3);
		assertNotNull(log3readFromCache);
		assertFalse(log3readFromCache.isEmpty());
		assertEquals(log3, log3readFromCache);
	}
}
