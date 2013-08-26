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

import java.util.Collection;
import java.util.Set;
import java.util.Iterator;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;

import com.cosylab.logging.client.cache.LogMultiFileCache;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

import junit.framework.TestCase;

/**
 * Test the LogMultiFileCache class
 * 
 * @author acaproni mcomin
 *
 */
public class MultiFileCacheTest extends TestCase {
	
	// The cache to test
	private LogMultiFileCache cache;
	
	// The size of the cache
	private static int TESTCACHE_SIZE=1500;
	
	/**
	 * Constructor
	 *
	 */
	public MultiFileCacheTest() throws Exception {
		super("MultiFileCacheTest");
	}
	
	@Override
	public void setUp() throws Exception {
		super.setUp();
		cache=new LogMultiFileCache(TESTCACHE_SIZE);
		cache.setDebugTrace(true);
		
		assertEquals("The cache size is not as expected",TESTCACHE_SIZE,cache.getMaxFileSize());
		
	}
	
	@Override
	public void tearDown() throws Exception {
		super.tearDown();
		cache.clear();
		cache=null;
	}
	
	public void testFileCreation() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> logCollection = CacheUtils.generateLogsType(100,LogTypeHelper.TRACE);
		assertEquals(100, logCollection.size());
		long fileSize = 0;
		
		Iterator<ILogEntry> ii = logCollection.iterator();

		System.out.println("Start testFileCreation1");
		
		cache.setDebugTrace(true);
		
		// The cache is empty ... no logs have been added, therefore
		// the deletion of a log should fail.

		System.out.println("\nACTION : Delete log from empty cache");

		try {
			cache.deleteLog(12);
		} catch (Exception e) {
			System.out.println("Delete Log FAILS : cache is empty");
		}
		
		// Add logs until a new file is generated
		
		System.out.println("\nACTION : fill half of the cache with logs");
		
		while (fileSize < TESTCACHE_SIZE/2 ) {
			fileSize = cache.getLogFileSize();

			ILogEntry log = ii.next();
			
			cache.add(log);
		}
		
		cache.printFileTableInfo();
		
		System.out.println("\nACTION : add single log");
		
		ILogEntry log = ii.next();
		cache.add(log);
		cache.printFileTableInfo();
		
		
		System.out.println("\nACTION : delete two logs");

		// Delete last log and check that the new file is not deleted

		cache.deleteLog(12);
		cache.deleteLog(2);
		cache.printFileTableInfo();
		
		System.out.println("\nACTION : fill cache and create a new log file");

		while (fileSize < TESTCACHE_SIZE ) {
			fileSize = cache.getLogFileSize();
			log = ii.next();
			cache.add(log);
		}
		
		cache.printFileTableInfo();
		
		
		System.out.println("\nACTION : delete first log of the new file");
		cache.deleteLog(26);
		cache.printFileTableInfo();

		System.out.println("\nACTION : Add single log");
		
		log = ii.next();
		cache.add(log);
		cache.printFileTableInfo();

		// Delete log twice : the log key has already been deleted
		// therefore the delete should fail.

		System.out.println("\nACTION : Delete a log twice");

		try {
			cache.deleteLog(27);
		} catch (Exception e) {
			System.out.println("Delete Log FAILS : log has already been deleted");
		}
	
		cache.printFileTableInfo();
		
		System.out.println("\nACTION : fill cache and create a new log file");
		
		while ((fileSize = cache.getLogFileSize()) < TESTCACHE_SIZE ) {
			log = ii.next();
			cache.add(log);
		}
		
		cache.printFileTableInfo();
		

		System.out.println("\nACTION : fill cache and create a new log file");
		
		log = ii.next();
		cache.add(log);
		
		while ((fileSize = cache.getLogFileSize()) < TESTCACHE_SIZE ) {
			log = ii.next();
			cache.add(log);
		}
		
		cache.printFileTableInfo();
		
		
		System.out.println("\nACTION : add one more log ");

		log = ii.next();
		cache.add(log);
		cache.printFileTableInfo();
	
		
		System.out.println("\nACTION : Delete all log keys within a log file");

		cache.setDebugTrace(false);
		
		for (int logKey = 28; logKey < 52 ; logKey++) {
			cache.deleteLog(logKey);
		}
		
		cache.setDebugTrace(true);
		cache.printFileTableInfo();

	}
	
	/**
	 * Test deletion of logs
	 * 
	 * @throws Exception
	 */
	public void testFileDeletion() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> logCollection = CacheUtils.generateLogs(1000);
		assertEquals(1000, logCollection.size());
		
		for (ILogEntry log: logCollection) {
			cache.add(log);
		}
		assertTrue(cache.getNumberOfCacheFiles()>0);
		assertEquals(Integer.valueOf(0), cache.getFirstLog());
		assertEquals(Integer.valueOf(999), cache.getLastLog());
		
		cache.printFileTableInfo();
		
		int start = cache.getFirstLog();
		int end = cache.getLastLog();
		for (int t=start; t<=end; t++) {
			System.out.println(">>>>"+t);
			cache.deleteLog(t);
		}
		System.out.println("First "+cache.getFirstLog()+", Last "+cache.getLastLog());
		cache.printFileTableInfo();
		assertEquals(0, cache.getSize());
		assertEquals(0, cache.getNumberOfCacheFiles());
	}
	
	/**
	 * Test the functioning of getSize()
	 * @throws Exception
	 */
	public void testGetSize() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> logCollection = CacheUtils.generateLogs(1000);
		assertEquals(1000, logCollection.size());
		for (ILogEntry log: logCollection) {
			cache.add(log);
		}
		assertEquals(Integer.valueOf(0), cache.getFirstLog());
		assertEquals(Integer.valueOf(999), cache.getLastLog());
		
		assertEquals(logCollection.size(), cache.getSize());
		
		// Remove the logs and check the size
		int removed=0;
		int start = cache.getFirstLog();
		int end = cache.getLastLog();
		for (int t=start; t<=end; t++) {
			cache.deleteLog(t);
			assertEquals(logCollection.size()-(++removed), cache.getSize());
		}
		assertEquals(0, cache.getSize());
	}
	
	/**
	 * Test the clearing of the cache
	 */
	public void testClear() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> logCollection = CacheUtils.generateLogs(1000);
		assertEquals(1000, logCollection.size());
		for (ILogEntry log: logCollection) {
			cache.add(log);
		}
		assertEquals(Integer.valueOf(0), cache.getFirstLog());
		assertEquals(Integer.valueOf(999), cache.getLastLog());
		
		assertEquals(logCollection.size(), cache.getSize());
		
		cache.clear();
		assertEquals(0, cache.getSize());
		assertEquals(0, cache.getNumberOfCacheFiles());
		assertNull(cache.getFirstLog());
		assertNull(cache.getLastLog());
	}
	
	/**
	 * Test getFirstLog()
	 */
	public void testGetFirstLog() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> logCollection = CacheUtils.generateLogs(1000);
		assertEquals(1000, logCollection.size());
		for (ILogEntry log: logCollection) {
			cache.add(log);
		}
		assertEquals(Integer.valueOf(0), cache.getFirstLog());
		assertEquals(Integer.valueOf(999), cache.getLastLog());
		
		assertEquals(logCollection.size(), cache.getSize());
		
		int start = cache.getFirstLog();
		int end = cache.getLastLog();
		for (int t=start; t<=end; t++) {
			cache.deleteLog(t);
			if (t<end) {
				assertEquals(Integer.valueOf(t+1), cache.getFirstLog());
			} else {
				assertNull(cache.getFirstLog());
			}
		}
	}
	
	/**
	 * Test getLastLog()
	 */
	public void testGetLastLog() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> logCollection = CacheUtils.generateLogs(1000);
		assertEquals(1000, logCollection.size());
		for (ILogEntry log: logCollection) {
			cache.add(log);
		}
		assertEquals(Integer.valueOf(0), cache.getFirstLog());
		assertEquals(Integer.valueOf(999), cache.getLastLog());
		
		assertEquals(logCollection.size(), cache.getSize());
		
		int start = cache.getFirstLog();
		int end = cache.getLastLog();
		for (int t=end; t>=start; t--) {
			System.out.println("Deleting "+t+", size="+cache.getSize());
			cache.deleteLog(t);
			if (t>start) {
				assertEquals(Integer.valueOf(t-1), cache.getLastLog());
			} else {
				cache.printFileTableInfo();
				assertNull(cache.getLastLog());
			}
		}
	}
	
	/**
	 * Test keySet()
	 * 
	 * @throws Exception
	 */
	public void testkKeySet() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> logCollection = CacheUtils.generateLogs(1000);
		assertEquals(1000, logCollection.size());
		for (ILogEntry log: logCollection) {
			cache.add(log);
		}
		assertEquals(Integer.valueOf(0), cache.getFirstLog());
		assertEquals(Integer.valueOf(999), cache.getLastLog());
		
		assertEquals(logCollection.size(), cache.getSize());
		
		Set<Integer> keys = cache.keySet();
		assertEquals(cache.getSize(), keys.size());
		
		// to check if the keys are all the integers in the cache without duplication
		// we iterate over the keys, get and remove the log having such a key
		// If the key appears twice the get will fail the second time
		for (Integer key: keys) {
			ILogEntry log = cache.getLog(key);
			assertNotNull(log);
			cache.deleteLog(key);
		}
		// If keySet() returned all the key, the cache is empty at the end
		assertTrue(cache.getSize()==0);
	}
	
	/**
	 * Test the getting of logs out of the cache
	 * 
	 * @throws Exception
	 */
	public void testGet() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> logCollection = CacheUtils.generateLogs(5000);
		assertEquals(5000, logCollection.size());
		for (ILogEntry log: logCollection) {
			cache.add(log);
		}
		assertEquals(Integer.valueOf(0), cache.getFirstLog());
		assertEquals(Integer.valueOf(4999), cache.getLastLog());
		
		assertEquals(logCollection.size(), cache.getSize());
		
		for (Integer t=cache.getFirstLog(); t<=cache.getLastLog(); t++) {
			ILogEntry log = cache.getLog(t);
			// To be sure the log is what we expect, it checks if the log message
			// contains the key
			String message = (String)log.getField(LogField.LOGMESSAGE);
			assertNotNull(message);
			assertTrue(message.contains(t.toString()));
		}
	}
	
	/**
	 * Test replacement of logs
	 * 
	 * @throws Exception
	 */
	public void testReplace() throws Exception {
		ACSLogParser parser = ACSLogParserFactory.getParser();
		assertNotNull(parser);
		// Create and populate the cache
		Collection<ILogEntry> logCollection = CacheUtils.generateLogs(5000);
		assertEquals(5000, logCollection.size());
		for (ILogEntry log: logCollection) {
			cache.add(log);
		}
		assertEquals(Integer.valueOf(0), cache.getFirstLog());
		assertEquals(Integer.valueOf(4999), cache.getLastLog());
		
		assertEquals(logCollection.size(), cache.getSize());
		
		for (Integer t=cache.getFirstLog(); t<=cache.getLastLog(); t++) {
			String logMsg ="This log replaced the log with key "+t;
			String logStr = "<Info TimeStamp=\"2005-11-29T16:00:00.000\" Routine=\"CacheTest::testReplace\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA["+logMsg+"]]></Info>";
			ILogEntry newLog = parser.parse(logStr);
			cache.replaceLog(t, newLog);
			
			ILogEntry replacedLog =cache.getLog(t);
			assertEquals(newLog.getField(LogField.LOGMESSAGE), replacedLog.getField(LogField.LOGMESSAGE));
		}
	}
		

}
