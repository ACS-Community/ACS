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
import java.util.Iterator;

import com.cosylab.logging.client.cache.LogMultiFileCache;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;

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
	
	private Collection<ILogEntry> logCollection;
	
	
	/**
	 * Constructor
	 *
	 */
	public MultiFileCacheTest() throws Exception {
		super("MultiFileCacheTest");
	}
	
	
	public void setUp() throws Exception {
		super.setUp();
		cache=new LogMultiFileCache(TESTCACHE_SIZE);
		cache.setDebugTrace(true);
		
		assertEquals("The cache size is not as expected",TESTCACHE_SIZE,cache.getMaxFileSize());
		
		// Create and populate the cache
		logCollection = CacheUtils.generateLogsType(100,LogTypeHelper.TRACE);	
	}
	
	public void tearDown() throws Exception {
		super.tearDown();
		cache.clear();
		cache=null;
	}
	
	public void testFileCreation1() throws Exception {
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

		cache.deleteLog(27);
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
	
	public void testFileDeletion() throws Exception {
	}
		

}
