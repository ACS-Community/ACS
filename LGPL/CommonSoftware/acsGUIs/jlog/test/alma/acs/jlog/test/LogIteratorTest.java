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
import java.util.NoSuchElementException;

import com.cosylab.logging.client.cache.ILogMap;
import com.cosylab.logging.engine.log.ILogEntry;

import junit.framework.TestCase;

/**
 * Test LogIterator
 * 
 * @author acaproni
 *
 */
public class LogIteratorTest extends TestCase {
	
	/**
	 * Constructor
	 *
	 */
	public LogIteratorTest() throws Exception {
		super("DeleteLogTest");
	}
	
	/**
	 * @see junit.framework.TestCase
	 */
	public void setUp() throws Exception {
		super.setUp();
	}
	
	/**
	 * @see junit.framework.TestCase
	 */
	public void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * Test if the next() and hasNext() methods work well together
	 * 
	 * @throws Exception
	 */
	public void testIterator() throws Exception {
		for (int type=0; type<CacheUtils.NUMOFCACHETYPES; type++) {
			ILogMap cache = CacheUtils.getCache(type,null);
			assertNotNull("The cache (type "+type+") is null",cache);
			// The cache is empty!
			Iterator<ILogEntry> emptyIter=cache.iterator();
			assertFalse("The cache (type )"+type+") is empty but the iterator thinks it is not",emptyIter.hasNext());
			emptyIter=null; // Release the iterator
			// Populate the cache with some logs
			int logsInCache=1024;
			CacheUtils.populateCache(cache,logsInCache);
			assertEquals("Wrong num. of logs in cache (cache type "+type+")",logsInCache,cache.getSize());
			Iterator<ILogEntry> iter = cache.iterator();
			// Read all the logs to check if the hasNext returns always
			// true but for the last log in cache
			int logsRead=0;
			while (iter.hasNext()) {
				iter.next();
				logsRead++;
			}
			assertEquals("The numer of logs read is not complete",logsRead,logsInCache);
		}
	}
	
	/**
	 * Test if the next() method works well.
	 * In this case hasNext is not used (an Exception signals that
	 * all the logs have been read)
	 * 
	 * @throws Exception
	 */
	public void testIteratorWithException() throws Exception {
		for (int type=0; type<CacheUtils.NUMOFCACHETYPES; type++) {
			ILogMap cache = CacheUtils.getCache(type,null);
			assertNotNull("The cache (type "+type+") is null",cache);
			// The cache is empty!
			Iterator<ILogEntry> emptyIter=cache.iterator();
			assertFalse("The cache (type )"+type+") is empty but the iterator thinks it is not",emptyIter.hasNext());
			emptyIter=null; // Release the iterator
			// Populate the cache with some logs
			int logsInCache=1024;
			CacheUtils.populateCache(cache,logsInCache);
			assertEquals("Wrong num. of logs in cache (cache type (cache type "+type+")"+type+")",logsInCache,cache.getSize());
			Iterator<ILogEntry> iter = cache.iterator();
			// Read all the logs to check if the hasNext returns always
			// true but for the last log in cache
			int logsRead=0;
			ILogEntry log;
			do {
				try {
					log = iter.next();
				} catch (NoSuchElementException e) {
					// Check if we have read all the logs
					assertEquals("The numer of logs read is not complete (cache type "+type+")",logsInCache,logsRead);
					return;
				}
				logsRead++;
			} while(log!=null);
		}
	}
	
	/**
	 * Test the remove method deleting all the logs in cache
	 * 
	 * @throws Exception
	 */
	public void testRemove() throws Exception {
		for (int type=0; type<CacheUtils.NUMOFCACHETYPES; type++) {
			ILogMap cache = CacheUtils.getCache(type,null);
			assertNotNull("The cache (type "+type+") is null",cache);
			// The cache is empty!
			Iterator<ILogEntry> emptyIter=cache.iterator();
			assertFalse("The cache (type )"+type+") is empty but the iterator thinks it is not",emptyIter.hasNext());
			emptyIter=null; // Release the iterator
			// Populate the cache with some logs
			int logsInCache=1024;
			CacheUtils.populateCache(cache,logsInCache);
			assertEquals("Wrong num. of logs in cache (cache type (cache type "+type+")"+type+")",logsInCache,cache.getSize());
			Iterator<ILogEntry> iter = cache.iterator();
			// Try to remove a log before calling next: it causes an exception
			boolean gotAnException=false;
			try {
				iter.remove();
			} catch (IllegalStateException e) {
				// This is ok
				gotAnException=true;
			}
			assertTrue("The remove did not throw the (excpected) exception",gotAnException);
			// Iterate over the logs
			while (iter.hasNext()) {
				iter.next();
				iter.remove();
				assertEquals("The logs has not been removed",cache.getSize(),--logsInCache);
			}
		}
	}
	
}
