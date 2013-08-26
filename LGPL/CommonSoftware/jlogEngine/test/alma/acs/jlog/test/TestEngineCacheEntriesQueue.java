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
package alma.acs.jlog.test;

import java.util.Random;
import java.util.Vector;
import java.util.concurrent.TimeUnit;

import com.cosylab.logging.engine.cache.CacheEntriesQueue;
import com.cosylab.logging.engine.cache.CacheEntry;

import junit.framework.TestCase;

/**
 * Test the CacheEntriesQueue
 * 
 * @author acaproni
 *
 */
public class TestEngineCacheEntriesQueue  extends TestCase {
	
	/**
	 * The number of <code>CacheEntry</code> for testing
	 */
	private static final int TEST_ENTRIES = 100000;
	
	/**
	 * The vector of <code>CacheEntry</code> sent and read from the queue
	 */
	private Vector<CacheEntry> entries = new Vector<CacheEntry>();
	
	/**
	 * The queue to test
	 */
	private CacheEntriesQueue queue;

	/**
	 * Constructor
	 */
	 public TestEngineCacheEntriesQueue() {
		 super("TestEngineCacheEntriesQueue");
	 }

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		populateEntries();
		queue=new CacheEntriesQueue();
		assertNotNull(queue);
	}

	@Override
	protected void tearDown() throws Exception {
		entries.clear();
		queue.clear();
		super.tearDown();
	}
	 
	/**
	 * Generate a set of <code>TEST_ENTRIES</code> entries in the <code>entries</code> vector.
	 */
	private void populateEntries() throws Exception {
		assertNotNull(entries);
		Random rnd = new Random(System.currentTimeMillis());
		for (int t=0; t<TEST_ENTRIES; t++) {
			//int key = Math.abs(rnd.nextInt());
			int key=t;
			long start=0;
			long end=0;
			while (start>=end) {
				start = Math.abs(rnd.nextLong());
				end = Math.abs(rnd.nextLong());
			}
			entries.add(new CacheEntry(key,start,end));
		}
	}
	
	/**
	 * Push and pop entries involving only the <code>LinkedBlockingQueue</code>
	 * (i.e non entries go in the vector, nor in the file)
	 * <P>
	 * <I>Implementation note</I>: 
	 * <OL>
	 * 	<LI> add the entries in the cache
	 * 	<LI> check the number of entries in the queue
	 * 	<LI> get all the entries from the queue checking if they are the same
	 * 		of the pushed ones
	 * </OL>
	 */
	public void testPushPopQueue() throws Exception {
		for (int t=0; t<CacheEntriesQueue.THRESHOLD-1; t++) {
			queue.put(entries.get(t));
		}
		assertEquals(queue.size(), CacheEntriesQueue.THRESHOLD-1);
		// Get the entries from the queue and compare each entry with the 
		// original in the vector
		for (int t=0; t<CacheEntriesQueue.THRESHOLD-1; t++) {
			CacheEntry e = queue.get();
			assertNotNull("Item "+t+" not found", e);
			assertEquals(entries.get(t).key, e.key);
			assertEquals(entries.get(t).start, e.start);
			assertEquals(entries.get(t).end, e.end);
		}
	}
	
	/**
	 * Push and pop entries involving the <code>LinkedBlockingQueue</code> and
	 * the vector but not the file 
	 * <P>
	 * <I>Implementation note</I>: 
	 * <OL>
	 * 	<LI> add the entries in the cache
	 * 	<LI> check the number of entries in the queue
	 * 	<LI> get all the entries from the queue checking if they are the same
	 * 		of the pushed ones
	 * </OL>
	 */
	public void testPushPopQueueVector() throws Exception {
		for (int t=0; t<CacheEntriesQueue.THRESHOLD+CacheEntriesQueue.PAGE_LEN-1; t++) {
			queue.put(entries.get(t));
		}
		assertEquals(queue.size(), CacheEntriesQueue.THRESHOLD+CacheEntriesQueue.PAGE_LEN-1);
		// Get the entries from the queue and compare each entry with the 
		// original in the vector
		for (int t=0; t<CacheEntriesQueue.THRESHOLD+CacheEntriesQueue.PAGE_LEN-1; t++) {
			CacheEntry e = queue.get();
			assertNotNull("Item "+t+" not found", e);
			assertEquals(entries.get(t).key, e.key);
			assertEquals(entries.get(t).start, e.start);
			assertEquals(entries.get(t).end, e.end);
		}
	}
	
	/**
	 * Push and pop entries involving the <code>LinkedBlockingQueue</code>,
	 * the vector and the file. 
	 * <P>
	 * <I>Implementation note</I>: 
	 * <OL>
	 * 	<LI> add the entries in the cache
	 * 	<LI> check the number of entries in the queue
	 * 	<LI> get all the entries from the queue checking if they are the same
	 * 		of the pushed ones
	 * </OL>
	 * <P>
	 * The limit of this test is that entries have been sent in the queue before reading.
	 */
	public void testPushPopQueueVectorFile() throws Exception {
		for (int t=0; t<TEST_ENTRIES; t++) {
			queue.put(entries.get(t));
		}
		assertEquals(queue.size(), TEST_ENTRIES);
		// Get the entries from the queue and compare each entry with the 
		// original in the vector
		for (int t=0; t<TEST_ENTRIES; t++) {
			CacheEntry e = queue.get();
			assertNotNull("Item "+t+" not found", e);
			assertEquals("Item "+t,entries.get(t).key, e.key);
			assertEquals("Item "+t,entries.get(t).start, e.start);
			assertEquals("Item "+t,entries.get(t).end, e.end);
		}
	}
	
	/**
	 * Test the clearing of the queue
	 */
	public void testClear() throws Exception {
		for (CacheEntry e: entries) {
			queue.put(e);
		}
		assertEquals(entries.size(), queue.size());
		queue.clear();
		assertEquals(0, queue.size());
	}
	
	/**
	 * This test completes what is done by <code>testPushPopQueueVectorFile</code>
	 * but now the sequence of reading and writing operations happens randomly
	 * 
	 * @throws Exception
	 */
	public void testRandomOrder() throws Exception {
		int writeCount;
		int readCount;
		// One push, one pop
		for (writeCount=0; writeCount<1000; writeCount++) {
			queue.put(entries.get(writeCount));
			CacheEntry e = queue.get();
			assertNotNull(e);
			assertEquals("Item "+writeCount,entries.get(writeCount).key, e.key);
			assertEquals("Item "+writeCount,entries.get(writeCount).start, e.start);
			assertEquals("Item "+writeCount,entries.get(writeCount).end, e.end);
		}
		// Several push and several (less) pop
		assertEquals(0, queue.size());
		writeCount=readCount=0;
		for (int t=0; t<TEST_ENTRIES; t+=4500) {
			// Put 4500 entries
			for (int i=0; i<4500 && writeCount<TEST_ENTRIES; i++) {
				CacheEntry e = entries.get(writeCount++);
				queue.put(e);
			}
			// read 2000 entries
			for (int y=0; y<3000; y++) {
				CacheEntry e = queue.get();
				assertEquals("Item "+readCount,entries.get(readCount).key, e.key);
				assertEquals("Item "+readCount,entries.get(readCount).start, e.start);
				assertEquals("Item "+readCount,entries.get(readCount).end, e.end);
				readCount++;
			}
		}
		boolean dump=false;
		// Get all remaining entries
		for (; readCount<TEST_ENTRIES; readCount++) {
			CacheEntry e = queue.get();
			assertNotNull(e);
			assertEquals("Item "+readCount,entries.get(readCount).key, e.key);
			assertEquals("Item "+readCount,entries.get(readCount).start, e.start);
			assertEquals("Item "+readCount,entries.get(readCount).end, e.end);
		}
		assertEquals(0, queue.size());
	}
	 
}
