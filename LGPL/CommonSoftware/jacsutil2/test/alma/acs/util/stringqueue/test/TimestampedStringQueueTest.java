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
package alma.acs.util.stringqueue.test;

import java.util.Random;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import alma.acs.util.stringqueue.QueueEntry;
import alma.acs.util.stringqueue.TimestampedStringQueue;
import junit.framework.TestCase;

/**
 * A class to test <code>TimestampedStringQueue</code>.
 * 
 * @author acaproni
 *
 */
public class TimestampedStringQueueTest extends TestCase {
	
	/**
	 * A runnable to push strings in the queue
	 * 
	 * @author acaproni
	 *
	 */
	private class StringPusher implements Runnable {
		
		/**
		 * The string to push in the queue
		 */
		private final Vector<String> strsToPush=new Vector<String>();
		
		private final CountDownLatch latch;
		
		private final String name;
		
		/**
		 * Constructor
		 * 
		 * @param numOfStringsToPush
		 */
		public StringPusher(int numOfStringsToPush, CountDownLatch latch, String name) {
			if (numOfStringsToPush<=0) {
				throw new IllegalArgumentException("numOfStringsToPush must be greater the 0");
			}
			populateStringsVector(strsToPush,numOfStringsToPush);
			this.latch=latch;
			this.name=name;
		}

		@Override
		public void run() {
			System.out.println(name+" started. Pushing "+strsToPush.size()+" strings in the queue");
			for (int t=0; t<strsToPush.size(); t++) {
				try {
					cache.push(strsToPush.elementAt(t));
				} catch (Throwable th) {
					th.printStackTrace();
				}
			}
			strsToPush.clear();
			latch.countDown();
			System.out.println(name+" terminated");
		}
		
	}
	
	/**
	 * A runnable to get strings out the queue
	 * 
	 * @author acaproni
	 *
	 */
	private class StringPopper implements Runnable {
		/**
		 * The string read from the queue
		 */
		private final Vector<String> poppedStrings= new Vector<String>();
		
		private final int numOfStrsToGet;
		
		private final CountDownLatch latch;
		
		private final String name;
		
		/**
		 * Constructor
		 * 
		 * @param numOfStrsToGet
		 */
		public StringPopper(int numOfStrsToGet, CountDownLatch latch, String name) {
			if (numOfStrsToGet<=0) {
				throw new IllegalArgumentException("numOfStringsToPush must be greater the 0");
			}
			this.numOfStrsToGet=numOfStrsToGet;
			this.latch=latch;
			this.name=name;
		}
		
		@Override
		public void run() {
			System.out.println(name+" started");
			int read=0;
			
			while (read<numOfStrsToGet) {
				String str=null;
				try {
					str=cache.pop();
				} catch (Throwable t) {
					t.printStackTrace();
				}
				if (str==null) {
					continue;
				}
				poppedStrings.add(str);
				read++;
			}
			latch.countDown();
			System.out.println(name+" terminated");
		}
	}
	
	// The size of each file of the cache
	private static final int CACHE_SIZE=30000;
	
	// The cache to test
	private TimestampedStringQueue cache;
	
	public TimestampedStringQueueTest() {
		super("TimestampedStringQueueTest");
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		// Build the cache
		cache = new TimestampedStringQueue(CACHE_SIZE,"TIMESTAMP=\"");
		assertNotNull(cache);
		assertEquals(0, cache.getActiveFilesSize());
		assertEquals(0, cache.size());
	}

	/* (non-Javadoc)
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		cache.close(true);
		super.tearDown();
	}
	
	/**
	 * Generate the strings to put in the cache.
	 * <P>
	 * The number of strings to put in the vector depends on the passed
	 * parameter. The sum of the length of all the strings in the
	 * vector is equal or greater to the passed parameter.
	 * In this way it is possible to check if the cache creates/deletes a
	 * file in the right moment.
	 *  
	 * @param size The length of all the strings in the vector
	 * @return A vector of strings
	 * 
	 */
	private Vector<String> generateStrings(int size) {
		Vector<String> strings = new Vector<String>();
		int currentSz=0;
		long t=0;
		while (currentSz<size) {
			String str = "TimeStamp=\"2005-12-02T13:45:02.761\" "+(t++);
			currentSz+=str.length();
			strings.add(str);
		}
		return strings;
	}
	
	/**
	 * Generate a vector of size strings.
	 * 
	 * @param strings
	 * @param size The number of strings to put in the vector
	 */
	private void populateStringsVector(Vector<String> strings, int size) {
		long t=0;
		while (strings.size()<size) {
			String str = "A string to test with threads, TimeStamp=\"2005-12-02T13:45:02.761\" "+(t++);
			strings.add(str);
		}
	}
	
	/**
	 * Test the normal behavior of the cache by pushing and getting
	 * strings into the cache without involving the creation of more
	 * then on file.
	 * 
	 * @throws Exception
	 */
	public void testPushPopSingleFile() throws Exception {
		Vector<String> items = generateStrings(CACHE_SIZE/2);
		// Push the string in the cache
		for (String str: items) {
			cache.push(str);
		}
		assertEquals(items.size(), cache.size());
		assertEquals(1, cache.getActiveFilesSize());
		
		for (int t=0; t<items.size(); t++) {
			String str = cache.pop();
			// str is null if the cache is empty
			assertNotNull(str);
			assertEquals(items.get(t), str);
		}
		// No more items and files in cache
		assertEquals(0, cache.size());
		assertEquals(1, cache.getActiveFilesSize());
		System.out.println("testPushPopSingleFile done");
	}
	
	/**
	 * Test the normal behavior of the cache by pushing and getting
	 * strings into the cache involving the creation of several files.
	 * 
	 * @throws Exception
	 */
	public void testPushPopSeveralFiles() throws Exception {
		// Create a vector of strings the trigger the cache to create 4 files
		Vector<String> items = generateStrings(3*CACHE_SIZE+CACHE_SIZE/2);
		// Push the string in the cache
		for (String str: items) {
			cache.push(str);
		}
		assertEquals(items.size(), cache.size());
		assertEquals(4, cache.getActiveFilesSize());
		
		for (int t=0; t<items.size(); t++) {
			String str = cache.pop();
			// str is null if the cache is empty
			assertNotNull(str);
			assertEquals(items.get(t), str);
		}
		// No more items and files in cache
		assertEquals(0, cache.size());
		assertEquals(1, cache.getActiveFilesSize());
		System.out.println("testPushPopSeveralFiles done");
	}
	
	/**
	 * Check the conversion in QueueEntry between the key, start and end position
	 * and their representations as an hexadecimal string.
	 */
	public void testCacheEntryTranslation() throws Exception {
		// Check for 0, 0, 1 (end must be greater then 0)
		QueueEntry zeroCE = new QueueEntry(0,0,1);
		
		String zeroStr = zeroCE.toHexadecimal();
		assertEquals(QueueEntry.ENTRY_LENGTH,zeroStr.length());
		QueueEntry check = new QueueEntry(zeroStr);
		assertEquals(zeroCE.key, check.key);
		assertEquals(zeroCE.start, check.start);
		assertEquals(zeroCE.end, check.end);
		
		// Check for to values
		QueueEntry topCE = new QueueEntry(Integer.MAX_VALUE,Long.MAX_VALUE-1,Long.MAX_VALUE);
		String topStr = topCE.toHexadecimal();
		assertEquals(QueueEntry.ENTRY_LENGTH,topStr.length());
		check= new QueueEntry(topStr);
		assertEquals(topCE.key, check.key);
		assertEquals(topCE.start, check.start);
		assertEquals(topCE.end, check.end);
		
		// Check for some random values
		Random rnd = new Random(System.currentTimeMillis());
		for (int t=0; t<1000; t++) {
			long start=0;
			long end=0;
			while (start>=end) {
				start = Math.abs(rnd.nextLong());
				end = Math.abs(rnd.nextLong());
			}
			QueueEntry test = new QueueEntry(Math.abs(rnd.nextInt()),start,end);
			String testStr = test.toHexadecimal();
			assertEquals(QueueEntry.ENTRY_LENGTH,testStr.length());
			check= new QueueEntry(testStr);
			assertEquals(test.key, check.key);
			assertEquals(test.start, check.start);
			assertEquals(test.end, check.end);
		}
		System.out.println("testCacheEntryTranslation done");
	}
	
	/**
	 * Check if getting an entry from an empty queue returns <code>null</code>
	 * after the timout elapses
	 */
	public void testEmptyTimeout() throws Exception {
		// Set the timeout to 1 sec.
		cache.setTimeout(1000);
		long now = System.currentTimeMillis();
		String str = cache.pop();
		long after = System.currentTimeMillis();
		assertTrue("The queue returned too early! No timeout? ", after-now>=1000);
		assertNull("The queue shall return NULL when empty!",str);
	}
	
	/**
	 * Test if the cache works as expected by running 5 threads in parallel
	 * to push .strings asynchronously.
	 */
	public void testAsyncPush() throws Exception {
		System.out.println("testAsyncPush started");
		cache.start();
		int numOfPushers=5;
		CountDownLatch latch = new CountDownLatch(numOfPushers);
		// Instantiate 5 threads to push 1000 items each
		StringPusher[] pushers = new StringPusher[numOfPushers];
		for (int t=0; t<pushers.length; t++) {
			pushers[t]=new StringPusher(1000, latch,"StringPusher-"+t);
		}
		
		// Create and run all the threads
		System.out.println("testAsyncPush creating threads");
		Vector<Thread> threads = new Vector<Thread>();
		for (int t=0; t<pushers.length; t++) {
			threads.add(new Thread(pushers[t]));
		}
		System.out.println("testAsyncPush starting threads");
		for (int t=0; t<threads.size(); t++) {
			threads.elementAt(t).start();
		}
		System.out.println("testAsyncPush waiting until all threads terminate");
		while (!latch.await(1,TimeUnit.MINUTES)) {
			System.out.println("Strings in cache: "+cache.size()+", files="+cache.getActiveFilesSize());
		}
		System.out.println("testAsyncPush pusher threads terminated");
		assertEquals("The cahce does not contain the expected number of strings!",5000, cache.size());
		System.out.println("testAsyncPush done");
	}
	
	/**
	 * Test if the cache works as expected by running 5 threads in parallel
	 * to pop strings asynchronously. 
	 */
	public void testAsyncPop() throws Exception {
		System.out.println("testAsyncPop started");
		cache.start();
		int numOfPoppers=5;
		CountDownLatch latch = new CountDownLatch(numOfPoppers);
		
		// Piush the strings in the cache
		Vector<String> strsToPush = new Vector<String>();
		populateStringsVector(strsToPush, 5000);
		for (String str: strsToPush) {
			cache.push(str);
		}
		assertEquals(5000, cache.size());
		
		// Instantiate the threads to read 1000 items each
		StringPopper[] poppers = new StringPopper[numOfPoppers];
		for (int t=0; t<poppers.length; t++) {
			poppers[t]=new StringPopper(1000, latch,"StringPopper-"+t);
		}
		
		// Create and run all the threads
		System.out.println("testAsyncPop creating threads");
		Vector<Thread> threads = new Vector<Thread>();
		for (int t=0; t<poppers.length; t++) {
			threads.add(new Thread(poppers[t]));
		}
		System.out.println("testAsyncPop starting threads");
		for (int t=0; t<threads.size(); t++) {
			threads.elementAt(t).start();
		}
		System.out.println("testAsyncPop waiting until all threads terminate");
		// Wait the thread to terminate with a timeout of 3 minutes
		while (!latch.await(1,TimeUnit.MINUTES)) {
			// This is an error!!!!
			System.out.println("Not all the threads to get strings from the cache terminated!");
			System.out.println("Strings in cache: "+cache.size()+", files="+cache.getActiveFilesSize());
		}
		// Here all the strings should have been read from the cache!
		assertTrue(cache.size()==0);
		System.out.println("testAsyncPop done");
	}
	
	/**
	 * Test if the cache works as expected by running 13 threads in parallel
	 * to push and pop strings asynchronously
	 */
	public void testAsyncPushPop() throws Exception {
		System.out.println("testAsyncPushPop started");
		cache.start();
		int numOfPushers=5;
		int numOfPoppers=8;
		CountDownLatch latch = new CountDownLatch(numOfPushers+numOfPoppers);
		// Instantiate 5 threads to push 1000 items each
		StringPusher[] pushers = new StringPusher[numOfPushers];
		for (int t=0; t<pushers.length; t++) {
			pushers[t]=new StringPusher(1000, latch,"StringPusher-"+t);
		}
		
		// Instantiate 8 threads to read 625 items each
		StringPopper[] poppers = new StringPopper[numOfPoppers];
		for (int t=0; t<poppers.length; t++) {
			poppers[t]=new StringPopper(625, latch,"StringPopper-"+t);
		}
		
		// Create and run all the threads
		System.out.println("testAsyncPushPop creating threads");
		Vector<Thread> threads = new Vector<Thread>();
		for (int t=0; t<poppers.length; t++) {
			threads.add(new Thread(poppers[t]));
		}
		for (int t=0; t<pushers.length; t++) {
			threads.add(new Thread(pushers[t]));
		}
		System.out.println("testAsyncPushPop starting threads");
		for (int t=0; t<threads.size(); t++) {
			threads.elementAt(t).start();
		}
		System.out.println("testAsyncPushPop waiting until all threads terminate");
		while (!latch.await(3,TimeUnit.MINUTES)) {
			// This is an error!!!!
			System.out.println("Not all the threads terminated!");
			System.out.println("Strings in cache: "+cache.size()+", files="+cache.getActiveFilesSize());
		}
		assertTrue(cache.size()==0);
		System.out.println("testAsyncPushPop done");
	}

}
