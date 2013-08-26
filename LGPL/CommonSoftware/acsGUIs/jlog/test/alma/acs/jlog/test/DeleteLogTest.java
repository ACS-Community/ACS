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

import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;
import java.util.Vector;

import com.cosylab.logging.client.cache.LogBufferedFileCache;
import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.client.cache.LogCacheException;
import com.cosylab.logging.client.cache.LogFileCache;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

import junit.framework.TestCase;

/**
 * A class to test the deletion of logs in the cache
 * The test involves LogCache, LogBufferedFileCache and LogFileCache
 * 
 * @author acaproni
 *
 */
public class DeleteLogTest extends TestCase {
	
	/**
	 * Constructor
	 *
	 */
	public DeleteLogTest() throws Exception {
		super("DeleteLogTest");
	}
	
	public void setUp() throws Exception {
		super.setUp();
	}
	
	public void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * Test the deletion of logs in the LogFileCache
	 * The test delete the first log, the last log and one log in the
	 * middel of the cache. After each deletion a bounce of checks assure
	 * the integrity of the cache
	 * 
	 * LogFileCache has no cache/buffering inmemory se we can
	 * test with few logs
	 *
	 */
	public void testLogFileCacheDelete() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> c = CacheUtils.generateLogs(512);
		LogFileCache cache;
		cache= new LogFileCache();
		for (ILogEntry temp : c) {
			cache.add(temp); 
		}
		assertEquals("Wrong number of log in cache",cache.getSize(),c.size());
		// Delete the first log
		cache.deleteLog(0);
		// Check if the right log has been deleted
		ILogEntry log=null;
		boolean deletedLog=false;
		try {
			log = cache.getLog(0);
		} catch (LogCacheException e) {
			deletedLog=true;
		}
		assertTrue("The deleted log is still in cache",deletedLog);
		assertEquals("The size of the cache is wrong",cache.getSize(),511);
		// Delete the last log
		cache.deleteLog(511);
		// Check if the last log is ok
		assertEquals("The size of the cache is wrong",cache.getSize(),510);
		
		log = cache.getLog(510);
		int logNumber = Integer.parseInt((String)log.getField(LogField.LOGMESSAGE));
		assertEquals("The log in last position is wrong",logNumber,510);
		
		// Delete a log in pos 100 
		cache.deleteLog(100);
		assertEquals("The size of the cache is wrong",cache.getSize(),509);
		ILogEntry log1 = cache.getLog(99); // The record before the deleted record
		logNumber = Integer.parseInt((String)log1.getField(LogField.LOGMESSAGE));
		assertEquals("The log in position 99 is wrong",logNumber,99);
		ILogEntry log2 = cache.getLog(101); // The record after the deleted one
		logNumber = Integer.parseInt((String)log2.getField(LogField.LOGMESSAGE));
		assertEquals("The log in position 101 is wrong",logNumber,101);
		// Try to delete a log not in the cache
		boolean gotAnException=false;
		try {
			cache.deleteLog(1500);
		} catch (LogCacheException e) {
			gotAnException=true;
		}
		assertTrue("Error deleting a log not in cache",gotAnException);
	}
	
	/**
	 * Generate a cache and randomly delete all its logs
	 * For each deleted log, the content of the cache is
	 * checked against the content of the collection to verify 
	 * the consistency its content 
	 * 
	 * @throws Exception
	 */
	public void testDeleteAllFromLogFileCache() throws Exception {
		//	Create and populate the cache
		Vector<ILogEntry> v = (Vector<ILogEntry>)CacheUtils.generateLogs(512);
		HashMap<Integer,ILogEntry> logs = new  HashMap<Integer,ILogEntry>();
		LogFileCache cache;
		cache= new LogFileCache();
		for (ILogEntry temp : v) {
			int key=cache.add(temp);
			logs.put(key,temp);
		}
		assertEquals("Wrong number of log in cache",cache.getSize(),logs.size());
		Random rnd = new Random(Calendar.getInstance().getTimeInMillis());
		while (logs.size()>0) {
			int index = rnd.nextInt(logs.size());
			Set<Integer> s =logs.keySet();  
			Object[] keys=s.toArray();
			Integer key = (Integer)keys[index];
			cache.deleteLog(key);
			logs.remove(key);
			assertEquals("The size of the cache and the collection differs",logs.size(),cache.getSize());
			s=logs.keySet();
			Iterator<Integer> iter=s.iterator();
			while (iter.hasNext()) {
				key=iter.next();
				assertEquals("Content of LogFileCache and collection differs",
						cache.getLog(key).getField(LogField.LOGMESSAGE),
						logs.get(key).getField(LogField.LOGMESSAGE));
			}
		}
	}
	
	/**
	 * Generate a cache and randomly delete all its logs
	 * For each deleted log, the content of the cache is
	 * checked against the content of the collection to verify 
	 * the consistency its content. Such test is done comparing the content 
	 * of the maessage of the log with the content of the collection
	 * The test check also the consistency of the arrays of times and types
	 * (logTimes and logTypes in LogCache)
	 * At each iteration we try to fill the in-memory cache because 
	 * we must stress this part.
	 * 
	 * @throws Exception
	 */
	public void testDeleteAllFromLogCache() throws Exception {
		//	Create and populate the cache
		Vector<ILogEntry> v = (Vector<ILogEntry>)CacheUtils.generateLogs(512);
		HashMap<Integer,ILogEntry> logs = new  HashMap<Integer,ILogEntry>();
		LogCache cache;
		try {
			cache= new LogCache(128);
		} catch (LogCacheException lce) {
			System.out.println("Error creating the LogFileCache");
			throw lce;
		}
		for (ILogEntry temp : v) {
			Integer key=cache.add(temp);
			logs.put(key,temp);
		}
		assertEquals("Wrong number of log in cache",cache.getSize(),logs.size());
		Random rnd = new Random(Calendar.getInstance().getTimeInMillis());
		while (logs.size()>0) {
			// Fill the in-memory cache executing some getLog()
			int filled=0;
			int index = rnd.nextInt(logs.size());
			Set<Integer> s =logs.keySet();  
			Object[] keys=s.toArray();
			int temp=0;
			while (temp<logs.size() && filled<128) {
				Integer key = (Integer)keys[temp++];
				filled++;
				cache.getLog(key);
			}
			index = rnd.nextInt(logs.size());
			s =logs.keySet();  
			keys=s.toArray();
			Integer key = (Integer)keys[index];
			cache.deleteLog(key);
			logs.remove(key);
			
			assertEquals("The size of the cache and the collection differs",logs.size(),cache.getSize());
			s=logs.keySet();
			Iterator<Integer> iter=s.iterator();
			while (iter.hasNext()) {
				key=iter.next();
				// Compare the content of the Collection and that of the cache
				assertEquals("Content of LogCache and collection differs",
						cache.getLog(key).getField(LogField.LOGMESSAGE),
						logs.get(key).getField(LogField.LOGMESSAGE));
				
				assertEquals("The types differ",
						cache.getLog(key).getField(LogField.ENTRYTYPE),
						cache.getLogType(key));
				assertEquals("The times differ",
						((Long)cache.getLog(key).getField(LogField.TIMESTAMP)),
						cache.getLogTimestamp(key));
			}
		}
	}
	
	/**
	 * Generate a cache and randomly delete all its logs
	 * For each deleted log, the content of the cache is
	 * checked against the content of the collection to verify 
	 * the consistency its content
	 * 
	 * The tests check 2 cases:
	 *    1 all the logs in the buffer
	 *    2 some log on disk and the others in the buffer
	 * 
	 * @throws Exception
	 */
	public void testDeleteAllFromLogBufferedFileCache() throws Exception {
		//	Create and populate the cache
		// It checks 2 cases:
		//   - testN=0: all the logs are in the buffer
		//   - testN=1: the logs are part in the buffer and part on disk
		for (int testN=0; testN<2; testN++) {
			Vector<ILogEntry> v = (Vector<ILogEntry>)CacheUtils.generateLogs(512);
			HashMap<Integer,ILogEntry> logs = new  HashMap<Integer,ILogEntry>();
			LogBufferedFileCache cache;
			try {
				if (testN==0) {
					cache= new LogBufferedFileCache(v.size()+1);
				} else {
					cache= new LogBufferedFileCache(v.size()/2+1);
				}
			} catch (LogCacheException lce) {
				System.out.println("Error creating the LogBufferedFileCache");
				throw lce;
			}
			for (ILogEntry temp : v) {
				Integer key=cache.add(temp);
				logs.put(key,temp);
			}
			assertTrue("Wrong number of log in cache",cache.getSize()==v.size());
			Random rnd = new Random(Calendar.getInstance().getTimeInMillis());
			while (logs.size()>0) {
				int index = rnd.nextInt(logs.size());
				Set<Integer> s =logs.keySet();  
				Object[] keys=s.toArray();
				Integer key = (Integer)keys[index];
				cache.deleteLog(key);
				logs.remove(key);
				
				assertEquals("The size of the cache and the collection differs",logs.size(),cache.getSize());
				s=logs.keySet();
				Iterator<Integer> iter=s.iterator();
				while (iter.hasNext()) {
					key=iter.next();
					assertEquals("Content of LogFileCache and collection differs",
							cache.getLog(key).getField(LogField.LOGMESSAGE),
							logs.get(key).getField(LogField.LOGMESSAGE));
				}
			}
		}
	}
	
	/**
	 * Test the deletion of logs in LogBufferedFileCache
	 * with all the logs in the buffer (i.e. the size of the buffer
	 * is so big to store all the allocated logs)
	 * 
	 * @throws Exception
	 */
	public void testLogBufferedFileCacheDelete() throws Exception {
		//	Create and populate the cache
		Collection<ILogEntry> c = CacheUtils.generateLogs(512);
		LogBufferedFileCache cache;
		try {
			cache= new LogBufferedFileCache(c.size()+1); // Enough room for all the logs in the collection
		} catch (LogCacheException lce) {
			System.out.println("Error creating the LogBufferedFileCache");
			throw lce;
		}
		for (ILogEntry temp : c) {
			cache.add(temp); 
		}
		assertEquals("Wrong number of log in cache",cache.getSize(),c.size());
		assertEquals("Wrong number of logs in buffer ",cache.getBufferSize(),c.size());
		// Delete the first log (this is not in the buffer)
		cache.deleteLog(0);
		ILogEntry log=null;
		boolean deletedLog=false;
		try {
			log= cache.getLog(0);
		} catch (LogCacheException e) {
			deletedLog=true;
		}
		assertEquals("The LogBufferedFileCache has wrong size",cache.getSize(),511);
		assertEquals("Wrong number of logs in buffer ",cache.getBufferSize(),511);
		assertTrue("The log is still in cache",deletedLog);
		// Remove the last log in cache
		cache.deleteLog(511);
		assertEquals("Wrong number of log in cache",cache.getSize(),510);
		assertEquals("Wrong number of logs in buffer ",cache.getBufferSize(),510);
		log = cache.getLog(510);
		int logNum = Integer.parseInt((String)(log.getField(LogField.LOGMESSAGE)));
		assertEquals("Wrong content",logNum,510);
		// Remove one log from the middle
		cache.deleteLog(100); // The content in pos 100 (its content is 101 because 0 was deleted)
		assertEquals("Wrong number of log in cache",cache.getSize(),509);
		assertEquals("Wrong number of logs in buffer ",cache.getBufferSize(),509);
		ILogEntry log1=cache.getLog(99);
		logNum = Integer.parseInt((String)(log1.getField(LogField.LOGMESSAGE)));
		assertEquals("Wrong content",logNum,99);
		ILogEntry log2=cache.getLog(101);
		logNum = Integer.parseInt((String)(log2.getField(LogField.LOGMESSAGE)));
		assertEquals("Wrong content",logNum,101);
	}
	
	
	
	
	
	/**
	 * Test the deletion of logs in the LogFileCache
	 * The test delete the first log, the last log and one log in the
	 * middel of the cache. After each deletion a bounce of checks assure
	 * the integrity of the cache
	 * 
	 * LogFileCache has no cache/buffering inmemory se we can
	 * test with few logs
	 *
	 */
	public void testLogCacheDelete() throws Exception {
		// Create and populate the cache
		Collection<ILogEntry> c = CacheUtils.generateLogs(512);
		LogCache cache;
		try {
			cache= new LogCache(128);
		} catch (LogCacheException lce) {
			System.out.println("Error creating the LogFileCache");
			throw lce;
		}
		for (ILogEntry temp : c) {
			cache.add(temp); 
		}
		assertTrue("Wrong number of log in cache",cache.getSize()==c.size());
		// At startup the in-memory cache is empty: we execute some getLog to 
		// move logs into the in-memory cache
		for (int t=0; t<128; t++) {
			cache.getLog(t);
		}
		// Delete the log in pos 0 (it is also in the in-memory cache)
		cache.deleteLog(0);
		// Check if the right log has been deleted
		ILogEntry log;
		boolean logDeleted=false;
		try {
			log= cache.getLog(0);
		} catch (LogCacheException e) {
			logDeleted=true;
		}
		assertTrue("The log has not been deleted",logDeleted);
		assertEquals("The size of the cache is wrong",cache.getSize(),511);
		// Fill again the cache
		for (int t=1; t<129; t++) {
			cache.getLog(t);
		}
		// Delete the last log in cache (it is not in the in-memory cache)
		cache.deleteLog(510);
		assertEquals("The size of the cache is wrong",cache.getSize(),510);
		log = cache.getLog(509);
		int logNum = Integer.parseInt((String)log.getField(LogField.LOGMESSAGE));
		assertEquals("The log in last position is wrong",logNum,509);
		// Get one log from the middle (it is till in the in-memory cache)
		cache.deleteLog(100); // The content in pos 100 is 101 because 0 was deleted
		assertEquals("Wrong number of log in cache",cache.getSize(),509);
		ILogEntry log1=cache.getLog(99);
		logNum = Integer.parseInt((String)(log1.getField(LogField.LOGMESSAGE)));
		assertEquals("Wrong content",logNum,99);
		ILogEntry log3=cache.getLog(101);
		logNum = Integer.parseInt((String)(log3.getField(LogField.LOGMESSAGE)));
		assertEquals("Wrong content",logNum,101);
		logDeleted=false;
		try {
			log=cache.getLog(100);
		} catch (LogCacheException e) {
			logDeleted=true;
		}
		assertTrue("The log has not been deleted",logDeleted);
	}
	
	/**
	 * Check the content of the cache (LogCache)
	 * 
	 * @throws Exception
	 */
	public void testContent() throws Exception {
		LogCache cache = new LogCache();
		Vector<ILogEntry> c = (Vector<ILogEntry>)CacheUtils.generateLogs(4096);
		for (ILogEntry temp : c) {
			cache.add(temp); 
		}
		assertEquals("Lengths differ",cache.getSize(),c.size());
		for (int t=0; t<c.size(); t++) {
			ILogEntry logCache = cache.getLog(t);
			ILogEntry logVector= c.get(t);
			assertEquals("Log msgs differ",logCache.getField(LogField.LOGMESSAGE),logVector.getField(LogField.LOGMESSAGE));
			assertEquals("Log type differ",logCache.getField(LogField.ENTRYTYPE),logVector.getField(LogField.ENTRYTYPE));
			assertEquals("Log time differ",logCache.getField(LogField.TIMESTAMP),logVector.getField(LogField.TIMESTAMP));
		}
	}
	
	/**
	 * Test the getFirstLog after deletion of logs for LogFileCache
	 * 
	 * @throws Exception
	 */
	public void testGetFirstLogLogFileCache() throws Exception{
		// First test the LogFileCache
		LogFileCache cache = new LogFileCache();
		assertNull("Error getting the first log from an empty cache",cache.getFirstLog());
		Vector<ILogEntry> c = (Vector<ILogEntry>)CacheUtils.generateLogs(4096);
		for (ILogEntry temp : c) {
			cache.add(temp); 
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		
		for (int t=0; t<2048; t++) {
			cache.deleteLog(t);
			assertEquals("Error getting the first log",cache.getFirstLog(),new Integer(t+1));
		}
	}
	
	/**
	 * Test the getFirstLog after deletion of logs for LogBufferedFileCache
	 * 
	 * @throws Exception
	 */
	public void testGetFirstLogLogBufferedFileCache() throws Exception{
		// First test the LogFileCache
		Vector<ILogEntry> c = (Vector<ILogEntry>)CacheUtils.generateLogs(4096);
		LogBufferedFileCache cache = new LogBufferedFileCache(2049);
		assertNull("Error getting the first log from an empty cache",cache.getFirstLog());
		for (ILogEntry temp : c) {
			cache.add(temp); 
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		// We have half logs in the buffer
		assertEquals("Wrong number of logs in buffer",cache.getBufferSize(),2047);
		for (int t=0; t<3000; t++) {
			cache.deleteLog(t);
			assertEquals("Error getting the first log",cache.getFirstLog(),new Integer(t+1));
		}
	}
	
	/**
	 * Test the getLastLog after deletion of logs for LogFileCache
	 * 
	 * @throws Exception
	 */
	public void testGetLastLogLogFileCache() throws Exception{
		// First test the LogFileCache
		LogFileCache cache = new LogFileCache();
		assertNull("Error getting the first log from an empty cache",cache.getFirstLog());
		Vector<ILogEntry> c = (Vector<ILogEntry>)CacheUtils.generateLogs(4096);
		for (ILogEntry temp : c) {
			cache.add(temp); 
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		
		for (int t=4095; t<2048; t--) {
			cache.deleteLog(t);
			assertEquals("Error getting the last log",cache.getLastLog(),new Integer(t-1));
		}
	}
	
	/**
	 * Test the getLastLog after deletion of logs for LogBufferedFileCache
	 * 
	 * @throws Exception
	 */
	public void testGetLastLogLogBufferedFileCache() throws Exception{
		// First test the LogFileCache
		Vector<ILogEntry> c = (Vector<ILogEntry>)CacheUtils.generateLogs(4096);
		LogBufferedFileCache cache = new LogBufferedFileCache(2049);
		assertNull("Error getting the first log from an empty cache",cache.getFirstLog());
		for (ILogEntry temp : c) {
			cache.add(temp); 
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		// We have half logs in the buffer
		assertEquals("Wrong number of logs in buffer",cache.getBufferSize(),2047);
		for (int t=4095; t<1000; t++) {
			cache.deleteLog(t);
			assertEquals("Error getting the first log",cache.getLastLog(),new Integer(t-1));
		}
	}
	
	/**
	 * Test the deletion of a collection of logs from
	 * LogFileCache
	 * 
	 * @throws Exception
	 */
	public void testDeleteLogsFromLogFileCache() throws Exception {
		// Fills the cache
		LogFileCache cache = new LogFileCache();
		assertNull("Error getting the first log from an empty cache",cache.getFirstLog());
		Vector<ILogEntry> c = (Vector<ILogEntry>)CacheUtils.generateLogs(4096);
		Vector<Integer> keysInCache = new Vector<Integer>(c.size());
		for (ILogEntry temp : c) {
			Integer key=cache.add(temp);
			keysInCache.add(key);
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		
		// Delete an empty collection of keys
		Vector<Integer> empty = new Vector<Integer>();
		cache.deleteLogs(empty);
		assertEquals("Wrong size after deleting an empty collection of keys",c.size(),cache.getSize());
		
		// Delete a collection with the first and last key
		Vector<Integer> firstLast = new Vector<Integer>(2);
		firstLast.add(0);
		firstLast.add(4095);
		cache.deleteLogs(firstLast);
		assertEquals("Wrong size after deletion",c.size()-2,cache.getSize());
		// Check if 0 and 4095 in cache
		ILogEntry log0;
		try {
			log0=cache.getLog(0);
		} catch (LogCacheException e) {
			log0=null;
		}
		assertNull("The log with key 0 is still in cache",log0);
		ILogEntry log4095;
		try {
			log4095=cache.getLog(4095);
		} catch (LogCacheException e) {
			log4095=null;
		}
		assertNull("The log with key 4095 is still in cache",log4095);
		
		// Remove a random collection
		keysInCache.remove(new Integer(0));
		keysInCache.remove(new Integer(4095));
		Collection<Integer> keys = CacheUtils.generateKeys(4094,false,1,4094,keysInCache);
		int oldSz=cache.getSize();
		cache.deleteLogs(keys);
		assertEquals("Wrong size after deletion",oldSz-keys.size(),cache.getSize());
		// Check the content of the cache after deletion
		for (Integer key: keysInCache) {
			ILogEntry log;
			try {
				log=cache.getLog(key);
			} catch (LogCacheException e) {
				log=null;
			}
			if (keys.contains(key)) {
				// This log should have been deleted
				assertNull("The log should have been deleted",log);
			} else {
				assertNotNull("The log should have not been deleted",log);
			}
		}
		
		// Release the cache
		cache.clear();
		keysInCache.clear();
		keysInCache=null;
		c.clear();
		c=null;
		cache=null;
		
		// Create a new cache to test the deletion of the whole cache
		cache = new LogFileCache();
		c = (Vector<ILogEntry>)CacheUtils.generateLogs(1024);
		keysInCache = new Vector<Integer>(c.size());
		for (ILogEntry temp : c) {
			Integer key=cache.add(temp);
			keysInCache.add(key);
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		cache.deleteLogs(keysInCache);
		assertEquals("Not all the logs have been deleted",cache.getSize(),0);
	}
	
	/**
	 * Test the deletion of a collection of logs from
	 * LogBufferedFileCache
	 * 
	 * @throws Exception
	 */
	public void testDeleteLogsFromLogBufferedFileCache() throws Exception {
		// Fills the cache
		LogBufferedFileCache cache = new LogBufferedFileCache();
		assertNull("Error getting the first log from an empty cache",cache.getFirstLog());
		Vector<ILogEntry> c = (Vector<ILogEntry>)CacheUtils.generateLogs(4096);
		Vector<Integer> keysInCache = new Vector<Integer>(c.size());
		for (ILogEntry temp : c) {
			Integer key=cache.add(temp);
			keysInCache.add(key);
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		
		// Delete an empty collection of keys
		Vector<Integer> empty = new Vector<Integer>();
		cache.deleteLogs(empty);
		assertEquals("Wrong size after deleting an empty collection of keys",c.size(),cache.getSize());
		
		// Delete a collection with the first and last key
		Vector<Integer> firstLast = new Vector<Integer>(2);
		firstLast.add(0);
		firstLast.add(4095);
		cache.deleteLogs(firstLast);
		assertEquals("Wrong size after deletion",c.size()-2,cache.getSize());
		// Check if 0 and 4095 in cache
		ILogEntry log0;
		try {
			log0=cache.getLog(0);
		} catch (LogCacheException e) {
			log0=null;
		}
		assertNull("The log with key 0 is still in cache",log0);
		ILogEntry log4095;
		try {
			log4095=cache.getLog(4095);
		} catch (LogCacheException e) {
			log4095=null;
		}
		assertNull("The log with key 4095 is still in cache",log4095);
		
		// Remove a random collection
		keysInCache.remove(new Integer(0));
		keysInCache.remove(new Integer(4095));
		Collection<Integer> keys = CacheUtils.generateKeys(4095,false,1,4094,keysInCache);
		int oldSz=cache.getSize();
		cache.deleteLogs(keys);
		assertEquals("Wrong size after deletion",oldSz-keys.size(),cache.getSize());
		// Check the content of the cache after deletion
		for (Integer key: keysInCache) {
			ILogEntry log;
			try {
				log=cache.getLog(key);
			} catch (LogCacheException e) {
				log=null;
			}
			if (keys.contains(key)) {
				// This log should have been deleted
				assertNull("The log should have been deleted",log);
			} else {
				assertNotNull("The log should have not been deleted",log);
			}
		}
		
		// Release the cache
		cache.clear();
		keysInCache.clear();
		keysInCache=null;
		c.clear();
		c=null;
		cache=null;
		
		// Create a new cache to test the deletion of the whole cache
		cache = new LogBufferedFileCache();
		c = (Vector<ILogEntry>)CacheUtils.generateLogs(1024);
		keysInCache = new Vector<Integer>(c.size());
		for (ILogEntry temp : c) {
			Integer key=cache.add(temp);
			keysInCache.add(key);
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		cache.deleteLogs(keysInCache);
		assertEquals("Not all the logs have been deleted",cache.getSize(),0);
	}
	
	/**
	 * Test the deletion of a collection of logs from
	 * LogCache
	 * 
	 * @throws Exception
	 */
	public void testDeleteLogsFromLogCache() throws Exception {
		// Fills the cache
		LogCache cache = new LogCache();
		assertNull("Error getting the first log from an empty cache",cache.getFirstLog());
		Vector<ILogEntry> c = (Vector<ILogEntry>)CacheUtils.generateLogs(4096);
		Vector<Integer> keysInCache = new Vector<Integer>(c.size());
		for (ILogEntry temp : c) {
			Integer key=cache.add(temp);
			keysInCache.add(key);
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		
		// Delete an empty collection of keys
		Vector<Integer> empty = new Vector<Integer>();
		cache.deleteLogs(empty);
		assertEquals("Wrong size after deleting an empty collection of keys",c.size(),cache.getSize());
		
		// Delete a collection with the first and last key
		Vector<Integer> firstLast = new Vector<Integer>(2);
		firstLast.add(0);
		firstLast.add(4095);
		cache.deleteLogs(firstLast);
		assertEquals("Wrong size after deletion",c.size()-2,cache.getSize());
		// Check if 0 and 4095 in cache
		ILogEntry log0;
		try {
			log0=cache.getLog(0);
		} catch (LogCacheException e) {
			log0=null;
		}
		assertNull("The log with key 0 is still in cache",log0);
		ILogEntry log4095;
		try {
			log4095=cache.getLog(4095);
		} catch (LogCacheException e) {
			log4095=null;
		}
		assertNull("The log with key 4095 is still in cache",log4095);
		
		// Remove a random collection
		keysInCache.remove(new Integer(0));
		keysInCache.remove(new Integer(4095));
		Collection<Integer> keys = CacheUtils.generateKeys(4095,false,1,4094,keysInCache);
		int oldSz=cache.getSize();
		cache.deleteLogs(keys);
		assertEquals("Wrong size after deletion",oldSz-keys.size(),cache.getSize());
		// Check the content of the cache after deletion
		for (Integer key: keysInCache) {
			ILogEntry log;
			try {
				log=cache.getLog(key);
			} catch (LogCacheException e) {
				log=null;
			}
			if (keys.contains(key)) {
				// This log should have been deleted
				assertNull("The log should have been deleted",log);
			} else {
				assertNotNull("The log should have not been deleted",log);
			}
		}
		
		// Release the cache
		cache.clear();
		keysInCache.clear();
		keysInCache=null;
		c.clear();
		c=null;
		cache=null;
		
		// Create a new cache to test the deletion of the whole cache
		cache = new LogCache();
		c = (Vector<ILogEntry>)CacheUtils.generateLogs(1024);
		keysInCache = new Vector<Integer>(c.size());
		for (ILogEntry temp : c) {
			Integer key=cache.add(temp);
			keysInCache.add(key);
		}
		assertEquals("Wrong number of logs in cache",cache.getSize(),c.size());
		cache.deleteLogs(keysInCache);
		assertEquals("Not all the logs have been deleted",cache.getSize(),0);
	}
}
