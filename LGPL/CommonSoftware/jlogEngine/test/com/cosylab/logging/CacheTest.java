package com.cosylab.logging;

import com.cosylab.logging.client.cache.*;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.client.GroupedList;
import com.cosylab.logging.engine.log.LogEntryXML;
import com.cosylab.logging.engine.ACS.ACSLogParser;

/**
 * The class to test the LogFileCache and the LogCache
 * 
 * @author acaproni
 *
 * @see junit.framework.TestCase
 */
public class CacheTest extends junit.framework.TestCase {
	LogCache cache;

	public CacheTest(String str) {
		super(str);
	}

	/**
	 * This execute for each test and we want to have
	 * a cache with some logs 
	 * 
	 * @see junit.framework.TestCase
	 */ 
	protected void setUp() throws Exception
	{ 
		try {
			cache = new LogCache();
		} catch (Exception e) {
			System.out.println("Error creating the cache "+e.getMessage());
			e.printStackTrace();
			System.exit(0);
		}
		load();
	}

	/** 
	 * @see junit.framework.TestCase
	 *
	 */
	protected void tearDown()
	{
		cache.clear();
		cache=null;
	}
	
	/**
	 * Load a cache with the read
	 * @throws Exception
	 */
	private void load() throws Exception {
		FiltersVector filters = new FiltersVector();
		filters.clear();
		GroupedList list = new GroupedList(cache);
		list.clear();
		cache.loadLogs("test.xml",filters,list,false);
		// The load is asynchronous
		// We need to poll to be sure that the load is terminated
		while (true) {
			if (!cache.isPerformingIO()) {
				// Ok, the load has finished
				break;
			}
			try {
				Thread.sleep(250);
			} catch (InterruptedException ie) {}
		}
	}
	
	/** 
	 * It checks if all the logs in the file are in the cache
	 * 
	 * @throws Exception
	 */
	public void testSize() throws Exception {
		assertEquals("Error loading logs",(long)cache.getSize(),300L);
	}
	
	/**
	 * Get all the logs in cache to check the get method
	 * 
	 * @throws Exception
	 */
	public void testGet() throws Exception {
		LogEntryXML log;
		for (int t=0; t<cache.getSize(); t++) {
			log = cache.getLog(0);
			assertNotNull("Error getting the log "+t,log);
		}
	}
	
	/**
	 * Check the add method by inserting and reading a log
	 *
	 */
	public void testAddLog() throws Exception {
		ACSLogParser parser = new ACSLogParser();
		int oldSize = cache.getSize();
		String logMsg = "Test log";
		String logStr = "<Info TimeStamp=\"2005-11-29T15:33:09.592\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA["+logMsg+"]]></Info>";
		LogEntryXML newLog = parser.parse(logStr);
		cache.add(newLog);
		assertEquals("Error adding a log",cache.getSize(),oldSize+1);
		LogEntryXML log = cache.getLog(cache.getSize()-1);
		String msg = (String)log.getField(LogEntryXML.FIELD_LOGMESSAGE);
		assertEquals("Error adding a log",logMsg,msg);
	}
	
	/**
	 * Test the replacement of a log
	 *
	 */
	public void testReplace() throws Exception {
		ACSLogParser parser = new ACSLogParser();
		String logMsg = "Replaced test log";
		String logStr = "<Info TimeStamp=\"2005-11-29T16:00:00.000\" Routine=\"CacheTest::testReplace\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA["+logMsg+"]]></Info>";
		LogEntryXML newLog = parser.parse(logStr);
		// Replace the first log
		cache.replaceLog(0,newLog);
		assertEquals("Error replacing log "+0,logMsg,(String)cache.getLog(0).getField(LogEntryXML.FIELD_LOGMESSAGE));
		// Replace the last log
		cache.replaceLog(cache.getSize()-1,newLog);
		assertEquals("Error replacing log "+(cache.getSize()-1),logMsg,(String)cache.getLog(cache.getSize()-1).getField(LogEntryXML.FIELD_LOGMESSAGE));
		// Replace a log in the middle
		int pos = cache.getSize()/2;
		cache.replaceLog(pos,newLog);
		assertEquals("Error replacing log "+pos,logMsg,(String)cache.getLog(pos).getField(LogEntryXML.FIELD_LOGMESSAGE));
	}
	
	/**
	 * test if a cleared cache contains exactly 0 logs
	 * 
	 * @throws Exception
	 */
	public void testClear() throws Exception {
		cache.clear();
		assertEquals("Error clearing the cache",(long)cache.getSize(),0L);
	}
	
	/**
	 * It is deifficult to test LogCache...
	 * This is better then nothing
	 * 
	 * I scan the list and for every log I read three logs are read again and checked
	 * These three logs should always be in memory but I cannot check nothing but
	 * they they are always the same!
	 *
	 */
	public void testMemoryCache() throws Exception {
		int first = 0;
		String firstMsg = (String)cache.getLog(first).getField(LogEntryXML.FIELD_LOGMESSAGE);
		int last = cache.getSize()-1;
		String lastMsg = (String)cache.getLog(last).getField(LogEntryXML.FIELD_LOGMESSAGE);
		int pos = cache.getSize()/2;
		String posMsg = (String)cache.getLog(pos).getField(LogEntryXML.FIELD_LOGMESSAGE);
		
		// Scans the list
		for (int t=0; t<last; t++) {
			cache.getLog(t);
			LogEntryXML firstLog = cache.getLog(first);
			assertEquals("Error in mem cache pos "+first,firstMsg, firstLog.getField(LogEntryXML.FIELD_LOGMESSAGE));
			LogEntryXML lastLog = cache.getLog(last);
			assertEquals("Error in mem cache pos "+last,lastMsg, lastLog.getField(LogEntryXML.FIELD_LOGMESSAGE));
			LogEntryXML posLog = cache.getLog(pos);
			assertEquals("Error in mem cache pos "+pos,posMsg, posLog.getField(LogEntryXML.FIELD_LOGMESSAGE));
		}
	}
}
