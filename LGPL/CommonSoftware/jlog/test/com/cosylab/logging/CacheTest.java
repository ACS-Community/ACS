package com.cosylab.logging;

import com.cosylab.logging.client.cache.*;
import com.cosylab.logging.engine.log.LogEntryXML;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.ACS.ACSLogParser;
import com.cosylab.logging.LCEngine;
import com.cosylab.logging.IOLogsHelper;
import com.cosylab.logging.engine.RemoteResponseCallback;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;

/**
 * The class to test the LogFileCache and the LogCache
 * 
 * @author acaproni
 *
 * @see junit.framework.TestCase
 */
public class CacheTest extends junit.framework.TestCase {
	
	// The cahce to stress

	LogCache cache;
	
	// Number of logs generated dynamically
	// Its value is returned by the fillCache methods
	long logsGenerated; 

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
		logsGenerated=fillCache();
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
	 * Fill the cache with dinamically generated logs
	 * The number of logs inserted in the list is gretare than the 
	 * memory cache size to stress the disk cache also.
	 * 
	 * @return The number of logs inserted in the cache
	 * 
	 * @throws Exception
	 */
	private long fillCache() throws Exception {
		ACSLogParser parser = new ACSLogParser();
		String logMsg = "Test log nr. ";
		
		
		cache.clear();
		long logToInsert = 2*cache.getCacheSize(); 
		for (int t=0; t<logToInsert; t++) {
			String newLogMsg=logMsg+"t";
			String logStr = "<Info TimeStamp=\"2005-11-29T15:33:09.592\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA["+newLogMsg+"]]></Info>";
			LogEntryXML newLog = parser.parse(logStr);
			cache.add(newLog);
		}
		return logToInsert;
	}
	
	/** 
	 * It checks if all the logs in the file are in the cache
	 * 
	 * @throws Exception
	 */
	public void testSize() throws Exception {
		assertEquals("Error loading logs",logsGenerated,(long)cache.getSize());
	}
	
	/**
	 * Get all the logs in cache to check the get method
	 * 
	 * @throws Exception
	 */
	public void testGet() throws Exception {
		ILogEntry log;
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
		String logStr = "<Info TimeStamp=\"2005-11-29T15:33:10.592\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA["+logMsg+"]]></Info>";
		LogEntryXML newLog = parser.parse(logStr);
		cache.add(newLog);
		assertEquals("Error adding a log",cache.getSize(),oldSize+1);
		ILogEntry log = cache.getLog(cache.getSize()-1);
		String msg = (String)log.getField(ILogEntry.FIELD_LOGMESSAGE);
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
		assertEquals("Error replacing log "+0,logMsg,(String)cache.getLog(0).getField(ILogEntry.FIELD_LOGMESSAGE));
		// Replace the last log
		cache.replaceLog(cache.getSize()-1,newLog);
		assertEquals("Error replacing log "+(cache.getSize()-1),logMsg,(String)cache.getLog(cache.getSize()-1).getField(ILogEntry.FIELD_LOGMESSAGE));
		// Replace a log in the middle
		int pos = cache.getSize()/2;
		cache.replaceLog(pos,newLog);
		assertEquals("Error replacing log "+pos,logMsg,(String)cache.getLog(pos).getField(ILogEntry.FIELD_LOGMESSAGE));
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
	 * The test is done by reading all the cache sequentially.
	 * The first, middle and last logs acquired in the beginning are 
	 * compared with those retrieved with the sequential scan.
	 *
	 */
	public void testMemoryCache() throws Exception {
		int first = 0;
		String firstMsg = (String)cache.getLog(first).getField(ILogEntry.FIELD_LOGMESSAGE);
		int last = cache.getSize()-1;
		String lastMsg = (String)cache.getLog(last).getField(ILogEntry.FIELD_LOGMESSAGE);
		int pos = cache.getSize()/2;
		String posMsg = (String)cache.getLog(pos).getField(ILogEntry.FIELD_LOGMESSAGE);
		
		// Scans the list
		for (int t=0; t<last; t++) {
			cache.getLog(t);
			ILogEntry firstLog = cache.getLog(first);
			assertEquals("Error in mem cache pos "+first,firstMsg, firstLog.getField(ILogEntry.FIELD_LOGMESSAGE));
			ILogEntry lastLog = cache.getLog(last);
			assertEquals("Error in mem cache pos "+last,lastMsg, lastLog.getField(ILogEntry.FIELD_LOGMESSAGE));
			ILogEntry posLog = cache.getLog(pos);
			assertEquals("Error in mem cache pos "+pos,posMsg, posLog.getField(ILogEntry.FIELD_LOGMESSAGE));
		}
	}
}
