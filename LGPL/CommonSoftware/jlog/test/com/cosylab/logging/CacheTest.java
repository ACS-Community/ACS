/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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
package com.cosylab.logging;

import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Set;

import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.util.IsoDateFormat;

/**
 * The class to test the LogFileCache and the LogCache
 * 
 * @author acaproni
 *
 * @see junit.framework.TestCase
 */
public class CacheTest extends junit.framework.TestCase {
	
	// The cache to stress

	private LogCache cache;
	
	// Number of logs generated dynamically
	// Its value is returned by the fillCache methods
	private long logsGenerated; 

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
			throw e;
		}
		logsGenerated=fillCache();
	}

	/** 
	 * @see junit.framework.TestCase
	 *
	 */
	protected void tearDown()
	{
		try {
			cache.clear();
		} catch (Exception e) {
			System.out.println("Exception while clearing the cache"+e.getMessage());
			e.printStackTrace();
		}
		cache=null;
	}
	
	/**
	 * Fill the cache with dynamically generated logs
	 * The number of logs inserted in the list is greater than the 
	 * memory cache size to stress the disk cache also.
	 * 
	 * @return The number of logs inserted in the cache
	 * 
	 * @throws Exception
	 */
	private long fillCache() throws Exception {
		ACSLogParser parser = ACSLogParserFactory.getParser();
		String logMsg = "Test log nr. ";
		
		long now = Calendar.getInstance().getTimeInMillis()-1000*60*60*24; // Yesterday
		SimpleDateFormat df = new IsoDateFormat();
		
		cache.clear();
		long logToInsert = 2*cache.getCacheSize(); 
		for (int t=0; t<logToInsert; t++) {
			Date dt = new Date(now+t*1000);
			StringBuffer dateSB = new StringBuffer();
			FieldPosition pos = new FieldPosition(0);
			df.format(dt,dateSB,pos);
			
			String newLogMsg=logMsg+"t";
			StringBuilder logStr = new StringBuilder("<Info TimeStamp=\"");
			logStr.append(dateSB.toString());
			logStr.append("\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[");
			logStr.append(newLogMsg);
			logStr.append("]]></Info>");
			ILogEntry newLog = parser.parse(logStr.toString());
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
		ACSLogParser parser = ACSLogParserFactory.getParser();
		int oldSize = cache.getSize();
		String logMsg = "Test log";
		String logStr = "<Info TimeStamp=\"2005-11-29T15:33:10.592\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA["+logMsg+"]]></Info>";
		ILogEntry newLog = parser.parse(logStr);
		cache.add(newLog);
		assertEquals("Error adding a log",cache.getSize(),oldSize+1);
		ILogEntry log = cache.getLog(cache.getSize()-1);
		String msg = (String)log.getField(LogField.LOGMESSAGE);
		assertEquals("Error adding a log",logMsg,msg);
	}
	
	/**
	 * Test the replacement of a log
	 *
	 */
	public void testReplace() throws Exception {
		ACSLogParser parser = ACSLogParserFactory.getParser();
		String logMsg = "Replaced test log";
		String logStr = "<Info TimeStamp=\"2005-11-29T16:00:00.000\" Routine=\"CacheTest::testReplace\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA["+logMsg+"]]></Info>";
		ILogEntry newLog = parser.parse(logStr);
		// Replace the first log
		cache.replaceLog(0,newLog);
		assertEquals("Error replacing log "+0,logMsg,(String)cache.getLog(0).getField(LogField.LOGMESSAGE));
		// Replace the last log
		cache.replaceLog(cache.getSize()-1,newLog);
		assertEquals("Error replacing log "+(cache.getSize()-1),logMsg,(String)cache.getLog(cache.getSize()-1).getField(LogField.LOGMESSAGE));
		// Replace a log in the middle
		int pos = cache.getSize()/2;
		cache.replaceLog(pos,newLog);
		assertEquals("Error replacing log "+pos,logMsg,(String)cache.getLog(pos).getField(LogField.LOGMESSAGE));
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
		String firstMsg = (String)cache.getLog(first).getField(LogField.LOGMESSAGE);
		int last = cache.getSize()-1;
		String lastMsg = (String)cache.getLog(last).getField(LogField.LOGMESSAGE);
		int pos = cache.getSize()/2;
		String posMsg = (String)cache.getLog(pos).getField(LogField.LOGMESSAGE);
		
		// Scans the list
		for (int t=0; t<last; t++) {
			cache.getLog(t);
			ILogEntry firstLog = cache.getLog(first);
			assertEquals("Error in mem cache pos "+first,firstMsg, firstLog.getField(LogField.LOGMESSAGE));
			ILogEntry lastLog = cache.getLog(last);
			assertEquals("Error in mem cache pos "+last,lastMsg, lastLog.getField(LogField.LOGMESSAGE));
			ILogEntry posLog = cache.getLog(pos);
			assertEquals("Error in mem cache pos "+pos,posMsg, posLog.getField(LogField.LOGMESSAGE));
		}
	}
	
	/**
	 * Check the calculation of the time frame 
	 * 
	 * @throws Exception
	 */
	public void testTimeFrameCalc() throws Exception {
		ACSLogParser parser = ACSLogParserFactory.getParser();
		// Create some logs with a time frame of 10sec
		String logStr1 = "<Info TimeStamp=\"2005-11-29T15:33:10.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test1]]></Info>";
		String logStr2 = "<Info TimeStamp=\"2005-11-29T15:33:20.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test2]]></Info>";
		String logStr3 = "<Info TimeStamp=\"2005-11-29T15:33:15.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test3]]></Info>";
		String logStr4 = "<Info TimeStamp=\"2005-11-29T15:33:12.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test4]]></Info>";
		cache.clear();
		cache.add(parser.parse(logStr1));
		cache.add(parser.parse(logStr2));
		cache.add(parser.parse(logStr3));
		cache.add(parser.parse(logStr4));
		Calendar cal=cache.getTimeFrame();
		assertEquals("The time frame is wrong",10*1000,cal.getTimeInMillis());
	}
	
	/**
	 * Check if the method returning the logs exceeding the time frame is
	 * working as expected.
	 * 
	 * @throws Exception
	 */
	public void testLogExceedingTimeFrame() throws Exception {
		ACSLogParser parser = ACSLogParserFactory.getParser();
		// Create some logs 
		// The important fields here are the times (we'll test against a time frame of 30 secs)
		// It is also important the message that is used to check which messages
		// are added in the collection and which not
		String logStr1 = "<Info TimeStamp=\"2005-11-29T15:33:55.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test1]]></Info>";
		String logStr2 = "<Info TimeStamp=\"2005-11-29T15:33:20.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test2]]></Info>";
		String logStr3 = "<Info TimeStamp=\"2005-11-29T15:33:10.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test3]]></Info>";
		String logStr4 = "<Info TimeStamp=\"2005-11-29T15:34:15.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test4]]></Info>";
		String logStr5 = "<Info TimeStamp=\"2005-11-29T15:33:12.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test5]]></Info>";
		String logStr6 = "<Info TimeStamp=\"2005-11-29T15:34:05.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test6]]></Info>";
		String logStr7 = "<Info TimeStamp=\"2005-11-29T15:34:15.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test7]]></Info>";
		String logStr8 = "<Info TimeStamp=\"2005-11-29T15:34:10.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test8]]></Info>";
		String logStr9 = "<Info TimeStamp=\"2005-11-29T15:33:25.000\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[Test9]]></Info>";
		cache.clear();
		cache.add(parser.parse(logStr1));
		cache.add(parser.parse(logStr2));
		cache.add(parser.parse(logStr3));
		cache.add(parser.parse(logStr4));
		cache.add(parser.parse(logStr5));
		cache.add(parser.parse(logStr6));
		cache.add(parser.parse(logStr7));
		cache.add(parser.parse(logStr8));
		cache.add(parser.parse(logStr9));
		assertEquals("Wrong cache size",9,cache.getSize());
		// An array of boolean: the items marked as true should appear in the Collection
		boolean[] b = {
			true,
			false,
			false,
			true,
			false,
			true,
			true,
			true,
			false };
		// Get the logs exceeding a time frame of 30 secs 
		Collection<Integer> logs = cache.getLogExceedingTimeFrame(30*1000);
		assertEquals("Wrong number of logs exceeding time frame",5,logs.size());
		// Check if the logs returned by getLogExceedingTimeFrame are ok
		boolean[] returned = new boolean[9];
		for (int t=0; t<returned.length; t++) {
			returned[t]=false; // Init all as false
		}
		// Mark as true the logs in the Collection
		for (Integer logN: logs) {
			ILogEntry log = cache.getLog(logN);
			String msg = (String)log.getField(LogField.LOGMESSAGE);
			msg=msg.replaceFirst("Test","");
			Integer pos = Integer.parseInt(msg);
			returned[pos-1]=true;
		}
		// Compare the expected array (b) with the returned values (returned)
		for (int t=0; t<b.length; t++) {
			assertEquals("The log "+t+" is/isn't in the Collection of logs",b[t],returned[t]);
		}
	}
	
	public void testGetFirstLog() throws Exception {
		Integer firstLog = cache.getFirstLog();
		assertEquals("The key of the first log is wrong",firstLog,new Integer(0));
	}
	
	public void testGetLastLog() throws Exception {
		Integer lastLog=cache.getLastLog();
		assertEquals("The key of the last log is wrong",lastLog,new Integer((int)logsGenerated-1));
	}
	
	public void testGetLogs() throws Exception {
		ArrayList<Integer> keys = new ArrayList<Integer>();
		int n = cache.getFirstLogs(1024,keys);
		assertEquals("Wrong number of keys",1024,n);
		for (Integer t=0; t<1024; t++) {
			assertEquals("Wrong key",t,keys.get(t));
		}
	}
	
	/**
	 * Test the keys returned by keyset
	 * 
	 * @throws Exception
	 */
	public void testKeySet() throws Exception {
		// Check the number of the keys
		assertEquals("The logs in cache and the number of keys differ",cache.getSize(),cache.keySet().size());
		// Get all the logs with the keys returned by keySet
		Set<Integer> keys = cache.keySet();
		for (Integer key: keys) {
			ILogEntry log = cache.getLog(key);
			assertNotNull("Got a null log!",log);
		}
		
	}
}
