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
package alma.acs.util.stringqueue.test;

import java.io.File;
import java.io.IOException;
import java.security.InvalidParameterException;
import java.util.Calendar;
import java.util.Date;
import java.util.Random;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.stringqueue.QueueEntry;
import alma.acs.util.stringqueue.TimestampedStringQueue;
import alma.acs.util.stringqueue.DefaultQueueFileHandlerImpl;
import alma.acs.util.stringqueue.TimestampedStringQueueFileHandler;

/** 
 * @author  acaproni
 * 
 * Tests the notification of {@link TimestampedStringQueue} through {@link IStringQueueFileHandler}.
 * 
 * @version $Id: StringQueueNotification.java,v 1.5 2012/11/09 17:01:54 acaproni Exp $
 * @since ACS 10.2    
 */
public class StringQueueNotification extends TestCase {
	
	/**
	 * The file handler for the test extends {@link DefaultQueueFileHandlerImpl}
	 * to instrument {@link #getNewFile()} and {@link #fileProcessed(File, String, String)}
	 * in order to know when they are really executed.
	 * 
	 * @author acaproni
	 *
	 */
	public class TestFileHandler extends TimestampedStringQueueFileHandler {
		
		
		/**
		 * The timestamp of the oldest log in cache to compare 
		 * with that received in {@link #fileProcessed(File, String, String)}.
		 */
		private String oldestDate=null;
		
		/**
		 * The timestamp of the youngest log in cache to compare 
		 * with that received in {@link #fileProcessed(File, String, String)}.
		 */
		private String youngestDate=null;
		
		/**
		 * Track the number of notifications received.
		 */
		private int receivedNotifications=0;
		
		/**
		 * Constructor
		 * 
		 * @param maxSize The size of each file of the cache
		 */
		public TestFileHandler(long maxSize) {
			super(maxSize);
			youngestDate=oldestDate=null;
		}
		
		@Override
		public File getNewFile() throws IOException {
			File f=null;
			String acstmp = System.getProperty("ACS.tmp");
			try {
				// Try to create the file in $ACS_TMP
				
				if (!acstmp.endsWith(File.separator)) {
					acstmp=acstmp+File.separator;
				}
				File dir = new File(acstmp);
				f = File.createTempFile(prefix,".tmp",dir);
			} catch (IOException ioe) {
				System.out.println("Error creating a file for cache in "+acstmp);
				ioe.printStackTrace(System.err);
				return null;
			}
			f.deleteOnExit();
			System.out.println("New queue file created with name: "+f.getAbsolutePath());
			return f;
		}

		@Override
		public void fileProcessed(
				File filePointer, 
				String minTime,
				String maxTime) {
			assertNotNull("minTime should never be null", minTime);
			assertNotNull("maxTime should never be null", maxTime);
			assertTrue("Error deleting "+filePointer.getAbsolutePath(),filePointer.delete());
			receivedNotifications++;
			System.out.println("Notification "+receivedNotifications+" received for file "+filePointer.getAbsolutePath());
			if (checkNotification) {
				System.out.print("Checking for correctness of the notification... ");
				try {
					assertEquals("Youngest date differ", youngestDate,minTime);
					assertEquals("Oldest date differ", oldestDate,maxTime);
					System.out.println("OK");
				} catch (Throwable t) {
					System.out.println("Ops...");
					t.printStackTrace();
				} finally {
					if (notificationArrived!=null) {
						notificationArrived.countDown();
					}					
				}
			}
		}

		/**
		 * Set the dates to compare with those received
		 *  in {@link #fileProcessed(File, String, String)}.
		 * 
		 * @param young The date of the youngest log in cache
		 * @param old The date of the oldest log in cache
		 */
		public void setExpectedDates(String young, String old) {
			if (young==null || young.isEmpty()) {
				throw new IllegalArgumentException("Invalid timestamp for youngest log "+young);
			}
			if (old==null || old.isEmpty()) {
				throw new IllegalArgumentException("Invalid timestamp for oldest log "+old);
			}
			oldestDate=old;
			youngestDate=young;
		}
		
	}
	
	/**
	 * A class to hold the dates of the log in each cache file.
	 * 
	 * @author acaproni
	 */
	private class LogDates {
		/**
		 * The date of the youngest log in cache
		 */
		public long minDate;
		
		/**
		 * The date of the oldest log in cache
		 */
		public long maxDate;

		/**
		 * Constructor 
		 * @param minDate The date of the youngest log in cache
		 * @param maxDate The date of the oldest log in cache
		 */
		public LogDates(long minDate, long maxDate) {
			super();
			this.minDate = minDate;
			this.maxDate = maxDate;
		}
		
		/**
		 * Constructor 
		 */
		public LogDates() {
			super();
			minDate = -1;
			maxDate = -1;
		}
		
		
		/**
		 * Constructor 
		 * @param initialTimestamp The date of the youngest and oldest logs in cache
		 */
		public LogDates(long initialTimestamp) {
			super();
			minDate = initialTimestamp;
			maxDate = initialTimestamp;
		}
		
		public void updateTimestamps(long timestamp) {
			if (minDate==-1 || timestamp<minDate) {
				minDate=timestamp;
			}
			if (maxDate==-1 || timestamp>maxDate) {
				maxDate=timestamp;
			}
		}

		@Override
		public String toString() {
			StringBuilder ret = new StringBuilder();
			ret. append('[');
			if (minDate==-1) {
				ret.append(minDate);
			} else {
				ret.append(IsoDateFormat.formatDate(new Date(minDate)));
			}
			ret.append(", ");
			if (maxDate==-1) {
				ret.append(maxDate);
			} else {
				ret.append(IsoDateFormat.formatDate(new Date(maxDate)));
			}
			ret. append(']');
			return ret.toString();
		}
		
		
	}
	
	/**
	 * The cache used to stress the file handler
	 */
	private TimestampedStringQueue stringQueue;
	
	/**
	 * The size of each file of the cache.
	 * <P>
	 * Note that when this size is reached then a new file is created and a notification
	 * sent to the listener.
	 * <P>
	 * {@link #fileProcessed(File, String, String)} checks the dates received 
	 * in the notification with those expected.
	 */
	private final long cacheFileSize=1024*50; // 50K
	
	/**
	 * The file handler of the test
	 */
	private TestFileHandler testFileHandler;
	
	/**
	 * The header of each log: the initial part of a log before the timestamp
	 */
	private final String logHdrTemplate = "<Debug TimeStamp=\"";
	
	/**
	 * Notifications must be checked only when performing a test.
	 * <BR>
	 * In fact when the {@link TimestampedStringQueue} is closed it sends a notification before
	 * deleting a file but in that case the dates must not be tested.
	 */
	private volatile boolean checkNotification=false;
	
	/**
	 * Set to wait for a notification.
	 * <P>
	 * This is needed because the deletion of the file (and the related
	 * notification) is done by a dedicated thread: see {@link TimestampedStringQueue#run()}
	 */
	private volatile CountDownLatch notificationArrived=null;
	
	/**
	 * The leading part of each log: the part of a log after the timestamp
	 */
	private final String logFooterTemplate = "\" File=\"org.jacorb.orb.dsi.ServerRequest\" Line=\"330\" Routine=\"reply\" Host=\"gas01\" Process=\"CONTROL/ACC/javaContainer\" SourceObject=\"jacorb@CONTROL/ACC/javaContainer\" Thread=\"RequestProcessor-2593\" LogId=\"27932171\"><![CDATA[ServerRequest: reply to getAtmosphericConditions]]></Debug>";
	
	/**
	 * Constructor
	 */
	public StringQueueNotification() {
		super(StringQueueNotification.class.getName());
	}
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		checkNotification=true;
		testFileHandler = new TestFileHandler(cacheFileSize);
		assertNotNull(testFileHandler);
		assertEquals("Size max size of cache differs from the passed one", testFileHandler.maxFilesSize,cacheFileSize );
		stringQueue = new TimestampedStringQueue(testFileHandler,"TIMESTAMP=\"");
		assertNotNull(stringQueue);
		stringQueue.start();
		System.out.println("Queue started: now the test begins");
	}

	@Override
	protected void tearDown() throws Exception {
		checkNotification=false;
		stringQueue.close(true);
		System.out.println("Queue closed");
		super.tearDown();
	}
	
	/**
	 * Add timestamped strings until a new file is created then
	 * check if the timestamp reported by the notification
	 * are the right ones
	 * <P>
	 * In this test, strings are added ordered by timestamp.
	 *  
	 * @throws Exception
	 */
	public void testNotifiedDatesOrdered() throws Exception {
		System.out.println("testNotifiedDatesOrdered started");
		
		// The date of the first log in the queue
		// This is also the timestamp of the first log of the first created file
		long startDate=System.currentTimeMillis();
		// The timestamp of the last log added in the queue
		long lastTimestamp = startDate;
		
		LogDates logDates = new LogDates();
		LogDates notUsedlogDates = new LogDates();
		
		// Add logs in cache
		int logsInCache=0;
		while (stringQueue.getActiveFilesSize()<=1) {
			addLogToCache(lastTimestamp, logDates, notUsedlogDates);
			lastTimestamp+=250;
			logsInCache++;
			assertEquals(logsInCache, stringQueue.size());
		}
		assertEquals("The queue does NOT contain the expected number of strings!",logsInCache, stringQueue.size());
		System.out.println("Stopped pushing; "+stringQueue.getActiveFilesSize()+" files used by the cache to store "+stringQueue.size()+" entries");
		// The timestamp expected for thefirst notification are:
		// startDate: the initial log added in the cache
		// lastTimestamp-500: in fact 250 is added by the loop and must be subtracted; the other 250 beloengs to the last added log
		//                    but this log belongs to the new file
		testFileHandler.setExpectedDates(IsoDateFormat.formatDate(new Date(startDate)), IsoDateFormat.formatDate(new Date(lastTimestamp-500)));
		notificationArrived=new CountDownLatch(1);
		// Pop logs to trigger the notification
		for (int t=0; t<logsInCache; t++) {
			assertNotNull(stringQueue.pop());
		}
		assertEquals("The queue should be empty!",0, stringQueue.size());
		// wait for the notification
		System.out.println("testNotifiedDatesOrdered awaiting for notification");
		if (!notificationArrived.await(2, TimeUnit.MINUTES)) {
			// Timeout :-(
			throw new Exception("Notification (fileProcessed) never called!");
		}
		System.out.println("testNotifiedDatesOrdered done.");
	}
	
	/**
	 * Add logs to the cache until the file handler is notified and then checks the 
	 * youngest and oldest date of the logs in cache.
	 * <P>
	 * In this test, logs are added without any order.
	 *  
	 * @throws Exception
	 */
	public void testNotifiedDatesNotOrdered() throws Exception {
		System.out.println("testNotifiedDatesNotOrdered");
		long timestamp = System.currentTimeMillis();
		Random rnd = new Random(System.currentTimeMillis()); // For the new timestamp
		
		LogDates logDates = new LogDates();
		LogDates notUsedlogDates = new LogDates();
		// Add the first log before the loop because it triggers
    	// the creation of a new cache file
		addLogToCache(timestamp+rnd.nextInt(), new LogDates(), logDates);
		int logsInCache=1;
		
		while (stringQueue.getActiveFilesSize()<=1) {
			int timeStampInc=rnd.nextInt();
			timestamp=timestamp+timeStampInc;
			addLogToCache(timestamp, logDates,notUsedlogDates);
			logsInCache++;
			assertEquals(logsInCache, stringQueue.size());
			//System.out.println("\t=>"+timeStampInc+", actual="+formatDate(timestamp)+"youngest="+formatDate(youngestTime)+", oldest="+formatDate(oldestTime));
		}
		System.out.println("Stopped pushing; "+stringQueue.getActiveFilesSize()+" files used by the cache to store "+stringQueue.size()+" entries");
		testFileHandler.setExpectedDates(IsoDateFormat.formatDate(new Date(logDates.minDate)), IsoDateFormat.formatDate(new Date(logDates.maxDate)));
		notificationArrived=new CountDownLatch(1);
		// Pop logs to trigger the notification
		while (stringQueue.size()>0) {
			assertNotNull(stringQueue.pop());
		}
		// wait for the notification
		System.out.println("testNotifiedDatesNotOrdered awaiting for notification");
		if (!notificationArrived.await(2, TimeUnit.MINUTES)) {
			// Timeout :-(
			throw new Exception("Notification (fileProcessed) never called!");
		}
		System.out.println("testNotifiedDatesNotOrdered done");
	}
	
	/**
	 * Up to now we tested the notifications with only one file.
	 * Now we check the correctness of the dates with 3 files
	 * and timestamps generated randomly as in {@link #testNotifiedDatesNotOrdered()}.
	 */
	public void testNotificationsWithSeveralFiles() throws Exception {
		System.out.println("testNotificationsWithSeveralFiles");
		
		// The vector will contain one LogDates for each file of the cache
		Vector<LogDates> logDatesVector = new Vector<StringQueueNotification.LogDates>();

		// The number of logs in each file of the cache
 		int[] logsInCache = new int[3];
		
		long timestamp = System.currentTimeMillis();
		Random rnd = new Random(System.currentTimeMillis()); // For the new timestamp
		
		LogDates actualFileLogDates = new LogDates();
		LogDates newFileLogDates = new LogDates();
		
		while (stringQueue.getActiveFilesSize()<=3) {
			int timeStampInc=rnd.nextInt();
			timestamp=timestamp+timeStampInc;
			boolean newFileCreated=addLogToCache(timestamp, actualFileLogDates, newFileLogDates);
			if (newFileCreated) {
				logDatesVector.add(actualFileLogDates);
				actualFileLogDates=newFileLogDates;
				newFileLogDates = new LogDates();
			}
		}
		
		// Pop the logs to trigger a notification for each of the files in cache
		int idx=1; // Skip index 0 created when the first log has been inserted
		testFileHandler.setExpectedDates(
				IsoDateFormat.formatDate(new Date(logDatesVector.get(idx).minDate)), 
				IsoDateFormat.formatDate(new Date(logDatesVector.get(idx).maxDate)));
		while (stringQueue.size()>0) {
			notificationArrived=new CountDownLatch(1);
			int filesInCache=stringQueue.getActiveFilesSize();
			assertNotNull(stringQueue.pop());
			if (filesInCache!=stringQueue.getActiveFilesSize()) {
				// wait for the notification
				System.out.println("testNotificationsWithSeveralFiles awaiting for notification");
				if (!notificationArrived.await(2, TimeUnit.MINUTES)) {
					// Timeout :-(
					throw new Exception("Notification (fileProcessed) never called!");
				}
				idx++;
				if (idx>=logDatesVector.size()) {
					continue;
				}
				testFileHandler.setExpectedDates(
						IsoDateFormat.formatDate(new Date(logDatesVector.get(idx).minDate)), 
						IsoDateFormat.formatDate(new Date(logDatesVector.get(idx).maxDate)));
			}
		}
		System.out.println("testNotificationsWithSeveralFiles done");
	}
	
	/**
	 * Generates a log with the passed time.
	 * <P>
	 * The log returned is in the format <code>logHdrTemplate + timestamp + logFooterTemplate</code>
	 * 
	 * @param timestamp The time of the log in msec
	 * @return The log
	 */
	private String generateLog(long timestamp) {
		// 2012-08-04T19:29:45.573
		Calendar cal = Calendar.getInstance();
		cal.setTimeInMillis(timestamp);
		return String.format("%s%s%s",
				logHdrTemplate,
				IsoDateFormat.formatDate(new Date(timestamp)),
				logFooterTemplate);
	}
	
	/**
	 * Add a new log to the cache signaling if a new cache file has been created.
	 * <P>
	 * The new log to add is automatically generated by {@link #generateLog(long)} starting from 
	 * the passed timestamp.
	 * <P>
	 * The method returns a flag to signal if a new cache file has been created.<BR>
	 * <code>addLogToCache</code> updates the dates in the LogDates objects passed in the constructor depending 
	 * if a new file has been created or not. If no new file has been created the the dates are stored in
	 * <code>logDates</code>. If a new file is created then the dates are updated in <code>newLogDates</code>.
	 * 
	 * @param timestamp The timestamp of the log to add to the cache
	 * @param logDates The dates of the logs in the file of the cache updated only if no new file has been created
	 * @param newLogDates The dates of the logs in the file of the cache updated only if a new file has been created
	 * @return <code>true</code> if a new cache file has been created
	 * throws Exception if the cache returned an error while adding a new log
	 */
	private boolean addLogToCache(long timestamp, LogDates logDates, LogDates newLogDates) throws Exception {
		if (logDates==null || newLogDates==null) {
			throw new InvalidParameterException("LogDates params can't be null");
		}
		int numoOfLogFilesBefore=stringQueue.getActiveFilesSize();
		String newLog=generateLog(timestamp);
		stringQueue.push(newLog);
		int numoOfLogFilesAfter=stringQueue.getActiveFilesSize();
		boolean aNewFileHasBeenCreated=numoOfLogFilesBefore!=numoOfLogFilesAfter;
		if (!aNewFileHasBeenCreated) {
			logDates.updateTimestamps(timestamp);
		} else {
			newLogDates.updateTimestamps(timestamp);
		}
		return aNewFileHasBeenCreated;
	}
	
	/**
	 * Check if a new file is created when expected by checking the size of 
	 * strings in cache against the max length of the file handler.
	 * It also implicitly test it the queue works with a generic time-stamped string instead of a log.
	 * <P>
	 * For this test the dates are not important but only the total
	 * length of the strings pushed in the queue. 
	 * In fact, now we push generic time-stamped strings instead of logs.
	 * <P>
	 * testFilesCreation pushes a set of generic string each of each 
	 * has a unique integer ID. When the size of the pushed string
	 * is greater then the max length of the file, we expect the cache to create new file.
	 * The test will ten also get out all the strings and check for their 
	 * integrity, checking the value of their IDs.
	 * @throws Exception
	 */
	public void testFilesCreation() throws Exception {
		System.out.println("testFilesCreation started");
		String strHdr="A generic timestamped string to push in queue "+"TIMESTAMP=\"";
		String strFooter="\" End of generic String ";
		
		assertEquals("A newly created cache should have 0 files!",stringQueue.getActiveFilesSize(), 0);
		// The size of the strings pushed in the queue
		long size=0;
		// The unique ID of each string (the first string in queue has ID=1)
		int ID=0;
		while (size<=cacheFileSize) {
			ID++;
			String now = IsoDateFormat.formatDate(new Date(System.currentTimeMillis()));
			String strToPush=strHdr+now+strFooter+ID;
			stringQueue.push(strToPush);
			size+=strToPush.length();
			assertEquals("The size of the cache differs from the number of pushed strings", ID, stringQueue.size());
		}
		// Now there should be 2 files in the cache
		assertEquals("Wrong number of files in queue",stringQueue.getActiveFilesSize(), 2);
		// Get the strings out and checks their IDs
		for (int t=1; t<=ID; t++) {
			String str=stringQueue.pop();
			assertNotNull(str);
			assertEquals("The size of the cache differs from expected", ID-t, stringQueue.size());
			// Check the integrity of the string
			assertTrue(str.startsWith(strHdr));
			assertTrue("Str red from queue is ["+str+"] but we expect it to have iD="+t,str.endsWith(strFooter+t));
		}
		// The queue should be empty here
		assertEquals("The queue should be empty!",0, stringQueue.size());
		// And should have no files
		assertEquals("The queue should have 0 files!",1, stringQueue.getActiveFilesSize());
		System.out.println("testFilesCreation done");
	}
	
}
