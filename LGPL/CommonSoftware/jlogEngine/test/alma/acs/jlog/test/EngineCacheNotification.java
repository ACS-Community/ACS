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
package alma.acs.jlog.test;

import java.io.File;
import java.io.IOException;
import java.security.InvalidParameterException;
import java.util.Calendar;
import java.util.Random;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import com.cosylab.logging.engine.cache.EngineCache;
import com.cosylab.logging.engine.cache.ILogQueueFileHandler;
import com.cosylab.logging.engine.cache.LogQueueFileHandlerImpl;

/** 
 * @author  acaproni
 * 
 * Tests the notification of {@link EngineCache} through {@link ILogQueueFileHandler}.
 * 
 * @version $Id: EngineCacheNotification.java,v 1.5 2012/11/09 17:01:54 acaproni Exp $
 * @since ACS 10.2    
 */
public class EngineCacheNotification extends TestCase {
	
	/**
	 * The {@link ILogQueueFileHandler} for the test.
	 * <P>
	 * <code>TestFileHandler</code> reuses {@link LogQueueFileHandlerImpl} but overrides
	 * {@link ILogQueueFileHandler#fileProcessed(File, String, String)} in order to test
	 * the notifications.
	 * 
	 * @author acaproni
	 *
	 */
	public class TestFileHandler implements ILogQueueFileHandler {
		
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
		 * The file handler used by jlog
		 */
		private final LogQueueFileHandlerImpl fileHandler;
		
		/**
		 * The timestamp of the oldest log in cache to compare 
		 * with that received in {@link #fileProcessed(File, String, String)}.
		 */
		private String oldestDate;
		
		/**
		 * The timestamp of the youngest log in cache to compare 
		 * with that received in {@link #fileProcessed(File, String, String)}.
		 */
		private String youngestDate;
		
		/**
		 * Track the number of notifications received.
		 */
		private int receivedNotifications=0;
		
		public TestFileHandler() {
			fileHandler=new LogQueueFileHandlerImpl(cacheFileSize);
			youngestDate=oldestDate=null;
		}

		@Override
		public File getNewFile() throws IOException {
			// Delegate to LogQueueFileHandlerImpl
			return fileHandler.getNewFile();
		}

		@Override
		public void fileProcessed(File filePointer, String minTime,
				String maxTime) {
			receivedNotifications++;
			fileHandler.fileProcessed(filePointer, minTime, maxTime);
			if (checkNotification) {
				System.out.print("Notification "+receivedNotifications+" received: checking for correctness... ");
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

		@Override
		public long getMaxFileSize() {
			// Delegate to LogQueueFileHandlerImpl
			return fileHandler.getMaxFileSize();
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
				ret.append(formatDate(minDate));
			}
			ret.append(", ");
			if (maxDate==-1) {
				ret.append(maxDate);
			} else {
				ret.append(formatDate(maxDate));
			}
			ret. append(']');
			return ret.toString();
		}
		
		
	}
	
	/**
	 * The cache used to stress the file handler
	 */
	private EngineCache engineCache;
	
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
	 * In fact when the {@link EngineCache} is closed it sends a notification before
	 * deleting a file but in that case the dates must not be tested.
	 */
	private volatile boolean checkNotification=false;
	
	/**
	 * Set to wait for a notification.
	 * <P>
	 * This is needed because the deletion of the file (and the related
	 * notification) is done by a dedicated thread: see {@link EngineCache#run()}
	 */
	private volatile CountDownLatch notificationArrived=null;
	
	/**
	 * The leading part of each log: the part of a log after the timestamp
	 */
	private final String logFooterTemplate = "\" File=\"org.jacorb.orb.dsi.ServerRequest\" Line=\"330\" Routine=\"reply\" Host=\"gas01\" Process=\"CONTROL/ACC/javaContainer\" SourceObject=\"jacorb@CONTROL/ACC/javaContainer\" Thread=\"RequestProcessor-2593\" LogId=\"27932171\"><![CDATA[ServerRequest: reply to getAtmosphericConditions]]></Debug>";
	
	/**
	 * Constructor
	 */
	public EngineCacheNotification() {
		super(EngineCacheNotification.class.getName());
	}
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		checkNotification=true;
		testFileHandler = new TestFileHandler();
		assertNotNull(testFileHandler);
		engineCache = new EngineCache(testFileHandler);
		assertNotNull(engineCache);
		engineCache.start();
	}

	@Override
	protected void tearDown() throws Exception {
		checkNotification=false;
		engineCache.close(true);
		super.tearDown();
	}
	
	/**
	 * Add logs to the cache until the file handler is notified and then checks the 
	 * youngest and oldest date of the logs in cache.
	 * <P>
	 * In this test, logs are added ordered by timestamp.
	 *  
	 * @throws Exception
	 */
	public void testNotifiedDatesOrdered() throws Exception {
		System.out.println("testNotifiedDatesOrdered");
		long startDate=System.currentTimeMillis();
		long lastTimestamp = startDate;
		
		LogDates logDates = new LogDates();
		LogDates notUsedlogDates = new LogDates();
		// Add the first log before the loop because it triggers
    	// the creation of a new cache file
		addLogToCache(lastTimestamp, new LogDates(), logDates);
		int logsInCache=1;
		
		
		while (engineCache.getActiveFilesSize()<=1) {
			addLogToCache(lastTimestamp, logDates,notUsedlogDates);
			lastTimestamp+=250;
			logsInCache++;
			assertEquals(logsInCache, engineCache.size());
		}
		// Pop logs to trigger the notification
		testFileHandler.setExpectedDates(formatDate(logDates.minDate), formatDate(logDates.maxDate));
		notificationArrived=new CountDownLatch(1);
		for (int t=0; t<logsInCache; t++) {
			assertNotNull(engineCache.pop());
		}
		// wait for the notification
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
		
		while (engineCache.getActiveFilesSize()<=1) {
			int timeStampInc=rnd.nextInt();
			timestamp=timestamp+timeStampInc;
			addLogToCache(timestamp, logDates,notUsedlogDates);
			logsInCache++;
			assertEquals(logsInCache, engineCache.size());
			//System.out.println("\t=>"+timeStampInc+", actual="+formatDate(timestamp)+"youngest="+formatDate(youngestTime)+", oldest="+formatDate(oldestTime));
		}
		testFileHandler.setExpectedDates(formatDate(logDates.minDate), formatDate(logDates.maxDate));
		notificationArrived=new CountDownLatch(1);
		// Pop logs to trigger the notification
		while (engineCache.size()>0) {
			assertNotNull(engineCache.pop());
		}
		// wait for the notification
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
		Vector<LogDates> logDatesVector = new Vector<EngineCacheNotification.LogDates>();

		// The number of logs in each file of the cache
 		int[] logsInCache = new int[3];
		
		long timestamp = System.currentTimeMillis();
		Random rnd = new Random(System.currentTimeMillis()); // For the new timestamp
		
		LogDates actualFileLogDates = new LogDates();
		LogDates newFileLogDates = new LogDates();
		
		while (engineCache.getActiveFilesSize()<=3) {
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
				formatDate(logDatesVector.get(idx).minDate), 
				formatDate(logDatesVector.get(idx).maxDate));
		while (engineCache.size()>0) {
			notificationArrived=new CountDownLatch(1);
			int filesInCache=engineCache.getActiveFilesSize();
			assertNotNull(engineCache.pop());
			if (filesInCache!=engineCache.getActiveFilesSize()) {
				// wait for the notification
				if (!notificationArrived.await(2, TimeUnit.MINUTES)) {
					// Timeout :-(
					throw new Exception("Notification (fileProcessed) never called!");
				}
				idx++;
				if (idx>=logDatesVector.size()) {
					continue;
				}
				testFileHandler.setExpectedDates(
						formatDate(logDatesVector.get(idx).minDate), 
						formatDate(logDatesVector.get(idx).maxDate));
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
				formatDate(timestamp),
				logFooterTemplate);
	}
	
	/**
	 * Format the date.
	 * 
	 * @param time The time of the log in msec
	 * @return A string representing the date
	 */
	public static String formatDate(long time) {
		Calendar cal = Calendar.getInstance();
		cal.setTimeInMillis(time);
		return String.format("%4d-%02d-%02dT%02d:%02d:%02d.%03d",
				cal.get(Calendar.YEAR),
				cal.get(Calendar.MONTH)+1,
				cal.get(Calendar.DAY_OF_MONTH),
				cal.get(Calendar.HOUR_OF_DAY),
				cal.get(Calendar.MINUTE),
				cal.get(Calendar.SECOND),
				cal.get(Calendar.MILLISECOND));
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
		int numoOfLogFilesBefore=engineCache.getActiveFilesSize();
		String newLog=generateLog(timestamp);
		engineCache.push(newLog);
		int numoOfLogFilesAfter=engineCache.getActiveFilesSize();
		boolean aNewFileHasBeenCreated=numoOfLogFilesBefore!=numoOfLogFilesAfter;
		if (!aNewFileHasBeenCreated) {
			logDates.updateTimestamps(timestamp);
		} else {
			newLogDates.updateTimestamps(timestamp);
		}
		return aNewFileHasBeenCreated;
	}
	
}
