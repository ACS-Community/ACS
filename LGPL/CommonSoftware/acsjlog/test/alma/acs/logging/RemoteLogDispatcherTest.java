/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.logging;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import junit.framework.TestCase;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.UserException;

import alma.Logging.XmlLogRecord;
import alma.acs.logging.formatters.AcsLogFormatter;
import alma.acs.logging.formatters.AcsXMLLogFormatter;
import alma.acs.testsupport.LogRecordCollectingLogger;



public class RemoteLogDispatcherTest extends TestCase {

	private ORB orb;
	private TestLogDispatcher dispatcher;
	private DispatchingLogQueue queue;
	private CollectingLogger collectingLogger;
	private TestAcsXMLLogFormatter formatter;

	/**
	 * JUnit's class re-loading strategy fails for these tests, so the setUp must reset the state of the member
	 * variables
	 * 
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		System.out.println("START----------------------------" + getName() + "-------------");
		orb = org.omg.CORBA.ORB.init();
		collectingLogger = LogRecordCollectingLogger.getCollectingLogger("RemoteLogDispatcherTest", CollectingLogger.class);
		collectingLogger.suppressLogs(true); // only interested in the LogRecords
		collectingLogger.clearLogRecords();
		formatter = new TestAcsXMLLogFormatter();
		dispatcher = new TestLogDispatcher(orb, formatter);
		queue = new DispatchingLogQueue();
	}

	protected void tearDown() throws Exception {
		queue.shutDown();
		System.out.println("END------------------------------" + getName() + "-------------\n\n");
	}

	
	/**
	 * Sends a sequence of LogRecords to the {@link TestLogDispatcher}
	 * and compares the number and ordering of the gathered XML records
	 * with those sent. 
	 * This effectively tests {@link RemoteLogDispatcher#sendLogRecords(LogRecord[])}.
	 */
	public void testSendLogRecords() {
		// produce some LogRecord objects to test the dispatcher with
		int numRecords = 10;
		collectingLogger.produceLogs1(numRecords);
		LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords();
		// 
		dispatcher.sendLogRecords(fakeLogRecords);
		String[] xmlRecords = dispatcher.getCollectedXmlLogRecords();
		// nothing lost?
		assertEquals(numRecords, xmlRecords.length);
		// order kept?
		assertTrue(xmlRecords[1].startsWith("<Emergency TimeStamp="));
		
		// test the same thing again, but this time sending log records via the new ACS extension to the Log service
		try {
			collectingLogger.clearLogRecords();
			collectingLogger.produceLogs1(numRecords);
			fakeLogRecords = collectingLogger.getCollectedLogRecords();
			assertEquals(numRecords, fakeLogRecords.length);
			
			System.setProperty(RemoteLogDispatcher.USE_ACS_LOGSERVICE_EXTENSIONS_PROPERTYNAME, "true");
			TestLogDispatcher dispatcherAcsLogService = new TestLogDispatcher(orb, formatter);
			assertTrue(dispatcherAcsLogService.useAcsLogServiceExtensions);
			
			dispatcherAcsLogService.sendLogRecords(fakeLogRecords);
			xmlRecords = dispatcherAcsLogService.getCollectedXmlLogRecords();
			// nothing lost?
			assertEquals(numRecords, xmlRecords.length);
			// order kept?
			assertTrue(xmlRecords[1].startsWith("<Emergency TimeStamp="));
		}
		finally {
			System.setProperty(RemoteLogDispatcher.USE_ACS_LOGSERVICE_EXTENSIONS_PROPERTYNAME, "false");
		}
	}


	/**
	 * Sends as many LogRecords via {@link DispatchingLogQueue#log(LogRecord)} 
	 * that the buffer size ("dispatchPacketSize") is not quite reached, and 
	 * verifies that all these logs are still queued.
	 * <p>
	 * Then sends another LogRecord and verifies that (now that the buffer was full) 
	 * all LogRecords are forwarded from the queue to the dispatcher.
	 * <p>
	 * Finally this test verifies that the dispatcher applies the time sorting correctly,
	 * thus restoring the original order of LogRecords. (The queue should sort by log level,
	 * but since in this test all records fit into one buffer, the original order should be restored.)
	 */
	public void testFlushByRecordNumber() {
		// no initial buffering beyond normal
		queue.setRemoteLogDispatcher(dispatcher);

		int numRecords = 5;
		dispatcher.setBufferSize(numRecords);
		collectingLogger.produceLogs1(numRecords);
		LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords();

		for (int i = 0; i < numRecords - 1; i++) {
			queue.log(fakeLogRecords[i]);
		}
		// so far all should be buffered
		sleep(500);
		assertEquals(0, dispatcher.getCollectedXmlLogRecords().length);

		// the next one should trigger a flush (in a separate thread, thus we wait a bit)
		queue.log(fakeLogRecords[numRecords - 1]);
		sleep(500);
		assertEquals(numRecords, dispatcher.getCollectedXmlLogRecords().length);

		// the queue should have sorted by by level/time, but the time-sorting of the dispatcher should have restored
		// the order in this case
		assertTrue(dispatcher.getCollectedXmlLogRecords()[1].startsWith("<Emergency TimeStamp="));
	}

	
	/**
	 * Sets up DispatchingLogQueue for periodic flushing every 2 seconds (cp. CDB attribute "flushPeriodSeconds")
	 * and sends some LogRecords to the queue. (Flushing by record number is disabled by using a sufficiently large buffer, 
	 * see also {@link #testFlushByRecordNumber()}).
	 * Then verifies that after the flushing period the log records have been forwarded to the dispatcher.
	 * <p>
	 * Then sends many more log records than fit into one buffer, thus testing the triggering of subsequent flushes
	 * until the queue is drained, and that this mechanism still works while we have scheduled flushing in place.
	 * <p>
	 * Finally stops the scheduled flushing and verifies that it's gone.
	 */
	public void testPeriodicFlush() {
		queue.setRemoteLogDispatcher(dispatcher);
		assertFalse(queue.flushesPeriodically());
		queue.setPeriodicFlushing(2000);
		assertTrue(queue.flushesPeriodically());

		int numRecords = 8;
		System.out.println("next should come " + numRecords + " records:");
		dispatcher.setBufferSize(numRecords + 1);
		collectingLogger.produceLogs1(numRecords);
		LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords();
		for (int i = 0; i < numRecords; i++) {
			queue.log(fakeLogRecords[i]);
		}
		// not enough records to flush directly, but should happen after 2 seconds anyway
		sleep(2500);
		assertEquals(numRecords, dispatcher.getCollectedXmlLogRecords().length);

		// now flood it with more logs to see if this interferes with the scheduled timer 
		// (note the 200ms sleep introduced by TestLogDispatcher#writeRecords)
		numRecords = numRecords * 10;
		System.out.println("next should come " + numRecords + " records:");
		dispatcher.clearCollectedXmlLogRecords();
		collectingLogger.clearLogRecords();
		collectingLogger.produceLogs1(numRecords);
		fakeLogRecords = collectingLogger.getCollectedLogRecords();
		for (int i = 0; i < numRecords; i++) {
			queue.log(fakeLogRecords[i]);
		}
		sleep(3000);
		assertEquals(0, queue.realQueueSize());
		int pendingLogFlushes = queue.pendingFlushes();
		System.out.println("all should be logged now. Pending flushes: " + pendingLogFlushes);
		assertEquals(0, pendingLogFlushes);

		// stop periodic flushing, and check pendingFlushes
		queue.setPeriodicFlushing(0);
		assertFalse(queue.flushesPeriodically());
		// wait for the underlying concurrent.BlockingQueue to remove the scheduled task,
		// now that flushesPeriodically() gives false and thus no longer deducts the scheduled task from the flush queue.
		sleep(3000);
		assertEquals(0, queue.pendingFlushes());
	}

	
	/**
	 * Submits LogRecords to {@link DispatchingLogQueue} without having hooked up 
	 * the {@link RemoteLogDispatcher} yet (as it happens during a container start),
	 * and checks that all records are queued.
	 * <p>
	 * Then adds the dispatcher, flushes the queue, and verifies that {@link DispatchingLogQueue#flush()}
	 * flushes some records, while a subsequent {@link DispatchingLogQueue#flushAllAndWait()} flushes all records
	 * and leaves the queue in a clean state without further flushing requests.
	 */
	public void testUnavailableRemoteLogService() throws Exception {
		// do not yet call queue.setRemoteLogDispatcher to simulate e.g. a container start when the log service is not yet accessible
		int numRecords = 100;
		collectingLogger.produceLogs1(numRecords);
		LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords();
		for (int i = 0; i < numRecords; i++) {
			queue.log(fakeLogRecords[i]);
		}
		// all log records should be queued
		assertEquals(numRecords, queue.recordQueueSize());
		// no flushing should be pending
		assertEquals(0, queue.pendingFlushes());
		sleep(1000);

		// now the remote logging becomes available
		dispatcher.setVerbose(false);
		int bufferSize = 12; // must not divide numRecords
		dispatcher.setBufferSize(bufferSize);
		queue.setRemoteLogDispatcher(dispatcher);

		// a single flush() (assuming it is successful) should trigger more flushes to run **after it returns**,
		// and thus take away more log records (largest multiple of the buffer size).
		// here we wait **only for the original flush** to finish
		boolean flushResult = queue.flush().get().booleanValue();
		assertTrue(flushResult);
		// if we want to wait for and validate the re-submitted flushes, before flushAllAndWait is called, uncomment the following lines:
		// sleep(10000);
		// assertEquals(numRecords % bufferSize, queue.recordQueueSize());
		// assertEquals(0, queue.pendingFlushes());

		// now flush the leftover records and wait (including the flushes triggered by the above flush() call)
		queue.flushAllAndWait();
		// make sure that flushAllAndWait really waited enough
		assertEquals(numRecords, dispatcher.getCollectedXmlLogRecords().length);
		dispatcher.clearCollectedXmlLogRecords();
		assertEquals(0, queue.recordQueueSize());
		// sometimes the flush executor still reports one residual flush request right after the last flush has finished, so we sleep a bit
		sleep(100);
		assertEquals(0, queue.pendingFlushes());
		// just in case check that no stray records were delivered after the above flushAllAndWait - clearCollectedXmlLogRecords
		assertEquals(0, dispatcher.getCollectedXmlLogRecords().length);
	}


	/**
	 * Tests that a failure in XML formatting leads to dropping of the records.
	 * <p>
	 * Then tests that even with a 70% chance of failure in dispatching the log records will eventually send all records,
	 * due to the records being taken out and re-inserted in the queue. 
	 * Chances to add an additional record to the queue are fairly low because the simulated failure in
	 * writeRecords(..) occurs immediately (before the 200ms sleep we otherwise get)
	 */
	public void testFailingRecords() {
		// simulate a failure in converting the LogRecord to XML
		formatter.setFormatFailureChance(1.0);
		queue.setRemoteLogDispatcher(dispatcher);

		int numRecords = 4;
		dispatcher.setBufferSize(2);
		collectingLogger.produceLogs1(numRecords);
		LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords();

		for (int i = 0; i < numRecords; i++) {
			queue.log(fakeLogRecords[i]);
		}
		queue.flushAllAndWait();
		// all records should have been dropped
		assertEquals(0, dispatcher.getCollectedXmlLogRecords().length);

		// now simulate a failure in sending the XML log records
		// These records should be resubmitted to the queue and finally get sent as long as the failure chance is below 1.0
		formatter.setFormatFailureChance(0.0);
		dispatcher.setWriteFailureChance(0.7);
		for (int i = 0; i < numRecords; i++) {
			queue.log(fakeLogRecords[i]);
		}
		queue.flushAllAndWait();
		assertEquals(numRecords, dispatcher.getCollectedXmlLogRecords().length);
	}

	
    /**
     * TODO: investigate the occasional test failure 
     * overflowing log queue accepted/rejected wrong number of records ('filterThreshold'=700) expected: 850 but was:851
     * 
     * Most likely this comes from log records being temporarily removed and then reinserted to the queue.
     */
    public void testQueueOverflow() {
        // simulate a failing log service 
        dispatcher.setWriteFailureChance(1.0);
        queue.setRemoteLogDispatcher(dispatcher);
        
        int numRecords = queue.getMaxQueueSize();
        assertEquals("Default maxQueueSize 1000 from CDB schema as well as Java hardcoded", 1000, numRecords);
        
        collectingLogger.produceLogs1(numRecords);
        LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords();
        dispatcher.setVerbose(false);
        int filterThreshold = numRecords * 7 / 10;
        int numLoggedExpected = 0;
        for (int i = 0; i < numRecords; i++) {
            /*boolean wasAddedToQueue = */queue.log(fakeLogRecords[i]);
            // first 70% of queue accepts any of these log records, later only INFO and above
            if ( (i+1 <= filterThreshold) ||
                 fakeLogRecords[i].getLevel().intValue() >= Level.INFO.intValue() ) {
                numLoggedExpected++;
            }
            sleep(2);
        }
        // check number of log records queued
        assertEquals("Simulated unavailability of remote Log service: overflowing log queue accepted/rejected wrong number of records ('filterThreshold'=" 
        				+ filterThreshold + ")", 
        				numLoggedExpected, 
        				queue.recordQueueSize());
        
        // now fill up the queue completely
        for (int i = numLoggedExpected; i < numRecords; i++) {
            queue.log(fakeLogRecords[0]); // level INFO, will be stored
            sleep(2);
        }
        assertEquals(numRecords, queue.recordQueueSize());
        // overflow
        queue.log(fakeLogRecords[1]);
        // log service is fine again; the automatically triggered periodic flushing should drain the queue quickly after 10 sec
        dispatcher.setWriteFailureChance(0.0);
        sleep(20000);
        assertEquals(numRecords % dispatcher.getBufferSize(), queue.recordQueueSize());        
    }
    
    /**
     * This test was created to get some number about the performance of the log queue under pressure.
     * This was used to check whether the new implementation of the log submission was
     * better or worse than the previous one (see CVS logs for details)
     */
    public void notestStressQueue() {

    	long begin, end, average = 0;
    	final int maxTimes = 50;

        for(int times=0; times != maxTimes; times++) {

    		dispatcher = new TestLogDispatcher(orb, formatter);
        	dispatcher.setVerbose(false);
            queue.setRemoteLogDispatcher(dispatcher);

            collectingLogger.clearLogRecords();
        	collectingLogger.produceLogs1(3000*dispatcher.getBufferSize());
        	LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords();
        	sleep(1000);

        	begin = System.currentTimeMillis();
        	int i = 0;
        	for(LogRecord e: fakeLogRecords) {
        		queue.log(e);
        		i++;
        		if( i%dispatcher.getBufferSize() == 0 )
        			queue.flushAllAndWait();
        	}
        	end = System.currentTimeMillis();

        	//System.out.println("Logging took " + (end-begin) + " - 300*10 = " + ((end-begin) - 3000));
        	System.out.println("Logging took " + (end-begin));

        	average += (end-begin);
        }

        System.out.println("Average time was: " + (double)average/maxTimes);
    }

    private void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}



class TestLogDispatcher extends RemoteLogDispatcher {

	private final List<String> xmlLogRecordList = new ArrayList<String>();
	private boolean verbose = true;
	private double writeFailureChance = 0.0;
	private final Random random = new Random(System.currentTimeMillis());

	TestLogDispatcher(ORB orb, AcsLogFormatter xmlLogFormatter) {
		super(orb, null, xmlLogFormatter);
	}

	@Override
	protected void writeRecords(Any[] anyLogRecordsArray) throws UserException {
        if (random.nextDouble() < writeFailureChance) {
            if (verbose) {
                System.out.println("--------writeRecords fails with simulated network problem------");
            }
            throw new NullPointerException("deliberate failure in log record sending.");
        }
        for (int i = 0; i < anyLogRecordsArray.length; i++) {
            String xmlLogRecord = anyLogRecordsArray[i].extract_string();
            xmlLogRecordList.add(xmlLogRecord);
            if (verbose) {
                System.out.println(xmlLogRecord.toString());
            }
        }
        if (verbose) {
            System.out.println("--------end writeRecords-------");
        }
        try {
            Thread.sleep(200);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

	@Override
	protected void writeRecords(XmlLogRecord[] remoteLogRecords) {
		for (XmlLogRecord remoteLogRecord : remoteLogRecords) {
			String xmlLogRecord = remoteLogRecord.xml;
			xmlLogRecordList.add(xmlLogRecord);
			if (verbose) {
				System.out.println(xmlLogRecord.toString());
			}
		}
		if (verbose) {
			System.out.println("--------end writeRecords-------");
		}
		try {
			Thread.sleep(200);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	String[] getCollectedXmlLogRecords() {
		return xmlLogRecordList.toArray(new String[xmlLogRecordList.size()]);
	}

    void clearCollectedXmlLogRecords() {
        xmlLogRecordList.clear();
    }

    void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }
    
    /**
     * Sets the chance that a call to writeRecords will fail with an exception.
     * @param writeFailureChance  probability between 0.0 and 1.0 inclusive
     */
    void setWriteFailureChance(double writeFailureChance) {
        if (0 <= writeFailureChance && 1 >= writeFailureChance) {
            this.writeFailureChance = writeFailureChance;
        }
    }  

}


class TestAcsXMLLogFormatter extends AcsXMLLogFormatter {
    private double formatFailureChance = 0.0;
    private Random random = new Random(System.currentTimeMillis());

    public String format(LogRecord logRecord) {
        if (random.nextDouble() < formatFailureChance) {
            throw new NullPointerException("deliberate failure in log record formatting.");
        }
        return super.format(logRecord);
    }

    /**
     * Sets the chance that a call to format will fail with an exception  
     * @param formatFailureChance  probability between 0.0 and 1.0 inclusive
     */
    void setFormatFailureChance(double formatFailureChance) {
        if (0 <= formatFailureChance && 1 >= formatFailureChance) {
            this.formatFailureChance = formatFailureChance;
        }
    }  
}
