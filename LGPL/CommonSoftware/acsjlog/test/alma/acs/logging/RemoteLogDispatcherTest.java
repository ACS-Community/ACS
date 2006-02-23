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
import java.util.logging.Formatter;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.CORBA.UserException;

import junit.framework.TestCase;

import alma.acs.logging.formatters.AcsXMLLogFormatter;



public class RemoteLogDispatcherTest extends TestCase {

    private TestLogDispatcher dispatcher;
    private DispatchingLogQueue queue;
    private CollectingLogger collectingLogger;
    private TestAcsXMLLogFormatter formatter;
    
    
    
    /**
     * JUnit's class re-loading strategy fails for these tests, so the setUp must reset the state of the member variables
     * 
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        System.out.println("START----------------------------" + getName() + "-------------");
        ORB orb = org.omg.CORBA.ORB.init();
        collectingLogger = CollectingLogger.getTestLogger("RemoteLogDispatcherTest");
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

    public void testSendLogRecords() {
        int numRecords = 10;
        collectingLogger.produceLogs1(numRecords);
        LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords(); 
        dispatcher.sendLogRecords(fakeLogRecords);
        String[] xmlRecords = dispatcher.getCollectedXmlLogRecords();
        // nothing lost?
        assertEquals(numRecords, xmlRecords.length);
        // order kept?
        assertTrue(xmlRecords[1].startsWith("<Emergency TimeStamp="));
    }
 
    
    public void testFlushByRecordNumber() {
        // no initial buffering beyond normal
        queue.setRemoteLogDispatcher(dispatcher);
        
        int numRecords = 4;
        dispatcher.setBufferSize(numRecords);
        collectingLogger.produceLogs1(numRecords);
        LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords(); 
        
        for (int i = 0; i < numRecords-1; i++) {
            queue.log(fakeLogRecords[i]);    
        }
        // so far all should be buffered
        sleep(1000);
        assertEquals(0, dispatcher.getCollectedXmlLogRecords().length);
        
        // the next one should trigger a flush (in a separate thread, thus we wait a bit)
        queue.log(fakeLogRecords[numRecords-1]);
        sleep(1000);
        assertEquals(numRecords, dispatcher.getCollectedXmlLogRecords().length);
        
        // the queue should have sorted by by level/time, but the time-sorting of the dispatcher should have restored the order in this case 
        assertTrue(dispatcher.getCollectedXmlLogRecords()[1].startsWith("<Emergency TimeStamp="));
    }
    
    public void testPeriodicFlush() {
        queue.setRemoteLogDispatcher(dispatcher);
        queue.setPeriodicFlushing(2000);
        
        int numRecords = 2;
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
        
        // now flood it with more logs to see if this interferes with the scheduled timer (note the 200ms sleep introduced by TestLogDispatcher) 
        numRecords = numRecords * 10;
        System.out.println("next should come " + numRecords + " records:");
        dispatcher.clearCollectedXmlLogRecords();
        collectingLogger.clearLogRecords();
        collectingLogger.produceLogs1(numRecords);
        fakeLogRecords = collectingLogger.getCollectedLogRecords();
        for (int i = 0; i < numRecords; i++) {
            queue.log(fakeLogRecords[i]);    
        }
        sleep(10000);
        int pendingLogFlushes = queue.pendingFlushes();
        System.out.println("all should be logged now. Pending flushes: " + pendingLogFlushes);
        assertEquals(0, pendingLogFlushes);

        // stop periodic flushing, and check pendingFlushes
        queue.setPeriodicFlushing(0);
        sleep(5000);
        assertEquals(0, queue.pendingFlushes());
    }
    
    
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
        
        // a single flush() (assuming it is successful) should trigger more flushes to run after it returns,
        // and thus take away more log records (largest multiple of the buffer size).
        // here we wait for the original flush to finish
        boolean flushResult = ((Boolean)queue.flush().get()).booleanValue();
        assertTrue(flushResult);
// if we want to let the flushes resubmit new flushes and finish before flushAllAndWait is called, uncomment the following lines:         
//        sleep(10000); 
//        assertEquals(numRecords % bufferSize, queue.recordQueueSize());
//        assertEquals(0, queue.pendingFlushes());
        
        // now flush the leftover records
        queue.flushAllAndWait();
        dispatcher.clearCollectedXmlLogRecords();
        assertEquals(0, queue.recordQueueSize());
        // sometimes the flush executor still reports one residual flush request right after the last flush has finished, so we sleep a bit 
        sleep(100);
        assertEquals(0, queue.pendingFlushes());
        // make sure that flushAllAndWait really waited enough, and no stray records were delivered afterwards
        assertEquals(0, dispatcher.getCollectedXmlLogRecords().length);
    }
    
    
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
    
    public void testQueueOverflow() {
        // simulate a failing log service 
        dispatcher.setWriteFailureChance(1.0);
        queue.setRemoteLogDispatcher(dispatcher);
        
        int numRecords = DispatchingLogQueue.MAX_QUEUE_SIZE;
        collectingLogger.produceLogs1(numRecords);
        LogRecord[] fakeLogRecords = collectingLogger.getCollectedLogRecords();
        dispatcher.setVerbose(false);
        int filterThreshold = numRecords * 7 / 10;
        int numLoggedExpected = 0;
        for (int i = 0; i < numRecords; i++) {
            queue.log(fakeLogRecords[i]);
            // first 70% of queue accepts any of these log records, later only INFO and above
            if ( (i+1 <= filterThreshold) ||
                 fakeLogRecords[i].getLevel().intValue() >= Level.INFO.intValue() ) {
                numLoggedExpected++;
            }
            sleep(2);
        }
        // check number of log records queued
        assertEquals(numLoggedExpected, queue.recordQueueSize());
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
    
    
    private void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

}





class TestLogDispatcher extends RemoteLogDispatcher {

    private List xmlLogRecordList = new ArrayList();
    private boolean verbose = true;
    private double writeFailureChance = 0.0;
    private Random random = new Random(System.currentTimeMillis());
    
    TestLogDispatcher(ORB orb, Formatter xmlLogFormatter) {
        super(orb, null, xmlLogFormatter);
    }

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
    
    String[] getCollectedXmlLogRecords() {
        return (String[]) xmlLogRecordList.toArray(new String[xmlLogRecordList.size()]);
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