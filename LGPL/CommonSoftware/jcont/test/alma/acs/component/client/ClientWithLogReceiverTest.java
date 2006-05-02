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
package alma.acs.component.client;

import java.io.PrintWriter;
import java.util.Random;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.engine.LogReceiver;
import alma.acs.logging.engine.LogReceiver.DelayedLogEntry;

/**
 * @author hsommer
 * created 29.04.2006 10:27:51
 */
public class ClientWithLogReceiverTest extends ComponentClientTestCase {

    private LogReceiver logReceiver;

    public ClientWithLogReceiverTest() throws Exception {
        super("ClientWithLogReceiverTest");
    }
    
    protected void setUp() throws Exception {
        super.setUp();
        logReceiver = getLogReceiver();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }
    
    public void testInitialized() {
        assertTrue(logReceiver.isInitialized());
    }
    
    /**
     * Logs a single record and waits for it to come back from the Log service 
     * and to pass the queue (which is set to zero delay).
     * Then the record is verified, and the same test is repeated a couple of times with different log records. 
     * @throws Exception
     */
    public void testLogQueueNoDelay() throws Exception {
        logReceiver.setDelayMillis(0);
        BlockingQueue<DelayedLogEntry> queue = logReceiver.getLogQueue();
        
        final int numLogs = 5;
        Level[] levels = new Level[] {Level.FINEST, Level.FINE, Level.INFO, Level.WARNING, Level.SEVERE};
        Random random = new Random(System.currentTimeMillis());
        for (int i=0; i<numLogs; i++) {
            Level level = levels[random.nextInt(levels.length)];
            String acsLevelName = AcsLogLevel.getNativeLevel(level).getEntryName();
            // it's pretty odd that jlog uses its own set of log type integers 
            int jlogLevelIndex = LogTypeHelper.parseLogTypeDescription(acsLevelName).intValue();
            
            String logMessage = "This is log number " + i;
            m_logger.log(level, logMessage);
            
            // in spite of zero queue sorting delay, we need a long timeout 
            // to compensate travel delay when sending log records to Log service,
            // and then getting them back over the network.
            long timeoutSec = 15L;
            DelayedLogEntry delayedLogEntry = queue.poll(timeoutSec, TimeUnit.SECONDS);
            if (delayedLogEntry != null) {
                ILogEntry logEntry = delayedLogEntry.getLogEntry();
                assertEquals(logMessage, logEntry.getField(ILogEntry.FIELD_LOGMESSAGE));
                assertEquals(jlogLevelIndex, ((Integer)logEntry.getField(ILogEntry.FIELD_ENTRYTYPE)).intValue());
                System.out.println("Received back log record #" + i);
            }
            else {
                fail("Did not receive the expected log record #" + i + " within " + timeoutSec + " seconds.");
            }
        }
        logReceiver.stop();
    }
    
    public void testLogCapture() throws Exception {
    	PrintWriter logWriter = new PrintWriter(System.out, true); //new BufferedWriter(new FileWriter(logFile)));
    	logReceiver.startCaptureLogs(logWriter, getContainerServices().getThreadFactory());

        final int numLogs = 20;
        for (int i=0; i<numLogs; i++) {
            String logMessage = "This is log number " + i;
            m_logger.log(Level.INFO, logMessage);
            Thread.sleep(50); // to get hopefully distinct timestamps in the log records
        }
        Thread.sleep(11000); // to make sure the log records were sent off to the Log service
    	logReceiver.stopCaptureLogs();
    	Thread.sleep(logReceiver.getDelayMillis()); // to make sure the queue has drained
    }
}
