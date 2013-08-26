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

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.engine.LogReceiver;
import alma.acs.logging.engine.LogReceiver.DelayedLogEntry;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.IsoDateFormat;

/**
 * Tests the {@link alma.acs.logging.engine.LogReceiver} obtained 
 * from {@link alma.acs.component.client.ComponentClientTestCase#getLogReceiver()}.
 * 
 * @author hsommer
 * created 29.04.2006 10:27:51
 */
public class ClientWithLogReceiverTest extends ComponentClientTestCase {

    private LogReceiver logReceiver;

    public ClientWithLogReceiverTest() throws Exception {
        super("ClientWithLogReceiverTest");
    }
    
    protected void setUp() throws Exception {
    	// to not suppress any remote logging (would usually be configured through the CDB)  
    	System.setProperty(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL, "1");
        super.setUp();
        m_logger.info("------------ setUp " + getName() + " --------------");
        logReceiver = getLogReceiver();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }
        
    /**
     * The log receiver is supposed to be initialized, 
     * since {@link LogReceiver#initialize(org.omg.CORBA.ORB, si.ijs.maci.Manager)} 
     */
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
    	LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
    	assertEquals("For this test, even low-level logs must be sent off remotely.", 
    			AcsLogLevelDefinition.TRACE, logConfig.getDefaultMinLogLevel());
        logReceiver.setDelayMillis(0);
        BlockingQueue<DelayedLogEntry> queue = logReceiver.getLogQueue();
        
        // the set of log levels to be randomly selected for the test logs 
        AcsLogLevelDefinition[] coreLevels = new AcsLogLevelDefinition[] {
        		AcsLogLevelDefinition.TRACE,
        		AcsLogLevelDefinition.DEBUG,
        		AcsLogLevelDefinition.INFO,
        		AcsLogLevelDefinition.WARNING, 
        		AcsLogLevelDefinition.EMERGENCY };
        
        int numberOfTestLogs = 10;
        Random random = new Random(System.currentTimeMillis());
        
        // loop for sending several test logs
        for (int i=0; i < numberOfTestLogs; i++) {
        	AcsLogLevelDefinition coreLevel = coreLevels[random.nextInt(coreLevels.length)];
        	// TODO: using getLowestMatchingJdkLevel here may be temporary, see COMP-3925. 
        	Level jdkLevel = AcsLogLevel.getLowestMatchingJdkLevel(coreLevel);
        	
        	// Log the test record
            String logMessage = "This is log number " + i;
            m_logger.log(jdkLevel, logMessage);
            
            // Wait for the test record to come back from the Log service.
            // In spite of zero queue sorting delay, we need a long timeout 
            // to compensate for travel delays to and from the Log service.
            int delayLogServiceSec = 20; // should be much less, but currently we have problems there.
            int timeoutSec = logConfig.getFlushPeriodSeconds() + delayLogServiceSec;
            long timeoutSysMillis = System.currentTimeMillis() + timeoutSec*1000;
            while (true) { // System.currentTimeMillis() < timeoutSysMillis
            	// wait on the queue for a log record to arrive
	            DelayedLogEntry delayedLogEntry = queue.poll(timeoutSysMillis - System.currentTimeMillis(), TimeUnit.MILLISECONDS);            
	            if (delayedLogEntry != null) {
	            	// got something, must check if it was the record we sent
	            	if (delayedLogEntry.isQueuePoison()) {
	            		fail("Unexpected end of log queue.");
	            	}
	                LogReceiver.ReceivedLogRecord logRecord = delayedLogEntry.getLogRecord();
	                String sourceObjectName = logRecord.getSourceObject();
	                if (sourceObjectName!=null && 
	                		sourceObjectName.equals("ClientWithLogReceiverTest#testLogQueueNoDelay") &&
	                		!logRecord.getMessage().startsWith("------------ setUp") &&
	                		!logRecord.getMessage().startsWith("Alarm system type:")) {
	                	// it's a log record sent from this process 
		                assertEquals("Log message text must match the test log record", 
		                		logMessage, logRecord.getMessage());
		                assertEquals("Log level must match the test log record", 
		                		coreLevel, logRecord.getLevel().getAcsCoreLevel());
		                System.out.println("Received back log record #" + i);
		                break; // and continue outer loop with next log record
	                }
	                else {
	                	// was some other stray log, perhaps from a previously running ACS component, or the pinging jmanager
	                	// we stay in the while loop and wait again for another record to be delivered by the queue
	                	System.out.println(IsoDateFormat.formatCurrentDate() + ": Ignoring received log " + IsoDateFormat.formatDate(logRecord.getTimestamp()) + 
	                			" [" + sourceObjectName + "]");
	                }
	            }
	            else {
	                fail("Did not receive the expected log record #" + i + " within " + timeoutSec + " seconds.");
	            }
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
