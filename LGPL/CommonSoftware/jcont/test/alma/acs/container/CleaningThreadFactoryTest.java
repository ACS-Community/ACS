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
package alma.acs.container;

import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.acs.logging.ClientLogManager;
import alma.acs.testsupport.LogRecordCollectingLogger;



/**
 * Tests the thread factory used by {@link ContainerServices#getThreadFactory()}, which can kill surviving user threads.
 * @author hsommer
 * created Apr 29, 2005 3:19:11 PM
 */
public class CleaningThreadFactoryTest extends TestCase {

    private LogRecordCollectingLogger logger;
    
    protected void setUp() throws Exception {
        Logger delegate = ClientLogManager.getAcsLogManager().getLoggerForApplication("ContainerTest", true);
        logger = LogRecordCollectingLogger.getCollectingLogger("CleaningThreadFactoryTest-CollectingLogger");
        logger.setDelegateLogger(delegate);
        logger.info("------------ setUp " + getName() + " --------------");
    }

    protected void tearDown() throws Exception {
        logger.info("------------ tearDown " + getName() + " --------------");
        logger.clearLogRecords();
        ClientLogManager.getAcsLogManager().shutdown(true);
    }

    
    
    public void testNoOp() {
        String factoryName = "ContainerTestThreadGroup-testNoOp";
        CleaningDaemonThreadFactory threadFactory = new CleaningDaemonThreadFactory(factoryName, logger);
        threadFactory.cleanUp();
    }

    /**
     * Creates 4 threads such that they have different behavior and status when cleanUp is called on the thread factory: 
     * <ol>
     * <li> thread that will have died neatly (the expected behavior of user threads)
     * <li> thread that will be sleeping 
     * <li> thread that will be busy
     * <li> thread that will not yet have been started
     * </ol>
     * This test verifies that 
     */
    public void testStoppingThreads() {
        String factoryName = "ContainerTestThreadGroup";
        CleaningDaemonThreadFactory threadFactory = new CleaningDaemonThreadFactory(factoryName, logger);
        
        Runnable terminatedRunnable = new Runnable() {
            public void run() {
                sleep(100);
                logger.info("Short-sleeping test runnable started and stopped running in thread " + Thread.currentThread().getName());
            }            
        };
        Runnable sleepingRunnable = new Runnable() {
            public void run() {
                logger.info("Sleeping test runnable starts running in thread " + Thread.currentThread().getName());
                // will be sleeping when cleanUp is called
                sleep(20000);
            }            
        };
        Runnable busyRunnable = new Runnable() {
            public void run() {
                logger.info("Busy runnable starts running in thread " + Thread.currentThread().getName());
                int i = 0;
                while (true) {
                    i++;
                }
            }            
        };
        Runnable virginRunnable = new Runnable() {
            public void run() {
                // should never be called.
                logger.warning("virgin runnable should not have been executed!");
            }            
        };
        
        Thread t1 = threadFactory.newThread(terminatedRunnable);
        ThreadGroup tg = t1.getThreadGroup();
        assertEquals(factoryName, tg.getName());
        assertFalse(tg.isDaemon());
        assertFalse(tg.isDestroyed());
        assertTrue(t1.isDaemon());
        assertEquals(factoryName + "-1", t1.getName());
        t1.start();
        
        Thread t2 = threadFactory.newThread(sleepingRunnable);
        assertEquals(tg, t2.getThreadGroup());
        t2.start();

        Thread t3 = threadFactory.newThread(busyRunnable);
//        t3.setPriority(t3.getPriority()-1);
        t3.start();

        Thread t4 = threadFactory.newThread(virginRunnable);
        
        sleep(1000);
        
        logger.clearLogRecords();
        threadFactory.cleanUp();
        // verify logged warnings about stopping threads t2 and t3
        LogRecord[] logs = logger.getCollectedLogRecords();
        assertEquals(2, logs.length);
        assertEquals(Level.WARNING, logs[0].getLevel());
        assertEquals("forcefully terminating surviving thread " + factoryName + "-2", logs[0].getMessage());
        assertEquals("forcefully terminating surviving thread " + factoryName + "-3", logs[1].getMessage());
    }
    
    /**
     * Verifies that a warning is logged when a user thread throws an otherwise uncaught exception.
     */
    public void testUserThreadException() {
        String factoryName = "StupidUserApplication";
        CleaningDaemonThreadFactory threadFactory = new CleaningDaemonThreadFactory(factoryName, logger);

        final String exMsg = "intended NPE from test application thread.";
        Runnable errorCmd = new Runnable() {
            public void run() {
                sleep(500);
                logger.info("Stupid user thread will now throw a NPE...");
                throw new NullPointerException(exMsg);
            }            
        };
        Thread t = threadFactory.newThread(errorCmd);
        logger.clearLogRecords();
        t.start();
        sleep(2000);
        
        // verify that a warning was logged for the user thread exception
        LogRecord[] logs = logger.getCollectedLogRecords();
        assertEquals(2, logs.length);
        LogRecord userExLogRecord = logs[1];
        assertEquals(Level.WARNING, userExLogRecord.getLevel());
        assertEquals("User thread '" + factoryName + "-1' terminated with error ", userExLogRecord.getMessage());
        assertNotNull(userExLogRecord.getThrown());
        assertEquals(exMsg, userExLogRecord.getThrown().getMessage());
    }
    
    
    private void sleep(int millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            logger.info("Thread " + Thread.currentThread().getName() + " was interrupted while sleeping.");
        }
    }
}
