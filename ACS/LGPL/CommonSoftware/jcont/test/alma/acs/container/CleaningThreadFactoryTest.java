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

import java.util.concurrent.CountDownLatch;
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
        Logger delegate = ClientLogManager.getAcsLogManager().getLoggerForApplication(getName(), false);
        logger = LogRecordCollectingLogger.getCollectingLogger("CleaningThreadFactoryTest-CollectingLogger-" + getName());
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
	 * <li>thread that will have died neatly (the expected behavior of user threads)
	 * <li>thread that will be sleeping
	 * <li>thread that will be busy
	 * <li>thread that will not yet have been started
	 * </ol>
	 * This test verifies that
	 */
	public void testStoppingThreads() throws Exception {
		String factoryName = "ContainerTestThreadGroup";
		CleaningDaemonThreadFactory threadFactory = new CleaningDaemonThreadFactory(factoryName, logger);

		final CountDownLatch synch1 = new CountDownLatch(1);
		Runnable terminatedRunnable = new Runnable() {
			public void run() {
				synch1.countDown();
				sleep(100);
				logger.info("Short-sleeping test runnable started and stopped running in thread "
						+ Thread.currentThread().getName());
			}
		};

		final CountDownLatch synch2 = new CountDownLatch(1);
		Runnable sleepingRunnable = new Runnable() {
			public void run() {
				synch2.countDown();
				logger.info("Sleeping test runnable starts running in thread " + Thread.currentThread().getName());
				// will be sleeping when cleanUp is called
				sleep(20000);
			}
		};

		final CountDownLatch synch3 = new CountDownLatch(1);
		Runnable busyRunnable = new Runnable() {
			public void run() {
				synch3.countDown();
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
		synch1.await(); // wait until thread is running

		Thread t2 = threadFactory.newThread(sleepingRunnable);
		assertEquals(tg, t2.getThreadGroup());
		t2.start();
		synch2.await(); // wait until thread is running

		Thread t3 = threadFactory.newThread(busyRunnable);
		// t3.setPriority(t3.getPriority()-1);
		t3.start();
		synch3.await(); // wait until thread is running

		// this thread just gets constructed but not started
		threadFactory.newThread(virginRunnable);

		sleep(1000);

		logger.clearLogRecords();
		threadFactory.cleanUp();
		// verify logged warnings about stopping threads t2 and t3
		LogRecord[] logs = logger.getCollectedLogRecords();
		if (logs.length != 3) {
			String messages = "";
			for (int i = 0; i < logs.length; i++) {
				messages += logs[i].getMessage() + "\n";
			}
			fail("Expected 3 logs about thread termination from the thread factory, but got " + logs.length + ": \n" + messages);
		}
		else {
			assertEquals(Level.WARNING, logs[0].getLevel());
			assertEquals("Forcibly terminating surviving thread " + factoryName + "-2", logs[0].getMessage());
			assertEquals("Thread " + factoryName + "-2 was interrupted while sleeping.", logs[1].getMessage());
			assertEquals("Forcibly terminating surviving thread " + factoryName + "-3", logs[2].getMessage());
		}
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

	public void testThreadStackDebugMessages() {
		LogRecordCollectingLogger collectingLogger = LogRecordCollectingLogger.getCollectingLogger("testThreadStackDebugMessages_CollectingLogger");
		System.setProperty(CleaningDaemonThreadFactory.LOG_THREAD_CREATION_CALLSTACK_PROPERTYNAME, "true");
		CleaningDaemonThreadFactory tf = new CleaningDaemonThreadFactory("factoryWithStackTrace", collectingLogger);

		Runnable dummyRunnable = new Runnable() {
			public void run() {
			}
		};
		tf.newThread(dummyRunnable);
		
		LogRecord[] records = collectingLogger.getCollectedLogRecords();
		assertEquals(1, records.length);
		assertTrue(records[0].getMessage().startsWith("Created thread 'factoryWithStackTrace-1'. Call stack: alma.acs.container.CleaningThreadFactoryTest.testThreadStackDebugMessages(CleaningThreadFactoryTest.java:202) <- "));
		
		System.setProperty(CleaningDaemonThreadFactory.LOG_THREAD_CREATION_CALLSTACK_PROPERTYNAME, "false");
	}
	
	
	private void sleep(int millis) {
		try {
			Thread.sleep(millis);
		} catch (InterruptedException e) {
			logger.info("Thread " + Thread.currentThread().getName() + " was interrupted while sleeping.");
		}
	}
}
