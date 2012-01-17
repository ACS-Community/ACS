package alma.acs.concurrent;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.acs.concurrent.ThreadLoopRunner.ScheduleDelayMode;
import alma.acs.testsupport.LogRecordCollectingLogger;
import alma.acs.testsupport.TestLogger;
import alma.acs.util.StopWatch;

public class ThreadLoopRunnerTest extends TestCase
{
	private ThreadFactory tf;
	private Logger logger;
	
	protected void setUp() throws Exception {
		super.setUp();
		tf = new DaemonThreadFactory("ThreadLoopRunner");
		logger  = TestLogger.getLogger(getClass().getName());
		logger.info("\n------------ " + getName() + " --------------");
	}

	
	
	public void testSimple() throws Exception {
		
		final LogRecordCollectingLogger logger2 = LogRecordCollectingLogger.getCollectingLogger("record-collecting logger");
		logger2.setDelegateLogger(logger);
		
		Runnable action = new Runnable() {
			public void run() {
				logger2.info("Simple action run.");
			}
		};
		ThreadLoopRunner threadLoopRunner = new ThreadLoopRunner(action, 1, TimeUnit.SECONDS, tf, logger);
		assertEquals(ScheduleDelayMode.FIXED_RATE, threadLoopRunner.getDelayMode());
		assertFalse(threadLoopRunner.isLoopRunning());

		threadLoopRunner.runLoop();

		assertTrue(threadLoopRunner.isLoopRunning());
		assertFalse(threadLoopRunner.isDisabled());
		assertEquals(ScheduleDelayMode.FIXED_RATE, threadLoopRunner.getDelayMode());
		
		// sleep 2.5 seconds, so that the loop can run 3 times (with our 1 sec delay) at around times 0, 1, 2 seconds
		int expectedNumberOfInvocations = 3;
		Thread.sleep((expectedNumberOfInvocations-1) * 1000 + 500);
		boolean shutdownRet = threadLoopRunner.shutdown(100, TimeUnit.MILLISECONDS);
		assertTrue("should have finished within 100 ms", shutdownRet);
		
		LogRecord[] records = logger2.getCollectedLogRecords();
		assertEquals("three records expected", expectedNumberOfInvocations, records.length);
		for (int i = 0; i < records.length; i++) {
			assertEquals("Simple action run.", records[i].getMessage());
		}
	}


	/**
	 * Tests a well-behaved but complex usage scenario, 
	 * including calls to suspend and start again the execution loop.
	 */
	public void testComplexWithSuspendAndRestart() throws Exception {
		
		int actionWaitMillis = 100;
		int delayMillis = 300;
		int numberOnOffCycles = 3;
		int expectedInvocationsPerCycle = 33;
		int allowedThreadJitterMillis = 200;
		
		CountDownLatch sync = new CountDownLatch(expectedInvocationsPerCycle);
		MyAction myAction = new MyAction(sync, actionWaitMillis, logger);

		ThreadLoopRunner threadLoopRunner = null;
		try {
			threadLoopRunner = new ThreadLoopRunner(myAction, delayMillis, TimeUnit.MILLISECONDS, tf, logger);
			StopWatch sw = new StopWatch();
			
			for (int i = 0; i < numberOnOffCycles; i++) {
				ScheduleDelayMode delayMode = i % 2 == 0 ? ScheduleDelayMode.FIXED_RATE : ScheduleDelayMode.FIXED_DELAY;
				
				logger.info("Will start the thread loop with delay mode " + delayMode.toString());
				threadLoopRunner.setDelayMode(delayMode);
				threadLoopRunner.runLoop();
				sw.reset();
				
				// wait till the last execution of the action is over
				int expectedDurationMillis = (expectedInvocationsPerCycle - 1)* delayMillis + actionWaitMillis;
				if (delayMode == ScheduleDelayMode.FIXED_DELAY) {
					expectedDurationMillis += (expectedInvocationsPerCycle - 1) * actionWaitMillis;
				}
				int timeoutMillis = expectedDurationMillis + allowedThreadJitterMillis;
				boolean awaitRet = sync.await(timeoutMillis, TimeUnit.MILLISECONDS);
				int actualDuration = (int) sw.getLapTimeMillis();
				int actualInvocations = myAction.getCount();
				assertTrue("Timed out after " + timeoutMillis + " ms", awaitRet);
				assertEquals(expectedInvocationsPerCycle, actualInvocations);
				assertTrue("Tasks were run faster (" + actualDuration + ") than expected (" + expectedDurationMillis + ")", 
						actualDuration > expectedDurationMillis - allowedThreadJitterMillis);
				
				assertTrue(threadLoopRunner.isLoopRunning());
				assertFalse(threadLoopRunner.isDisabled());

				// suspend and assert that no further actions get executed 
				logger.info("Will suspend the thread loop");
				threadLoopRunner.suspendLoop();
				assertFalse(threadLoopRunner.isLoopRunning());
				assertFalse(threadLoopRunner.isDisabled());
				assertEquals(delayMode, threadLoopRunner.getDelayMode());
				Thread.sleep((actionWaitMillis + delayMillis)*2);
				assertEquals(expectedInvocationsPerCycle, myAction.getCount());
				
				// run again
				sync = new CountDownLatch(expectedInvocationsPerCycle);
				myAction.reset(sync);
			}
		} 
		finally {
			if (threadLoopRunner != null) {
				assertTrue(threadLoopRunner.shutdown(100, TimeUnit.MILLISECONDS));
			}
		}
	}

	
	public void testSetDelayTimeAndMode() throws Exception {
		int actionWaitMillis = 1000;
		int delayMillis = 1500;
		
		CountDownLatch sync = new CountDownLatch(1);
		MyAction myAction = new MyAction(sync, actionWaitMillis, logger);

		ThreadLoopRunner threadLoopRunner = null;
		Exception mainBlockException = null;
		
		try {
			threadLoopRunner = new ThreadLoopRunner(myAction, delayMillis, TimeUnit.MILLISECONDS, tf, logger);
			// setting delay mode before running loop is fine
			threadLoopRunner.setDelayMode(ScheduleDelayMode.FIXED_RATE);
			threadLoopRunner.runLoop();
			// setting delay mode while running loop must give exception
			try {
				threadLoopRunner.setDelayMode(ScheduleDelayMode.FIXED_DELAY);
				fail("Expected IllegalStateException");
			} catch (IllegalStateException ex) {
				// good
			}
			// First sleep a bit to make sure task has started
			Thread.sleep(100);
			// task runs for 1 second, so it should still be running now.
			assertTrue(threadLoopRunner.isTaskRunning());
			// changing the delay time while the task is running should stop the loop, return immediately, 
			// and later restart the loop.
			StopWatch sw = new StopWatch();
			delayMillis = 200;
			threadLoopRunner.setDelayTime(delayMillis, TimeUnit.MILLISECONDS);
			assertTrue("Calling setDelayTime should return very quickly, certainly in less than 100 ms.", 
					sw.getLapTimeMillis() <= 100);

			// task loop should be stopped right after call to setDelayTime
			assertFalse(threadLoopRunner.isLoopRunning());
			// when task has finished (should be the case after actionWaitMillis and all the calls above),
			// the task loop should be running again, with the new delay time 200  ms.
			Thread.sleep(actionWaitMillis);
//			assertTrue(threadLoopRunner.isLoopRunning());
			assertEquals(delayMillis, threadLoopRunner.getDelayTimeMillis());
			
			// verify after 10 repetitions that the new shorter delay time was actually applied
			sync = new CountDownLatch(10);
			myAction.reset(sync);
			logger.info("Now waiting for the loop to execute 10 times:");
			
			assertTrue("Got timeout, after just " + myAction.getCount() + " task executions", 
					sync.await((actionWaitMillis + delayMillis) * 10 + 100, TimeUnit.MILLISECONDS));
		}
		catch (Exception ex1) {
			mainBlockException = ex1;
		}
		finally {
			if (threadLoopRunner != null) {
				try {
					assertTrue("Failed to shutdown thread loop runner in 2000 ms.", 
							threadLoopRunner.shutdown(2000, TimeUnit.MILLISECONDS));
				} catch (Exception ex2) {
					if (mainBlockException != null) {
						throw mainBlockException;
					}
					else {
						throw ex2;
					}
				}
			}
		}
	}



//	public void testDetailsSuspend() throws Exception {
//		int repeatMillis = 1000;
//		int sleepMillis = 500;
//		int allowedThreadJitterMillis = 50;
//		
//		// Test that suspend() returns without waiting for the currently executing action to finish
//		
//		CountDownLatch sync = new CountDownLatch(1);
//		MyAction myAction = new MyAction(sync, sleepMillis, logger);
//
//		ThreadLoopRunner threadLoopRunner = new ThreadLoopRunner(tf, myAction, logger);
//		threadLoopRunner.setDelayTime(repeatMillis , TimeUnit.MILLISECONDS);
//		
//		threadLoopRunner.runLoop(ScheduleDelayMode.FIXED_DELAY);
//		StopWatch sw = new StopWatch();
//		threadLoopRunner.suspendLoop();
//		int timeToSuspendMillis = (int) sw.getLapTimeMillis();
//		logger.info("Time to suspend an action running for " + sleepMillis + " ms: " + timeToSuspendMillis);
//		assertTrue(timeToSuspendMillis <= allowedThreadJitterMillis);
//		
//		
//		
//				
//				
//				
//		// Test restarting the loop before last action has finished
//		
//		threadLoopRunner.runLoop(ScheduleDelayMode.FIXED_DELAY);
//	}
//	
//	
////	public void testLongRunningAction() throws Exception {
////		int delayMillis = 1000;
////		int sleepMillis = 2000;
////		int allowedThreadJitterMillis = 50;
////		
////		// test action that takes longer than foreseen time in FIXED_RATE mode
////		CountDownLatch sync = new CountDownLatch(2);
////		MyActionWithCancelCheck myAction = new MyActionWithCancelCheck(sync, sleepMillis, logger);
////		
////		ThreadLoopRunner threadLoopRunner = new ThreadLoopRunner(tf, myAction, logger);
////		threadLoopRunner.setRepeatTime(delayMillis, TimeUnit.MILLISECONDS);
////
////		// call shutdown during execution of the loop action and test cancel functionality
////		threadLoopRunner.runLoop(ScheduleDelayMode.FIXED_RATEFIXED_DELAY);
////		StopWatch sw = new StopWatch();
////		threadLoopRunner.shutdown(timeout, unit);
////		assertTrue("Got a timeout", sync.await(delayMillis / 2 + allowedThreadJitterMillis, TimeUnit.MILLISECONDS));
////		assertTrue(sw.getLapTimeMillis() > delayMillis / 2 - allowedThreadJitterMillis);
////		assertEquals(1, myAction.getSleepCancelCount());
////	}
////	

	
//	public void testIllegalInvocations() throws Exception {
//		
//	}

	/**
	 * Action that tracks the invocations of the run() method and allows clients to wait for a certain number of such invocations
	 */
	private static class MyAction extends ThreadLoopRunner.CancelableRunnable {
		private volatile CountDownLatch sync;
		protected final int sleepMillis;
		protected final Logger logger;
		private volatile int count;
		
		MyAction(CountDownLatch sync, int sleepMillis, Logger logger) {
			this.sleepMillis = sleepMillis;
			this.logger = logger;
			reset(sync);
		}
		
		void reset(CountDownLatch newSync) {
			this.sync = newSync;
			count = 0;
		}
		
		public void run() {
			logger.info("Will sleep for " + sleepMillis + " ms.");
			try {
				sleep();
			} 
			catch (InterruptedException ex) {
				ex.printStackTrace();
			} 
			finally {
				logger.info("Woke up - " + count);
				count++;
				sync.countDown();
			}
		}
		
		protected void sleep() throws InterruptedException {
			Thread.sleep(sleepMillis);
		}

		int getCount() {
			return count;
		}
	}

//	
//	/**
//	 * Variant that makes use of the {@link CancelableRunnable#shouldTerminate} flag
//	 */
//	private static class MyActionWithCancelCheck extends MyAction {
//
//		private int countSleepCancel;
//		
//		MyActionWithCancelCheck(CountDownLatch sync, int sleepMillis, Logger logger) {
//			super(sync, sleepMillis, logger);
//		}
//
//		protected void sleep() throws InterruptedException {
//			// first half
//			Thread.sleep(sleepMillis/2);
//			if (!shouldTerminate) {
//				// second half
//				Thread.sleep(sleepMillis / 2);
//			}
//			else {
//				countSleepCancel++;
//				logger.info("run (sleep) action cancelled, returning early.");
//			}
//		}
//
//		int getSleepCancelCount() {
//			return countSleepCancel;
//		}
//	}
}
