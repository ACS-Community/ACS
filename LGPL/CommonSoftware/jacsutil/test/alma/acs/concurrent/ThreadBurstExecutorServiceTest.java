package alma.acs.concurrent;

import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import alma.acs.util.StopWatch;



public class ThreadBurstExecutorServiceTest extends TestCase
{
	public void testParallelExecutor() throws Exception {
		
		ThreadFactory tf = new DaemonThreadFactory("ThreadBurstExecutorServiceTest");
		ThreadBurstExecutorService exec = new ThreadBurstExecutorService(tf);

		int num = 333;
		long threadSleepMillis = 1000;
		
		// Set up <num> threads to be executed at once
		StopWatch sw = new StopWatch();
		CountDownLatch sharedExecutionVerifier = new CountDownLatch(num);
		for (int i = 0; i < num; i++) {
			Callable<Void> task = new TestCallable(i, sharedExecutionVerifier, threadSleepMillis);
			exec.submit(task, threadSleepMillis*2, TimeUnit.MILLISECONDS);
		}
		assertEquals(num, sharedExecutionVerifier.getCount());
		System.out.println("All " + num + " threads are now running. It took " + sw.getLapTimeMillis() + " ms to set them up.");
		
		// execute these threads and wait for tasks to finish
		assertTrue(exec.executeAllAndWait(threadSleepMillis + 1000, TimeUnit.MILLISECONDS));

		// the call() methods of our TestCallable should have decremented the sharedExecutionVerifier counter
		assertEquals(0, sharedExecutionVerifier.getCount());
		
		// try to call it again
		try {
			exec.executeAllAndWait(threadSleepMillis + 1000, TimeUnit.MILLISECONDS);
			fail("Expected IllegalStateException when calling executeAllAndWait a second time.");
		} catch (IllegalStateException ex) {
			// expected
		}
		
		// try to submit another task
		try {
			Runnable task2 = new Runnable() {
				public void run() {
				}
			};
			exec.submit(task2, threadSleepMillis + 1000, TimeUnit.MILLISECONDS);
			fail("Expected IllegalStateException when calling submit after executeAllAndWait.");
		} catch (IllegalStateException ex) {
			// expected
		}
	}

	/**
	 * 
	 */
	private static class TestCallable implements Callable<Void> {
		private final int index;
		private final CountDownLatch sharedExecutionVerifier;
		private final long threadSleepMillis;
		
		TestCallable(int index, CountDownLatch sharedExecutionVerifier, long threadSleepMillis) {
			this.index = index;
			this.sharedExecutionVerifier = sharedExecutionVerifier;
			this.threadSleepMillis = threadSleepMillis;
		}
		
		public Void call() throws Exception {
			System.out.println("---TestCallable-" + index + " will sleep for " + threadSleepMillis + " ms.");
			try {
				Thread.sleep(threadSleepMillis);
			}
			finally {
				sharedExecutionVerifier.countDown();
				System.out.println("===TestCallable-" + index + " woke up");
			}
			return null;
		}
	}

}
