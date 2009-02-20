package alma.acs.concurrent;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import alma.acs.util.StopWatch;

import junit.framework.TestCase;

public class ParallelExecutorTest extends TestCase
{
	public void testParallelExecutor() throws Exception {
		
		ThreadFactory tf = new DaemonThreadFactory("ParallelExecutorTest");
		ParallelExecutor parallelExecutor = new ParallelExecutor(tf);

		int num = 222;
		long threadSleepMillis = 1000;
		
		// Set up <num> threads to be executed at once
		StopWatch sw = new StopWatch();
		CountDownLatch sharedExecutionVerifier = new CountDownLatch(num);
		for (int i = 0; i < num; i++) {
			Runnable runnable = new TestRunnable(i, sharedExecutionVerifier, threadSleepMillis);
			parallelExecutor.scheduleForExecution(runnable, threadSleepMillis * 2);
		}
		assertEquals(num, sharedExecutionVerifier.getCount());
		
		// execute these threads
		parallelExecutor.execute();
		System.out.println("All " + num + " threads are now running. It took " + sw.getLapTimeMillis() + " ms to set them up.");
		
		// the run() methods of our TestRunnable should soon decrement the sharedExecutionVerifier counter
		assertTrue("Unexpected timeout", sharedExecutionVerifier.await(10, TimeUnit.SECONDS));
		
		// just to live to see the message that our threads wake up again
		Thread.sleep(threadSleepMillis + 1000);
	}

	private static class TestRunnable implements Runnable {
		private final int index;
		private final CountDownLatch sharedExecutionVerifier;
		private final long threadSleepMillis;
		
		TestRunnable(int index, CountDownLatch sharedExecutionVerifier, long threadSleepMillis) {
			this.index = index;
			this.sharedExecutionVerifier = sharedExecutionVerifier;
			this.threadSleepMillis = threadSleepMillis;
		}
		
		public void run() {
			System.out.println("---TestRunnable-" + index + " will sleep for " + threadSleepMillis + " ms.");
			try {
				Thread.sleep(threadSleepMillis);
			} catch (InterruptedException ex) {
				ex.printStackTrace();
			}
			finally {
				sharedExecutionVerifier.countDown();
				System.out.println("===TestRunnable-" + index + " woke up");
			}
		}
	}

}
