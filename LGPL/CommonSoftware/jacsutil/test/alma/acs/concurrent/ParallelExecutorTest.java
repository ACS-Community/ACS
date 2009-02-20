package alma.acs.concurrent;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

public class ParallelExecutorTest extends TestCase
{
	public void testParallelExecutor() throws Exception {
		
		ThreadFactory tf = new DaemonThreadFactory("ParallelExecutorTest");
		ParallelExecutor parallelExecutor = new ParallelExecutor(tf);

		int num = 300;
		// Set up <num> threads to be executed at once
		CountDownLatch sharedExecutionVerifier = new CountDownLatch(num);
		for (int i = 0; i < num; i++) {
			parallelExecutor.scheduleForExecution(new TestRunnable(i, sharedExecutionVerifier), 1000);
		}
		assertEquals(num, sharedExecutionVerifier.getCount());

		// execute these threads
		parallelExecutor.execute();
		
		// the run() methods of our TestRunnable should soon decrement the sharedExecutionVerifier counter
		assertTrue("Unexpected timeout", sharedExecutionVerifier.await(10, TimeUnit.SECONDS));
	}

	private static class TestRunnable implements Runnable {
		private final int index;
		private final CountDownLatch sharedExecutionVerifier;
		
		TestRunnable(int index, CountDownLatch sharedExecutionVerifier) {
			this.index = index;
			this.sharedExecutionVerifier = sharedExecutionVerifier;
		}
		
		public void run() {
			sharedExecutionVerifier.countDown();
		}
	}

}
