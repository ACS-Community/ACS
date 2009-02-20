package alma.acs.concurrent;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

public class ParallelExecutorTest extends TestCase
{
//	private ParallelExecutor parallelExecutor;
	
	public void testParallelExecutor() throws Exception {
		int num = 2;
		ThreadFactory tf = new DaemonThreadFactory("ParallelExecutorTest");
		CountDownLatch sharedExecutionVerifier = new CountDownLatch(num);
		ParallelExecutor parallelExecutor = new ParallelExecutor(tf);
		for (int i = 0; i < 2; i++) {
			parallelExecutor.scheduleForExecution(new TestRunnable(i, sharedExecutionVerifier), 1000);
		}
		assertEquals(2, sharedExecutionVerifier.getCount());		

		parallelExecutor.execute();
		// the run() methods of our TestRunnable should soon decrement the sharedExecutionVerifier counter
		assertTrue("Unexpected timeout", sharedExecutionVerifier.await(2, TimeUnit.SECONDS));
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
