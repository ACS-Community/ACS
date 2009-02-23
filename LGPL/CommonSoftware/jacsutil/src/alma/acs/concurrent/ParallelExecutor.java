package alma.acs.concurrent;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

/**
 * Helper class that allows tests (but also other code) to be executed concurrently.
 * Unlike {@link java.util.concurrent.Executor}, this class synchronizes the actual execution of all threads,
 * while the simple creation and starting of threads does not guarantee that these threads run after {@link Thread#start()} returns.
 * 
 * Thus using this class should be particularly helpful for "mean" tests that try to bombard a tested class with parallel calls,
 * all arriving in the smallest possible time window.
 * 
 * @author hsommer
 */
public class ParallelExecutor
{
	private final ThreadFactory tf;
	private final CountDownLatch threadGate;
	private volatile boolean executed = false;
	
	/**
	 * @param tf For components, clients and unit tests, this should be taken from container services method getThreadFactory.
	 */
	public ParallelExecutor(ThreadFactory tf) {
		this.tf = tf;
		threadGate = new CountDownLatch(1);
	}


	/**
	 * Creates and starts a new thread, waits until it runs, 
	 * but blocks it from calling {@code runnable#run}
	 * until {@link #execute()} is called.
	 * 
	 * @param runnable  your code that should be run in a separate thread
	 * @param timeoutMillis  timeout in milliseconds: code will not be run if {@link #execute()} 
	 *        is not called within this time.
	 *        This is meant to avoid troubles with deadlocked threads in case something goes wrong.
	 * @throws InterruptedException
	 * @throws IllegalStateException If called after {@link #execute()} or {@link #execute(List, long)}. 
	 */
	public void scheduleForExecution(Runnable runnable, long timeoutMillis) throws InterruptedException {
		if (executed) {
			throw new IllegalStateException();
		}
		CountDownLatch confirmWaiting = new CountDownLatch(1);
		InterceptingRunnable interceptingRunnable = new InterceptingRunnable(runnable, threadGate, confirmWaiting, timeoutMillis);
		Thread t = tf.newThread(interceptingRunnable);
		t.start();
		// wait till the thread has started, otherwise the execute() call might come too early 
		// and the threads would wait for it till they time out.
		confirmWaiting.await(timeoutMillis, TimeUnit.MILLISECONDS);
	}


	/**
	 * Unleash all threads. The run methods of all runnables submitted to {@link #scheduleForExecution(Runnable, long)}
	 * should then be called immediately.
	 */
	public void execute() {
		threadGate.countDown();
		executed = true;
	}
	
	/**
	 * Convenience method that combines {@link #scheduleForExecution(Runnable)} and {@link #execute()}.
	 * @param runnables
	 * @throws InterruptedException 
	 */
	public void execute(List<Runnable> runnables, long timeoutMillis) throws InterruptedException {
		for (Runnable runnable : runnables) {
			scheduleForExecution(runnable, timeoutMillis);
		}
		execute();
	}
	
	private static class InterceptingRunnable implements Runnable {
		private final Runnable delegate;
		private final CountDownLatch threadGate;
		private final CountDownLatch confirmWaiting;
		private final long timeoutMillis;

		InterceptingRunnable(Runnable delegate, CountDownLatch threadGate, CountDownLatch confirmWaiting, long timeoutMillis) {
			this.delegate = delegate;
			this.threadGate = threadGate;
			this.confirmWaiting = confirmWaiting;
			this.timeoutMillis = timeoutMillis;
		}
		
		public void run() {
			boolean success = false;
			try {
				confirmWaiting.countDown();
				// condition.await will unlock the lock and wait...
				if(threadGate.await(timeoutMillis, TimeUnit.MILLISECONDS) == false) {
					// got a timeout
					System.out.println("Got a timeout from condition.await()");
				}
				else {
					delegate.run();
					success = true;
				}
			} catch (InterruptedException ex) {
				// TODO Auto-generated catch block
				ex.printStackTrace();
			}
			// TODO: handle !success better than this
			if (!success) {
				System.out.println("Damn, run() failed in thread " + Thread.currentThread().getName());
			}
		}
	}
}
