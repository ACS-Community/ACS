package alma.acs.concurrent;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Helper class that allows tests (but also other code) to be executed concurrently.
 * One of the advantages of using this class is that it synchronizes the actual execution of all threads,
 * while simple creation and starting of threads does not guarantee that these threads run after {@link Thread#start()} returns.
 * 
 * Thus using this class should be particularly helpful for "mean" tests that try to bombard a tested class with parallel calls,
 * all arriving in the smallest possible time window.
 * 
 * @author hsommer
 */
public class ParallelExecutor
{
	private final ThreadFactory tf;
	private final ReentrantLock lock;
	private final Condition readyToRun;
	private final List<Thread> scheduledThreads;
	
	/**
	 * @param tf For components, clients and unit tests, this should be taken from container services method getThreadFactory.
	 */
	public ParallelExecutor(ThreadFactory tf) {
		this.tf = tf;
		lock = new ReentrantLock();
		readyToRun = lock.newCondition();
		scheduledThreads = new ArrayList<Thread>();
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
	 */
	public void scheduleForExecution(Runnable runnable, long timeoutMillis) throws InterruptedException {
		CountDownLatch confirmWaiting = new CountDownLatch(1);
		InterceptingRunnable interceptingRunnable = new InterceptingRunnable(runnable, lock, readyToRun, confirmWaiting, timeoutMillis);
		Thread t = tf.newThread(interceptingRunnable);
		scheduledThreads.add(t);
		t.start();
		// wait till the thread has started, otherwise the execute() call might come too early 
		// and the threads would wait for it till they time out.
		confirmWaiting.await(timeoutMillis, TimeUnit.MILLISECONDS);
		// now that the thread has confirmed that it's running, we also know from the implementation 
		// of InterceptingRunnable#run that it already holds the lock.
		// It will release this lock a few lines down by calling condition.await, so that by the time we can get the lock here,
		// we know that the runnable is really parked on condition and will therefore be woken up by a future call to #execute()
		lock.lock();
		lock.unlock();
	}


	/**
	 * Unleash all threads. The run methods of all runnables submitted to {@link #scheduleForExecution(Runnable, long)}
	 * should then be called immediately.
	 */
	public void execute() {
		lock.lock();
		try {
			readyToRun.signalAll();
		}
		finally {
			lock.unlock();
		}
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
		private final Lock lock;
		private final Condition condition;
		private final CountDownLatch confirmWaiting;
		private final long timeoutMillis;

		InterceptingRunnable(Runnable delegate, Lock lock, Condition condition, CountDownLatch confirmWaiting, long timeoutMillis) {
			this.delegate = delegate;
			this.lock = lock;
			this.condition = condition;
			this.confirmWaiting = confirmWaiting;
			this.timeoutMillis = timeoutMillis;
		}
		
		public void run() {
			boolean success = false;
			lock.lock();
			try {
				confirmWaiting.countDown();
				// condition.await will unlock the lock and wait...
				if(condition.await(timeoutMillis, TimeUnit.MILLISECONDS) == false) {
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
			finally {
				lock.unlock();
			}
			// TODO: handle !success
		}
	}
}
