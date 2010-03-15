/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acs.concurrent;

import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;


/**
 * Helper class that allows tests (but also other code) to be executed concurrently,
 * in an attempt to "bombard" a tested class with parallel calls,
 * all arriving in the smallest possible time window.
 * <p>
 * This class is similar to {@link java.util.concurrent.ExecutorService}, 
 * but it synchronizes the actual execution of all threads.
 * Note that the simple creation and starting of threads does not guarantee 
 * that these threads run after {@link Thread#start()} returns.
 * Therefore this class does not support methods such as {@link ExecutorService#execute(Runnable)}
 * which execute one <code>Runnable</code> in one thread independently of the other threads that it runs.
 * <p>
 * An instance is good for only one burst of threads, otherwise IllegalStateException will be thrown.
 * 
 * @author hsommer
 */
public class ThreadBurstExecutorService
{
	private final ThreadPoolExecutor delegate;
	private final CountDownLatch threadGate;
	private final AtomicBoolean executed;

	
	public ThreadBurstExecutorService(ThreadFactory threadFactory) {
		delegate = new ThreadPoolExecutor(
				0, // corePoolSize
				Integer.MAX_VALUE, // maximumPoolSize
				0L, TimeUnit.SECONDS, // keepAliveTime
				new SynchronousQueue<Runnable>(), // pseudo workQueue (length 0)
				threadFactory
				// no custom RejectedExecutionHandler used -> RejectedExecutionException thrown if queue is full
			);
		threadGate = new CountDownLatch(1);
		executed = new AtomicBoolean(false);
	}
	

	/**
	 * Submits a task and waits until the thread for this task has started (without yet executing the task!).
	 * Then this thread will block until {@link #executeAllAndWait(long, TimeUnit)} is called
	 * or the <code>awaitExecutionTimeout</code> occurs.
	 * @param task  
	 *         The task that should be executed when {@link #executeAllAndWait(long, TimeUnit)} is called.
	 * @param awaitExecutionTimeout 
	 *         This timeout is used twice: (a) to wait for the thread to be created and started,
	 *         and to wait for the user to call {@link #executeAllAndWait(long, TimeUnit)}.
	 * @return An object handle that can be used to wait for completion of this task, 
	 *         or exceptions if any are thrown (e.g. ExecutionException), or to cancel the task.
	 * @throws InterruptedException If waiting for the thread to be created and started is interrupted.
	 * @throws IllegalStateException if {@link #executeAllAndWait(long, TimeUnit)} was called already.
	 */
	public <T> Future<T> submit(Callable<T> task, long awaitExecutionTimeout, TimeUnit unit) throws InterruptedException {
		if (executed.get()) {
			throw new IllegalStateException();
		}
		CountDownLatch confirmWaiting = new CountDownLatch(1);

		InterceptingCallable<T> interceptingCallable = 
			new InterceptingCallable<T>(task, threadGate, confirmWaiting, awaitExecutionTimeout, unit);
		
		Future<T> future = delegate.submit(interceptingCallable);
		
		// wait till the thread has started, otherwise a subsequent executeAllAndWait call might come too early
		confirmWaiting.await(awaitExecutionTimeout, unit);
		
		return future;
	}
	
	
	/**
	 * Variant of {@link #submit(Callable, long, TimeUnit)} which gives the task as a {@link Runnable}.
	 * This does not allow to pass return values or exceptions from the thread to the caller,
	 * but may be more customary to use.
	 */
	public Future<Void> submit(final Runnable task, long awaitExecutionTimeout, TimeUnit unit) throws InterruptedException {
		Callable<Void> adapter = new Callable<Void>() {
			public Void call() throws Exception {
				task.run();
				return null;
			}
		};
		return submit(adapter, awaitExecutionTimeout, unit);
	}
	
	
	/**
	 * Unleashes all submitted (and therefore already started) threads at the same time.
	 * Then waits until all tasks finish, or until the given <code>timeout</code> occurs.
	 * <p>
	 * This method must only be called once for a given instance of ThreadBurstExecutor.
	 * 
	 * @return true if all tasks have run, and false if the timeout elapsed before termination.
	 * @throws IllegalStateException if {@link #executeAllAndWait(long, TimeUnit)} was called already.
	 * @throws InterruptedException  If waiting for the tasks to finish was interrupted.
	 */
	public boolean executeAllAndWait(long timeout, TimeUnit unit) throws InterruptedException {
		if (executed.getAndSet(true)) {
			throw new IllegalStateException();
		}
		
		// this will unleash all threads that block on threadGate
		threadGate.countDown();
		
		delegate.shutdown();
		return delegate.awaitTermination(timeout, unit);
	}
	
	/**
	 * Attempts to stop all running threads, waiting no longer than the given <code>timeout</code>.
	 * This method can be called either before {@link #executeAllAndWait} to stop the threads
	 * that block on the thread gate to open, or afterwards to stop running tasks.
	 * <p>
	 * There are no guarantees beyond best-effort attempts to stop processing actively executing tasks. This
	 * implementation cancels tasks via {@link Thread#interrupt}, so any task that fails to respond to interrupts may
	 * never terminate.
	 * 
	 * @param timeout
	 * @param unit time unit
	 * @return true if this executor terminated and false if the timeout elapsed before termination
	 * @throws InterruptedException 
	 */
	public boolean terminateAllAndWait(long timeout, TimeUnit unit) throws InterruptedException {
		delegate.shutdownNow();
		return delegate.awaitTermination(timeout, unit);
	}
	
	
	
	/**
	 * Wrapper used to block a thread created by {@link ThreadBurstExecutorService#delegate}, until the thread gate opens.
	 */
	private static class InterceptingCallable<V> implements Callable<V> {
		private final Callable<V> delegateCallable;
		private final CountDownLatch threadGate;
		private final CountDownLatch confirmWaiting;
		private final long awaitExecutionTimeout;
		private final TimeUnit unit;

		InterceptingCallable(Callable<V> delegate, CountDownLatch threadGate, CountDownLatch confirmWaiting, long awaitExecutionTimeout, TimeUnit unit) {
			this.delegateCallable = delegate;
			this.threadGate = threadGate;
			this.confirmWaiting = confirmWaiting;
			this.awaitExecutionTimeout = awaitExecutionTimeout;
			this.unit = unit;
		}
		
		/**
		 * The executor has started this thread, but here we wait for the thread gate to open,
		 * and only then call the delegate runnable submitted by the user.
		 */
		public V call() throws Exception {
			confirmWaiting.countDown();
			if(threadGate.await(awaitExecutionTimeout, unit) == false) {
				String msg = "Timeout occured while waiting for thread gate to be opened.";
				System.out.println(msg);
				throw new TimeoutException(msg);
			}
			else {
				return delegateCallable.call();
			}
		}
	}
}
