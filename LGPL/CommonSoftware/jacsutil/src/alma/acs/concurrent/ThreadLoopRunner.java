/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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


import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;

/**
 * This class allows running some task repeatedly with a given delay.
 * It is similar to the loop in C++ class ACS::Thread.
 * <p>
 * The code of the task to be executed gets passed to the constructor as a {@link Runnable} object.
 * The time between executions is given in the constructor and can later be changed through 
 * {@link #setDelayTime(long, TimeUnit)}.
 * <p> 
 * If the task takes some time to execute, you should consider implementing a soft cancel option. To do this,
 * extend {@link #CancelableRunnable} instead of simply implementing <code>Runnable</code>.
 * When the action should be canceled (e.g. due to {@link #shutdown(long, TimeUnit)}), the <code>shouldTerminate</code>
 * flag will be set. Your code should check this flag at convenient times, and should return if the flag is set.
 * Alternatively you can override the cancel method and terminate the task thread in different ways.
 * <p>
 * Method {@link #runLoop()} starts the loop.
 * Optionally the loop can be stopped with {@link #suspendLoop()} or {@link #suspendLoopAndWait(long, TimeUnit)}, 
 * and can also be restarted with another call to {@link #runLoop()}.
 * To stop the loop for good, call {@link #shutdown(long, TimeUnit)}.
 * <p>
 * While the underlying classes from the JDK <code>concurrent</code> package could also be used directly,
 * this class allows for shorter code that is also more similar to the style used in C++.
 * Especially it imposes the limit of running one task repeatedly, which gives an easier API,
 * at the expense of creating a separate instance of ThreadLoopRunner for some other repeated task.
 * (The older JDK class {@link java.util.Timer} has problems recovering from errors and should not be used.)
 * 
 * @author hsommer
 */
public class ThreadLoopRunner
{
	private final Logger logger;

	/**
	 * The single-threaded executor that backs our ThreadLoopRunner.
	 */
	private final ScheduledExecutorService runner;
	
	/**
	 * Wraps the user-supplied task, to enable this class to synchronize
	 * with the task execution that otherwise is delegated to standard JDK {@link #runner}.
	 */
	private final TaskWrapper taskWrapper;
	
	public static enum ScheduleDelayMode {FIXED_RATE, FIXED_DELAY}
	private volatile ScheduleDelayMode delayMode;
	
	/**
	 * Delay time in ns.
	 */
	private volatile long delayTimeNanos;
	
	/**
	 * The task loop. Must be protected by {@link #loopLock}.
	 */
	private volatile ScheduledFuture<?> loop;
	
	/**
	 * A flag set by {@link #shutdown(long, TimeUnit)} and used by other methods
	 * to throw <code>IllegalStateException</code> during or after shutdown.
	 * It can be queried by {@link #isDisabled()}.
	 */
	private final AtomicBoolean isDefunct;
	
	/**
	 * A lock to synchronize on changes of the task loop ({@link #loop}).
	 */
	private final ReentrantLock loopLock = new ReentrantLock();

	/**
	 * Used to make logs and thread names more readable. May be null.
	 */
	private final String loopName;
	


	/**
	 * Creates a <code>ThreadLoopRunner</code> that can repeatedly execute <code>task</code>.
	 * The mode defaults to {@link ScheduleDelayMode#FIXED_RATE} unless being changed 
	 * via {@link #setDelayMode(ScheduleDelayMode)}.
	 * 
	 * @param task user-supplied {@link Runnable}, or better subtype {@link ThreadLoopRunner.CancelableRunnable}.
	 * @param delayTime 
	 * @param unit
	 * @param tf ThreadFactory from which the loop thread will be created.
	 * @param logger Logger used by this class.
	 * @deprecated (since ACS 10.1) Use the variant with additional name parameter.
	 */
	public ThreadLoopRunner(Runnable task, long delayTime, TimeUnit unit, ThreadFactory tf, Logger logger) {
		this(task, delayTime, unit, tf, logger, null);
	}

	/**
	 * Creates a <code>ThreadLoopRunner</code> that can repeatedly execute <code>task</code>.
	 * The mode defaults to {@link ScheduleDelayMode#FIXED_RATE} unless being changed 
	 * via {@link #setDelayMode(ScheduleDelayMode)}.
	 * 
	 * @param task user-supplied {@link Runnable}, or better subtype {@link ThreadLoopRunner.CancelableRunnable}.
	 * @param delayTime 
	 * @param unit
	 * @param tf ThreadFactory from which the loop thread will be created.
	 * @param logger Logger used by this class.
	 * @param name Facilitates debugging, by using a meaningful name for logs and threads.
	 */
	public ThreadLoopRunner(Runnable task, long delayTime, TimeUnit unit, final ThreadFactory tf, Logger logger, String name) {
		this.logger = logger;
		this.loopName = ( (name != null && !name.trim().isEmpty()) ? name.trim() : null );
		
		this.runner = new ScheduledThreadPoolExecutor(1, new NamedThreadFactory(tf, loopName));
		
		this.taskWrapper = new TaskWrapper(task, loopLock, logger);
		this.delayMode = ScheduleDelayMode.FIXED_RATE;
		isDefunct = new AtomicBoolean(false);
		setDelayTime(delayTime, unit);
	}

	/**
	 * @return The delay time in ms, as set in the constructor or changed afterwards 
	 * in {@link #setDelayTime(long, TimeUnit)}.
	 */
	public long getDelayTimeMillis() {
		return TimeUnit.MILLISECONDS.convert(delayTimeNanos, TimeUnit.NANOSECONDS);
	}
	
	/**
	 * Sets the time between calls to the loop action object,
	 * where the time for the task itself is included or not,
	 * depending on the chosen {@link ScheduleDelayMode}.
	 * <p>
	 * If this method is called while the thread loop is already running,
	 * the task will be run again right after the currently running task
	 * has finished; only after that run we'll get into the proper delay timing.
	 * <p>
	 * Implementation note: If this method is called while the thread loop is already running, 
	 * it will stop the loop, apply the new value, and then re-start the loop. 
	 * It is a limitation in the underlying {@link ScheduledThreadPoolExecutor}
	 * that the delay time cannot be changed without stopping and restarting the loop (= repetitive task).
	 * If this becomes a problem, we could use the concurrent lib classes in a more customized way.
	 * 
	 * @param delayTime new delay time
	 * @param unit
	 * @see #setDelayMode(ScheduleDelayMode)
	 */
	public void setDelayTime(final long delayTime, final TimeUnit unit) {

		if (isDefunct.get()) {
			throw new IllegalStateException("["+ loopName + "] already disabled");
		}
		
		loopLock.lock();
		try {
			// store the value
			delayTimeNanos = TimeUnit.NANOSECONDS.convert(delayTime, unit);
			if (isLoopRunning()) {
				// stop the loop
				suspendLoop();
				
				// restart the loop
				if (isTaskRunning()) {
					// schedule restarting the loop after the currently running task has finished.
					// The same loopLock is used so that setting up this task now guarantees getting it run after the currently executing task.
					taskWrapper.restartLoopAfterCurrentTaskFinished(new Runnable() {
						@Override
						public void run() {
							logger.finer("["+ loopName + "] will restart the loop now, which was stopped to change the delay time.");
							// @TODO: here we could sleep a bit to keep the intended delay rate/length, if needed.
							runLoop();
						}
					});
				}
				else {
					// The task was not running after we suspended the loop. We thus assume that no task has been started in the meantime
					// and simply restart the loop directly.
					runLoop();
				}
			}
		}
		finally {
			loopLock.unlock();
		}
	}


	/**
	 * @return The delay mode, either in use if the loop is already running, 
	 * or to be used when calling {@link #runLoop()}.
	 */
	public ScheduleDelayMode getDelayMode() {
		return delayMode;
	}
	
	/**
	 * Sets the delay mode to be used for the next {@link #runLoop()}.
	 * <p>
	 * This method must not be called when the loop is already running (see {@link #isLoopRunning()}),
	 * in which case it throws an IllegalStateException.
	 * The reason for this is that we see no need to change this mode on the fly, 
	 * and rather avoid the overhead of automatically stopping and restarting the loop 
	 * with the possible complications if the run() method does not terminate.
	 * Also we don't want {@link #getDelayMode()} to give results that are not correct for the currently running loop.
	 * Note that the same issue is handled differently in {@link #setDelayTime(long, TimeUnit)}
	 * where it seems desirable to change the delay time while the loop is running.
	 * 
	 * @param delayMode 
	 *           FIXED_RATE or FIXED_DELAY, 
	 *           see {@link ScheduledExecutorService#scheduleAtFixedRate(Runnable, long, long, TimeUnit)} and
	 *           {@link ScheduledExecutorService#scheduleWithFixedDelay(Runnable, long, long, TimeUnit)}.
	 *           Note that the C++ implementation uses equivalent of FIXED_RATE.
	 * @throws IllegalStateException if called when the loop is running, or after shutdown.
	 */
	public void setDelayMode(ScheduleDelayMode delayMode) {
		if (isDefunct.get()) {
			throw new IllegalStateException("["+ loopName + "] already disabled");
		}
		if (delayMode == null) {
			throw new IllegalArgumentException("delayMode must not be null");
		}
		if (isLoopRunning()) {
			throw new IllegalStateException("Cannot set delay mode while the loop is running");
		}
		this.delayMode = delayMode;
	}

	/**
	 * Runs the loop, either for the first time, or after a call to {@link #suspendLoop()}.
	 * @throws IllegalStateException 
	 *                   if the loop is already running, 
	 *                   or if the <code>run()</code> method of a previous loop is still executing, 
	 *                   or after shutdown
	 * @see #isLoopRunning()
	 */
	public void runLoop() {
		
		if (isDefunct.get()) {
			throw new IllegalStateException("["+ loopName + "] already disabled");
		}

		loopLock.lock();
		
		try {
			if (isLoopRunning()) {
				throw new IllegalStateException("Loop is already running");
			}
			
			if (isTaskRunning()) {
				throw new IllegalStateException("The task's run method is still being executed");
			}
			
			if (delayMode == ScheduleDelayMode.FIXED_RATE) {
				loop = runner.scheduleAtFixedRate(taskWrapper, 0, delayTimeNanos, TimeUnit.NANOSECONDS);
				logger.finer("["+ loopName + "] started task loop with FIXED_RATE=" + TimeUnit.MILLISECONDS.convert(delayTimeNanos, TimeUnit.NANOSECONDS) + " ms.");
			}
			else {
				loop = runner.scheduleWithFixedDelay(taskWrapper, 0, delayTimeNanos, TimeUnit.NANOSECONDS);
				logger.finer("["+ loopName + "] started task loop with FIXED_DELAY=" + TimeUnit.MILLISECONDS.convert(delayTimeNanos, TimeUnit.NANOSECONDS) + " ms.");
			}
		} finally {
			loopLock.unlock();
		}
	}

	
	/**
	 * @return <code>true</code> if the loop is running, regardless of whether the task is currently being executed.
	 */
	public boolean isLoopRunning() {
		loopLock.lock();
		try {
			return (loop != null);
		} finally {
			loopLock.unlock();
		}
	}
	
	/**
	 * @return <code>true</code> if the task is running, regardless of whether the loop is still running or has been stopped already.
	 * @see #suspendLoopAndWait(long, TimeUnit)
	 */
	public boolean isTaskRunning() {
		return taskWrapper.isRunning();
	}
	
	/**
	 * Returns <code>true</code> after {@link #shutdown(long, TimeUnit)} was called.
	 * Then invoking any control method of this class will throw an IllegalStateException.
	 */
	public boolean isDisabled() {
		return isDefunct.get();
	}
	

	/**
	 * Stops the loop, without attempting to cancel the possibly running action even if it was provided
	 * as a {@link CancelableRunnable}.
	 * Note also that this call returns quickly, without waiting for a possibly running action to finish.
	 * <p>
	 * The loop can be started again later via {@link #runLoop()}, once the running task has finished.
	 * Suspending and restarting the loop does not lead to the creation of a new Thread.
	 * 
	 * @throws IllegalStateException if called after shutdown. 
	 * @see #suspendLoopAndWait(long, TimeUnit)
	 */
	public void suspendLoop() {
		if (isDefunct.get()) {
			throw new IllegalStateException("["+ loopName + "] already disabled");
		}
		
		loopLock.lock();
		try {
			if (isLoopRunning()) {
				if (loop.cancel(false)) {
					logger.finer("["+ loopName + "] suspended the task loop.");
				}
				else {
					logger.fine("["+ loopName + "] failed to suspend the loop (without having attempted to cancel a running task, if any).");
				}
				loop = null;
				// also remove special post-run task, if any
				this.taskWrapper.restartLoopAfterCurrentTaskFinished(null);
			}
			else {
				logger.fine("["+ loopName + "] loop was not running, nothing to suspend.");
			}
		}
		finally {
			loopLock.unlock();
		}
	}
	
	/**
	 * Like {@link #suspendLoop()}, but additionally waits for the currently running task (if any) 
	 * to finish, with the given timeout applied.
	 * <p>
	 * If there is a task running and it fails to terminate, 
	 * a subsequent call to {@link #runLoop()} will fail with an IllegalStateException.
	 * 
	 * @param timeout
	 * @param unit
	 * @return true if all went fine within the given time, 
	 *         otherwise false (in which case the loop is still canceled)
	 * @throws InterruptedException 
	 *            if the calling thread is interrupted while waiting for the <code>run</code> method to finish.
	 * @throws IllegalStateException if called after shutdown. 
	 */
	public boolean suspendLoopAndWait(long timeout, TimeUnit unit) throws InterruptedException {
		suspendLoop();
		return taskWrapper.awaitTaskFinish(timeout, unit);
	}

	/**
	 * Shuts down this thread loop runner, 
	 * attempting to gracefully stop the running task if {@link CancelableRunnable} was provided,
	 * or otherwise letting the currently running loop action finish.
	 * <p>
	 * The <code>ThreadLoopRunner</code> cannot be used any more after this method has been called.
	 * (Then {@link #isDisabled()} will return <code>true</code>, other methods will throw IllegalStateException.)
	 * <p>
	 * The <code>timeout</code> refers to how long this method waits for the task to terminate.
	 * If it terminates before the given timeout, then <code>true</code> is returned, otherwise <code>false</code>
	 * which means that the Runnable action object is still in use and should not be reused later unless it is 
	 * re-entrant.
	 * 
	 * @param timeout
	 * @param unit
	 * @return true if loop action terminated before the given timeout, or if the loop was not running.
	 * @throws InterruptedException
	 * @throws IllegalStateException if called after shutdown. 
	 */
	public boolean shutdown(long timeout, TimeUnit unit) throws InterruptedException {
		if (isDefunct.getAndSet(true)) {
			throw new IllegalStateException("["+ loopName + "] already disabled");
		}
		
		loopLock.lock();
		try {
			if (isLoopRunning()) {
				// prevent further actions from being scheduled
				loop.cancel(false);
				loop = null;
				// cancel current loop action
				taskWrapper.attemptCancelTask();
				runner.shutdown();
				logger.finest("["+ loopName + "] task loop has been shut down, will wait for it to fully terminate.");
			}
			else {
				logger.finer("["+ loopName + "] nothing to shut down, task loop was not running.");
				return true;
			}
		} finally {
			loopLock.unlock();
		}
		// We wait outside of loopLock do avoid deadlocks if the run method also needs loopLock before terminating.
		boolean ret = runner.awaitTermination(timeout, unit);
		logger.finer("["+ loopName + "] task " + (ret ? "finished" : "failed to finish") + " within the specified " + timeout + " " + unit.toString().toLowerCase());
		
		// The runner's worker thread#isAlive still returns true right after this shutdown. 
		// We wait a tiny bit, to avoid "Forcibly terminating surviving thread" complaints 
		// from ContainerService's ThreadFactory during component shutdown.
		// Alternatively we could try to synchronize with ThreadPoolExecutor.terminated() callback.
		Thread.sleep(2);
		
		return ret;
	}


	/**
	 * Variation of {@link Runnable} that allows other threads to give a hint to the
	 * {{@link #run()} method that it should terminate.
	 * This is useful mainly with implementations of <code>run()</code> that don't finish immediately.
	 * Note that in Java {@link Thread#stop()} and similar methods are deprecated, and that 
	 * the proper way to terminate asynchronously running code is to signal the termination request
	 * via some variable that the thread is supposed to check at convenient points.
	 * <p>
	 * Therefore if your <code>run</code> method takes non-negligible time, you should 
	 * <ol>
	 * <li> provide a subclass of this <code>CancelableRunnable</code> as the loop action in 
	 *      {@link ThreadLoopRunner#ThreadLoopRunner(ThreadFactory, Runnable, Logger)}
	 * <li> implement <code>run()</code> to check at some points whether the flag {@link #shouldTerminate}
	 *      has been set (e.g. by {@link ThreadLoopRunner#shutdown} calling {@link CancelableRunnable#cancel()}),
	 *      and if so, to return from the run method as quickly as possible, but yet cleaning up.
	 * </ol>
	 */
	public abstract static class CancelableRunnable implements Runnable {
		protected volatile boolean shouldTerminate = false;
		/**
		 * Either the subclass's run() method evaluates the "shouldTerminate" flag,
		 * or the cancel() method gets overridden so that the subclass can react directly to it.
		 */
		public void cancel() {
			shouldTerminate = true;
		}
	}


	/**
	 * Wrapper of the user-supplied Runnable, which 
	 * can inform callers about the execution status of the run() method, or block clients until it finishes.
	 */
	private static class TaskWrapper implements Runnable {
		
		private final Logger logger;
//		private boolean extraDebugLogs;

		/**
		 * The delegate Runnable that gets called from inside our run method.
		 */
		private final Runnable delegate;
		
		/**
		 * A lock to synchronize on a single task execution.
		 */
		private final ReentrantLock runLock;
		
		/**
		 * A lock to synchronize on changes of the task loop.
		 */
		private final ReentrantLock loopLock;
		
		private volatile boolean isRunning;
		
		/**
		 * Access must be protected by loopLock.
		 */
		private volatile Runnable restartLoopRunnable;
		
		TaskWrapper(Runnable delegate, ReentrantLock loopLock, Logger logger) {
			this.logger = logger;
//			extraDebugLogs = !delegate.getClass().getName().toLowerCase().contains("alarm");
			this.delegate = delegate;
			runLock = new ReentrantLock();
			this.loopLock = loopLock;
			isRunning = false;
		}

		@Override
		public void run() {
			runLock.lock();
			try {
				isRunning = true;
//				if (extraDebugLogs) logger.finest("About to call delegate#run on delegate=" + delegate.getClass().getName());
				delegate.run();
//				if (extraDebugLogs) logger.finest("Returned from delegate#run");
			}
			finally {
				runLock.unlock();
				isRunning = false;
			}
			
			// execute special post-run-task, if any.
			loopLock.lock();
			try {
				if (restartLoopRunnable != null) {
//					if (extraDebugLogs) logger.finest("About to call restartLoopRunnable#run");
					restartLoopRunnable.run();
//					if (extraDebugLogs) logger.finest("Returned from restartLoopRunnable#run");
					restartLoopRunnable = null;
				}
			} finally {
				loopLock.unlock();
			}
		}
		
		/**
		 * Tests if <code>delegate#run</code> is currently executing.
		 * @return
		 */
		boolean isRunning() {
			return isRunning;
		}
		
		/**
		 * Checks if the delegate Runnable is of subtype {@link CancelableRunnable},
		 * and if so, calls the <code>cancel()</code> method.
		 */
		void attemptCancelTask() {
			if (delegate instanceof CancelableRunnable) {
				((CancelableRunnable) delegate).cancel();
			}
		}
		
		/**
		 * Blocks the calling thread if and as long as the <code>delegate#run</code> method executes,
		 * but at most for the given <code>timeout</code>.
		 * @throws InterruptedException 
		 */
		boolean awaitTaskFinish(long timeout, TimeUnit unit) throws InterruptedException {
			boolean gotIt = false;
			try {
				gotIt = runLock.tryLock(timeout, unit);
			} 
			finally { 
				if (gotIt) {
					runLock.unlock();
				}
			}
			return gotIt;
		}
		
		/**
		 * Allows to restart the loop from inside TaskWrapper#run.
		 */
		void restartLoopAfterCurrentTaskFinished(Runnable restartLoopRunnable) {
			loopLock.lock();
			this.restartLoopRunnable = restartLoopRunnable;
			loopLock.unlock();
			
//			if (extraDebugLogs) {
//				String msg = "Registered restartLoopRunnable: " + (restartLoopRunnable == null ? "null" : restartLoopRunnable.getClass().getName());
//				logger.fine(msg);
//			}
		}
	}
	
}
