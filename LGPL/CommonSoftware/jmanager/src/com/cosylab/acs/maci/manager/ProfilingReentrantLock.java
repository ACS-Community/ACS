package com.cosylab.acs.maci.manager;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;


/**
 * A Lock that internally delegates to a {@link ReentrantLock} and profiles the time 
 * it takes a thread to acquire the lock, and the time for which the lock is held. 
 * When a thread calls {@link #unlock()} then these times are printed, together with the code location 
 * which used the lock.
 * <p>
 * This lock can be used as an alternative to "synchronized" blocks, when we want to detect lock contention.
 * It may also be used as an alternative to some other {@link Lock}, although at the moment only methods {@link Lock#lock()}
 * and {@link Lock#unlock()} are implemented. For the other Lock methods we throw {@link UnsupportedOperationException},
 * but it should be easy enough to also implement them when needed.
 * <p>
 * This class is added to jmanager for work on http://jira.alma.cl/browse/COMP-6488.
 * Later it should be moved to module jacsutil.
 * 
 * @author hsommer
 * @since ACS 10.2
 */
public class ProfilingReentrantLock implements Lock
{
	private final ReentrantLock delegate;

	private final String lockName;
	
	public static final String PROFILING_ENABLED_PROPERTYNAME = "acs.enableReentrantLockProfiling";
	public static final boolean isProfilingEnabled = Boolean.getBoolean(PROFILING_ENABLED_PROPERTYNAME);
	
	public ProfilingReentrantLock(String lockName) {
		this.lockName = lockName;
		this.delegate = new ReentrantLock();
	}
	
	/**
	 * Factory method that encapsulates the checking of property {@link #PROFILING_ENABLED_PROPERTYNAME}
	 * and the corresponding creation of either a ProfilingReentrantLock or a ReentrantLock.
	 * <p>
	 * We return only the base type "Lock" to keep the implementation of ProfilingReentrantLock simple for the time being. 
	 * Some methods of ReentrantLock are final, so that delegation anyway is easier than subtyping. 
	 * If we really need the additional methods, we could refactor ProfilingReentrantLock to inherit from ReentrantLock 
	 * and change the return type to ReentrantLock.
	 * <p>
	 * @TODO: Perhaps also pass a Logger and the "fair" boolean.
	 * @param lockName Only used for the profiling variant, for log output.
	 */
	public static Lock createReentrantLock(String lockName) {
		if (isProfilingEnabled) {
			return new ProfilingReentrantLock(lockName);
		}
		else {
			return new ReentrantLock();
		}
	}
	
	private static class LockInfo {
		public LockInfo(String codeId, long timeAttempted, int queueLength) {
			this.codeId = codeId;
			this.timeAttempted = timeAttempted;
			this.queueLength = queueLength;
		}
		final String codeId;
		final long timeAttempted;
		final int queueLength;
		long timeLocked;
		long timeUnlocked;
	}
	
	/**
	 * key = thread id
	 */
	private Map<Long, LockInfo> lockInfoMap = new HashMap<Long, LockInfo>();
	
	@Override
	public void lock() {
		if (!delegate.isHeldByCurrentThread()) {
			String codeId = createCodeId(1);
			long t0 = System.currentTimeMillis();
			int queueLength = delegate.getQueueLength();
			LockInfo lockInfo = new LockInfo(codeId, t0, queueLength);
			lockInfoMap.put(Thread.currentThread().getId(), lockInfo);
			delegate.lock();
			lockInfo.timeLocked = System.currentTimeMillis();
		} 
		else {
			// no profiling for nested locking, just lock
			delegate.lock();
		}
	}

	@Override
	public void lockInterruptibly() throws InterruptedException {
		throw new UnsupportedOperationException("lockInterruptibly not yet supported");
//		delegate.lockInterruptibly();
	}

	@Override
	public boolean tryLock() {
		throw new UnsupportedOperationException("tryLock not yet supported");
//		return delegate.tryLock();
	}

	@Override
	public boolean tryLock(long timeout, TimeUnit unit) throws InterruptedException {
		throw new UnsupportedOperationException("tryLock not yet supported");
//		return delegate.tryLock(timeout, unit);
	}

	@Override
	public void unlock() {
		delegate.unlock();
		if (!delegate.isHeldByCurrentThread()) {
			LockInfo lockInfo = lockInfoMap.remove(Thread.currentThread().getId());
			lockInfo.timeUnlocked = System.currentTimeMillis();
			printProfilingMessage(lockInfo, Thread.currentThread().getName());
		}
		else {
			// no profiling for nested unlocking when we still hold the lock
		}
	}

	@Override
	public Condition newCondition() {
		return delegate.newCondition();
	}

	
	/**
	 * @return The file name and line of file from where this method gets called, skipping an extra <code>skipLevels</code> from the call stack.
	 *         <code>null</code> if no call stack can be retrieved.
	 */
	String createCodeId(int skipLevels) {
		String ret = null;
		StackTraceElement[] stackTraceElems = (new Throwable()).getStackTrace();
		if (stackTraceElems.length >= skipLevels + 1) {
			ret = stackTraceElems[skipLevels + 1].getFileName() + "/" + stackTraceElems[skipLevels + 1].getLineNumber();
		} else {
			ret = "<unknown>";
		}
		return ret;
	}
	
	private void printProfilingMessage(LockInfo lockInfo, String threadName) {
		System.out.println("Profiling for lock '" + lockName + "' used in code='" + lockInfo.codeId + "', thread='" + threadName 
				+ "': queueLength=" + lockInfo.queueLength + ", acquiredMillis=" + (lockInfo.timeLocked - lockInfo.timeAttempted) + ", heldMillis=" + (lockInfo.timeUnlocked - lockInfo.timeLocked));
	}

}
