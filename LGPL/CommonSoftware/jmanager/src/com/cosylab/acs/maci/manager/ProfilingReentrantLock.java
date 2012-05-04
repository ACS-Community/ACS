package com.cosylab.acs.maci.manager;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;


/**
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
	
	private static class LockInfo {
		public LockInfo(String codeId, long timeAttempted) {
			this.codeId = codeId;
			this.timeAttempted = timeAttempted;
		}
		final String codeId;
		final long timeAttempted;
		long timeLocked;
		long timeUnlocked;
	}
	
	/**
	 * key = thread id
	 */
	private Map<Long, LockInfo> lockInfoMap = new HashMap<Long, LockInfo>();
	
	@Override
	public void lock() {
		if (!lockInfoMap.containsKey(Thread.currentThread().getId())) {
			String codeId = createCodeId(1);
			long t0 = System.currentTimeMillis();
			LockInfo lockInfo = new LockInfo(codeId, t0);
			lockInfoMap.put(Thread.currentThread().getId(), lockInfo);
			delegate.lock();
			lockInfo.timeLocked = System.currentTimeMillis();
		} 
		else {
			// no profiling for nested locking
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
			// no profiling for nested unlocking
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
		System.out.println("Profiling for lock '" + lockName + "', code='" + lockInfo.codeId + "', thread='" + threadName 
				+ "': acquiredMillis=" + (lockInfo.timeLocked - lockInfo.timeAttempted) + ", heldMillis=" + (lockInfo.timeUnlocked - lockInfo.timeLocked));
	}

}
