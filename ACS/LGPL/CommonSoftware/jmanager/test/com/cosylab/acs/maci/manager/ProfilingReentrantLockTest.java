package com.cosylab.acs.maci.manager;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

public class ProfilingReentrantLockTest extends TestCase
{
	private ProfilingReentrantLock lock;
	
	protected void setUp() throws Exception {
		super.setUp();
		lock = new ProfilingReentrantLock("Lock-" + getName());
		System.out.println("\n------------ " + getName() + " --------------");
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testCodeId() {
		assertEquals("Current line expected.", "ProfilingReentrantLockTest.java/23", lock.createCodeId(0));
		assertEquals("Line from other method expected.", "ProfilingReentrantLockTest.java/32", createCodeIdWithExtraCallStackElement(0));
		assertEquals("Current line expected.", "ProfilingReentrantLockTest.java/25", createCodeIdWithExtraCallStackElement(1));
	}
	
	/**
	 * This is part of {@link #testCodeId()}.
	 */
	private String createCodeIdWithExtraCallStackElement(int skipLevels) {
		return lock.createCodeId(skipLevels);
	}
	
	public void testSimple() throws Exception {
		System.out.println("About to acquire the lock.");
		lock.lock();
		System.out.println("Acquired the lock.");
		try {
			Thread.sleep(100);
		} finally {
			lock.unlock();
			System.out.println("Unlocked the lock.");
		}
	}
	
	/**
	 * Acquires the lock from multiple threads, each of which then hold it for 100 ms. 
	 * The first thread should have acquiredMillis ~0, the other acquiredMillis ~ n * 100 ms.
	 * All threads should have "heldMillis ~100".
	 * This test is currently only useful for manual verification of the logged profiling data.
	 */
	public void testWithMultipleThreads() throws Exception {
		final int threadNumber = 5;
		final CountDownLatch sync = new CountDownLatch(threadNumber);
		
		Runnable r = new Runnable() {
			@Override
			public void run() {
				lock.lock();
				try {
					try {
						Thread.sleep(100);
					} catch (InterruptedException ex) {
						ex.printStackTrace();
					}
				} finally {
					lock.unlock();
					sync.countDown();
				}
			}
		};
		
		for (int i = 0; i < threadNumber; i++) {
			(new Thread(r)).start();
			// sleep a bit because otherwise the queue length best-effort output (from ReentrantLock#getQueueLength) 
			// does not take the new thread into account. 
			Thread.sleep(1);
		}
		
		sync.await(1, TimeUnit.SECONDS);
	}
	
	public void testNestedLocking() {
		final int nestingLevels = 5;
		for (int i = 0; i < nestingLevels; i++) {
			lock.lock();
		}
		for (int i = 0; i < nestingLevels; i++) {
			lock.unlock();
		}
		System.out.println("Nested locking/unlocking should have produced exactly one line of profiling output.");
	}
}
