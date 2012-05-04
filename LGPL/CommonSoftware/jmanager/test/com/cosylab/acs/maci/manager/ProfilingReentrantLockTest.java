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
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testCodeId() {
		assertEquals("Current line expected.", "ProfilingReentrantLockTest.java/22", lock.createCodeId(0));
		assertEquals("Line from other method expected.", "ProfilingReentrantLockTest.java/31", createCodeIdWithExtraCallStackElement(0));
		assertEquals("Current line expected.", "ProfilingReentrantLockTest.java/24", createCodeIdWithExtraCallStackElement(1));
	}
	
	/**
	 * This is part of {@link #testCodeId()}.
	 */
	private String createCodeIdWithExtraCallStackElement(int skipLevels) {
		return lock.createCodeId(skipLevels);
	}
	
	public void testSimple() throws Exception {
		lock.lock();
		try {
			Thread.sleep(100);
		} finally {
			lock.unlock();
		}
	}
	
	/**
	 * One of the threads should have acquiredMillis ~0, the other acquiredMillis ~100.
	 * Both threads should have heldMillis ~100.
	 * Currently only useful for manual verification.
	 */
	public void testWithTwoThreads() throws Exception {
		final CountDownLatch sync = new CountDownLatch(2);
		
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
		
		(new Thread(r)).start();
		(new Thread(r)).start();
		
		sync.await(1, TimeUnit.SECONDS);
	}
}
