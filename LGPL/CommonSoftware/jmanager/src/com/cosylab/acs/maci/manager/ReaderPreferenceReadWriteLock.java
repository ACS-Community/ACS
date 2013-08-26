package com.cosylab.acs.maci.manager;

/**
 * A ReadWriteLock that prefers waiting writers over waiting readers when there
 * is contention. This class is adapted from the versions described in CPJ,
 * improving on the ones there a bit by segregating reader and writer wait
 * queues, which is typically more efficient.
 * <p>
 * The locks are <em>NOT</em> reentrant. In particular, even though it may
 * appear to usually work OK, a thread holding a read lock should not attempt to
 * re-acquire it. Doing so risks lockouts when there are also waiting writers.
 * <p>[<a
 * href="http://gee.cs.oswego.edu/dl/classes/EDU/oswego/cs/dl/util/concurrent/intro.html">
 * Introduction to this package. </a>]
 */

public class ReaderPreferenceReadWriteLock {

	protected long activeReaders_ = 0;

	protected Thread activeWriter_ = null;

	protected long waitingReaders_ = 0;

	protected long waitingWriters_ = 0;

	protected final ReaderLock readerLock_ = new ReaderLock();

	protected final WriterLock writerLock_ = new WriterLock();

	public Sync writeLock() {
		return writerLock_;
	}

	public Sync readLock() {
		return readerLock_;
	}

	/*
	 * A bunch of small synchronized methods are needed to allow communication
	 * from the Lock objects back to this object, that serves as controller
	 */

	protected synchronized void cancelledWaitingReader() {
		--waitingReaders_;
	}

	protected synchronized void cancelledWaitingWriter() {
		--waitingWriters_;
	}

	protected boolean allowReader() {
		return activeWriter_ == null;
		// writer preference code: return activeWriter_ == null &&
		// waitingWriters_ == 0;
	}

	protected synchronized boolean startRead() {
		boolean allowRead = allowReader();
		if (allowRead)
			++activeReaders_;
		return allowRead;
	}

	protected synchronized boolean startWrite() {

		// The allowWrite expression cannot be modified without
		// also changing startWrite, so is hard-wired

		boolean allowWrite = (activeWriter_ == null && activeReaders_ == 0);
		if (allowWrite)
			activeWriter_ = Thread.currentThread();
		return allowWrite;
	}

	/*
	 * Each of these variants is needed to maintain atomicity of wait counts
	 * during wait loops. They could be made faster by manually inlining each
	 * other. We hope that compilers do this for us though.
	 */

	protected synchronized boolean startReadFromNewReader() {
		boolean pass = startRead();
		if (!pass)
			++waitingReaders_;
		return pass;
	}

	protected synchronized boolean startWriteFromNewWriter() {
		boolean pass = startWrite();
		if (!pass)
			++waitingWriters_;
		return pass;
	}

	protected synchronized boolean startReadFromWaitingReader() {
		boolean pass = startRead();
		if (pass)
			--waitingReaders_;
		return pass;
	}

	protected synchronized boolean startWriteFromWaitingWriter() {
		boolean pass = startWrite();
		if (pass)
			--waitingWriters_;
		return pass;
	}

	/**
	 * Called upon termination of a read. Returns the object to signal to wake
	 * up a waiter, or null if no such
	 */
	protected synchronized Signaller endRead() {
		if (--activeReaders_ == 0 && waitingWriters_ > 0)
			return writerLock_;
		else
			return null;
	}

	/**
	 * Called upon termination of a write. Returns the object to signal to wake
	 * up a waiter, or null if no such
	 */
	protected synchronized Signaller endWrite() {
		activeWriter_ = null;
		if (waitingReaders_ > 0 && allowReader())
			return readerLock_;
		else if (waitingWriters_ > 0)
			return writerLock_;
		else
			return null;
	}

	/**
	 * Reader and Writer requests are maintained in two different wait sets, by
	 * two different objects. These objects do not know whether the wait sets
	 * need notification since they don't know preference rules. So, each
	 * supports a method that can be selected by main controlling object to
	 * perform the notifications. This base class simplifies mechanics.
	 */

	protected abstract class Signaller { // base for ReaderLock and
											// WriterLock
		abstract void signalWaiters();
	}

	/**
	 * Main interface for locks, gates, and conditions.
	 * <p>
	 * Sync objects isolate waiting and notification for particular logical
	 * states, resource availability, events, and the like that are shared
	 * across multiple threads. Use of Syncs sometimes (but by no means always)
	 * adds flexibility and efficiency compared to the use of plain java monitor
	 * methods and locking, and are sometimes (but by no means always) simpler
	 * to program with.
	 * <p>
	 * 
	 * Most Syncs are intended to be used primarily (although not exclusively)
	 * in before/after constructions such as:
	 * 
	 * <pre>
	 * class X {
	 * 	Sync gate;
	 * 
	 * 	// ...
	 * 
	 * 	public void m() { 
	 *      try {
	 *        gate.acquire();  // block until condition holds
	 *        try {
	 *          // ... method body
	 *        }
	 *        finally {
	 *          gate.release()
	 *        }
	 *      }
	 *      catch (InterruptedException ex) {
	 *        // ... evasive action
	 *      }
	 *    }
	 * 
	 * 	public void m2(Sync cond) { // use supplied condition
	 *      try {
	 *        if (cond.attempt(10)) {         // try the condition for 10 ms
	 *          try {
	 *            // ... method body
	 *          }
	 *          finally {
	 *            cond.release()
	 *          }
	 *        }
	 *      }
	 *      catch (InterruptedException ex) {
	 *        // ... evasive action
	 *      }
	 *    }
	 * }
	 * </pre>
	 * 
	 * Syncs may be used in somewhat tedious but more flexible replacements for
	 * built-in Java synchronized blocks. For example:
	 * 
	 * <pre>
	 * class HandSynched {
	 * 	private double state_ = 0.0;
	 * 
	 * 	private final Sync lock; // use lock type supplied in constructor
	 * 
	 * 	public HandSynched(Sync l) {
	 * 		lock = l;
	 * 	}
	 * 
	 * 	public void changeState(double d) {
	 * 		try {
	 * 			lock.acquire();
	 * 			try {
	 * 				state_ = updateFunction(d);
	 * 			} finally {
	 * 				lock.release();
	 * 			}
	 * 		} catch (InterruptedException ex) {
	 * 		}
	 * 	}
	 * 
	 * 	public double getState() {
	 * 		double d = 0.0;
	 * 		try {
	 * 			lock.acquire();
	 * 			try {
	 * 				d = accessFunction(state_);
	 * 			} finally {
	 * 				lock.release();
	 * 			}
	 * 		} catch (InterruptedException ex) {
	 * 		}
	 * 		return d;
	 * 	}
	 * 
	 * 	private double updateFunction(double d) { ... }
	 * 
	 * 	private double accessFunction(double d) { ... }
	 * }
	 * </pre>
	 * 
	 * If you have a lot of such methods, and they take a common form, you can
	 * standardize this using wrappers. Some of these wrappers are standardized
	 * in LockedExecutor, but you can make others. For example:
	 * 
	 * <pre>
	 * class HandSynchedV2 {
	 * 	private double state_ = 0.0;
	 * 
	 * 	private final Sync lock; // use lock type supplied in constructor
	 * 
	 * 	public HandSynchedV2(Sync l) {
	 * 		lock = l;
	 * 	}
	 * 
	 * 	protected void runSafely(Runnable r) {
	 * 		try {
	 * 			lock.acquire();
	 * 			try {
	 * 				r.run();
	 * 			} finally {
	 * 				lock.release();
	 * 			}
	 * 		} catch (InterruptedException ex) { // propagate without throwing
	 * 			Thread.currentThread().interrupt();
	 * 		}
	 * 	}
	 * 
	 * 	public void changeState(double d) {
	 * 		runSafely(new Runnable() {
	 * 			public void run() {
	 * 				state_ = updateFunction(d);
	 * 			}
	 * 		});
	 * 	}
	 * 	// ...
	 * }
	 * </pre>
	 * 
	 * <p>
	 * One reason to bother with such constructions is to use deadlock- avoiding
	 * back-offs when dealing with locks involving multiple objects. For
	 * example, here is a Cell class that uses attempt to back-off and retry if
	 * two Cells are trying to swap values with each other at the same time.
	 * 
	 * <pre>
	 *  class Cell {
	 *    long value;
	 *    Sync lock = ... // some sync implementation class
	 *    void swapValue(Cell other) {
	 *      for (;;) { 
	 *        try {
	 *          lock.acquire();
	 *          try {
	 *            if (other.lock.attempt(100)) {
	 *              try { 
	 *                long t = value; 
	 *                value = other.value;
	 *                other.value = t;
	 *                return;
	 *              }
	 *              finally { other.lock.release(); }
	 *            }
	 *          }
	 *          finally { lock.release(); }
	 *        } 
	 *        catch (InterruptedException ex) { return; }
	 *      }
	 *    }
	 *  }
	 * </pre>
	 * 
	 * <p>
	 * Here is an even fancier version, that uses lock re-ordering upon
	 * conflict:
	 * 
	 * <pre>
	 *  class Cell { 
	 *    long value;
	 *    Sync lock = ...;
	 *    private static boolean trySwap(Cell a, Cell b) {
	 *      a.lock.acquire();
	 *      try {
	 *        if (!b.lock.attempt(0)) 
	 *          return false;
	 *        try { 
	 *          long t = a.value;
	 *          a.value = b.value;
	 *          b.value = t;
	 *          return true;
	 *        }
	 *        finally { other.lock.release(); }
	 *      }
	 *      finally { lock.release(); }
	 *      return false;
	 *    }
	 * 
	 *   void swapValue(Cell other) {
	 *     try {
	 *       while (!trySwap(this, other) &amp;&amp;
	 *             !tryswap(other, this)) 
	 *         Thread.sleep(1);
	 *     }
	 *     catch (InterruptedException ex) { return; }
	 *   }
	 * }
	 * </pre>
	 * 
	 * <p>
	 * Interruptions are in general handled as early as possible. Normally,
	 * InterruptionExceptions are thrown in acquire and attempt(msec) if
	 * interruption is detected upon entry to the method, as well as in any
	 * later context surrounding waits. However, interruption status is ignored
	 * in release();
	 * <p>
	 * Timed versions of attempt report failure via return value. If so desired,
	 * you can transform such constructions to use exception throws via
	 * 
	 * <pre>
	 * if (!c.attempt(timeval))
	 * 	throw new TimeoutException(timeval);
	 * </pre>
	 * 
	 * <p>
	 * The TimoutSync wrapper class can be used to automate such usages.
	 * <p>
	 * All time values are expressed in milliseconds as longs, which have a
	 * maximum value of Long.MAX_VALUE, or almost 300,000 centuries. It is not
	 * known whether JVMs actually deal correctly with such extreme values. For
	 * convenience, some useful time values are defined as static constants.
	 * <p>
	 * All implementations of the three Sync methods guarantee to somehow employ
	 * Java <code>synchronized</code> methods or blocks, and so entail the
	 * memory operations described in JLS chapter 17 which ensure that variables
	 * are loaded and flushed within before/after constructions.
	 * <p>
	 * Syncs may also be used in spinlock constructions. Although it is normally
	 * best to just use acquire(), various forms of busy waits can be
	 * implemented. For a simple example (but one that would probably never be
	 * preferable to using acquire()):
	 * 
	 * <pre>
	 *  class X {
	 *    Sync lock = ...
	 *    void spinUntilAcquired() throws InterruptedException {
	 *      // Two phase. 
	 *      // First spin without pausing.
	 *      int purespins = 10; 
	 *      for (int i = 0; i &lt; purespins; ++i) {
	 *        if (lock.attempt(0))
	 *          return true;
	 *      }
	 *      // Second phase - use timed waits
	 *      long waitTime = 1; // 1 millisecond
	 *      for (;;) {
	 *        if (lock.attempt(waitTime))
	 *          return true;
	 *        else 
	 *          waitTime = waitTime * 3 / 2 + 1; // increase 50% 
	 *      }
	 *    }
	 *  }
	 * </pre>
	 * 
	 * <p>
	 * In addition pure synchronization control, Syncs may be useful in any
	 * context requiring before/after methods. For example, you can use an
	 * ObservableSync (perhaps as part of a LayeredSync) in order to obtain
	 * callbacks before and after each method invocation for a given class.
	 * <p>
	 * 
	 * <p>[<a
	 * href="http://gee.cs.oswego.edu/dl/classes/EDU/oswego/cs/dl/util/concurrent/intro.html">
	 * Introduction to this package. </a>]
	 */

	public interface Sync {

		/**
		 * Wait (possibly forever) until successful passage. Fail only upon
		 * interuption. Interruptions always result in `clean' failures. On
		 * failure, you can be sure that it has not been acquired, and that no
		 * corresponding release should be performed. Conversely, a normal
		 * return guarantees that the acquire was successful.
		 */

		public void lock();

		/**
		 * Wait at most msecs to pass; report whether passed.
		 * <p>
		 * The method has best-effort semantics: The msecs bound cannot be
		 * guaranteed to be a precise upper bound on wait time in Java.
		 * Implementations generally can only attempt to return as soon as
		 * possible after the specified bound. Also, timers in Java do not stop
		 * during garbage collection, so timeouts can occur just because a GC
		 * intervened. So, msecs arguments should be used in a coarse-grained
		 * manner. Further, implementations cannot always guarantee that this
		 * method will return at all without blocking indefinitely when used in
		 * unintended ways. For example, deadlocks may be encountered when
		 * called in an unintended context.
		 * <p>
		 * 
		 * @param msecs
		 *            the number of milleseconds to wait. An argument less than
		 *            or equal to zero means not to wait at all. However, this
		 *            may still require access to a synchronization lock, which
		 *            can impose unbounded delay if there is a lot of contention
		 *            among threads.
		 * @return true if acquired
		 */

		public boolean attempt(long msecs) throws InterruptedException;

		/**
		 * Potentially enable others to pass.
		 * <p>
		 * Because release does not raise exceptions, it can be used in
		 * `finally' clauses without requiring extra embedded try/catch blocks.
		 * But keep in mind that as with any java method, implementations may
		 * still throw unchecked exceptions such as Error or
		 * NullPointerException when faced with uncontinuable errors. However,
		 * these should normally only be caught by higher-level error handlers.
		 */

		public void unlock();

		/** One second, in milliseconds; convenient as a time-out value * */
		public static final long ONE_SECOND = 1000;

		/** One minute, in milliseconds; convenient as a time-out value * */
		public static final long ONE_MINUTE = 60 * ONE_SECOND;

		/** One hour, in milliseconds; convenient as a time-out value * */
		public static final long ONE_HOUR = 60 * ONE_MINUTE;

		/** One day, in milliseconds; convenient as a time-out value * */
		public static final long ONE_DAY = 24 * ONE_HOUR;

		/** One week, in milliseconds; convenient as a time-out value * */
		public static final long ONE_WEEK = 7 * ONE_DAY;

		/** One year in milliseconds; convenient as a time-out value * */
		// Not that it matters, but there is some variation across
		// standard sources about value at msec precision.
		// The value used is the same as in java.util.GregorianCalendar
		public static final long ONE_YEAR = (long) (365.2425 * ONE_DAY);

		/** One century in milliseconds; convenient as a time-out value * */
		public static final long ONE_CENTURY = 100 * ONE_YEAR;

	}

	protected class ReaderLock extends Signaller implements Sync {

		public void lock() {
			InterruptedException ie = null;
			synchronized (this) {
				if (!startReadFromNewReader()) {
					for (;;) {
						try {
							ReaderLock.this.wait();
							if (startReadFromWaitingReader())
								return;
						} catch (InterruptedException ex) {
							cancelledWaitingReader();
							ie = ex;
							break;
						}
					}
				}
			}
			if (ie != null) {
				// fall through outside synch on interrupt.
				// This notification is not really needed here,
				// but may be in plausible subclasses
				writerLock_.signalWaiters();
			}
		}

		public void unlock() {
			Signaller s = endRead();
			if (s != null)
				s.signalWaiters();
		}

		synchronized void signalWaiters() {
			ReaderLock.this.notifyAll();
		}

		public boolean attempt(long msecs) throws InterruptedException {
			if (Thread.interrupted())
				throw new InterruptedException();
			InterruptedException ie = null;
			synchronized (this) {
				if (msecs <= 0)
					return startRead();
				else if (startReadFromNewReader())
					return true;
				else {
					long waitTime = msecs;
					long start = System.currentTimeMillis();
					for (;;) {
						try {
							ReaderLock.this.wait(waitTime);
						} catch (InterruptedException ex) {
							cancelledWaitingReader();
							ie = ex;
							break;
						}
						if (startReadFromWaitingReader())
							return true;
						else {
							waitTime = msecs
									- (System.currentTimeMillis() - start);
							if (waitTime <= 0) {
								cancelledWaitingReader();
								break;
							}
						}
					}
				}
			}
			// safeguard on interrupt or timeout:
			writerLock_.signalWaiters();
			if (ie != null)
				throw ie;
			else
				return false; // timed out
		}

	}

	protected class WriterLock extends Signaller implements Sync {

		public void lock() {
			InterruptedException ie = null;
			synchronized (this) {
				if (!startWriteFromNewWriter()) {
					for (;;) {
						try {
							WriterLock.this.wait();
							if (startWriteFromWaitingWriter())
								return;
						} catch (InterruptedException ex) {
							cancelledWaitingWriter();
							WriterLock.this.notify();
							ie = ex;
							break;
						}
					}
				}
			}
			if (ie != null) {
				// Fall through outside synch on interrupt.
				// On exception, we may need to signal readers.
				// It is not worth checking here whether it is strictly
				// necessary.
				readerLock_.signalWaiters();
			}
		}

		public void unlock() {
			Signaller s = endWrite();
			if (s != null)
				s.signalWaiters();
		}

		synchronized void signalWaiters() {
			WriterLock.this.notify();
		}

		public boolean attempt(long msecs) throws InterruptedException {
			if (Thread.interrupted())
				throw new InterruptedException();
			InterruptedException ie = null;
			synchronized (this) {
				if (msecs <= 0)
					return startWrite();
				else if (startWriteFromNewWriter())
					return true;
				else {
					long waitTime = msecs;
					long start = System.currentTimeMillis();
					for (;;) {
						try {
							WriterLock.this.wait(waitTime);
						} catch (InterruptedException ex) {
							cancelledWaitingWriter();
							WriterLock.this.notify();
							ie = ex;
							break;
						}
						if (startWriteFromWaitingWriter())
							return true;
						else {
							waitTime = msecs
									- (System.currentTimeMillis() - start);
							if (waitTime <= 0) {
								cancelledWaitingWriter();
								WriterLock.this.notify();
								break;
							}
						}
					}
				}
			}

			readerLock_.signalWaiters();
			if (ie != null)
				throw ie;
			else
				return false; // timed out
		}

	}

}
