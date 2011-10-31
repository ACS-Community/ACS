/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.container;

import java.lang.Thread.UncaughtExceptionHandler;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;

/**
 * Thread factory that remembers all threads it creates.
 * Method <code>cleanUp</code> allows killing all threads.  
 * <p>
 * A <code>ThreadGroup</code> is used to deal with all threads at a time. Its name is that given in the constructor.
 * Even though Josh Bloch tells us in "Effective Java", Item 53, that thread groups are almost useless,
 * we like the UncaughtExceptionHandler which JDK 1.4 only offers through ThreadGroups.
 * TODO: revisit this in JDK 1.5 where Thread itself has this handler.
 * 
 * @author hsommer
 * created Mar 8, 2005 1:27:38 PM
 */
public class CleaningDaemonThreadFactory implements ThreadFactory
{
	/**
	 * If this property is set to <code>true</code>, the creators of new threads will be identified through a INFO log showing the calling stack trace, 
	 * which helps for example in finding out who created a thread but later fails to clean it up 
	 * (see logs "Forcibly terminating surviving thread ...").
	 */
	public static final String LOG_THREAD_CREATION_CALLSTACK_PROPERTYNAME = "alma.acs.threadfactory.trace_creators";
	
	/**
	 * Cache for value of {@link #LOG_THREAD_CREATION_CALLSTACK_PROPERTYNAME}.
	 */
	protected boolean logThreadCreationCallstack = Boolean.getBoolean(LOG_THREAD_CREATION_CALLSTACK_PROPERTYNAME);
	/**
	 * We keep track of our threads also outside of the thread group.
	 */
	private final List<Thread> threadList = new ArrayList<Thread>();

	/**
	 * Running index used for thread names.
	 */
	private final AtomicInteger threadNumber = new AtomicInteger(1);

	private final Logger logger;
	private final String name;
	private final String ownerName;
	
	private LoggingThreadGroup group;

	private ClassLoader newThreadContextCL;


	
	/**
	 * Normal constructor.
	 * @param name  the name of the {@link ThreadGroup} to which all threads created by this factory will belong.
	 * @param logger  the logger to be used by this class
	 */
	public CleaningDaemonThreadFactory(String name, Logger logger) {
		this(name, logger, "User");
	}

	/**
	 * Special constructor, e.g. when used internally by the container.
	 * @param name  the name of the {@link ThreadGroup} to which all threads created by this factory will belong.
	 * @param logger  the logger to be used by this class
	 * @param ownerName  Can show up in log messages such as warnings by {@link LoggingThreadGroup#uncaughtException(Thread, Throwable)}. 
	 */
	public CleaningDaemonThreadFactory(String name, Logger logger, String ownerName) {
		this.logger = logger;
		this.name = name;
		this.ownerName = ownerName;
	}

	/**
	 * Creates a new daemon thread that is part of the same factory thread group 
	 * as all other threads created by this method.
	 * The thread's name will be that of the group, with an integer value appended.
	 * 
	 * @see java.util.concurrent.ThreadFactory#newThread(java.lang.Runnable)
	 */
	public Thread newThread(Runnable command) {
		ensureGroupCreated();
		
		String threadName = group.getName() + "-" + threadNumber.getAndIncrement();
		Thread t = new Thread(group, command, threadName);
		t.setDaemon(true);
		if (newThreadContextCL != null) {
			t.setContextClassLoader(newThreadContextCL);
		}
		synchronized (threadList) {
			threadList.add(t);
		}
		if (logThreadCreationCallstack) {
			try {
				throw new Exception();
			} catch (Exception ex) {
				StackTraceElement[] stack = ex.getStackTrace();
				StringBuilder str = new StringBuilder();
				for (int i = 1; i < stack.length; i++) { // skip 0-th element which represents this class itself
					str.append(stack[i].toString());
					if (i < stack.length - 1) {
						str.append(" <- ");
					}
				}
				String msg = "Created thread '" + threadName + "'. Call stack: " + str;
				logger.log(AcsLogLevel.INFO, msg);
			}
		}
		return t;
	}

	private synchronized void ensureGroupCreated() {
		if (group == null) {
			group = new LoggingThreadGroup(name, logger, ownerName);
			group.setMaxPriority(Thread.currentThread().getPriority());
		}
	}
	
	void setNewThreadContextClassLoader(ClassLoader cl) {
		if (cl != null) {
			newThreadContextCL = cl;
		}
	}

	/**
	 * Gets a copy of the list of all threads created by this factory up to this call.
	 * This method should only be used for testing, but not in operational code.
	 */
	public List<Thread> _getAllThreadsCreated() {
		synchronized (threadList) {
			return new ArrayList<Thread>(threadList);
		}
	}

	/**
	 * Kills running threads via {@link Thread#interrupt()} or {@link Thread#stop()}, see code comments about the two cases.
	 * Should be called by the container or similar classes when all threads 
	 * created by this factory supposedly have terminated anyway thanks to smart applications. 
	 * The safety concerns which led to the deprecation of the stop method thus don't seem to apply here.
	 */
	public synchronized void cleanUp() {
		if (group == null) {
			return;
		}
		group.setShuttingDown();
		synchronized (threadList) {
			for (Thread t : threadList) {
				try {
					if (t.isAlive()) {
						logger.warning("Forcibly terminating surviving thread " + t.getName());
					}
					// @TODO HSO 2008-04: now that jbaci uses an external ThreadFactory, which is typically supplied by container services,
					//       we got jbaci test failures as long as stop() is called (see COMP-2362).
					//       We must check if we fix jbaci and go back to thread.stop, or stay with thread.interrupt.
					t.interrupt();
//					t.stop();
					
					// The following sleep of 1 ms is a concession to unit testing of this class,
					// so that log messages produced during the interrupt() call will appear in the order of the calls. 
					try {
						Thread.sleep(1);
					} catch (InterruptedException ex) {
						// no harm in operations, but unit tests may fail
					}
				} catch (RuntimeException e) {
					logger.finer("exception while stopping thread '" + t.getName() + "': " + e.toString());
				}
			}
			threadList.clear();
		}
		try {
			// if there are threads in the group which have never started, destroy() will fail
			if (!group.isDestroyed() && group.activeCount() == 0) {
				group.destroy();
			}
		} catch (Exception e) {
			logger.finer("unexpectedly failed to destroy thread group " + group.getName() + e.toString());
		}
		// in case this factory gets used again, we should build a new thread group
		group = null;
	}

	private static class LoggingThreadGroup extends ThreadGroup
	{
		private final Logger logger;
		private volatile boolean shuttingDown = false;
		private final String ownerName;

		LoggingThreadGroup(String name, Logger logger, String ownerName) {
			super(name);
			this.logger = logger;
			this.ownerName = ownerName;
		}

		void setShuttingDown() {
			shuttingDown = true;
		}

		/**
		 * Called by the JVM if any of the threads in this thread group terminates with an error.
		 * Logs a warning to the logger provided in the ctor.
		 * <p>
		 * The error <code>ThreadDeath</code> is even logged during expected thread lifetime,
		 * because user threads are not supposed to be terminated through the deprecated <code>stop()</code> method
		 * during their normal operation. <br>
		 * During shutdown, exceptions are not logged, because the killing of surviving user threads
		 * is logged already in the <code>cleanUp</code> method. 
		 * <p>
		 * @TODO: since JDK 1.5 an {@link UncaughtExceptionHandler} can be attached to each thread,
		 * which may be an alternative to using a thread group for this purpose.
		 * 
		 * @see java.lang.ThreadGroup#uncaughtException(java.lang.Thread, java.lang.Throwable)
		 */
		public void uncaughtException(Thread t, Throwable e) {
			if (!shuttingDown) {
				logger.log(Level.WARNING, ownerName + " thread '" + t.getName() + "' terminated with error ", e);
			}
			// ThreadDeath must move on to really let the thread die
			if (e instanceof ThreadDeath) {
				super.uncaughtException(t, e);
			}
		}

	}
}
