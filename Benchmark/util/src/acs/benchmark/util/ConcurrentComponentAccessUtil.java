/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package acs.benchmark.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;

import si.ijs.maci.ComponentSpec;

import alma.ACS.ACSComponentOperations;
import alma.acs.container.ContainerServices;

/**
 * <code>ConcurrentComponentAccessUtil</code> extends {@link ComponentAccessUtil}, allowing to 
 * instantiate and release components concurrently.
 * <P>
 * The class owns a thread pool to execute the tasks concurrently. The size of the thread pool
 * can be set in the constructor; otherwise a default value of {@link ConcurrentComponentAccessUtil#defaultThreadNumber}
 * is used.
 * <P>
 * This class allows to release components in parallel by executing each release in one of the 
 * threads of the pool.
 * <P>
 * Life cycle: 
 * <UL>
 * 	<LI>start() must be executed before using methods from this class
 *  <LI>stop() must be executed when the object is not needed anymore.
 * </UL>
 *  
 * @author acaproni
 */
public class ConcurrentComponentAccessUtil extends ComponentAccessUtil {
	
	/**
	 * The class to get a component in a thread of the pool
	 */
	private class ComponentActivator<V extends ACSComponentOperations> implements Callable<V> {
		
		/**
		 * The ComponentSpec of the dynamic component to get
		 */
		private final ComponentSpec compSpec;
		
		/**
		 * The class of the idl interface
		 */
		private final Class<V> idlOpInterface;
		
		/**
		 * Constructor
		 * 
		 * @param compSpec The ComponentSpec of the dynamic component to get
		 * @param idlOpInterface The class of the idl interface
		 */
		public ComponentActivator(ComponentSpec compSpec, Class<V> idlOpInterface) {
			this.compSpec=compSpec;
			this.idlOpInterface=idlOpInterface;
		}
		
		@Override
		public V call() throws Exception {
			V comp = getDynamicComponent(compSpec, idlOpInterface);
			logger.info("ComponentActivator got component " + compSpec.component_name);
			return comp;
		}
	}
	
	/**
	 * The class to release a component in a thread
	 */
	private class ComponentDeactivator implements Callable<Void> {
		
		private final String compName;
		
		/**
		 * @param componentName
		 * @param sync may be null
		 */
		public ComponentDeactivator(String componentName) {
			this.compName=componentName;
		}

		@Override
		public Void call() {
			// here, from a separate thread, we do a *synchronous* comp release
			ConcurrentComponentAccessUtil.this.releaseComponent(compName, true);
			return null;
		}
	}
	
	
	/**
	 * The thread pool to add statistics to the FutureTask.
	 */
	public class BenchmarkTreadPoolExecutor extends ThreadPoolExecutor {

		public BenchmarkTreadPoolExecutor(int corePoolSize,
				int maximumPoolSize, long keepAliveTime, TimeUnit unit,
				BlockingQueue<Runnable> workQueue, ThreadFactory threadFactory) {
			super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory);
		}

		/**
		 * The {@link FutureTask} is of type {@link InstrumentedFutureTask}
		 */
		@Override
		protected <T> RunnableFuture<T> newTaskFor(Runnable runnable, T value) {
			return new InstrumentedFutureTask<T>(runnable, value);
		}

		/**
		 * The {@link FutureTask} is of type {@link InstrumentedFutureTask}
		 */
		@Override
		protected <T> RunnableFuture<T> newTaskFor(Callable<T> callable) {
			return new InstrumentedFutureTask<T>(callable);
		}

		/**
		 * Set the start execution time
		 */
		@Override
		protected void beforeExecute(Thread t, Runnable r) {
			super.beforeExecute(t, r);
			if (r instanceof InstrumentedFutureTask) {
				InstrumentedFutureTask ift=(InstrumentedFutureTask)r;
				ift.startTime=System.currentTimeMillis();
			}
		}

		/**
		 * Set the end execution time
		 */
		@Override
		protected void afterExecute(Runnable r, Throwable t) {
			if (r instanceof InstrumentedFutureTask) {
				InstrumentedFutureTask ift=(InstrumentedFutureTask)r;
				ift.endTime=System.currentTimeMillis();
			}
			super.afterExecute(r, t);
		}
	}

	
	/**
	 * An instrumented future task contains fields useful for getting statistics.
	 * @param <T>
	 */
	public class InstrumentedFutureTask<T> extends FutureTask<T> {
		
		/**
		 * The start time of the execution of the task
		 */
		private volatile long startTime=0;
		
		/**
		 * The end time of the execution of the task
		 */
		private volatile long endTime=0;

		public InstrumentedFutureTask(Callable<T> callable) {
			super(callable);
		}

		public InstrumentedFutureTask(Runnable runnable, T result) {
			super(runnable, result);
		}

		/**
		 * @return the startTime
		 */
		public long getStartTime() {
			return startTime;
		}

		/**
		 * @return the endTime
		 */
		public long getEndTime() {
			return endTime;
		}
		
		/**
		 * Return the execution time calculated from the start and the end time.
		 * <P>
		 * The execution time is available only when the task has been executed.
		 * 
		 * @return The execution
		 * @throws Exception If the execution time is read before the task terminates 
		 */
		public long getExecutionTime() throws Exception {
			if (isDone()) {
				return endTime-startTime;
			}
			throw new Exception("Execution time not yet available");
		}
		
	}
	
	/**
	 * The default number of threads.
	 * <P> 
	 * The number of threads is set in the constructor. The default is used if such
	 * a number is not specified in the constructor.
	 */
	private static final int defaultThreadNumber = 100;

	/**
	 * Number of threads to concurrently start/release components
	 */
	private final int totThreads;
	
	/**
	 * The executor service to concurrently activate the components
	 */
	private final BenchmarkTreadPoolExecutor executor;

	
	/**
	 * Constructor.
	 * <P>
	 * Build the ConcurrentComponentAccessUtil with a default number of threads
	 * 
	 * @param contSrv The {@link ContainerServices}
	 */
	public ConcurrentComponentAccessUtil(ContainerServices contSrv) {
		this(contSrv, defaultThreadNumber);
	}
	
	/**
	 * Constructor.
	 * 
	 * @param contSrv The {@link ContainerServices}
	 * @param threadPoolSize The number of threads in the thread pool
	 */
	public ConcurrentComponentAccessUtil(ContainerServices contSrv, int threadPoolSize) {
		this(contSrv, threadPoolSize, false);
	}
	
	/**
	 * Constructor.
	 * 
	 * @param contSrv The {@link ContainerServices}
	 * @param threadPoolSize The number of threads in the thread pool
	 * @param prestartCoreThreads 
	 */
	public ConcurrentComponentAccessUtil(ContainerServices contSrv, int threadPoolSize, boolean prestartCoreThreads) {
		super(contSrv);
		totThreads = threadPoolSize;
		executor = new BenchmarkTreadPoolExecutor(totThreads, totThreads, 1, TimeUnit.MINUTES,
									new LinkedBlockingQueue<Runnable>(), contSrv.getThreadFactory());
		executor.allowCoreThreadTimeOut(true);
		if (prestartCoreThreads) {
			int n = executor.prestartAllCoreThreads();
			logger.info(n+" threads pre-started");
		}
	}
	
	/**
	 * Life cycle method: this method must be called before using methods from this class 
	 */
	public void start() {
		// TODO: Say if it's intended to have this empty, but keep the method for future use.
	}
	
	/**
	 * Life cycle method: this method must be called when the object is not needed
	 * anymore
	 */
	public void stop() {
		executor.shutdown();
		try {
			if (!executor.awaitTermination(5, TimeUnit.MINUTES)) {
				executor.shutdownNow();
			}
		} catch (InterruptedException ie) {
			executor.shutdownNow();
		}
	}
	
	/**
	 * Releases a component in a dedicated thread of the pool.
	 * <p>
	 * Note that <code>ComponentAccessUtil#releaseComponent(compName, waitForCompRelease=false)</code>
	 * is very similar in the sense that it is also asynchronous, making an async call to the manager
	 * instead of spawning a new thread already here in the client. 
	 * The main difference though is that in this method we can measure the time it takes to release a component,
	 * which is important for performance tests.
	 * 
	 * @param name The name of the component to deactivate
	 * @return handle to sync up with the finishing of the component release.
	 */
	public Future<Void> releaseComponentConcurrent(String name) {
		ComponentDeactivator deactivator = new ComponentDeactivator(name);
		return executor.submit(deactivator);
	}
	
	/**
	 * Concurrently releases the components with the passed names.
	 * <p>
	 * This method overrides the base class version, bringing in a thread pool for parallel 
	 * component release, instead of sequentially releaseing every component. 
	 * Mostly makes sense for <code>waitCompsTermination == true</code>.
	 * <p>
	 * To avoid hanging tests, a generous timeout of 30 minutes gets applied.
	 * 
	 * @param compNames
	 *            The names of the components to release
	 * @param waitCompsTermination
	 *            if <code>true</code> the method waits for all the threads to terminate before returning, otherwise
	 *            returns immediately
	 */
	@Override
	public void releaseComponents(Collection<String> compNames, boolean waitCompsTermination) {
		List<Future<Void>> compReleaseFutures = new ArrayList<Future<Void>>();
		
		for (String compName : compNames) {
			compReleaseFutures.add(releaseComponentConcurrent(compName));
		}
		if (waitCompsTermination) {
			logger.info("Awaiting termination of " + compReleaseFutures.size() + " component release requests (with a 30 min timeout).");
			
			boolean printErrorLogs = true;
			for (Future<Void> future : compReleaseFutures) {
				try {
					// The following get() call will block until the component release has finished,
					// either normally or with an error, or if a timeout occurred.
					future.get(30, TimeUnit.MINUTES);
					
				} catch (InterruptedException ex) {
					logger.log(Level.WARNING, "Async component release interrupted.", ex);
				} catch (CancellationException ex) {
					logger.log(Level.WARNING, "Async component release cancelled.", ex);
				} catch (ExecutionException ex) {
					logger.log(Level.WARNING, "Async component release failed with exception ", ex.getCause());
				} catch (TimeoutException ex) {
					if (printErrorLogs) {
						logger.log(Level.WARNING, "Async component release timed out.", ex);
						printErrorLogs = false;
					}
				}
			}
		}
	}
	
	/**
	 * The parallel version of {@link ComponentAccessUtil#getDynamicComponent(ComponentSpec, Class)}
	 * gets the dynamic component in a thread of the pool.
	 * 
	 * @param compSpec The spec to start the dynamic component
	 * @param idlOpInterface The idl interface of the component
	 * @return the future task to be able to get the result of the activation of the component
	 */
	public <T extends ACSComponentOperations> Future<T> getDynamicComponentConcurrent(
			ComponentSpec compSpec,
			Class<T> idlOpInterface) {
		ComponentActivator<T> activator = new ComponentActivator<T>(compSpec, idlOpInterface);
		return executor.submit(activator);
	}

}
