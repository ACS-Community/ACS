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
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import si.ijs.maci.ComponentSpec;

import alma.ACS.ACSComponentOperations;
import alma.acs.container.ContainerServices;

/**
 * <code>ConcurrentComponentAccessUtil</code> extends {@link ComponentAccessUtil} allowing to 
 * instantiate and release components concurrently.
 * <P>
 * The class owns a thread pool to execute the tasks concurrently. The size of the thred pool
 * can be set in the constructor otherwise the default value of {@link ConcurrentComponentAccessUtil#defaultThreadNumber}
 * is used.
 * <P>
 * This class allows to release components in parallel by executing each release in one of the 
 * thread of the pool.
 * <P>
 * The activation of a component requests to specify the class of the components to activate and a return mechanism
 * for the instantiated component.
 * <BR> To limit the complexity of the method, the parallel instantiation of more then one dynamic components
 * is limited to components of the same type. 
 * <P>
 * One parallel operation can be performed in a certain time. If the user
 * want to issue a new parallel command, he has to wait until the current one 
 * terminates.
 * <P>
 * Life cycle: 
 * <UL>
 * 	<LI>start() must be executed before using methods from this class
 *  <LI>stop() must be executed when the object is not needed anymore
 * </UL>
 *  
 * @author acaproni
 *
 */
public class ConcurrentComponentAccessUtil extends ComponentAccessUtil {
	
	/**
	 * The class to get a component in a thread of the pool
	 * 
	 * @author acaproni
	 *
	 */
	private class ComponentActivator<V extends ACSComponentOperations> implements Callable<V> {
		
		
		
		@Override
		public V call() throws Exception {
			V comp=null;
			comp=ConcurrentComponentAccessUtil.super.getDynamicComponent(compSpec, idlOpInterface);
			logger.info("ComponentActivator got component "+compSpec.component_name);
			return comp;
		}

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
	}
	
	/**
	 * The class to release a component in a thread
	 * 
	 * @author acaproni
	 *
	 */
	private class ComponentDeactivator implements Callable<Void> {
		
		/**
		 * The name of the component to release
		 */
		private final String name;
		
		/**
		 * Constructor
		 * 
		 * @param componentName The name of the component to release
		 */
		public ComponentDeactivator(String componentName) {
			this.name=componentName;
		}

		@Override
		public Void call() {
			ConcurrentComponentAccessUtil.this.releaseComponent(name, true);
			countDown.countDown();
			return null;
		}
	}
	
	/**
	 * The thread pool to add statistics to the FutureTask.
	 * 
	 * @author acaproni
	 *
	 */
	public class BenchmarkTreadPoolExecutor extends ThreadPoolExecutor {

		public BenchmarkTreadPoolExecutor(int corePoolSize,
				int maximumPoolSize, long keepAliveTime, TimeUnit unit,
				BlockingQueue<Runnable> workQueue,
				RejectedExecutionHandler handler) {
			super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, handler);
		}

		public BenchmarkTreadPoolExecutor(int corePoolSize,
				int maximumPoolSize, long keepAliveTime, TimeUnit unit,
				BlockingQueue<Runnable> workQueue, ThreadFactory threadFactory,
				RejectedExecutionHandler handler) {
			super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue,
					threadFactory, handler);
		}

		public BenchmarkTreadPoolExecutor(int corePoolSize,
				int maximumPoolSize, long keepAliveTime, TimeUnit unit,
				BlockingQueue<Runnable> workQueue, ThreadFactory threadFactory) {
			super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue,
					threadFactory);
		}

		public BenchmarkTreadPoolExecutor(int corePoolSize,
				int maximumPoolSize, long keepAliveTime, TimeUnit unit,
				BlockingQueue<Runnable> workQueue) {
			super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue);
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
	 * 
	 * @author acaproni
	 *
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
	private static final int defaultThreadNumber= 100;

	/**
	 * Number of threads to concurrently start/release components
	 */
	private final int totThreads;
	
	/**
	 * The ACS thread factory
	 */
	private final ThreadFactory acsThreadFactory;
	
	/**
	 * The executor service to concurrently activate the components
	 */
	private final BenchmarkTreadPoolExecutor executor;
	
	/**
	 * The {@link CountDownLatch} to wait until all the tasks terminate.
	 */
	private volatile CountDownLatch countDown=new CountDownLatch(0);
	
	/**
	 * Constructor.
	 * 
	 * @param contSrv The {@link ContainerServices}
	 * @param threadPoolSize The number of threads in the thread pool
	 */
	public ConcurrentComponentAccessUtil(ContainerServices contSrv, int threadPoolSize) {
		super(contSrv);
		totThreads=threadPoolSize;
		acsThreadFactory= this.contSrv.getThreadFactory();
		executor = new BenchmarkTreadPoolExecutor(totThreads, totThreads,
                0L, TimeUnit.MILLISECONDS,
                new LinkedBlockingQueue<Runnable>(),
                acsThreadFactory);
		// Start the threads
		int n=executor.prestartAllCoreThreads();
		logger.info(n+" threads started");

	}
	
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
	 * Life cycle method: this method must be called before using methods from this class 
	 */
	public void start() {
		
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
	 * Release a component in a dedicated thread of the pool.
	 * 
	 * @param name The name of the component to deactivate
	 * @return
	 */
	public Future<Void> releaseComponentConcurrent(String name) {
		ComponentDeactivator deactivator=new ComponentDeactivator(name);
		return executor.submit(deactivator);
	}
	
	/**
	 * Concurrently release the components with the passed names
	 * 
	 * @param compNames The names of the components to release
	 * @param if <code>true</code> the methods wait for all the threads to terminate before returning
	 * 		otherwise return immediately
	 * 
	 * TODO: limit the waiting time 
	 */
	@Override
	public void releaseComponents(Collection<String> compNames, boolean waitCompsTermination) {
		try {
			countDown.await(); 
		} catch (InterruptedException ie) {}
		countDown= new CountDownLatch(compNames.size());
		for (String compName : compNames) {
			ComponentDeactivator deactivator=new ComponentDeactivator(compName);
			executor.submit(deactivator);
		}
		if (waitCompsTermination) {
			logger.info("Awaiting termination of "+countDown.getCount()+" threads");
			try {
				countDown.await(); 
			} catch (InterruptedException ie) {}
		}
	}
	
	/**
	 * Concurrently release all the components.
	 * 
	 * @param if <code>true</code> the methods wait for all the threads to terminate before returning
	 * 		otherwise return immediately
	 */
	@Override
	public void releaseAllComponents(boolean waitCompsTermination) {
		List<String> compNames;
		synchronized (compName2Comp) {
			compNames = new ArrayList<String>(compName2Comp.keySet()); // to avoid ConcurrentModificationException
		}
		releaseComponents(compNames,waitCompsTermination);
	}
	
	/**
	 * The parallel version of {@link ComponentAccessUtil#getDynamicComponent(ComponentSpec, Class)}
	 * get the dynamic component in a thread of the pool.
	 * 
	 * @param compSpec The spec to start the dynamic component
	 * @param idlOpInterface The idl interface of the component
	 * @return the future task to be able to get the result of the activation of the component
	 */
	public <T extends ACSComponentOperations> Future<T> getDynamicComponentConcurrent(
			ComponentSpec compSpec,
			Class<T> idlOpInterface) {
		ComponentActivator<T> activator = new ComponentActivator<T>(compSpec, idlOpInterface);
		//FutureTask<T> futureTask = new FutureTask<T>(activator);
		return executor.submit(activator);
		//return futureTask;
	}

}
