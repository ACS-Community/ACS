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

package alma.ACS.MasterComponentImpl;

import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACS.ACSComponent;
import alma.ACS.ComponentStates;

/**
 * Monitor for any kind of resource whose state a subsystem master component 
 * wants to observe.
 * <p> 
 * This class is used by <code>MasterComponentImplBase</code>
 * and should not be exposed to subsystem developers directly.
 * However, the contained interfaces {@link SubsysResourceMonitor.ResourceChecker}
 * and {@link SubsysResourceMonitor.ResourceErrorHandler} can be implemented
 * by master components to supply customized checkers and error handlers
 * for various resource types and error recovery strategies.
 * 
 * @author hsommer
 * @since ACS 6.0
 */
public class SubsysResourceMonitor {

	/**
	 * The delay between two monitoring calls to resource.
	 */
	public final int delaySeconds;
	
	private final ScheduledExecutorService scheduler;
	private final Random random;
	private final Logger logger;
	private final ExecutorService monitorCallThreadPool;
	private boolean isShuttingDown;
	private boolean isShutDown;
	
    /**
     * Ctor, with default value for <code>delaySeconds = 10</code>.
     * @see #SubsysResourceMonitor(Logger, ThreadFactory, int)
     */
    SubsysResourceMonitor(Logger logger, ThreadFactory threadFactory) {
        this(logger, threadFactory, 10);
    }
    
    /**
     * @param logger  Logger to be used by this object
     * @param threadFactory  all threads for scheduling and calling the resources will be created by this factory.  
     * @param delaySeconds  the delay between two monitoring calls to a resource.
     */
    SubsysResourceMonitor(Logger logger, ThreadFactory threadFactory, int delaySeconds) {
        this.delaySeconds = delaySeconds;
		this.logger = logger;
		monitorCallThreadPool = Executors.newCachedThreadPool(threadFactory);
		isShuttingDown = false;
		isShutDown = false;
		scheduler = Executors.newScheduledThreadPool(1, threadFactory);
		random = new Random(System.currentTimeMillis());
	}
	
	/**
	 * Starts to periodically monitor the resource that <code>checker</code> contains.
	 * The monitor interval length is given by {@link #delaySeconds}.
	 * <p> 
	 * In order to randomize check times even if many resources get signed up for monitoring one after the other, 
	 * the initial delay before the first check is run is taken randomly between 1 second and the period time. 
	 * @param checker
	 * @throws IllegalStateException if {@link #destroy(long, TimeUnit)} has been called.
	 */
	<T> void monitorResource(ResourceChecker<T> checker, ResourceErrorHandler<T> err) {
		if (isShuttingDown) {
			throw new IllegalStateException("Resource monitor is already destroyed.");
		}
		SubsysResourceMonitor.ResourceCheckRunner<T> checkRunner = new SubsysResourceMonitor.ResourceCheckRunner<T>(checker, err, logger, monitorCallThreadPool);
		scheduler.scheduleAtFixedRate(checkRunner, random.nextInt(delaySeconds) + 1, delaySeconds, TimeUnit.SECONDS);
	}
	
	/**
	 * Cancels monitoring of all resources and leaves this object in an unusable state.
	 * <p>
	 * This method is synchronized so that a second call can return immediately, but only if the first call has finished.
	 * @param timeout the timeout for waiting that the internal threads, queues, jobs etc are freed, or <code>0</code> to not wait at all.
	 * @param unit
	 * @throws InterruptedException
	 */
	synchronized void destroy(long timeout, TimeUnit unit) throws InterruptedException {
		isShuttingDown = true;
		if (isShutDown) {
			return;
		}
		monitorCallThreadPool.shutdownNow();
		scheduler.shutdownNow();
		if (timeout > 0) {
			long timeoutMillis = unit.toMillis(timeout); // to safely divide by 2 in case it was given as 1 second
			monitorCallThreadPool.awaitTermination(timeoutMillis/2, TimeUnit.MILLISECONDS);
			scheduler.awaitTermination(timeoutMillis/2, TimeUnit.MILLISECONDS);
		} else {
			isShutDown = scheduler.isTerminated();
		}
	}
	
	static class ResourceCheckRunner<T> implements Runnable {
		
		public static final int callTimeoutSeconds = 10;
		
		private final ResourceChecker<T> resourceChecker;
		private final ResourceErrorHandler<T> err;
        private final Logger logger;
    	private final ExecutorService threadPool;
		
		ResourceCheckRunner(ResourceChecker<T> resourceChecker, ResourceErrorHandler<T> err, Logger logger, ExecutorService threadPool) {
			this.resourceChecker = resourceChecker;
			this.err = err;
            this.logger = logger;
    		this.threadPool = threadPool;
		}
		
		public void run() {
			// run the check in a thread from the thread pool
			Runnable timeoutCaller = new Runnable() {
				public void run() {
					String badState = resourceChecker.checkState();
					if (badState != null) {
						try {
							err.badState(resourceChecker.getResource(), badState);                            
						} catch (Exception e) {
							logger.log(Level.WARNING, "Failed to propagate offending state of resource '" + resourceChecker.getResourceName() + "' to the error handler!", e);
						}
					}
				}
			};
			Future future = threadPool.submit(timeoutCaller);
			long timeBeforeCall = System.currentTimeMillis();
			Throwable callError = null;
			boolean wasTimedOut = false;
			try {
				future.get(callTimeoutSeconds, TimeUnit.SECONDS);
			} catch (TimeoutException e) {
				wasTimedOut = true;
			} catch (InterruptedException e) {
				if (System.currentTimeMillis() - callTimeoutSeconds >= timeBeforeCall) {
					// most likely a timeout occured. 
					// TODO: check why we did not get a TimeoutException
					wasTimedOut = true;
				}
				else {
					// some other strange InterruptedException
					callError = e;
				}
// TODO: check how CORBA timeout behaves, and whether a corba exception would be wrapped as an ExecutionException
//			} catch (???CORBATimeoutEx??? e) {
//				wasTimedOut = true;
//			}
			} catch (Throwable thr) { // ExecutionException or unexpected
				callError = thr;
			}
			if (wasTimedOut) {
				try {
					// notify the error handler
					err.resourceUnreachable(resourceChecker.getResource()); 
				} catch (Throwable thr) {
                    logger.log(Level.WARNING, "Failed to propagate unavailability of resource '" + resourceChecker.getResourceName() + "' to the error handler!", thr);
				} 				
			}			
			else if (callError != null) {
                // the asynchronous call "resourceChecker.checkState()" failed, but not because of a timeout.
                logger.log(Level.WARNING, "Failed to check the status of resource '" + resourceChecker.getResourceName() + "'.", callError);				
			}
		}		
	}
	
	/**
     * Encapsulates the details of a particular resource,
     * so that all resources (components, offshoots, databases, ...) can
     * be monitored in the same way.
	 * @param <T> The type of the resource object, for example an ACS component type
     * @see ComponentChecker
	 */
	public static interface ResourceChecker<T> {
		/**
		 * This method tries to connect to the monitored resource and check its state if applicable.
		 * If this call does not return within a certain time, then resource unavailability will be assumed.
		 * @return name of an offending state or status if one is found, otherwise <code>null</code>.  
		 */
		public String checkState();
		
		public T getResource();
        
        /**
         * Returns a name that identifies the resource. 
         * The name is used for log messages. It should be unique within a master component,
         * although currently no use is made of uniqueness. 
         */
        public String getResourceName();
	}

	
	/**
     * Implementation of <code>ResourceChecker</code> for ACS components. 
     * Calls {@link ACSComponentOperations#componentState()} to determine
     * responsiveness and state of the component resource.
	 */
	public static class ComponentChecker<T extends ACSComponent> implements ResourceChecker<T> {
		
		private final T comp;
        
        /**
         * We keep the component name separately because later when there are problems 
         * it may no longer be possible to obtain it remotely.
         */
        private String compName;
		
		ComponentChecker(T comp) {
			this.comp = comp;
            this.compName = comp.name(); // todo: timeout and exception
		}

		public String checkState() {
            ComponentStates state = comp.componentState();
			if (state.value() != ComponentStates.COMPSTATE_OPERATIONAL.value()) {
                return state.toString();
            }
            else {
                return null;
            }
		}
		
		public T getResource() {
			return comp;
		}
        
        public String getResourceName() {
            return compName;
        }
	}
    
    /**
     * Error handler that gets notified when a monitored resource
     * becomes unavailable or degraded.
     * <p> 
     * By implementing a custom error handler, a master component
     * can attempt first to cure the situation, or go into ERROR state
     * by calling <code>doTransition(SubsystemStateEvent.SUBSYSEVENT_ERROR);</code>.
     */
    public interface ResourceErrorHandler<T> {

        /**
         * Called when the resource could not be reached at all.
         * The resource object is passed to allow using one handler for many resources.
         * <p>
         * The return value controls controls whether monitoring of this resource will be stopped:
         * <ol>
         * <li><code>true</code> means that the error handler decided that this resource is unreachable beyond repair, 
         *     and that no further monitoring calls should be made.
         *     This can avoid potential problems with an increasing number of hanging calls and eventually stopping the respective threads.
         * <li><code>false</code> means that monitoring calls should continue.
         * </ol>
         */
        abstract boolean resourceUnreachable(T resource);
        
        /**
         * Called when {@link SubsysResourceMonitor}
         * @param resource
         */
        abstract void badState(T resource, String stateName);
    }

}
