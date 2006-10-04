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

	public final int delaySeconds;
	
	private final ScheduledExecutorService scheduler;
	private final Random random;
	private final Logger logger;
	private final ThreadFactory threadFactory;
	
    SubsysResourceMonitor(Logger logger, ThreadFactory threadFactory) {
        this(logger, threadFactory, 10);
    }
    
    SubsysResourceMonitor(Logger logger, ThreadFactory threadFactory, int delaySeconds) {
        this.delaySeconds = delaySeconds;
		this.logger = logger;
		this.threadFactory = threadFactory;
		scheduler = Executors.newScheduledThreadPool(1, threadFactory);
		random = new Random(System.currentTimeMillis());
	}
	
	/**
	 * Starts to periodically monitor the resource that <code>checkRunner</code> contains.
	 * The interval length is given by {@link #delaySeconds}. 
	 * In order to randomize check times even if many resources get signed up for monitoring one after the other, 
	 * the initial delay before the first check is run is taken randomly between 1 second and the period time. 
	 * @param checkRunner
	 */
	<T> void monitorResource(ResourceChecker<T> checker, ResourceErrorHandler<T> err) {
        SubsysResourceMonitor.ResourceCheckRunner<T> checkRunner = new SubsysResourceMonitor.ResourceCheckRunner<T>(checker, err, logger, threadFactory);
        scheduler.scheduleAtFixedRate(checkRunner, random.nextInt(delaySeconds) + 1, delaySeconds, TimeUnit.SECONDS);
	}
	
	
	private static class ResourceCheckRunner<T> implements Runnable {
		
		public static final int callTimeoutSeconds = 10;
		private static ExecutorService threadPool;
		
		private final ResourceChecker<T> resourceChecker;
		private final ResourceErrorHandler<T> err;
        private final Logger logger;
    	private final ThreadFactory threadFactory;
		
		ResourceCheckRunner(ResourceChecker<T> resourceChecker, ResourceErrorHandler<T> err, Logger logger, ThreadFactory threadFactory) {
			this.resourceChecker = resourceChecker;
			this.err = err;
            this.logger = logger;
    		this.threadFactory = threadFactory;
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
			Future future = getThreadPool().submit(timeoutCaller);
			try {
				future.get(callTimeoutSeconds, TimeUnit.SECONDS);
			} catch (TimeoutException e) {
				try {
					err.resourceUnreachable(resourceChecker.getResource()); 
				} catch (Throwable thr) {
                    logger.log(Level.WARNING, "Failed to propagate unavailability of resource '" + resourceChecker.getResourceName() + "' to the error handler!", e);
				} 
			} catch (Exception e) { // InterruptedException, ExecutionException
                // the asynchronous call "resourceChecker.checkState()" failed, but not because of a timeout.
                logger.log(Level.WARNING, "Failed to check the status of resource '" + resourceChecker.getResourceName() + "'.", e);
			}
		}
		
		protected synchronized ExecutorService getThreadPool() {
			if (threadPool == null) {
				threadPool =  Executors.newCachedThreadPool(threadFactory);
			}
			return threadPool;
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
         */
        abstract void resourceUnreachable(T resource);
        
        /**
         * Called when {@link SubsysResourceMonitor}
         * @param resource
         */
        abstract void badState(T resource, String stateName);
    }

}
