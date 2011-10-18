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

import java.util.HashSet;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACS.ACSComponent;
import alma.ACS.ACSComponentOperations;
import alma.ACS.ComponentStates;
import alma.ACS.PingableResourceOperations;
import alma.acs.logging.AcsLogLevel;

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
	public final int defaultDelaySeconds;
	
	private final ScheduledThreadPoolExecutor scheduler;
	private final Random random;
	private final Logger logger;
	private final ExecutorService monitorCallThreadPool;
    private final Set<ResourceCheckRunner> resourceRunners;
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
     * @param defaultDelaySeconds  the default delay between finishing one monitoring call and starting the next call to the same resource.
     *             Values &lt;= 1 will be changed to == 1. This default can be overridden in method 
     *             {@link #monitorResource(alma.ACS.MasterComponentImpl.SubsysResourceMonitor.ResourceChecker, alma.ACS.MasterComponentImpl.SubsysResourceMonitor.ResourceErrorHandler, int)}.
     */
    SubsysResourceMonitor(Logger logger, ThreadFactory threadFactory, int defaultDelaySeconds) {
        this.defaultDelaySeconds = Math.max(1, defaultDelaySeconds);
		this.logger = logger;
		monitorCallThreadPool = Executors.newCachedThreadPool(threadFactory);
        resourceRunners = new HashSet<ResourceCheckRunner>();
		isShuttingDown = false;
		isShutDown = false;
		scheduler = new ScheduledThreadPoolExecutor(1, threadFactory);
		random = new Random(System.currentTimeMillis());
	}
	
    
    /**
     * Same as {@link #monitorResource(alma.ACS.MasterComponentImpl.SubsysResourceMonitor.ResourceChecker, alma.ACS.MasterComponentImpl.SubsysResourceMonitor.ResourceErrorHandler, int)},
     * but with the default delay instead of a delay parameter.  
     */
    <T> void monitorResource(ResourceChecker<T> checker, ResourceErrorHandler<T> err) {
    	monitorResource(checker, err, defaultDelaySeconds);
    }
    
    
	/**
	 * Starts to periodically monitor the resource that <code>checker</code> contains.
	 * The monitor delay length is given by {@link #delaySeconds}.
	 * <p> 
	 * In order to randomize check times even if many resources get signed up for monitoring one after the other, 
	 * the initial delay before the first check is run is taken randomly between 1 second and the period time. 
	 * @param checker
	 * @param delaySeconds determines the delay between ending a check call and starting the next one. If &lt;1, then the default is used.
	 * @throws IllegalStateException if {@link #destroy(long, TimeUnit)} has been called.
     * @throws IllegalArgumentException if any of the arguments are <code>null</code><code>, 
     *         or if checker.getResource()</code> or <code>checker.getResourceName()</code> returns <code>null</code>.
	 */
	<T> void monitorResource(ResourceChecker<T> checker, ResourceErrorHandler<T> err, int delaySeconds) {
		if (isShuttingDown) {
			throw new IllegalStateException("Resource monitor is already destroyed.");
		}        
        if (checker == null || checker.getResource() == null || checker.getResourceName() == null) {
            throw new IllegalArgumentException("ResourceChecker must be non-null and must deliver non-null resource and resource name.");
        }
        if (err == null) {
            throw new IllegalArgumentException("ResourceErrorHandler must not be null");
        }
        if (delaySeconds < 1) {
        	delaySeconds = defaultDelaySeconds;
        }
        synchronized (resourceRunners) {
            for (ResourceCheckRunner<T> otherRunner : resourceRunners) {
                ResourceChecker<T> otherChecker = otherRunner.getResourceChecker();
                Object otherResource = otherChecker.getResource();
                String otherResourceName = otherChecker.getResourceName();
                // @TODO: enforce that no 2 resources can have the same name (important for #stopResourceMonitoring)
                if (otherResource == checker.getResource()) {
                    String msg = "Resource '" + checker.getResourceName() + "' is already being monitored. ";
                    if (!otherResourceName.equals(checker.getResourceName())) {
                        msg += "However it was known under the different name '" + otherResourceName + "'! ";
                    }
                    msg += "Will re-schedule the monitoring now.";
                    logger.info(msg);
                    Future<?> future = otherRunner.getScheduleFuture();
                    future.cancel(true);
                    resourceRunners.remove(otherRunner);
                    break;
                }
            }
        	SubsysResourceMonitor.ResourceCheckRunner<T> checkRunner = new SubsysResourceMonitor.ResourceCheckRunner<T>(checker, err, logger, monitorCallThreadPool);
        	int initialDelaySeconds = random.nextInt(delaySeconds);
        	Future<?> future = scheduler.scheduleWithFixedDelay(checkRunner, initialDelaySeconds, delaySeconds, TimeUnit.SECONDS);
            checkRunner.setScheduleFuture(future);
            resourceRunners.add(checkRunner);
//            logger.info("Will monitor resource '" + checker.getResourceName() + "'.");
        }
	}
	
	/**
	 * For testing only.
     * Returns the number of actively running threads, plus the number of tasks in the queue that is used for scheduling the monitoring calls.
	 */
	public int getNumberOfMonitorTasks() {
//logger.info("*** scheduler active count =" + scheduler.getActiveCount() + "; scheduler queue size = " + scheduler.getQueue().size());		
		return ( scheduler.getActiveCount() + scheduler.getQueue().size() );
	}
	
    /**
     * For testing only!
     * <p>
     * Gets the <code>ResourceCheckRunner</code> that is used for running the monitor checks 
     * that use the given <code>ResourceChecker</code>.
     * @return the corresponding  <code>ResourceCheckRunner</code>, or <code>null</code> if no runner matches.
     */
    SubsysResourceMonitor.ResourceCheckRunner getResourceCheckRunner(ResourceChecker checker) {
        for (ResourceCheckRunner runner : resourceRunners) {
            if (runner.getResourceChecker() == checker) {
                return runner;
            }
        }
        return null;
    }
    
    /**
     * Suspends monitoring of all resources until {@link #resume()} is called.
     * Currently running monitor calls do not get stopped, only future calls are prevented.
     * <p> 
     * Note that the monitoring queue remains intact, while the monitor call itself becomes a no-op. 
     */
    public void suspend() {
    	for (ResourceCheckRunner runner : resourceRunners) {
			runner.suspend();
		}
    }
    
    /**
     * Resumes monitoring of all resources.
     * <p>
     * This method has no effect if monitoring has not been previously suspended.
     * @see #suspend()
     */
    public void resume() {
    	for (ResourceCheckRunner runner : resourceRunners) {
			runner.resume();
		}
    }
    
    
    /**
     * Stops monitoring a given resource.
     * @param resourceName unique resource name
     */
    public void stopMonitoring(String resourceName) {
    	synchronized (resourceRunners) {
	    	for (ResourceCheckRunner runner : resourceRunners) {
	    		if (resourceName.equals(runner.getResourceChecker().getResourceName())) {    			 
	    			runner.suspend(); // to invalidate an ongoing monitor call
	    			runner.getScheduleFuture().cancel(false);
	    			resourceRunners.remove(runner);
	    			// don't break here, just in case we have more than 1 resource of that name (which of course should not happen)
	    		}
	    	}
    	}
    }
    
    /**
     * Stops monitoring all resources.
     */
    public void stopMonitoringAll() {
    	synchronized (resourceRunners) {
	    	for (ResourceCheckRunner runner : resourceRunners) {
    			runner.suspend(); // to invalidate an ongoing monitor call
    			runner.getScheduleFuture().cancel(false);
	    	}
			resourceRunners.clear();
    	}    	
    }
    
    
	/**
	 * Cancels monitoring of all resources and leaves this object in an unusable state.
	 * <p>
	 * Impl note: this method is synchronized so that a second call can return immediately, but only if the first call has finished.
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
	
    
    
    
    
    
	/**
     * The <code>Runnable</code> used for the scheduling queue of <code>SubsysResourceMonitor</code>.
	 */
	static class ResourceCheckRunner<T> implements Runnable {
		
		private volatile int callTimeoutSeconds = 10;
		private final ResourceChecker<T> resourceChecker;
		private final ResourceErrorHandler<T> err;
        private final Logger logger;
    	private final ExecutorService threadPool;
        private Future<?> scheduleFuture;
        private volatile boolean isSuspended;
        private volatile boolean lastCheckSucceeded;
		
		ResourceCheckRunner(ResourceChecker<T> resourceChecker, ResourceErrorHandler<T> err, Logger logger, ExecutorService threadPool) {
			this.resourceChecker = resourceChecker;
			this.err = err;
            this.logger = logger;
    		this.threadPool = threadPool;
    		isSuspended = false;
    		lastCheckSucceeded = true;
		}

		/**
		 * Sets the future that was obtained from the scheduler when starting the monitoring job. The future object can
		 * be used to cancel the execution of this check runner. Unfortunately this object is not yet available at
		 * construction time, that's why we have this separate setter method.
		 */
		void setScheduleFuture(Future<?> scheduleFuture) {
			this.scheduleFuture = scheduleFuture;
		}

		Future<?> getScheduleFuture() {
			return scheduleFuture;
		}

		/**
		 * To be called from run()
		 */
		private void notifyRecovery() {
			err.resourceRecovered(resourceChecker.getResource());
		}
		
		public void run() {
			
			if (isSuspended) {
				return;
			}
			
			// run the check in a thread from the thread pool
			class CheckStateCallerWithTimeout implements Runnable {
				private volatile boolean timeout = false;
				public void run() {
					String badState = resourceChecker.checkState();
					if (badState != null && !timeout) { // we don't want to report a bad state after a timeout, since the timeout has already been reported
						lastCheckSucceeded = false;
						try {
							err.badState(resourceChecker.getResource(), badState);
						} catch (Exception e) {
							logger.log(Level.WARNING, "Failed to propagate offending state of resource '" + resourceChecker.getResourceName() + "' to the error handler!", e);
						}
					}
					else if (!lastCheckSucceeded) {
						// all is well, but previous check failed
						notifyRecovery();
						lastCheckSucceeded = true;
					}
				}
				void cancel() {
					// it's cleaner to let this thread die and simply ignore its results after a timeout
					timeout = true;
				}
			}
			CheckStateCallerWithTimeout checkStateCallerWithTimeout = new CheckStateCallerWithTimeout();
			Future future = threadPool.submit(checkStateCallerWithTimeout);
			long timeBeforeCall = System.currentTimeMillis();
			Throwable callError = null;
			boolean wasTimedOut = false;
			String timedOutDescription = null; // introduced to debug http://jira.alma.cl/browse/AIV-5983
			try {
				future.get(callTimeoutSeconds, TimeUnit.SECONDS);
			} catch (TimeoutException e) {
				wasTimedOut = true;
				timedOutDescription = "TimeoutException after " + (System.currentTimeMillis() - timeBeforeCall) + " ms.";
			} catch (InterruptedException e) {
				if (System.currentTimeMillis() - callTimeoutSeconds >= timeBeforeCall) {
					// most likely a timeout occurred. 
					// TODO: check why we did not get a TimeoutException
					wasTimedOut = true;
					timedOutDescription = "InterruptedException after " + (System.currentTimeMillis() - timeBeforeCall) + " ms; interpreting as timeout.";
				}
				else {
					// some other strange InterruptedException. 
					callError = e;
					timedOutDescription = "InterruptedException after " + (System.currentTimeMillis() - timeBeforeCall) + " ms.";
				}
// TODO: check how CORBA timeout behaves, and whether a corba exception would be wrapped as an ExecutionException
//			} catch (???CORBATimeoutEx??? e) {
//				wasTimedOut = true;
//			}
			} catch (ExecutionException ex) {
				callError = ex.getCause();
				if (callError instanceof org.omg.CORBA.TRANSIENT) {
					// Corba failed to connect to the server, for example because a container process has disappeared
					wasTimedOut = true;
					timedOutDescription = "TRANSIENT after " + (System.currentTimeMillis() - timeBeforeCall) + " ms.";
				}
			} catch (Throwable thr) { // unexpected
				callError = thr;
			}
			finally {
				if (wasTimedOut) {
					checkStateCallerWithTimeout.cancel();  // to suppress a possible later bad-state message 
				}
			}
			
			// perhaps isSuspended was set while calling, so we check again
			if (isSuspended) {
				return;
			}
			
			// analyze result and react
			
			boolean beyondRepair = false;
			if (wasTimedOut) {
				lastCheckSucceeded = false;
				try {
					logger.log(AcsLogLevel.DEBUG, "About to call error handler " + err.getClass().getName() + "#resourceUnreachable. Timeout detail: " + timedOutDescription);
					// notify the error handler
					// TODO: call in separate thread with timeout. Decide about value of "beyondRepair" if method 'resourceUnreachable' times out
					beyondRepair = err.resourceUnreachable(resourceChecker.getResource()); 
				} catch (Throwable thr) {
					logger.log(Level.WARNING, "Failed to propagate unavailability of resource '" + resourceChecker.getResourceName() + "' to the error handler!", thr);
				}
			}
			else if (callError != null) {
				// the asynchronous call "resourceChecker.checkState()" failed, but not because of a timeout.
				// This is not expected, and we must log the exception.
				lastCheckSucceeded = false;
				logger.log(Level.WARNING, "Failed to check the status of resource '" + resourceChecker.getResourceName() + "' because of an exception.", callError);
				try {
					// notify the error handler
					// @TODO: call in separate thread with timeout. Decide about value of "beyondRepair" if method 'resourceUnreachable' times out
					beyondRepair = err.resourceUnreachable(resourceChecker.getResource()); 
				} catch (Throwable thr) {
					logger.log(Level.WARNING, "Failed to propagate unavailability of resource '" + resourceChecker.getResourceName() + "' to the error handler!", thr);
				}
			}

			if (beyondRepair) {
				String msg = "Resource '" + resourceChecker.getResourceName() + "' appears permanently unreachable and will no longer be monitored.";			
				logger.info(msg);
                if (scheduleFuture != null) {
                    scheduleFuture.cancel(true);
                }
                else {
                    // this should never be necessary, but if so, it should also cancel the scheduled job
                    throw new RuntimeException(msg);
                }
			}
		}
        
        /**
         * Gets the timeout value in seconds, which is used to abandon hanging resource checker tasks.
         */
        int getCallTimeoutSeconds() {
            return callTimeoutSeconds;
        }
        
        /**
         * For testing only.
         */
        void setCallTimeoutSeconds(int timeout) {
            callTimeoutSeconds = timeout;
        }
        
        ResourceChecker<T> getResourceChecker() {
            return resourceChecker;
        }
        
        void suspend() {
        	isSuspended = true;
        }
        void resume() {
        	isSuspended = false;
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
	 * Implementation of <code>ResourceChecker</code> for ACS components. Calls
	 * {@link ACSComponentOperations#componentState()} to determine responsiveness and state of the component resource.
	 */
	public static class ComponentChecker<T extends ACSComponent> implements ResourceChecker<T>
	{

		private final T comp;

		/**
		 * We keep the component name separately because later when there are problems it may no longer be possible to
		 * obtain it remotely.
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
			} else {
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
	 * A custom <code>ResourceChecker</code> for objects implementing
	 * PingableResource interface.
	 * <p>
	 * @TODO: Since ACS 9.1 the ping() method has parameters, incl. one for recursion.
	 *        The current choice is fast=false, recursive=false, but maybe this should be adjusted.
	 */
	public static class PingableResourceChecker<T extends PingableResourceOperations> implements
			SubsysResourceMonitor.ResourceChecker<T> {

		private T resource;

		private String resourceName;

		public PingableResourceChecker(T resource, String resourceName) {
			this.resource = resource;
			this.resourceName = resourceName;
		}

		public String checkState() {
			String errMsg = null;
			if (!resource.ping(false, false, -1)) {
				errMsg = "ping(false, false, -1) failed.";
			}
			return errMsg;
		}

		public T getResource() {
			return resource;
		}

		public String getResourceName() {
			return resourceName;
		}
	}
	
	/**
	 * Error handler that gets notified when a monitored resource becomes unavailable or degraded.
	 * <p>
	 * By implementing a custom error handler, a master component can attempt first to cure the situation, or go into
	 * ERROR state by calling <code>doTransition(SubsystemStateEvent.SUBSYSEVENT_ERROR);</code>.
	 */
	public interface ResourceErrorHandler<T>
	{
		/**
		 * Called when the resource could not be reached at all because of a timeout or network/middleware communication errors.
		 * The resource object is passed to allow using one handler for many resources.
		 * <p>
		 * The return value controls whether monitoring of this resource will be stopped:
		 * <ol>
		 * <li><code>true</code> means that the error handler decided that this resource is unreachable beyond repair,
		 *     and that no further monitoring calls should be made. This can avoid potential problems with an increasing
		 *     number of hanging calls and eventually stopping the respective threads.
		 * <li><code>false</code> means that monitoring calls should continue.
		 * </ol>
		 */
		abstract boolean resourceUnreachable(T resource);

		/**
		 * Called when {@link SubsysResourceMonitor} was found in a bad state, but still replied in time.
		 * 
		 * @param resource 
		 *            The resource object is passed to allow using one handler for many resources.
		 * @param stateName
		 *            Name of the bad state the resource was found in. If the resource does not support named states,
		 *            it may return any String that indicates the problem. For example, {@linkplain PingableResourceChecker}
		 *            returns <code>"ping() failed."</code> which is then used as the <code>stateName</code>.
		 * @see ResourceChecker#checkState()
		 */
		abstract void badState(T resource, String stateName);

		/**
		 * Notification that the monitored resource has recovered after a previous failure or timeout. This notification
		 * can only work if monitoring has continued after the problem was detected, which is always the case for
		 * <code>badState</code> problems, but depends on the return value of <code>resourceUnreachable</code> in case of timeout.
		 * problems.
		 * 
		 * @since ACS 8.0.0 (has existed in sub-interface RecoverableResourceErrorHandler since 6.0.3)
		 */
		abstract void resourceRecovered(T resource);
	}

}
