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
package alma.ACS.MasterComponentImpl;


import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.AssertionFailedError;
import junit.framework.TestCase;

import alma.acs.container.CleaningDaemonThreadFactory;
import alma.acs.logging.ClientLogManager;

/**
 * Test for {@link SubsysResourceMonitor}.
 * @author hsommer
 */
public class SubsysResourceMonitorTest extends TestCase {

	private static final int checkDelaySeconds = 5;

    private SubsysResourceMonitor subsysResourceMonitor;
    private Logger logger;
    private CleaningDaemonThreadFactory threadFactory; 
    
    
    
    protected void setUp() throws Exception {
        logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getName(), false);
        logger.info("-------------------- setUp " + getName() + " --------------------");

        threadFactory = new CleaningDaemonThreadFactory("monitor", logger) {
			public Thread newThread(Runnable command) {
				Thread newThread = super.newThread(command);
				logger.info("Created thread '" + newThread.getName() + "'.");
				return newThread;
			}        	
        };
        
        // create a monitor that checks every 5 seconds
        subsysResourceMonitor = new SubsysResourceMonitor(logger, threadFactory, checkDelaySeconds);        
    }

    protected void tearDown() throws Exception {
    	logger.info("tearDown: will destroy scheduler and threads");
    	subsysResourceMonitor.destroy(10, TimeUnit.SECONDS);
    	
        super.tearDown();
    }

    
    /**
     * Verifies the regular resource status checks under ideal conditions. 
     * Also tests the basic functioning of the test handlers and listeners used by this class.
     */
    public void testMonitorEasy() throws Exception {
    	
    	// a well-behaved resource
        TestResource resource = new TestResource(logger);
        resource.setState(TestResource.STATE_OK);
        int testDelaySeconds = 1;
        resource.setTestDelaySeconds(testDelaySeconds);
        int numMonitorCalls = 4;
        CountDownLatch resourceCheckCounter = new CountDownLatch(numMonitorCalls);
        resource.setStateCheckCounter(resourceCheckCounter);
        
        // a benign resource checker
        TestResourceChecker checker = new TestResourceChecker(resource, logger);
        assertSame(resource, checker.getResource());
        assertEquals(resource.getName(), checker.getResourceName());
        
        // and a sweet error handler
        TestErrorHandler handler = new TestErrorHandler(logger);
        assertEquals(0, handler.getUnreachableCount());
        assertEquals(0, handler.getBadStateCount());
        assertFalse(handler.isPermanentlyUnreachable());
        assertEquals(0, subsysResourceMonitor.getNumberOfMonitorTasks());
        
        // the call to monitorResource returns immediately, thus we need to wait afterwards
        subsysResourceMonitor.monitorResource(checker, handler);
        int maxTotalTimeSeconds = (testDelaySeconds + checkDelaySeconds) * numMonitorCalls + 1;
        assertTrue("timeout occured while waiting for resource check calls", resourceCheckCounter.await(maxTotalTimeSeconds, TimeUnit.SECONDS));

        assertEquals(0, handler.getUnreachableCount());
        assertEquals(0, handler.getBadStateCount());
        
        // we expect one thread for the monitor scheduler, and another thread for the asynchronous call to the resource 
        List<Thread> threadsCreated = threadFactory._getAllThreadsCreated();
        assertEquals("The monitoring should have created 2 threads", 2, threadsCreated.size());
    }

    /**
     * Tests if resource unreachability is propagated to the client when it is not detected by a timeout 
     * in the monitoring framework, but by getting a RuntimeException such as TRANSIENT for a Corba resource.
     * 
     */
    public void testResourceCheckException() throws Exception {
		TestResource resource = new TestResource(logger);		
		TestResourceChecker checker = new TestResourceChecker(resource, logger);        
		TestErrorHandler handler = new TestErrorHandler(logger);

		int delaySeconds = 2;
		
		// (1) NPE, as if the check had failed for a stupid error.
		//     The NPE is not expected, so an additional warning gets logged.
		//     The unreachable handler must be called (it will decide to continue monitoring).
		int numMonitorCalls = 2;
		resource.setCheckStateRuntimeEx(NullPointerException.class);
		CountDownLatch timeoutSync = new CountDownLatch(numMonitorCalls);
        handler.setUnreachabilitySync(timeoutSync);
		handler.setIsPermanentlyUnreachable(false);
		subsysResourceMonitor.monitorResource(checker, handler, delaySeconds);
		assertTrue("timeout occured while waiting for unreachability notification", timeoutSync.await(delaySeconds * numMonitorCalls, TimeUnit.SECONDS));
		logger.info("Survived the " + numMonitorCalls + " NPEs");

		// (2) org.omg.CORBA.TRANSIENT, as if a remote container had disappeared.
		//     The TRANSIENT exception is treated the same way as a timeout.
		//     The unreachable handler must be called (it will decide to stop monitoring).
		Thread.sleep(100); // because above some stuff happens after timeoutSync was called
		numMonitorCalls = 1;
		resource.setCheckStateRuntimeEx(org.omg.CORBA.TRANSIENT.class);
		timeoutSync = new CountDownLatch(numMonitorCalls);
        handler.setUnreachabilitySync(timeoutSync);
		handler.setIsPermanentlyUnreachable(true);
		assertTrue("timeout occured while waiting for unreachability notification", timeoutSync.await(delaySeconds * numMonitorCalls, TimeUnit.SECONDS));
		Thread.sleep(100); // because above some stuff happens after timeoutSync was called
    }
    
    /**
     * Simulates hanging monitor calls, and tests the notification of the error handler,
     * the creation and reuse of threads, the continuation and eventual termination of monitoring calls.  
     */
    public void testResourceUnavailable() throws Exception {
        try {
			TestResource resource = new TestResource(logger);
			TestResourceChecker checker = new TestResourceChecker(resource, logger);        
			TestErrorHandler handler = new TestErrorHandler(logger);

			subsysResourceMonitor.monitorResource(checker, handler);
            SubsysResourceMonitor.ResourceCheckRunner runner = subsysResourceMonitor.getResourceCheckRunner(checker);
            assertNotNull(runner);
			runner.setCallTimeoutSeconds(3); // to speed up the test a bit, compared to the default of 10
            
			// (1) we simulate an unavailable resource, as in a CORBA call that eventually times out
			int timeoutSeconds = runner.getCallTimeoutSeconds();
			int testDelaySeconds = 6 * timeoutSeconds + 1;  // must be greater than timeoutSeconds to allow testing the behavior for unavailable resources
            assertTrue("for testing, the artifical resource check hanging time should not be an integer multiple of (timeoutSeconds + checkDelaySeconds), to avoid hitting the boundary where an exisitng thread can be reused vs. creating a new thread", testDelaySeconds % (timeoutSeconds + checkDelaySeconds) > 0);
			resource.setTestDelaySeconds(testDelaySeconds);
			int numMonitorCalls = 10;
			CountDownLatch timeoutSync = new CountDownLatch(numMonitorCalls);
            handler.setUnreachabilitySync(timeoutSync);
			int maxTotalTimeSeconds = (timeoutSeconds + checkDelaySeconds) * numMonitorCalls + 1;
			logger.info("Test thread will wait for " + numMonitorCalls + " unavailability notifications (at most " + maxTotalTimeSeconds + " seconds)");
			assertTrue("timeout occured while waiting for deliberately slow resource check calls", timeoutSync.await(maxTotalTimeSeconds, TimeUnit.SECONDS));
			logger.info("Test thread continues...");
            assertEquals(1, subsysResourceMonitor.getNumberOfMonitorTasks());
			assertEquals(numMonitorCalls, handler.getUnreachableCount());
			assertEquals(0, handler.getBadStateCount());
			
			List<Thread> threadsCreated = threadFactory._getAllThreadsCreated();
			// we expect one thread for the monitor scheduler, and some more threads for the asynchronous calls to the resource. 
			// Their number depends on how many checker threads had to be started before an initially hanging thread could be reused. 
            int numCheckerThreads = 1 + testDelaySeconds/(timeoutSeconds + checkDelaySeconds);
			assertEquals("The monitoring should have created " + (numCheckerThreads + 1) + " threads.", numCheckerThreads+1, threadsCreated.size());
			
			// (2) we simulate a resource that's gone beyond repair, and whose error handler will request no further monitoring
			handler.setIsPermanentlyUnreachable(true);
			Thread.sleep((checkDelaySeconds + timeoutSeconds + 1)*1000); // wait to make sure that the permanent failure has been discovered
            assertEquals("Monitoring should have been cancelled due to the permanent failure.", 0, subsysResourceMonitor.getNumberOfMonitorTasks()); 
			
			logger.info("done");
		} catch (AssertionFailedError e) {
			// we want to log this in order to compare timestamps with the other asynchronous activities
			logger.log(Level.SEVERE, "assertion failure", e);
			throw e;
		}
    }
    
    
    /**
     * Tests whether bad user-supplied parameters are handled gracefully.
     * @throws Exception
     */
    public void testUsageErrorResponse() throws Exception {
        TestResource resourceOk = new TestResource(logger);
                    
        TestResourceChecker checkerOk = new TestResourceChecker(resourceOk, logger);            
        TestErrorHandler handler = new TestErrorHandler(logger);

        // (1) ResourceChecker and error handler are null
        try {
            subsysResourceMonitor.monitorResource(null, null);
            fail("IllegalArgumentException expected");
        } catch (IllegalArgumentException ex) {
            assertEquals("ResourceChecker must be non-null and must deliver non-null resource and resource name.", ex.getMessage());
        }
        // (2) ResourceChecker wraps a null resource
        TestResourceChecker checkerNullResource = new TestResourceChecker(null, logger);
        try {
            subsysResourceMonitor.monitorResource(checkerNullResource, handler);
            fail("IllegalArgumentException expected");
        } catch (IllegalArgumentException ex) {
            assertEquals("ResourceChecker must be non-null and must deliver non-null resource and resource name.", ex.getMessage());
        }
        // (3) ResourceChecker wraps a resource with a null name
        TestResource resourceNullName = new TestResource(logger);
        resourceNullName.setName(null);
        TestResourceChecker checkerResourceNullName = new TestResourceChecker(resourceNullName, logger);           
        try {
            subsysResourceMonitor.monitorResource(checkerResourceNullName, handler);
            fail("IllegalArgumentException expected");
        } catch (IllegalArgumentException ex) {
            assertEquals("ResourceChecker must be non-null and must deliver non-null resource and resource name.", ex.getMessage());
        }
        // (4) decent ResourceChecker, but null error handler
        try {
            subsysResourceMonitor.monitorResource(checkerOk, null);
            fail("IllegalArgumentException expected");
        } catch (IllegalArgumentException ex) {
            assertEquals("ResourceErrorHandler must not be null", ex.getMessage());
        }
        
        // (5)
        logger.info("Will submit the same resource checker twice");
        subsysResourceMonitor.monitorResource(checkerOk, handler);
        subsysResourceMonitor.monitorResource(checkerOk, handler); // will re-schedule the task
        Thread.sleep(10000); // to allow the canceled first thread to disappear
        assertEquals("Only one monitoring task expected.", 1, subsysResourceMonitor.getNumberOfMonitorTasks()); 
        
        // (6)
        logger.info("Will submit a new resource checker for a resource that is already being monitored");
        TestResourceChecker checkerOk2 = new TestResourceChecker(resourceOk, logger);            
        subsysResourceMonitor.monitorResource(checkerOk2, handler); // will re-schedule the task
        Thread.sleep(10000); // to allow the canceled first thread to disappear
        assertEquals("Only one monitoring task expected.", 1, subsysResourceMonitor.getNumberOfMonitorTasks()); 
    }
    
    
    
    private static class TestResource {
        public static final String STATE_OK = "OK";
        private volatile String state;
        private int testDelaySeconds;
        private final Logger logger;
		private volatile CountDownLatch counter;
        private String name;
        private Class<? extends RuntimeException> checkStateRuntimeExClass; // != null to simulate a problem
//        private Error checkStateError; // != null to simulate a problem

        public TestResource(Logger logger) {
            this.logger = logger;
            setTestDelaySeconds(1);
            setState(STATE_OK);
            setName("Your faithful test resource");
            this.checkStateRuntimeExClass = null;
//            this.checkStateError = null;
        }
        String getName() {
            return name;
        }
        void setName(String name) {
            this.name = name;
        }
        /**
         * This method must be reentrant so that the next scheduled check can work even the last thread is still sleeping. 
         * Otherwise the timeouts in the tests get confused.
         */
        String getState() {
        	if (checkStateRuntimeExClass != null) {
                logger.info("TestResource#getState called in thread '" + Thread.currentThread().getName() + "'. Will throw an exception of type " + checkStateRuntimeExClass.getName());
        		RuntimeException ex = null;
        		try {
					ex = checkStateRuntimeExClass.newInstance();
				} catch (Throwable thr) {
					logger.log(Level.WARNING, "unexpected exception while instantiating test exception", thr);
				}
				throw ex;
        	}
        	
            logger.info("TestResource#getState called in thread '" + Thread.currentThread().getName() + "'. Will wait " + testDelaySeconds + " seconds unless interrupted.");
            try {
                Thread.sleep(testDelaySeconds * 1000);
            } catch (InterruptedException ex) {
                logger.log(Level.WARNING, "Sleeping TestResource#getState() interrupted in thread '" + Thread.currentThread().getName() + "'.");
            }
            if (counter != null) {
            	counter.countDown();
                logger.info("TestResource#getState (thread " + Thread.currentThread().getName() + ") is done and will return state '" + state + "'. Counter=" + counter.getCount());
            }
            else {
            	logger.info("TestResource#getState (thread " + Thread.currentThread().getName() + ") is done and will return state '" + state + "'.");
            }
            return state;
        }
        void setState(String state) {
            this.state = state;
        }
        void setStateCheckCounter(CountDownLatch counter) {
        	this.counter = counter;
        }
        int getTestDelaySeconds() {
            return testDelaySeconds;
        }
        void setTestDelaySeconds(int testDelaySeconds) {
            this.testDelaySeconds = testDelaySeconds;
        }
        Class<? extends RuntimeException> getCheckStateRuntimeEx() {
        	return this.checkStateRuntimeExClass;
        }
        void setCheckStateRuntimeEx(Class<? extends RuntimeException> exClass) {
        	this.checkStateRuntimeExClass = exClass;
        }
    }
    
    
    private static class TestResourceChecker implements SubsysResourceMonitor.ResourceChecker<TestResource> {

        private final TestResource resource;
        private Logger logger;
        
        TestResourceChecker(TestResource resource, Logger logger) {
            this.resource = resource;
            this.logger = logger;
        }
        
        public String checkState() {
            String state = resource.getState();
            if (state.equals(TestResource.STATE_OK)) {
                return null;
            }
            else {
                return state;
            }
        }

        public TestResource getResource() {
            return resource;
        }
        public String getResourceName() {
            return resource.getName();
        }
    }
    

	private static class TestErrorHandler implements SubsysResourceMonitor.ResourceErrorHandler<TestResource> {
		private final Logger logger;
		private volatile int badStateCount;
		private volatile int unreachableCount;
		private boolean isPermanentlyUnreachable;
		private volatile CountDownLatch unreachabilitySync;

		TestErrorHandler(Logger logger) {
			this.logger = logger;
			isPermanentlyUnreachable = false;
			resetCounters();
		}

		public void badState(TestResource resource, String stateName) {
			badStateCount++;
			logger.info("TestErrorHandler#badState (thread " + Thread.currentThread().getName() + ") called - " + unreachableCount);
		}

		public boolean resourceUnreachable(TestResource resource) {
			unreachableCount++;
			logger.info("TestErrorHandler#resourceUnreachable (thread " + Thread.currentThread().getName() + ") called - " + unreachableCount);
			if (unreachabilitySync != null) {
				unreachabilitySync.countDown();
			}
			return isPermanentlyUnreachable;
		}

		public void resourceRecovered(TestResource resource) {
			//@todo test also invocations of this method.
		}

		void resetCounters() {
			badStateCount = 0;
			unreachableCount = 0;
		}

		int getBadStateCount() {
			return badStateCount;
		}
		int getUnreachableCount() {
			return unreachableCount;
		}
		boolean isPermanentlyUnreachable() {
			return isPermanentlyUnreachable;
		}
		void setIsPermanentlyUnreachable(boolean isPermanentlyUnreachable) {
			this.isPermanentlyUnreachable = isPermanentlyUnreachable;
		}
		void setUnreachabilitySync(CountDownLatch sync) {
			this.unreachabilitySync = sync;
		}
	}

}
