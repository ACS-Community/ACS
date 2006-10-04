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
        logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("SubsysResourceMonitorTest", false);
        logger.info("-------------------------------- setUp " + getName());

        threadFactory = new CleaningDaemonThreadFactory("subsysResMonitor", logger);
        
        // create a monitor that checks every 5 seconds
        subsysResourceMonitor = new SubsysResourceMonitor(logger, threadFactory, checkDelaySeconds);        
    }

    protected void tearDown() throws Exception {
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
        resource.setTestDelaySeconds(1);
        int numberCheckCalls = 4;
        CountDownLatch resourceCheckCounter = new CountDownLatch(numberCheckCalls);
        resource.setStateCheckCounter(resourceCheckCounter);
        
        // a benign resource checker
        TestResourceChecker checker = new TestResourceChecker(resource, logger);
        assertSame(resource, checker.getResource());
        assertEquals(resource.getName(), checker.getResourceName());
        
        // and a sweet error handler
        TestErrorHandler handler = new TestErrorHandler(logger);
        assertEquals(0, handler.getUnreachableCount());
        assertEquals(0, handler.getBadStateCount());
        
        // the call to monitorResource returns immediately, thus we need to wait afterwards
        subsysResourceMonitor.monitorResource(checker, handler);
        assertTrue("timeout occured while waiting for resource check calls", resourceCheckCounter.await(checkDelaySeconds * numberCheckCalls + 1, TimeUnit.SECONDS));

        assertEquals(0, handler.getUnreachableCount());
        assertEquals(0, handler.getBadStateCount());
        
        List<Thread> threadsCreated = threadFactory._getAllThreadsCreated();
        for (Thread thread : threadsCreated) {
			logger.info("Monitor framework created thread '" + thread.getName() + "' during test run.");
		}
        // we expect one thread for the monitor scheduler, and another thread for the asynchronous call to the resource 
        assertEquals("The monitoring should have created 2 threads", 2, threadsCreated.size());
    }

    
    
    private static class TestResource {
        public static final String STATE_OK = "OK";
        private String state;
        private int testDelaySeconds;
        private Logger logger;
		private CountDownLatch counter;

        public TestResource(Logger logger) {
            this.logger = logger;
            setTestDelaySeconds(1);
            setState(STATE_OK);
        }
        String getName() {
            return "Your faithful test resource";
        }
        synchronized String getState() {
            logger.info("TestResource#getState will take " + testDelaySeconds + " seconds");
            try {
                Thread.sleep(testDelaySeconds * 1000);
            } catch (InterruptedException ex) {
                logger.log(Level.WARNING, "interrupted", ex);
                ex.printStackTrace();
            }
            if (counter != null) {
            	counter.countDown();
            }
            return state;
        }
        synchronized void setState(String state) {
            this.state = state;
        }
        synchronized void setStateCheckCounter(CountDownLatch counter) {
        	this.counter = counter;
        }
        int getTestDelaySeconds() {
            return testDelaySeconds;
        }
        void setTestDelaySeconds(int testDelaySeconds) {
            this.testDelaySeconds = testDelaySeconds;
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
        private Logger logger;
        private int badStateCount;
        private int unreachableCount;

        TestErrorHandler(Logger logger) {
            this.logger = logger;
            resetCounters();
        }
        public void badState(TestResource resource, String stateName) {
        }

        public void resourceUnreachable(TestResource resource) {
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
    }
    
}
