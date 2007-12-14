/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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
package alma.ACS.MasterComponentImpl.statemachine;

import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.ACS.MasterComponentImpl.StateChangeSemaphore;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateActionException;
import alma.acs.genfw.runtime.sm.AcsStateChangeListener;
import alma.acs.genfw.runtime.sm.AcsStateUtil;
import alma.acs.logging.AcsLogger;


/**
 * Tests the state machine from <code>alma.ACS.MasterComponentImpl.statemachine</code>
 * without the Master component on top of it. No running ACS needed.
 *  
 * @author hsommer
 * created Mar 3, 2004 3:27:56 PM
 */
public class StateMachineTest extends TestCase implements AcsStateChangeListener
{
    private AlmaSubsystemContext m_context;
    private DummyActionImpl m_actionImpl;
	private AcsLogger m_logger;
	
	private MyStateChangeSemaphore m_sync;
	
	
    public void setUp() {
    	m_logger = AcsLogger.createUnconfiguredLogger("StateMachineTest", null);
    	m_actionImpl = new DummyActionImpl(m_logger);
        m_context = new AlmaSubsystemContext(m_actionImpl, m_logger, new DaemonThreadFactory());
        m_context.addAcsStateChangeListener(this);
        
        m_sync = new MyStateChangeSemaphore(m_logger);
        m_context.addAcsStateChangeListener(m_sync);
    }

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsStateChangeListener#stateChangedNotify(alma.ACS.MasterComponentImpl.statemachine.AcsState[], alma.ACS.MasterComponentImpl.statemachine.AcsState[])
	 */
	public void stateChangedNotify(AcsState[] oldStateHierarchy, AcsState[] currentStateHierarchy) 
	{
		String oldHi = AcsStateUtil.stateHierarchyToString(oldStateHierarchy);
		String newHi = AcsStateUtil.stateHierarchyToString(currentStateHierarchy);
		m_logger.info("state machine switched from " + oldHi + " to " + newHi);
	}

	
    public void testLegalLifecycle() throws Exception 
	{
    	final int n = 20;
        m_logger.info("============ starting testLegalLifecycle with " + n + " iterations ===========");
    	
    	for (int i=0; i< n; i++) {
	        assertStateHierarchy("AVAILABLE/OFFLINE/SHUTDOWN");
	        m_logger.info("---> Event initPass1 will be sent.");
	        m_sync.reset();
	        m_context.initPass1();
	        m_sync.waitForStateChanges(2); 
	        assertStateHierarchy("AVAILABLE/OFFLINE/PREINITIALIZED");
	        
	        m_logger.info("---> Event initPass2 will be sent.");
	        m_sync.reset();
	        m_context.initPass2();
	        m_sync.waitForStateChanges(2); 
	        assertStateHierarchy("AVAILABLE/ONLINE");
	        
	        m_logger.info("---> Event start will be sent.");
	        m_context.start();
	        assertStateHierarchy("AVAILABLE/OPERATIONAL");
	        m_logger.info("---> Event start will be sent again.");
	        m_context.start();
	        assertStateHierarchy("AVAILABLE/OPERATIONAL");
	        m_logger.info("---> Event stop will be sent.");
	        m_context.stop();
	        assertStateHierarchy("AVAILABLE/ONLINE");
	        
	        m_logger.info("---> Event shutdownPass1 will be sent.");
	        m_sync.reset();
	        m_context.shutdownPass1();
	        m_sync.waitForStateChanges(2); 
	        assertStateHierarchy("AVAILABLE/OFFLINE/PRESHUTDOWN");
	        
	        m_logger.info("---> Event shutdownPass2 will be sent.");
	        m_sync.reset();
	        m_context.shutdownPass2();
	        m_sync.waitForStateChanges(2); 
	        assertStateHierarchy("AVAILABLE/OFFLINE/SHUTDOWN");
    	}        
        m_logger.info("============ testLegalLifecycle successful! ===========\n");
    }

    
    public void testIllegalEvent() throws Exception {
        m_logger.info("============ starting testIllegalEvent ===========");

        m_sync.reset();
        m_context.initPass1();
        m_sync.waitForStateChanges(2);
        assertStateHierarchy("AVAILABLE/OFFLINE/PREINITIALIZED");
        
        try {
        	// illegal event "start" in "AVAILABLE/OFFLINE/PREINITIALIZED"
			m_context.start();
			fail("AcsStateIllegalEventException expected!");
		}
		catch (AcsJIllegalStateEventEx e) {
			assertEquals("OFFLINE", e.getState());
			assertEquals("start", e.getEvent());
		}
        assertStateHierarchy("AVAILABLE/OFFLINE/PREINITIALIZED");
		
        m_logger.info("============ testIllegalEvent successful! ===========\n");
    }

    
    public void testActionException() throws Exception 
    {
        m_logger.info("============ starting testActionException ===========");

        m_actionImpl.throwExInInitPass1(true); // we enforce the exception    	

        m_sync.reset();
        m_context.initPass1();
        m_sync.waitForStateChanges(2); 
        assertStateHierarchy("AVAILABLE/ERROR");
        
        try {
			m_context.initPass2();
			fail("AcsJIllegalStateEventEx expected, with state machine in ERROR state");
		}
		catch (AcsJIllegalStateEventEx e) {
			assertEquals("initPass2", e.getEvent());
			assertEquals("ERROR", e.getState());
		}
        // let's get out of error again...
        m_context.shutdownPass1();
        m_sync.waitForStateChanges(2); 
        assertStateHierarchy("AVAILABLE/OFFLINE/PRESHUTDOWN");

        m_context.shutdownPass2();
        m_sync.waitForStateChanges(2); 
        assertStateHierarchy("AVAILABLE/OFFLINE/SHUTDOWN");
        
        m_logger.info("============ testActionException successful! ===========\n");
    }

    public void testReentryToCompositeState() throws Exception {
        assertStateHierarchy("AVAILABLE/OFFLINE/SHUTDOWN");
        m_logger.info("---> Event initPass1 will be sent.");
        m_sync.reset();
        m_context.initPass1();
        m_sync.waitForStateChanges(2); 
        assertStateHierarchy("AVAILABLE/OFFLINE/PREINITIALIZED");
        
        m_logger.info("---> Event initPass2 will be sent.");
        m_sync.reset();
        m_context.initPass2();
        m_sync.waitForStateChanges(2); 
        assertStateHierarchy("AVAILABLE/ONLINE");
    	
        m_logger.info("---> Event reinit will be sent.");
    	assertEquals(0, m_actionImpl.getCallsToReinit());
        m_sync.reset();
        m_context.reinit();
        m_sync.waitForStateChanges(2); 
        assertStateHierarchy("AVAILABLE/ONLINE");
    	Thread.sleep(200); // for the lack of sync'ing with the reinit action method
    	assertEquals(1, m_actionImpl.getCallsToReinit());
        
    	// do another reinit. There used to be a bug (COMP-1776) with the reinit action method not called again, 
    	// because from the point of view of its composite state (OFFLINE), the substate reinit did not change.    	
        m_logger.info("---> Event reinit will be sent again.");
        m_sync.reset();
        m_context.reinit();
        m_sync.waitForStateChanges(2);
        assertStateHierarchy("AVAILABLE/ONLINE");
    	Thread.sleep(200); // for the lack of sync'ing with the reinit action method
    	assertEquals(2, m_actionImpl.getCallsToReinit());    	
    }
    
    private void assertStateHierarchy(String expectedPath) {
		AcsState[] actualHierarchy = m_context.getCurrentTopLevelState().getStateHierarchy();
		String actualPath = AcsStateUtil.stateHierarchyToString(actualHierarchy);
		assertEquals("current state is not as expected!", expectedPath, actualPath);
    }


    /**
     * Test implementation of the state machine's action interface.
     * Methods will be called by the state machine.
     */
    private static class DummyActionImpl implements AlmaSubsystemActions 
	{
    	private Logger m_actionLogger;
		private boolean m_throwExInInitPass1;
		private int callsToReinit;
    	
    	DummyActionImpl(Logger logger) {
    		m_actionLogger = logger;
    		m_throwExInInitPass1 = false;
    	}
    	
    	/**
    	 * Determines the optional test feature of letting {@link #initSubsysPass1()} 
    	 * throw an exception.
    	 */
    	void throwExInInitPass1(boolean throwEx) {
    		m_throwExInInitPass1 = throwEx;
    	}

    	/**
    	 * Allows the tests to check how often reinit has been called.
    	 * @TODO: better use a real sync mechanism so that test can wait till the method was called, or fail with a time out.
    	 */
    	int getCallsToReinit() {
    		return callsToReinit;
    	}
    	
    	
        public synchronized void initSubsysPass1() throws AcsStateActionException {
            if (m_throwExInInitPass1) {
                log("initSubsysPass1 (will throw AcsStateActionException)");  
                throw new AcsStateActionException("sorry, just a test: initPass1 failed!");          	
            }
            else {
            	log("initSubsysPass1");
            }            
        }

        public synchronized void initSubsysPass2() {
           	log("initSubsysPass2");
           	try {
				Thread.sleep(100);
			}
			catch (InterruptedException e) {
				e.printStackTrace();
			}
        }

        public synchronized void reinitSubsystem() {
        	callsToReinit++;
            log("reinitSubsystem");
        }

        public synchronized void shutDownSubsysPass1() {
            log("shutDownSubsysPass1");
        }

        public synchronized void shutDownSubsysPass2() {
            log("shutDownSubsysPass2");
        }

        private void log(String action) {
        	m_actionLogger.info("*** action " + action + " called. ***");
        }
    }
    
    
    
    private static class MyStateChangeSemaphore extends StateChangeSemaphore implements AcsStateChangeListener 
	{
    	MyStateChangeSemaphore(Logger logger) {
    		super(logger);
    	}
    	
		public synchronized void stateChangedNotify(AcsState[] oldStateHierarchy, AcsState[] currentStateHierarchy) 
		{
			stateChangedNotify();
		}
	}
}
