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

import alma.ACS.MasterComponent;
import alma.ACS.ROstringSeq;
import alma.ACS.SUBSYSSTATE_AVAILABLE;
import alma.ACS.SUBSYSSTATE_INITIALIZING_PASS1;
import alma.ACS.SUBSYSSTATE_OFFLINE;
import alma.ACS.SUBSYSSTATE_ONLINE;
import alma.ACS.SUBSYSSTATE_OPERATIONAL;
import alma.ACS.SUBSYSSTATE_PREINITIALIZED;
import alma.ACS.SUBSYSSTATE_PRESHUTDOWN;
import alma.ACS.SUBSYSSTATE_REINITIALIZING;
import alma.ACS.SUBSYSSTATE_SHUTDOWN;
import alma.ACS.MasterComponentPackage.SubsystemStateEvent;
import alma.ACSErr.ACSErrTypeOK;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.IllegalStateEventEx;
import alma.ACSErrTypeOK.ACSErrOK;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.exceptions.AcsJCompletion;

/**
 * @author hsommer
 * created Apr 13, 2004 4:32:51 PM
 */
public class MasterComponentTest extends ComponentClientTestCase
{
	private MasterComponent m_masterComp;

	/**
	 * Constructor for MasterComponentTest.
	 */
	public MasterComponentTest() throws Exception {		
			super("MASTERCOMP1-Test");
	}

	protected void initAcsLogging() {
		// empty, workaround for logging bug...
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		
		org.omg.CORBA.Object compObj = getContainerServices().getComponent("MASTERCOMP1");
		
		assertNotNull(compObj);
		m_masterComp = alma.ACS.MasterComponentHelper.narrow(compObj);
		assertNotNull(m_masterComp);
		
//		MasterComponentReadOnly maCompRO = alma.ACS.MasterComponentReadOnlyHelper.narrow(compObj);
//		assertNotNull(maCompRO);
	}

	protected void tearDown() throws Exception
	{
		// will release MASTERCOMP1 
		super.tearDown();
	}

	
	public void testInitPass1() throws Exception 
	{
		ROstringSeq statesProperty = m_masterComp.currentStateHierarchy();
		assertNotNull(statesProperty);

		// verify initial state
		String[] expectedHierarchy = new String[] {
				SUBSYSSTATE_AVAILABLE.value, SUBSYSSTATE_OFFLINE.value, SUBSYSSTATE_SHUTDOWN.value };
		verifyCurrentState(statesProperty, expectedHierarchy);
		
		// send event
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_INITPASS1);
		
		// verify transient state (available for 2 seconds thanks to a sleepy test component action)
		expectedHierarchy[2] = SUBSYSSTATE_INITIALIZING_PASS1.value;
		verifyCurrentState(statesProperty, expectedHierarchy);
		
		// verify new state 
		Thread.sleep(3000);
		expectedHierarchy[2] = SUBSYSSTATE_PREINITIALIZED.value;
		verifyCurrentState(statesProperty, expectedHierarchy);
		
		// verify correct error for illegal event
		try {
			m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_STOP);
			fail("expected IllegalStateEventEx");
		}
		catch (IllegalStateEventEx e) {
//			assertEquals("xyz", e.errorTrace.shortDescription);
			m_logger.info("got IllegalStateEventEx as expected.");
		}
		catch (Exception e) {
			fail("expected IllegalStateEventEx, but not this one: " + e.toString());
		}

	}

	public void testFullLifecycle() throws Exception 
	{
		ROstringSeq statesProperty = m_masterComp.currentStateHierarchy();
		assertNotNull(statesProperty);

		// verify initial state
		String[] expectedHierarchy = new String[] {
				SUBSYSSTATE_AVAILABLE.value, SUBSYSSTATE_OFFLINE.value, SUBSYSSTATE_SHUTDOWN.value };
		verifyCurrentState(statesProperty, expectedHierarchy);
		
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_INITPASS1);		
		Thread.sleep(2500);
//		expectedHierarchy[2] = SUBSYSSTATE_PREINITIALIZED.value;
//		verifyCurrentState(statesProperty, expectedHierarchy);
		
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_INITPASS2);
		expectedHierarchy = new String[] {
				SUBSYSSTATE_AVAILABLE.value, SUBSYSSTATE_ONLINE.value };
		verifyCurrentState(statesProperty, expectedHierarchy);
		
		// todo: send more events
	}


	/**
	 * Uses state change notification to synchronize sending the next event,
	 * instead of stupid Thread.sleep like the above tests.
	 * <p>
	 * This method could be taken as an example of how subsystems can use these synchronization helper classes 
	 * to write unit tests for their own master components.
	 * 
	 * @throws Exception
	 */
	public void testEventSync() throws Exception 
	{
		ROstringSeq statesProperty = m_masterComp.currentStateHierarchy();
		assertNotNull(statesProperty);

		StateChangeListener listener = new StateChangeListener(m_logger);		
		listener.createMonitor(statesProperty, getContainerServices());

		String[] expectedHierarchy = new String[] {
				SUBSYSSTATE_AVAILABLE.value, SUBSYSSTATE_OFFLINE.value, SUBSYSSTATE_SHUTDOWN.value };
		listener.verifyCurrentState(expectedHierarchy);
		
		StateChangeSemaphore sync = listener.getStateChangeSemaphore();		
		sync.reset();
		
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_INITPASS1);
		sync.waitForStateChanges(2); // SUBSYSSTATE_INITIALIZING_PASS1, SUBSYSSTATE_PREINITIALIZED	
		expectedHierarchy[2] = SUBSYSSTATE_PREINITIALIZED.value;
		assertTrue(listener.verifyCurrentState(expectedHierarchy));
		
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_INITPASS2);
		sync.waitForStateChanges(2); // // SUBSYSSTATE_INITIALIZING_PASS2, SUBSYSSTATE_ONLINE
		expectedHierarchy = new String[] {SUBSYSSTATE_AVAILABLE.value, SUBSYSSTATE_ONLINE.value };
		assertTrue(listener.verifyCurrentState(expectedHierarchy));
		
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_START);
		sync.waitForStateChanges(1); // SUBSYSSTATE_OPERATIONAL
		expectedHierarchy[1] = SUBSYSSTATE_OPERATIONAL.value;
		assertTrue(listener.verifyCurrentState(expectedHierarchy));
		
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_REINIT);
		sync.waitForStateChanges(2); // SUBSYSSTATE_REINITIALIZING, SUBSYSSTATE_ONLINE
		expectedHierarchy[1] = SUBSYSSTATE_ONLINE.value;
		assertTrue(listener.verifyCurrentState(expectedHierarchy));
						
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_START);
		sync.waitForStateChanges(1); // SUBSYSSTATE_OPERATIONAL
		expectedHierarchy[1] = SUBSYSSTATE_OPERATIONAL.value;
		assertTrue(listener.verifyCurrentState(expectedHierarchy));
		
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_STOP);
		sync.waitForStateChanges(1); // SUBSYSSTATE_ONLINE
		expectedHierarchy[1] = SUBSYSSTATE_ONLINE.value;
		assertTrue(listener.verifyCurrentState(expectedHierarchy));
				
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_SHUTDOWNPASS1);
		sync.waitForStateChanges(2); // SUBSYSSTATE_SHUTTINGDOWN_PASS1, SUBSYSSTATE_PRESHUTDOWN
		expectedHierarchy = new String[] {SUBSYSSTATE_AVAILABLE.value, SUBSYSSTATE_OFFLINE.value, SUBSYSSTATE_PRESHUTDOWN.value };
		assertTrue(listener.verifyCurrentState(expectedHierarchy));

		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_SHUTDOWNPASS2);
		sync.waitForStateChanges(2); // SUBSYSSTATE_SHUTTINGDOWN_PASS2, SUBSYSSTATE_SHUTDOWN
		expectedHierarchy[2] = SUBSYSSTATE_SHUTDOWN.value;
		assertTrue(listener.verifyCurrentState(expectedHierarchy));
		
		listener.destroyMonitor();
	}
	
	
	
	/**
	 * Helper method for the repeated task of getting the current state hierarchy and 
	 * comparing it against the expected hierarchy.
	 * <p>
	 * This method is a replica of {@link StateChangeListener#verifyCurrentState(String[])} 
	 * which is necessary for testing w/o using the synchronization facilities offered by <code>StateChangeListener</code>.
	 */
	private void verifyCurrentState(ROstringSeq statesProperty, String[] expectedHierarchy) 
	{ 
		CompletionHolder ch = new CompletionHolder();
		String[] states = statesProperty.get_sync(ch);
		
		AcsJCompletion statesSyncCompletion = AcsJCompletion.fromCorbaCompletion(ch.value);
		assertFalse(statesSyncCompletion.isError());
		assertEquals(ACSErrTypeOK.value, statesSyncCompletion.getType());
		assertEquals(ACSErrOK.value, statesSyncCompletion.getCode());
		
		// verify state
		assertNotNull(states);
		assertTrue(states.length == expectedHierarchy.length);
		for (int i = 0; i < states.length; i++) {
			assertEquals(expectedHierarchy[i], states[i]);
		}
	}

}
