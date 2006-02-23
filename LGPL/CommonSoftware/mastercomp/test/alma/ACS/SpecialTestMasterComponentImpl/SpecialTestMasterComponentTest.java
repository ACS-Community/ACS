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
package alma.ACS.SpecialTestMasterComponentImpl;

import org.omg.CosPropertyService.PropertySet;

import alma.ACS.ROstringSeq;
import alma.ACS.RWdouble;
import alma.ACS.SpecialTestMasterComponent;
import alma.ACS.MasterComponentImpl.StateChangeListener;
import alma.ACS.MasterComponentImpl.StateChangeSemaphore;
import alma.ACS.MasterComponentPackage.SubsystemStateEvent;
import alma.ACSErr.CompletionHolder;
import alma.acs.component.client.ComponentClientTestCase;

/**
 * @author hsommer
 * created Aug 09, 2004
 */
public class SpecialTestMasterComponentTest extends ComponentClientTestCase
{
	private SpecialTestMasterComponent m_masterComp;

	public SpecialTestMasterComponentTest() throws Exception {		
			super("MASTERCOMP2-Test");
	}

	protected void initAcsLogging() {
		// empty, workaround for logging bug...
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		
		org.omg.CORBA.Object compObj = getContainerServices().getComponent("MASTERCOMP2");
		
		assertNotNull(compObj);
		m_masterComp = alma.ACS.SpecialTestMasterComponentHelper.narrow(compObj);
		assertNotNull(m_masterComp);
	}

	protected void tearDown() throws Exception
	{
		// will release MASTERCOMP2 
		super.tearDown();
	}

	

	/**
	 * Sends the normally expected events <code>SUBSYSEVENT_INITPASS1</code>,
	 * <code>SUBSYSEVENT_INITPASS2</code>, <code>SUBSYSEVENT_START</code>,  
	 * <code>SUBSYSEVENT_STOP</code>, <code>SUBSYSEVENT_SHUTDOWNPASS1</code>,
	 * <code>SUBSYSEVENT_SHUTDOWNPASS2</code>
	 * to our test master component.
	 * Uses state change notification to synchronize sending the next event.
	 * 
	 * @throws Exception
	 */
	public void testHealthySubsystemLifecycle() throws Exception {
		ROstringSeq statesProperty = m_masterComp.currentStateHierarchy();
		assertNotNull(statesProperty);

		StateChangeListener listener = new StateChangeListener(m_logger);		
		listener.createMonitor(statesProperty, getContainerServices());

		StateChangeSemaphore sync = listener.getStateChangeSemaphore();
		
		sync.reset();
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_INITPASS1);
		sync.waitForStateChanges(2);		
		
		sync.reset();
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_INITPASS2);
		sync.waitForStateChanges(2);		
		
		sync.reset();
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_START);
		sync.waitForStateChanges(1);
		
		sync.reset();
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_STOP);
		sync.waitForStateChanges(1);
		
		sync.reset();
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_SHUTDOWNPASS1);
		sync.waitForStateChanges(2);		

		sync.reset();
		m_masterComp.doTransition(SubsystemStateEvent.SUBSYSEVENT_SHUTDOWNPASS2);
		sync.waitForStateChanges(2);		
		
		listener.destroyMonitor();
	}
	
	
	public void testOtherProperty() {
		
		RWdouble doubleProperty = m_masterComp.someOtherProperty();
		assertNotNull(doubleProperty);
		CompletionHolder ch = new CompletionHolder();
		
		doubleProperty.set_sync(3.14);
		double actualValue = doubleProperty.get_sync(ch);
		assertTrue("value of property 'someOtherProperty' must equal 3.14", actualValue < 3.15 && actualValue > 3.13);
		
//		PropertySet allCharacteristics = doubleProperty.get_all_characteristics();
//		assertNotNull(allCharacteristics);
	}
}
