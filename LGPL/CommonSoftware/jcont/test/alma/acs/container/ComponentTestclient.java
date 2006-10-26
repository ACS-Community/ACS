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
package alma.acs.container;

import org.omg.CORBA.OBJECT_NOT_EXIST;
import org.omg.CORBA.StringHolder;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.client.ComponentClientTestCase;
import alma.jconttest.ContainerServicesTester;
import alma.jconttest.ContainerServicesTesterHelper;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentHelper;

import java.util.List;
import java.util.concurrent.TimeUnit;


/**
 * @author hsommer
 * created Sep 17, 2004 3:49:45 PM
 */
public class ComponentTestclient extends ComponentClientTestCase
{
	private static final String CONTSRVCOMP_INSTANCE = "CONT_SERVICES_TESTER";
	private static final String DEFAULT_DUMMYCOMP_INSTANCE = "DefaultDummyComp";
	
	private ContainerServicesTester m_contSrvTesterComp;
	
	public ComponentTestclient() throws Exception
	{
		super("ComponentTestclient");
	}

	protected void setUp() throws Exception
	{
		super.setUp();
		
		org.omg.CORBA.Object compObj = getContainerServices().getComponent(CONTSRVCOMP_INSTANCE);
		assertNotNull(compObj);
		m_contSrvTesterComp = ContainerServicesTesterHelper.narrow(compObj);
	}
	
	
    public void testComponentName() {
		StringHolder nameHolder = new StringHolder();
		boolean ret = m_contSrvTesterComp.testComponentName(nameHolder);
		assertTrue("test execution successful on the server component", ret);		
		assertEquals(CONTSRVCOMP_INSTANCE, nameHolder.value);
	}
	
	public void testStateManager() {
		StringHolder stateNameHolder = new StringHolder();
		boolean ret = m_contSrvTesterComp.testStateManager(stateNameHolder);
		assertTrue("test execution successful on the server component", ret);		
		assertEquals("OPERATIONAL", stateNameHolder.value);
	}
	
	public void testGetDynamicDummyComponent() {
		StringHolder compNameHolder = new StringHolder();
		boolean ret = m_contSrvTesterComp.testGetDynamicDummyComponent(compNameHolder);
		assertTrue("test execution successful on the server component", ret);
		System.out.println("got dummy component called " + compNameHolder.value);
//		assertEquals("OPERATIONAL", compNameHolder.value);
	}

    public void testGetThreadFactory() {
        boolean ret = m_contSrvTesterComp.testGetThreadFactory(40, 100000, true);
        assertTrue("test execution successful on the server component", ret);
    }
    
    public void testGetCollocatedComponent() throws Exception {
    	try {
			// a new component in the same container as our main test component
			m_contSrvTesterComp.testGetCollocatedComponent("MyCollocatedDummy1", CONTSRVCOMP_INSTANCE);
			
			// now we check whether a non-activated target component also works
			m_contSrvTesterComp.testGetCollocatedComponent("MyCollocatedDummy2", "MyCollocationTargetDummy");
			
    	} catch (CouldntPerformActionEx ex) {
    		throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
    	}
    }

    public void testGetComponentNonSticky() throws Exception {
		// without previous activation, the call should fail
    	try {
			m_contSrvTesterComp.testGetComponentNonSticky(DEFAULT_DUMMYCOMP_INSTANCE, false);
			fail("Exception expected, since non-sticky comp ref should not have been available.");
    	} catch (CouldntPerformActionEx ex) {
    		;// expected
    	}
    	
    	// now our test is the real client that activates the component beforehand
    	getContainerServices().getComponent(DEFAULT_DUMMYCOMP_INSTANCE);
    	
    	try {
    		// this time the test comp should get the non-sticky ref
    		m_contSrvTesterComp.testGetComponentNonSticky(DEFAULT_DUMMYCOMP_INSTANCE, false);
    	} catch (CouldntPerformActionEx ex) {
    		throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
    	}

    	try {
    		// and the same thing again, but this time testing that releasing the non-sticky ref goes ok
    		m_contSrvTesterComp.testGetComponentNonSticky(DEFAULT_DUMMYCOMP_INSTANCE, true);
    	} catch (CouldntPerformActionEx ex) {
    		throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
    	}

    	getContainerServices().releaseComponent(DEFAULT_DUMMYCOMP_INSTANCE);  // returns only when comp has been unloaded
    	
    	try {
			// with the target comp gone, this call should fail again
    		m_contSrvTesterComp.testGetComponentNonSticky(DEFAULT_DUMMYCOMP_INSTANCE, true);
			fail("Exception expected, since non-sticky comp ref should not have been available.");
    	} catch (CouldntPerformActionEx ex) {
    		// expected
    		AcsJCouldntPerformActionEx jEx = AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
    		assertEquals("testGetComponentNonSticky failed for component 'DefaultDummyComp'", jEx.getMessage());
    	}
    }

    public void testForceReleaseComponent() throws Exception {
    	try {
    		// first something very easy: "forcefully" unloading a component to which no other client has a reference
    		m_contSrvTesterComp.testForceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE, true);
    		Thread.sleep(2000);
    		
    		// the normal case: 2 references and a forceful unload
    		DummyComponent defaultDummy = DummyComponentHelper.narrow(getContainerServices().getComponent(DEFAULT_DUMMYCOMP_INSTANCE));
    		assertEquals(DEFAULT_DUMMYCOMP_INSTANCE, defaultDummy.name());    		
			m_contSrvTesterComp.testForceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE, true);
			// our reference should no longer work
			try {
				String name = defaultDummy.name();
				fail("Did not expect to get the proper component name '" + name + "' after forceful unloading!");
			} catch (OBJECT_NOT_EXIST ex) {
				; // that's good
			}
			
			// component trying to forcefully unload another comp to which it never got a reference.
			// The container services should prevent this, thus we can't test whether manager would check it as well.
			String onlyMyDummyName = "onlyMyDummy";
			DummyComponent onlyMyDummy = DummyComponentHelper.narrow(getContainerServices().getDynamicComponent(new ComponentQueryDescriptor(onlyMyDummyName, "IDL:alma/jconttest/DummyComponent:1.0"), false));
			m_contSrvTesterComp.testForceReleaseComponent(onlyMyDummyName, false);
			
    	} catch (CouldntPerformActionEx ex) {
    		throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
    	}
    }

    
    public void testComponentListening () throws Exception {
		try {
			BlockingComponentListener blockLizzy = new BlockingComponentListener(m_logger);
    		getContainerServices().registerComponentListener(blockLizzy);
    		
    		// we shouldn't get notified as we're not a client of the component yet
			m_contSrvTesterComp.testForceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE, true);
			Thread.sleep(2000); // to make sure we would have gotten the notification if there were any
			assertEquals(0, blockLizzy.getAllCompsAvailable().size());
			assertEquals(0, blockLizzy.getAllCompNamesUnavailable().size());

			
			DummyComponent defaultDummy = DummyComponentHelper.narrow(getContainerServices().getComponent(DEFAULT_DUMMYCOMP_INSTANCE));
    		// from now on we should be notified
			
			// component already active, this call will kill it . Should yield one notification
    		blockLizzy.clearAndExpect(1);
    		m_contSrvTesterComp.testForceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE, true);
    		blockLizzy.awaitNotifications(10, TimeUnit.SECONDS);
			assertEquals(0, blockLizzy.getAllCompsAvailable().size());
			assertEquals(1, blockLizzy.getAllCompNamesUnavailable().size());
			
			// this call will both activate and kill the component . Should yield two notifications in total
    		blockLizzy.clearAndExpect(2);
			m_contSrvTesterComp.testForceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE, true);
    		blockLizzy.awaitNotifications(10, TimeUnit.SECONDS);
			assertEquals(1, blockLizzy.getAllCompsAvailable().size());
			assertEquals(1, blockLizzy.getAllCompNamesUnavailable().size());
			
		} catch (CouldntPerformActionEx ex) {
			throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
		}
	}
    
}


