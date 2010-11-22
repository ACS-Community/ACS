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

import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.omg.CORBA.StringHolder;

import alma.ACS.stringSeqHolder;
import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.corba.AcsCorba;
import alma.jconttest.ContainerServicesTester;
import alma.jconttest.ContainerServicesTesterHelper;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentHelper;
import alma.maciErrType.wrappers.AcsJNoPermissionEx;
import alma.acs.util.StopWatch;


/**
 * @author hsommer
 * created Sep 17, 2004 3:49:45 PM
 */
public class ComponentTestclient extends ComponentClientTestCase
{
	private static final String CONTSRVCOMP_INSTANCE = "CONT_SERVICES_TESTER";
	private static final String DEFAULT_DUMMYCOMP_INSTANCE = "DefaultDummyComp";
	private static final String DUMMYCOMP2_TYPENAME = "IDL:alma/jconttest/DummyComponent2:1.0";

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
	
    /**
    * This test will use custom client side timeout, ignoring Container Timeout 
    **/
    public void testTimeout() throws Exception {
        org.omg.CORBA.Object compObj = getContainerServices().getComponent(DEFAULT_DUMMYCOMP_INSTANCE);
        DummyComponent comp = DummyComponentHelper.narrow(compObj);
        int waitTime = 20 ; //secs.
        int timeout = 10 ; //secs.
        //first let's try to call the IF without a timeout
        try{
            comp.callThatTakesSomeTime(waitTime * 1000);	
        }catch(org.omg.CORBA.TIMEOUT e){
            fail("It is not supposed to get a timeout before waitTime");
        }
    
        //then we call the object to assing a timeout of 10 seconds
        compObj = getContainerServices().getReferenceWithCustomClientSideTimeout(compObj,timeout);
        comp = DummyComponentHelper.narrow(compObj);
        
        //we call again the IF
        StopWatch sw = new StopWatch(m_logger);
        try{
            comp.callThatTakesSomeTime(waitTime *1000);	
            fail("It is supposed to get a timeout before waitTime");
        }catch(org.omg.CORBA.TIMEOUT e){
            //Expected exception catch
        }
   
        int elapsedTime = (int) sw.getLapTimeMillis()/1000;
        if(Math.abs(elapsedTime-timeout) > 2){
            fail("The timeout was much greater/lesser than the ammount assigned to elapsed="+elapsedTime);
        }
        getContainerServices().releaseComponent(DEFAULT_DUMMYCOMP_INSTANCE);
    }
	public void testGetReferenceWithCustomClientSideTimeout() {
        String compName = "DefaultDummyComp";
		boolean ret = m_contSrvTesterComp.testGetReferenceWithCustomClientSideTimeout(compName);
		assertTrue("test execution successful on the server component", ret);
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

	public void testFindDummyComponentsByType() throws Exception {
		String[] curls = null;
		try {
			stringSeqHolder curlRet = new stringSeqHolder();
			m_contSrvTesterComp.testFindDummyComponentsByType(curlRet);
			curls = curlRet.value;
		} catch (CouldntPerformActionEx ex) {
			throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
		}
		assertNotNull(curls);
		assertEquals(2, curls.length);
		assertEquals("DefaultDummyComp", curls[0]);
	}
	
	public void testGetCollocatedComponent() throws Exception {
		try {
			// a new component in the same container as our main test component
			m_contSrvTesterComp.testGetCollocatedComponent(ComponentQueryDescriptor.ANY, DUMMYCOMP2_TYPENAME, CONTSRVCOMP_INSTANCE);
			Thread.sleep(1500);
			// now we check whether a non-activated target component also works
			m_contSrvTesterComp.testGetCollocatedComponent("MyCollocatedDummy2", ComponentQueryDescriptor.ANY, "MyCollocationTargetDummy");

			Thread.sleep(1500);			
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

    
    public void testComponentListening() throws Exception {
    	
    	// a second client to the manager. Shares Corba stuff, but has its own connection to the manager.
    	OtherComponentClient secondClient = new OtherComponentClient(null, m_managerLoc, "ComponentTestclient-secondClient", acsCorba);
    	
		BlockingComponentListener blockLizzy = new BlockingComponentListener(m_logger);
		getContainerServices().registerComponentListener(blockLizzy);

		m_logger.info("Second client will activate and kill '" + DEFAULT_DUMMYCOMP_INSTANCE + 
				"'. This test is not a client and should not be notified.");
    	secondClient.getComponent(DEFAULT_DUMMYCOMP_INSTANCE);
    	secondClient.forceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE);
		Thread.sleep(2000); // to make sure we would have gotten the notification if there were any
		assertEquals(0, blockLizzy.getAllCompsAvailable().size());
		assertEquals(0, blockLizzy.getAllCompNamesUnavailable().size());

		getContainerServices().getComponent(DEFAULT_DUMMYCOMP_INSTANCE);

		m_logger.info("A second client will request again and kill '" + DEFAULT_DUMMYCOMP_INSTANCE +
				"'. This test has already activated the component, thus only the component unloading should yield 1 notification.");
		blockLizzy.clearAndExpect(1);
    	secondClient.getComponent(DEFAULT_DUMMYCOMP_INSTANCE);  
    	secondClient.forceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE);
		assertTrue("Failed to get expected notification from manager within 10 seconds", 
					blockLizzy.awaitNotifications(10, TimeUnit.SECONDS));
		assertEquals(0, blockLizzy.getAllCompsAvailable().size());
		assertEquals(1, blockLizzy.getAllCompNamesUnavailable().size());

		m_logger.info("A third client will request and kill '" + DEFAULT_DUMMYCOMP_INSTANCE +
		"'. This test is still registered as a client, thus 2 notifications for component loading and unloading are expected.");
		blockLizzy.clearAndExpect(2);
		secondClient.getComponent(DEFAULT_DUMMYCOMP_INSTANCE);
		secondClient.forceReleaseComponent(DEFAULT_DUMMYCOMP_INSTANCE);
		assertTrue("Failed to get expected notification from manager within 10 seconds", 
					blockLizzy.awaitNotifications(10, TimeUnit.SECONDS));
		assertEquals(1, blockLizzy.getAllCompsAvailable().size());
		assertEquals(1, blockLizzy.getAllCompNamesUnavailable().size());

		secondClient.tearDown();
	}
}


/**
 * Helper client that allows activating and destroying a component independently of our test,
 * in the sense that the manager thinks it's a different application. 
 */
class OtherComponentClient extends AdvancedComponentClient {

	public OtherComponentClient(Logger logger, String managerLoc, String clientName, AcsCorba externalAcsCorba) throws Exception {
		super(logger, managerLoc, clientName, externalAcsCorba);
	}
	
	org.omg.CORBA.Object getComponent(String name) throws AcsJContainerServicesEx {
		return getContainerServices().getComponent(name);
	}
	
	/**
	 * Commands the manager directly to forcefully release the given component.
	 * Note that such a call is only OK for a unit test, but not for normal operational code.
	 * @param name  name of the component to be forcefully released
	 * @throws AcsJNoPermissionEx 
	 */
	void forceReleaseComponent(String name) throws AcsJNoPermissionEx {
		// first a regular release so that this client's container services count down the reference
		getContainerServices().releaseComponent(name);
		
		// and now the forceful release that will unload the component regardless of other clients' references to it
		int numClients = getAcsManagerProxy().force_release_component(getAcsManagerProxy().getManagerHandle(), name);
		
		m_logger.info("Forcibly released component " + name + " which had " + numClients + " other clients.");
	}
	
}
