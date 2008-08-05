/*
 * ALMA - Atacama Large Millimiter Array
 * (c) Universidad Tecnica Federico Santa Maria, 2008
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.container.corba;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.util.StopWatch;
import alma.jconttest.DummyComponentWrapper;
import alma.jconttest.DummyComponentWrapperHelper;
import alma.jconttest.util.JconttestUtil;
import alma.maci.containerconfig.Container;

/**
 * Tests the container timeout which should be given by the CDB attribute Container.Timeout.
 * The system-level corba timeout set through <code>jacorb.connection.client.pending_reply_timeout</code>
 * should not have any effect, since there should always be a (default) timeout even if this container's CDB would not define
 * a timeout explicitly.
 */
public class ContainerClientPendingReplyTimeoutTest extends ComponentClientTestCase {

	private JconttestUtil jconttestUtil;
	private int syslevelOrbTimeoutSec;
	private int syslevelOrbTimeoutSecDefault; // not sure yet what to use it for...
    private int syslevelOrbTimeoutSecDefined;

	public ContainerClientPendingReplyTimeoutTest() throws Exception {
		super("ContainerClientPendingReplyTimeoutTest");
        syslevelOrbTimeoutSecDefined = Integer.parseInt(System.getProperty("jacorb.connection.client.pending_reply_timeout","180000"))/1000;
	}

	protected void setUp() throws Exception {
		super.setUp();
		jconttestUtil = new JconttestUtil(getContainerServices());
		syslevelOrbTimeoutSec = jconttestUtil.getSystemLevelOrbTimeoutMillis() / 1000;		
		assertEquals("system-level jacorb timeout has changed (in orb.properties or cmd line). Please verify that this test still works as intended!", syslevelOrbTimeoutSecDefined, syslevelOrbTimeoutSec);
		syslevelOrbTimeoutSecDefault = (int) new Container().getTimeout();
	}

	protected void tearDown() throws Exception {
		m_logger.info("done, tearDown");
		super.tearDown();
	}
	
	
	/**
	 * Tests the client-side relative roundtrip ORB timeout for Java containers "frodoContainerWithTimeoutX".
	 * Here the values in the CDB are supposed to override the general ORB timeout setting from orb.properties.
	 */
	public void testOrbLevelTimeout() throws Exception {
		
		assertEquals(syslevelOrbTimeoutSecDefined, syslevelOrbTimeoutSec);
        //seconds defined in CDB
		String container = "frodoContainerWithTimeout";
        String component = "DummyCompWrapper_ContainerTimeout";
        String container1, component1;
        int [] timeout = {30,10, 20};
        int n = 1; //Change this value from 1 to 2 or 3 if you want to test other configurations by hand. This is set to 1 to avoid a timeout from NRI tests.
        
        for (int i = 0; i < n;i++){
            container1 = container+(i+1);
            component1 = component+(i+1);
           
		    DummyComponentWrapper wrapper1 = DummyComponentWrapperHelper.narrow(
				getContainerServices().getComponent(component1));
            int timeout1Sec = (int) jconttestUtil.getContainerLevelOrbTimeout(container1);
	        	
            assertEquals("Unexpected CDB timeout for container " + container1, timeout[i], timeout1Sec);
            //This is to prevent run components that have 
            //because this test is run several times with different values of syslevelOrbTimeoutSec
		    if(syslevelOrbTimeoutSec <= timeout1Sec ) break;
            assertTrue(container1 + "'s timeout should be shorter than the system-level timeout", syslevelOrbTimeoutSec-timeout1Sec >= 5);
		    try {
			    assertFalse(wrapper1.callDummyComponentWithTime((timeout1Sec-5)*1000));
			    //Here we will check that the timeout is similar (+- 5 s) to the timeout defined in CDB
                StopWatch sw = new StopWatch(m_logger);
			    boolean gotTimeoutException = wrapper1.callDummyComponentWithTime((timeout1Sec+5)*1000);
			    int actualTimeout1Sec = (int) sw.getLapTimeMillis()/1000;
			    assertTrue("timeout exception expected", gotTimeoutException);
			    int deviationSec = Math.abs(actualTimeout1Sec - timeout1Sec);
			    assertTrue("Expected timeout exception was thrown, but after unexpected " + actualTimeout1Sec + " ms.", deviationSec < 2);
		    } catch (CouldntPerformActionEx ex) {
			    // so that junit can display the exception trace
			    throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
		    }
		}
		// @TODO: use also DummyCompWrapper_ContainerTimeout2 and DummyCompWrapper_ContainerTimeout3
		// to check different container timeout settings.
		// This should also check the effect of variations of the system-level timeout, as described in COMP-1063/19/Jun/08 10:03 AM
		// (The magic 3 minutes even if system level is at 7 minutes. To be verified usign command line override of
		// property jacorb.connection.client.pending_reply_timeout)

       //This test will be run 2 times with different values of the system level timeout timeout using the prologue of TAT
       
	}

}
