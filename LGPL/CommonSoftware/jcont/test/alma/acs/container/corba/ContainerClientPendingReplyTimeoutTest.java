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

import alma.acs.component.client.ComponentClientTestCase;
import alma.jconttest.DummyComponentWrapper;
import alma.jconttest.DummyComponentWrapperHelper;

/**
 * Tests the <code>jacorb.connection.client.pending_reply_timeout</code> property setting for a
 * container as client.
 * Requires ACS runtime with a remote container.
 * 
 * @author rtobar
 */
public class ContainerClientPendingReplyTimeoutTest extends ComponentClientTestCase {

	private static final String DUMMYWRAPPERCOMP_TYPENAME = "IDL:alma/jconttest/DummyComponentWrapper:1.0";
	private static final String PROPERTY_NAME = "jacorb.connection.client.pending_reply_timeout";
	private DummyComponentWrapper dummyComponentWrapper;

	// simple otherthread-to-mainthread exception passing
	//private volatile Throwable exceptionInThread;

	public ContainerClientPendingReplyTimeoutTest() throws Exception {
		super("ContainerClientPendingReplyTimeoutTest");
	}

	
	protected void setUp() throws Exception {
		Thread.sleep(2000); // to make sure that logging client is up and captures all logs
		super.setUp();
	}

	protected void tearDown() throws Exception {
		m_logger.info("done, tearDown");
		super.tearDown();
	}

	
	public void testClientPendingReplyTimeout() throws Exception {
		org.omg.CORBA.Object compObj = getContainerServices().getDefaultComponent(DUMMYWRAPPERCOMP_TYPENAME);
		assertNotNull(compObj);
		dummyComponentWrapper = DummyComponentWrapperHelper.narrow(compObj);
		String compName = dummyComponentWrapper.name();
		assertNotNull(compName);
	
		// We get the property value from the ORB configuration for the component
		// Need to cast the ORB object since it declared as org.omg.orb.ORB, and it
		// doesn't have the getConfiguration() method present in org.jacorb.orb.ORB
		int timeout = -1;
		if( acsCorba.getORB() instanceof org.jacorb.orb.ORB )
			timeout = ((org.jacorb.orb.ORB)acsCorba.getORB()).getConfiguration().getAttributeAsInteger(PROPERTY_NAME);
		else
			assertTrue("The ORB corresponds to the org.jacorb.orb.ORB implementation",false);

		m_logger.info("Timeout for receiving replies is: " + timeout);

		// If timeout is > 0, then the property is set and can be tested
		// else, nothing can be proben
		if( timeout > 0 ) {
			assertTrue(!dummyComponentWrapper.callDummyComponentWithTime((int)(timeout/3)));
			assertTrue(!dummyComponentWrapper.callDummyComponentWithTime((int)(timeout/2)));
		}

		// This will make dummyComponentWrapper to call the DummyComponent#callThatTakesSomeTime
		// with the value of PROPERTY_NAME on the Container's ORB configuration. This should cause
		// a CORBA.TIMEOUT on the DummyComponent call, so here too.
		boolean timeoutException = false;
		try {
			dummyComponentWrapper.callDummyComponentWithTime(-1);
		} catch (org.omg.CORBA.TIMEOUT e) {
			timeoutException = true;
		}
		assertTrue(timeoutException);

	}
	
}
