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
import alma.acs.util.StopWatch;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentHelper;
import alma.jconttest.util.JconttestUtil;

/**
 * Tests the effect of the <code>jacorb.connection.client.pending_reply_timeout</code> property setting
 * on clients which, unlike containers, don't apply additional timeout settings from CDB or elsewhere.
 * Requires ACS runtime with a remote container.
 * 
 * @author rtobar
 */
public class ClientPendingReplyTimeoutTest extends ComponentClientTestCase {

	private static final String DUMMYCOMP_TYPENAME = "IDL:alma/jconttest/DummyComponent:1.0";

	private DummyComponent dummyComponent;
	private JconttestUtil jconttestUtil;

	public ClientPendingReplyTimeoutTest() throws Exception {
		super("ClientPendingReplyTimeoutTest");
	}
	
	protected void setUp() throws Exception {
		super.setUp();
		jconttestUtil = new JconttestUtil(getContainerServices());
	}

	protected void tearDown() throws Exception {
		m_logger.info("done, tearDown");
		super.tearDown();
	}

	
	/**
	 * Tests the client-side Corba timeout. Since this client is a JUnit test, the system-level ORB timeout should
	 * apply, regardless of any timeout setting for the container whose component we call.
	 */
	public void testClientPendingReplyTimeout() throws Exception {
		org.omg.CORBA.Object compObj = getContainerServices().getDefaultComponent(DUMMYCOMP_TYPENAME);
		assertNotNull(compObj);
		dummyComponent = DummyComponentHelper.narrow(compObj);

		int syslevelOrbTimeoutMillis = jconttestUtil.getSystemLevelOrbTimeoutMillis();
		assertEquals("system-level jacorb timeout has changed (orb.properties or cmd line), please verify that this test still works as intended!", 180000, syslevelOrbTimeoutMillis);

		StopWatch sw = new StopWatch();
		try {
			// call must last longer than the expected timeout
			dummyComponent.callThatTakesSomeTime(syslevelOrbTimeoutMillis + 10000);
			fail("Client side timeout was expected after " + syslevelOrbTimeoutMillis/1000 + " seconds!");
		} catch (org.omg.CORBA.TIMEOUT e) {
			// good, but check the time
			long elapsedMillis = sw.getLapTimeMillis();
			int deviationSec = (int) Math.abs(elapsedMillis - syslevelOrbTimeoutMillis)/1000;
			assertTrue("Expected timeout exception was thrown, but after unexpected " + elapsedMillis + " ms.", deviationSec < 2);
		}

		// This call should take no time, so no exception should be thrown
		try {
			dummyComponent.callThatTakesSomeTime(0);
		} catch (org.omg.CORBA.TIMEOUT e) {
			fail("No TIMOUT exception expected for a fast call!");
		}
	}

}
