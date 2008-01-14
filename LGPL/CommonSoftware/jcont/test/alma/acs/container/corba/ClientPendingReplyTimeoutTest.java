package alma.acs.container.corba;

import java.util.logging.Level;

import alma.acs.component.client.ComponentClientTestCase;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentHelper;

/**
 * Tests the <code>jacorb.connection.client.pending_reply_timeout</code> property setting.
 * Requires ACS runtime with a remote container.
 * 
 * @author rtobar
 */
public class ClientPendingReplyTimeoutTest extends ComponentClientTestCase {

	private static final String DUMMYCOMP_TYPENAME = "IDL:alma/jconttest/DummyComponent:1.0";
	private static final String PROPERTY_NAME = "jacorb.connection.client.pending_reply_timeout";
	private DummyComponent dummyComponent;

	// simple otherthread-to-mainthread exception passing
	//private volatile Throwable exceptionInThread;

	public ClientPendingReplyTimeoutTest() throws Exception {
		super("ClientPendingReplyTimeoutTest");
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
		org.omg.CORBA.Object compObj = getContainerServices().getDefaultComponent(DUMMYCOMP_TYPENAME);
		assertNotNull(compObj);
		dummyComponent = DummyComponentHelper.narrow(compObj);
		String compName = dummyComponent.name();
		assertNotNull(compName);
	
		// We get the property value from the ORB configuration for the component
		// Need to cast the ORB object since it declared as org.omg.orb.ORB, and it
		// doesn't have the getConfiguration() method
		int timeout = -1;
		if( acsCorba.getORB() instanceof org.jacorb.orb.ORB )
			timeout = ((org.jacorb.orb.ORB)acsCorba.getORB()).getConfiguration().getAttributeAsInteger(PROPERTY_NAME);

		m_logger.info("Timeout for receiving replies is: " + timeout);
		boolean timeoutException = false;

		// If timeout is > 0, then the property is set and can be tested
		// else, nothing can be proben
		if( timeout > 0 ) {
			try {
				dummyComponent.callThatTakesSomeTime((int)(timeout));
			} catch (org.omg.CORBA.TIMEOUT e) {
				timeoutException = true;
			}
		}
		
		// This call should take no time, so no exception should be trhown
		timeoutException = false;
		if( timeout > 0 ) {
			try {
				dummyComponent.callThatTakesSomeTime(0);
			} catch (org.omg.CORBA.TIMEOUT e) {
				timeoutException = true;
			}
			assertTrue(!timeoutException);
		}
	}
	
}
