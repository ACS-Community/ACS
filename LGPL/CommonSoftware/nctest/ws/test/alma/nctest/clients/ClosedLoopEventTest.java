package alma.nctest.clients;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.nc.SimpleSupplier;
import alma.simpleAcsTest.StructWithMyString;


/**
 * Sends various kinds of events and subscribes to form a closed roundtrip loop.
 * Compares sent and received envents.
 *  
 * @author hsommer
 */
public class ClosedLoopEventTest extends ComponentClientTestCase {

	/**
	 * The channel used for theses tests.
	 * Note that usually the channel name must be an IDL-defined string to avoid mismatches between senders and receivers.
	 */
	public static final String ADHOC_CHANNEL_NAME = "ClosedLoopEventTestChannel";
	
	
	public ClosedLoopEventTest() throws Exception {
		super("ClosedLoopEventTest");
	}
	
	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testStructWithTypedef() throws Exception {
		
		StructWithMyString event = new StructWithMyString("myRealString", "myTypedefdString");
		SimpleSupplier supplier = new SimpleSupplier(ADHOC_CHANNEL_NAME, getContainerServices());
		supplier.publishEvent(event);
	}
}
