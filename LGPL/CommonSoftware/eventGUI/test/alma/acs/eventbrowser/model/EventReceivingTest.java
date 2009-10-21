package alma.acs.eventbrowser.model;

import alma.acs.eventbrowser.Application;
import junit.framework.TestCase;

public class EventReceivingTest extends TestCase {

	private static final int EVENTS_TO_SEND = 10000;
	private EventModel em;
	private AdminConsumer consumer = null;
	private EventSupplierImpl supplier;
	private ContainerServices cs;
	private AcsLogger logger;

	public EventReceivingTest(String name) {
		super(name);
		try {
			em = EventModel.getInstance();
			cs = em.getContainerServices();
			logger = cs.getLogger();
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		try {
			consumer = em.getAdminConsumer("blar");
			consumer.consumerReady();
		} catch (AcsJException e) {
			e.printStackTrace();
			fail();
		}
		try {
			supplier = new EventSupplierImpl(logger, cs, "EventReceivingTest");
		} catch (Exception e) {
			e.printStackTrace();
			fail();
		}
		try {
			supplier.initialize(cs);
		} catch (ComponentLifecycleException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			fail();
		}
	}

	@Override
	protected void tearDown() throws Exception {
		if (consumer != null) {
			consumer.disconnect();
			consumer = null;
		}
		if (supplier != null) {
			supplier.cleanUp();
			supplier = null;
		}
		Application.equeue.clear();
		super.tearDown();
	}
	
	public void testReceiveEvents() throws InterruptedException {
		long startTime = System.currentTimeMillis();
		supplier.sendEvents((short) EVENTS_TO_SEND);
		Thread.sleep(2*EVENTS_TO_SEND);
		assertEquals(50000-EVENTS_TO_SEND, Application.equeue.remainingCapacity());
		long endTime = System.currentTimeMillis();
		long diff = endTime - startTime;
		logger.info("Time to send "+EVENTS_TO_SEND+" events was "+diff+" ms.");
	}

}
