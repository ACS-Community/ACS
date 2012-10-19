package alma.acs.nc.testsupport;

import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.AcsEventSubscriber.GenericCallback;
import alma.acs.nc.AcsEventSubscriberImplBase;
import alma.acs.util.StopWatch;
import alma.acsnc.EventDescription;


public class InMemoryNcTest
{
	@Rule 
	public TestName testName = new TestName();
	
	private AcsLogger logger;
	private ContainerServicesBase services;
	
//	/**
//	 * For compatibility with JUnit3 based TATJUnitRunner
//	 */
//	public static junit.framework.Test suite() {
//		return new JUnit4TestAdapter(EventSubscriberSmEngineTest.class);
//	}
//

	////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////// Dummy data and receivers ///////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Event base type, corresponds to IDLEntity in the world of IDL-defined event structs.
	 */
	private static class TestEventTypeBase {
		// some data..
	}
	
	private static class TestEventType1 extends TestEventTypeBase {
	}

	private static class TestEventType2 extends TestEventTypeBase {
	}

	private static class ReceivedEventInfo {
		Object eventData;
		EventDescription eventDesc;
		CollectingReceiver receiver;
		ReceivedEventInfo(Object eventData, EventDescription eventDesc, CollectingReceiver receiver) {
			this.eventData = eventData;
			this.eventDesc = eventDesc;
			this.receiver = receiver;
		}
	}
	
	
	/**
	 * Meant to be used as a single instance, to which multiple receivers dump their data
	 * so that the unit test can see all events together.
	 */
	private static interface EventCollector {
		void storeEvent(TestEventTypeBase eventData, EventDescription eventDescrip, CollectingReceiver receiver);
	}
	
	/**
	 * Collects the events received by one or more test receivers,
	 * for later verification in the tests.
	 */
	private static class StoringEventCollector implements EventCollector {
		/**
		 * Even with a ConcurrentLinkedQueue and no 'synchronized' instead of a synchronized ArrayList we would get warnings about slow receiver, e.g. 
		 * "WARNING [testThroughput] More events came in from the NC than the receiver processed. eventName= alma.acs.nc.testsupport.InMemoryNcTest$TestEventType1; numEventsDiscarded=0; logOcurrencesNumber=1.
		 */
		private final List<ReceivedEventInfo> collectedEvents = new ArrayList<ReceivedEventInfo>();
		
		@Override
		public synchronized void storeEvent(TestEventTypeBase eventData, EventDescription eventDescrip, CollectingReceiver receiver) {
			collectedEvents.add(new ReceivedEventInfo(eventData, eventDescrip, receiver));
//			System.out.println("Got an event of type " + eventData.getClass().getSimpleName() + " from receiver " + receiver);
//			new Exception().printStackTrace();
		}
		
		synchronized List<ReceivedEventInfo> getAndClearEvents() {
			List<ReceivedEventInfo> ret = new ArrayList<ReceivedEventInfo>(collectedEvents);
			collectedEvents.clear();
			return ret;
		}
	}
	
	/**
	 * Collector that allows publishers to wait until a batch of events has been collected.
	 * @see InMemoryNcTest#testConcurrentUse()
	 */
	private static class SyncingEventCollector implements EventCollector {
		final CyclicBarrier sync;
		private final int numEventsToReceivePerBatch;
		private final int numEventsToReceiveTotal;
		private long numEventsReceivedTotal;
		private long numEventsReceivedInBatch;
		
		SyncingEventCollector(int numPublishers, int numEventsToReceivePerBatch, int numEventsToReceiveTotal) {
			this.numEventsToReceivePerBatch = numEventsToReceivePerBatch;
			this.numEventsToReceiveTotal = numEventsToReceiveTotal;
			sync = new CyclicBarrier(numPublishers+1);
		}
		
		@Override
		public synchronized void storeEvent(TestEventTypeBase eventData, EventDescription eventDescrip, CollectingReceiver receiver) {
			numEventsReceivedTotal++;
			numEventsReceivedInBatch++;
			if (numEventsReceivedInBatch == numEventsToReceivePerBatch || 
				numEventsReceivedTotal == numEventsToReceiveTotal) {
//				logger.fine("Received " + numEventsReceivedTotal + ", releasing the waiting publishers...");
				try {
					sync.await(1, TimeUnit.MINUTES); 
					if (numEventsReceivedTotal == numEventsToReceiveTotal) {
						sync.await(1, TimeUnit.MINUTES); // extra call when totally done, as expected by publishers
					}
				} catch (Exception ex) {
					ex.printStackTrace();
				}
				numEventsReceivedInBatch = 0;
			}
		}
		
		/**
		 * To be called by publishers
		 */
		void awaitEventBatchReception() throws Exception {
			sync.await(1, TimeUnit.MINUTES); // timeout 1 min just to protect failing test from hanging forever
		}
		
		synchronized long getNumEventsReceivedTotal() {
			return numEventsReceivedTotal;
		}
	}

	private static class CollectingReceiver {
		protected final EventCollector collector;

		CollectingReceiver(EventCollector collector) {
			this.collector = collector;
		}
	}
	
	/**
	 * This receiver makes no sense in the analogy of IDL structs where 
	 * no event is directly of type IDLEntity and structs don't have inheritance.
	 * However it may be possible to publish instantiated event base classes in the future
	 * with other pub-sub frameworks.
	 */
	private static class TestEventReceiverBase extends CollectingReceiver 
			implements AcsEventSubscriber.Callback<TestEventTypeBase> {

		TestEventReceiverBase(EventCollector collector) {
			super(collector);
		}

		@Override
		public void receive(TestEventTypeBase eventData, EventDescription eventDescrip) {
			collector.storeEvent(eventData, eventDescrip, this);
		}
		
		@Override
		public Class<TestEventTypeBase> getEventType() {
			return TestEventTypeBase.class;
		}
	}
	
	private static class TestEventReceiver1 extends CollectingReceiver
			implements AcsEventSubscriber.Callback<TestEventType1> {

		TestEventReceiver1(EventCollector collector) {
			super(collector);
		}

		@Override
		public void receive(TestEventType1 eventData, EventDescription eventDescrip) {
			collector.storeEvent(eventData, eventDescrip, this);
		}
		@Override
		public Class<TestEventType1> getEventType() {
			return TestEventType1.class;
		}
	}
	
	private static class TestEventReceiver2 extends CollectingReceiver
			implements AcsEventSubscriber.Callback<TestEventType2>, GenericCallback {

		TestEventReceiver2(EventCollector collector) {
			super(collector);
		}

		@Override
		public void receive(TestEventType2 eventData, EventDescription eventDescrip) {
			collector.storeEvent(eventData, eventDescrip, this);
		}
		@Override
		public Class<TestEventType2> getEventType() {
			return TestEventType2.class;
		}

		@Override
		public void receiveGeneric(Object eventData, EventDescription eventDescrip) {
			collector.storeEvent((TestEventTypeBase)eventData, eventDescrip, this);
		}
	}
	
	////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////// Test methods /////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////

	
	@Before
	public void setUp() throws Exception {
		String testMethodName = testName.getMethodName();
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(testMethodName, false);
		services = new DummyContainerServicesBase(testMethodName, logger);
	}

	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Tests that events get delivered correctly, 
	 * using various typed and generic subscriptions.
	 * <p>
	 * @TODO: Test suspend /resume
	 */
	@Test
	public void testSubscriptions() throws Exception {
		InMemoryNcFake nc = new InMemoryNcFake(services, "myTestChannel");

		// create subscribers and register typed receivers
		
		AcsEventSubscriber<TestEventType1> sub1 = nc.createSubscriber("myTestSubscriber1", TestEventType1.class);
		sub1.startReceivingEvents();
		AcsEventSubscriber<TestEventType2> sub2 = nc.createSubscriber("myTestSubscriber2", TestEventType2.class);
		sub2.startReceivingEvents();
		AcsEventSubscriber<TestEventTypeBase> subBase = nc.createSubscriber("myTestSubscriberBase", TestEventTypeBase.class);
		subBase.startReceivingEvents();

		StoringEventCollector eventCollector = new StoringEventCollector();
		
		TestEventReceiver1 rec1 = new TestEventReceiver1(eventCollector);
		TestEventReceiver2 rec2 = new TestEventReceiver2(eventCollector);
		TestEventReceiverBase recBaseBase = new TestEventReceiverBase(eventCollector);
		TestEventReceiver1 recBase1 = new TestEventReceiver1(eventCollector);
		TestEventReceiver2 recBase2 = new TestEventReceiver2(eventCollector);
		
		sub1.addSubscription(rec1);			// subscriber 1 subscribes only to event 1
		sub2.addSubscription(rec2);			// subscriber 2 subscribes only to event 2
		subBase.addSubscription(recBase1);	// subscriber base subscribes to event 1
		subBase.addSubscription(recBase2);	// subscriber base subscribes to event 2
		subBase.addSubscription(recBaseBase); // subscriber base subscribes to event base
		
		// publishers
		
		AcsEventPublisher<TestEventType1> pub1 = nc.createPublisher("myTestPublisher1", TestEventType1.class);
		AcsEventPublisher<TestEventType2> pub2 = nc.createPublisher("myTestPublisher2", TestEventType2.class);
		AcsEventPublisher<TestEventTypeBase> pubBase = nc.createPublisher("myTestPublisherBase", TestEventTypeBase.class);
		
		// event1 should be received by sub1.rec1 and subBase.recBase1
		
		TestEventType1 event1 = new TestEventType1();
		pub1.publishEvent(event1);
		Thread.sleep(100); // Wait for async delivery from subscribers to receivers
		List<ReceivedEventInfo> events = eventCollector.getAndClearEvents();
		assertThat(events.size(), equalTo(2));
		assertThat((TestEventType1)events.get(0).eventData, sameInstance(event1));
		assertThat((TestEventType1)events.get(1).eventData, sameInstance(event1));
		assertThat(Arrays.asList(events.get(0).receiver, events.get(1).receiver), 
				containsInAnyOrder((CollectingReceiver)rec1, (CollectingReceiver)recBase1));
		logger.info("OK: Received event1 by rec1 and recBase1.");
		
		
		// now add a generic subscription to sub2, which should then also receive event1,
		// in addition to the type-specific receivers.
		
		sub2.addGenericSubscription(rec2);
		pub1.publishEvent(event1);
		Thread.sleep(100); 
		events = eventCollector.getAndClearEvents();
		assertThat(events.size(), equalTo(3));
		assertThat(Arrays.asList(events.get(0).receiver, events.get(1).receiver, events.get(2).receiver), 
				containsInAnyOrder((CollectingReceiver)rec1, (CollectingReceiver)rec2, (CollectingReceiver)recBase1));
		logger.info("OK: Received event1 by rec1 and recBase1 (typed) and by rec2 (generic)");
		
		
		// remove the typed subscriptions to event1 and also recBase2.event2, 
		// and check that sub2.event2 is received only once, in spite of generic subscription
		
		sub1.removeSubscription(TestEventType1.class);
		subBase.removeSubscription(TestEventType1.class);
		subBase.removeSubscription(TestEventType2.class);
		TestEventType2 event2 = new TestEventType2();
		pub2.publishEvent(event2);
		Thread.sleep(100); 
		events = eventCollector.getAndClearEvents();
		assertThat(events.size(), equalTo(1));
		assertThat(events.get(0).receiver, sameInstance((CollectingReceiver)rec2));
		logger.info("OK: Received event2 by rec2.");

		
		// One subscriber gets two events, one through typed and one through generic subscription.
		// event2 and eventBase should be received by sub2
		
		sub1.disconnect();
		subBase.removeSubscription(TestEventTypeBase.class);
		TestEventTypeBase eventBase = new TestEventTypeBase();
		pub2.publishEvent(event2);
		pubBase.publishEvent(eventBase);
		Thread.sleep(100); 
		events = eventCollector.getAndClearEvents();
		assertThat(events.size(), equalTo(2));
		assertThat(Arrays.asList(events.get(0).eventData, events.get(1).eventData), 
				containsInAnyOrder((Object)event2, (Object)eventBase));
		assertThat(events.get(0).receiver, sameInstance((CollectingReceiver)rec2));
		assertThat(events.get(1).receiver, sameInstance((CollectingReceiver)rec2));
		logger.info("OK: received event2 and eventData in rec2 after disconnecting / unsubscribing other subscribers.");
	}

	
	/**
	 * Heavy-duty test to check for concurrency problems
	 * and to do basic verification of throughput performance (which is limited by having a single receiver).
	 * <p>
	 * We want to test also the asynchronous event processing in AcsEventSubscriberImplBase, 
	 * but then must throttle the publishers so that the subscribers don't lose data.
	 * Still we want the publishers to fire fast enough so that the subscribers get stressed at times.
	 * This is achieved by letting the publishers fire batches of events at maximum speed,
	 * but then wait for the entire batch to be received by the registered receiver class.
	 * These pulses of events are calculated to at most fill up the subscriber queue completely,
	 * which means that we may get warnings about slow receivers ("More events came in from the NC than the receiver processed"), 
	 * but still no data should be lost ("numEventsDiscarded=0").
	 */
	@Test
	public void testConcurrentUse() throws Exception {
		InMemoryNcFake nc = new InMemoryNcFake(services, "myTestChannel");
		
		final int numEventsPerPublisher = 2000;
		final int numPublishers = 5;
		final int numEventsPublishedTotal = numEventsPerPublisher * numPublishers;
		final int numActiveSubscribers = 5;
		final int numInactiveSubscribers = 2;
		final int numEventsToReceiveTotal = numEventsPublishedTotal * numActiveSubscribers;
		final int eventBatchSize = Math.min(numEventsPerPublisher, AcsEventSubscriberImplBase.EVENT_QUEUE_CAPACITY / numPublishers);
		assertThat("Current choice of test parameters leads to illegal batch size.", eventBatchSize, greaterThanOrEqualTo(1)); 
		final int numEventsToReceivePerBatch = eventBatchSize * numPublishers * numActiveSubscribers;
		logger.info("Will use " + numPublishers + " publishers to each publish " + numEventsPerPublisher + 
						" events (in batches of " + eventBatchSize + " each synchronized with the receivers), and " +
						numActiveSubscribers + " subscribers for these events. In addition we have " + numInactiveSubscribers +
						" subscribers that should not receive these events.");
		
		StopWatch sw = new StopWatch(logger);
		
		// set up publishers (unlike above we do it before the subscribers, just to make sure that works as well)
		List<InMemoryPublisher<TestEventType1>> publishers = new ArrayList<InMemoryPublisher<TestEventType1>>(numPublishers);
		for (int i = 1; i <= numPublishers; i++) {
			AcsEventPublisher<TestEventType1> pub = nc.createPublisher("myTestPublisher"+i, TestEventType1.class);
			publishers.add((InMemoryPublisher)pub);
		}
		sw.logLapTime("create " + numPublishers + " publishers");
		
		
		// set up subscribers
		
		final SyncingEventCollector eventCollector = new SyncingEventCollector(numPublishers, numEventsToReceivePerBatch, numEventsToReceiveTotal);
		TestEventReceiver1 sharedReceiver = new TestEventReceiver1(eventCollector);
		List<AcsEventSubscriber<?>> subscribers = new ArrayList<AcsEventSubscriber<?>>(numActiveSubscribers);
		for (int i = 1; i <= numActiveSubscribers; i++) {
			AcsEventSubscriber<TestEventType1> sub = nc.createSubscriber("myTestSubscriber"+i, TestEventType1.class);
			subscribers.add(sub);
			sub.addSubscription(sharedReceiver);
			sub.startReceivingEvents();
		}
		for (int i = 1; i <= numInactiveSubscribers; i++) {
			// make the inactive subscribers a mix of subscribers for the right event but disconnected, and subscribers for a different event
			
			if (i % 2 == 0) {
				AcsEventSubscriber<TestEventType1> sub = nc.createSubscriber("myInactiveTestSubscriber"+i, TestEventType1.class);
				subscribers.add(sub);
				sub.addSubscription(sharedReceiver);
				// do not call sub.startReceivingEvents() for this inactive subscriber
			}
			else {
				AcsEventSubscriber<TestEventType2> sub = nc.createSubscriber("myTestSubscriber"+i, TestEventType2.class);
				subscribers.add(sub);
				sub.startReceivingEvents();
			}
		}
		sw.logLapTime("create " + (numActiveSubscribers + numInactiveSubscribers) + " subscribers");
		
		
		// Publish and receive "event1" as specified above
		
		final TestEventType1 event1 = new TestEventType1();
		final List<Throwable> asyncThrowables = Collections.synchronizedList(new ArrayList<Throwable>());
		final CountDownLatch synchOnPublishers = new CountDownLatch(numPublishers);
		
		class PublisherRunnable implements Runnable {
			private final InMemoryPublisher<TestEventType1> publisher;
			PublisherRunnable(InMemoryPublisher<TestEventType1> publisher) {
				this.publisher = publisher;
			}
			@Override
			public void run() {
				for (int i = 1; i <= numEventsPerPublisher; i++) {
					try {
						publisher.publishEvent(event1);
						if (i % eventBatchSize == 0) {
							awaitEventReception();
						}
					} catch (Exception ex) {
						asyncThrowables.add(ex);
					}
				}
				// test getEventCount()
				if (publisher.getEventCount() != numEventsPerPublisher) {
					asyncThrowables.add(new Exception("Published only " + publisher.getEventCount() + " events when " + numEventsPerPublisher + " were expected."));
				}
				try {
					publisher.disconnect();
				} catch (AcsJIllegalStateEventEx ex) {
					asyncThrowables.add(ex);
				}
				// the last batch may be smaller than eventBatchSize, so that we need to sync on their reception with this extra call
				awaitEventReception();
				synchOnPublishers.countDown();
			}
			private void awaitEventReception() {
				try {
//					StopWatch swWait = new StopWatch(logger);
					eventCollector.awaitEventBatchReception();
//					logger.fine("Publisher in thread " + Thread.currentThread().getName() + " returned from awaitEventBatchReception() in " + swWait.getLapTimeMillis() + " ms.");
				} catch (Exception ex) {
					asyncThrowables.add(ex);
				}
			}
		}
		
		// let each publisher fire its events from a separate thread
		for (InMemoryPublisher<TestEventType1> publisher : publishers) {
			services.getThreadFactory().newThread(new PublisherRunnable(publisher)).start();
		}
		
		// wait for publishers to fire all events (which includes already their waiting for event reception)
		assertThat(synchOnPublishers.await(1, TimeUnit.MINUTES), is(true));
		
		// verify results
		assertThat(asyncThrowables, is(empty()));
		assertThat(eventCollector.getNumEventsReceivedTotal(), equalTo((long)numEventsToReceiveTotal));
		
		sw.logLapTime("publish " + numEventsPublishedTotal + " and receive " + numEventsToReceiveTotal + " events");
	}
}
