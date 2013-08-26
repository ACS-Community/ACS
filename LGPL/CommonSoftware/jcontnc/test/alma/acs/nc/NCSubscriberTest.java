/*
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2009 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.acs.nc;

import static org.hamcrest.Matchers.empty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.omg.CORBA.portable.IDLEntity;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ADMINTEST1.statusBlockEvent1;
import alma.ADMINTEST2.statusBlockEvent2;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.NCSubscriber;
import alma.acs.nc.AcsEventSubscriber.Callback;
import alma.acs.nc.AcsEventSubscriber.GenericCallback;
import alma.acs.nc.NCSubscriber.NoEventReceiverListener;
import alma.acs.util.AcsLocations;
import alma.acsErrTypeLifeCycle.wrappers.AcsJEventSubscriptionEx;
import alma.acsnc.EventDescription;

/**
 * Tests for NCSubscriber.
 * 
 * @author rtobar, hsommer
 */
public class NCSubscriberTest extends ComponentClient {


	private static String CHANNEL_NAME = "pink-floyd";
	
	
	/**
	 * The shared subscriber for the tests.
	 * For more complete testing we use the actual type, not just AcsEventSubscriber interface,
	 * and therefore need a cast in {@link #newSharedSubscriber()}.
	 */
	private NCSubscriber<IDLEntity> m_subscriber;
	
	/**
	 * We'll use this publisher to publish events of different types
	 * (statusBlockEvent1 and statusBlockEvent2).
	 * Thus we cannot parameterize it to any one of these types, but have to use the 
	 * generic base type IDLEntity or Object.
	 */
	private AcsEventPublisher<IDLEntity> m_publisher;

	/**
	 * Test event types, to be used when calling {@link #publish(int, EventType)}.
	 */
	private enum EventType {
		statusBlock1,
		statusBlock2
	}

	public NCSubscriberTest() throws Exception {
		super(null, AcsLocations.figureOutManagerLocation(), NCSubscriberTest.class.getSimpleName());
	}

	/**
	 * TODO: Check if this rule and the getMethodName() call in setUp() can be moved up to ComponentClient,
	 *      if that adds a runtime dependency on junit, and how bad that would be.
	 *      Probably we should add a class ComponentClientTestCaseJUnit4 that extends ComponentClient
	 *      and only adds this testname business.
	 */
	@Rule 
	public TestName testName = new TestName();
	

	@Before
	public void setUp() throws Exception {
		String testMethodName = testName.getMethodName();
		m_logger.info("----------------- " + testMethodName + " ----------------- ");
		
		m_publisher = getContainerServices().createNotificationChannelPublisher(CHANNEL_NAME, IDLEntity.class);
		newSharedSubscriber();

		// This is the all-exclusive filter
		assertEquals(1, m_subscriber.proxySupplier.get_all_filters().length);
	}

	@After
	public void tearDown() throws Exception {
		m_publisher.disconnect();
		if (m_subscriber != null && !m_subscriber.isDisconnected()) {
			m_subscriber.disconnect();
		}
		super.tearDown();
	}


	/**
	 * Creates an NCSubscriber and stores it in {@link #m_subscriber}.
	 * If we already had a subscriber, that one gets disconnected first.
	 */
	private void newSharedSubscriber() throws AcsJContainerServicesEx, AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		if (m_subscriber != null && !m_subscriber.isDisconnected()) {
			m_subscriber.disconnect();
		}
		m_subscriber = (NCSubscriber<IDLEntity>)getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME, IDLEntity.class);
	}


	/**
	 */
	@Test
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void testAddSubscription() throws Exception {

		// Invalid receiver (returns null)
		try {
			m_subscriber.addSubscription(new EventReceiver1() {
				public Class<statusBlockEvent1> getEventType() {
					return null;
				}	
			});
			fail("Event receiver is invalid, as it returns a null event type");
		} catch(AcsJEventSubscriptionEx e) { }

		// Invalid receiver (returns String.class)
		try {
			m_subscriber.addSubscription(new Callback() {
				public void receive(Object event,
						EventDescription eventDescrip) {
				}
				public Class getEventType() {
					return String.class;
				}
			});
			fail("Event receiver is invalid, as it returns a java.lang.String as the event type");
		} catch(AcsJEventSubscriptionEx e) { }

		// Several receiver subscriptions for the same type overwrite the previous one
		m_subscriber.addSubscription(new EventReceiver1());
		m_subscriber.addSubscription(new EventReceiver1());
		m_subscriber.addSubscription(new EventReceiver1());
		m_subscriber.addSubscription(new EventReceiver1());
		assertEquals(1, m_subscriber.subscriptionsFilters.size());
		assertEquals(1, m_subscriber.getNumberOfReceivers());
		assertFalse(m_subscriber.hasGenericReceiver());
		assertEquals(2, m_subscriber.proxySupplier.get_all_filters().length);

		m_subscriber.addSubscription(new EventReceiver2());
		m_subscriber.addSubscription(new EventReceiver2());
		m_subscriber.addSubscription(new EventReceiver2());
		m_subscriber.addSubscription(new EventReceiver2());
		assertEquals(2, m_subscriber.subscriptionsFilters.size());
		assertEquals(2, m_subscriber.getNumberOfReceivers());
		assertFalse(m_subscriber.hasGenericReceiver());
		assertEquals(3, m_subscriber.proxySupplier.get_all_filters().length);

		m_subscriber.addGenericSubscription(new GenericEventReceiver());
		m_subscriber.addGenericSubscription(new GenericEventReceiver());
		m_subscriber.addGenericSubscription(new GenericEventReceiver());
		m_subscriber.addGenericSubscription(new GenericEventReceiver());
		assertEquals(3, m_subscriber.subscriptionsFilters.size());
		assertEquals(2, m_subscriber.getNumberOfReceivers());
		assertTrue(m_subscriber.hasGenericReceiver());
		assertEquals(4, m_subscriber.proxySupplier.get_all_filters().length);

	}

	@Test
	public void testRemoveSubscription() throws Exception {

		// Invalid removals, then subscription //

		// event 1
		try {
			m_subscriber.removeSubscription(statusBlockEvent1.class);
			fail("Should fail because we don't have a subscription yet to statusBlockEvent1");
		} catch (AcsJEventSubscriptionEx e) { }
		m_subscriber.addSubscription(new EventReceiver1());

		// event 2
		try {
			m_subscriber.removeSubscription(statusBlockEvent2.class);
			fail("Should fail because we don't have a subscription yet to statusBlockEvent2");
		} catch (AcsJEventSubscriptionEx e) { }
		m_subscriber.addSubscription(new EventReceiver2());

		// generic
		try {
			m_subscriber.removeGenericSubscription();
			fail("Should fail because we don't have a generic subscription");
		} catch(AcsJEventSubscriptionEx e) {}
		m_subscriber.addGenericSubscription(new GenericEventReceiver());

		// Now we can safely remove all subscriptions
		m_subscriber.removeGenericSubscription();
		m_subscriber.removeSubscription(statusBlockEvent1.class);
		m_subscriber.removeSubscription(statusBlockEvent2.class);
		assertEquals(0, m_subscriber.subscriptionsFilters.size());
		assertEquals(0, m_subscriber.getNumberOfReceivers());
		assertFalse(m_subscriber.hasGenericReceiver());

		// After we remove all subscriptions, only the all-exclusive filter is set in the server
		assertEquals(1, m_subscriber.proxySupplier.get_all_filters().length);

		// Trying to remove subscriptions should fail now
		try {
			m_subscriber.removeSubscription(statusBlockEvent1.class);
			fail("Should fail because we don't have a subscription yet to statusBlockEvent1");
		} catch (AcsJEventSubscriptionEx e) { }
		try {
			m_subscriber.removeSubscription(statusBlockEvent2.class);
			fail("Should fail because we don't have a subscription yet to statusBlockEvent2");
		} catch (AcsJEventSubscriptionEx e) { }

	}

	@Test
	public void testSubscriptionByEventTypeReceiving() throws Exception {

		final int nEvents = 10;

		// Simple case: 1 receiver per event type, publisher publishes same amount of events for each type
		CountDownLatch c1 = new CountDownLatch(nEvents);
		CountDownLatch c2 = new CountDownLatch(nEvents);
		m_subscriber.addSubscription(new EventReceiver1(c1));
		m_subscriber.addSubscription(new EventReceiver2(c2));
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		assertTrue(c1.await(10, TimeUnit.SECONDS));
		assertTrue(c2.await(10, TimeUnit.SECONDS));

		// Overriding case: 1 receiver per event type, 2nd receiver is overridden
		newSharedSubscriber();
		c1 = new CountDownLatch(nEvents);
		c2 = new CountDownLatch(nEvents);
		CountDownLatch c3 = new CountDownLatch(nEvents);
		m_subscriber.addSubscription(new EventReceiver1(c1));
		m_subscriber.addSubscription(new EventReceiver2(c2));
		m_subscriber.addSubscription(new EventReceiver2(c3));
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		assertTrue(c1.await(10, TimeUnit.SECONDS));
		assertTrue(c3.await(10, TimeUnit.SECONDS));
		assertEquals(nEvents, c2.getCount()); 

		// Overriding case 2: 1 receiver per event type, 2nd receiver is overridden two times, but second time is invalid
		newSharedSubscriber();
		c1 = new CountDownLatch(nEvents);
		c2 = new CountDownLatch(nEvents);
		c3 = new CountDownLatch(nEvents);
		m_subscriber.addSubscription(new EventReceiver1(c1));
		m_subscriber.addSubscription(new EventReceiver2(c2));
		try {
			m_subscriber.addSubscription(new EventReceiver1(c3) {
				public Class<statusBlockEvent1> getEventType() {
					return null;
				}	
			});
			fail("Event receiver is invalid, as it returns a null event type");
		} catch(AcsJEventSubscriptionEx e) { }
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		assertTrue(c1.await(10, TimeUnit.SECONDS));
		assertTrue(c2.await(10, TimeUnit.SECONDS));
		assertFalse(c3.await(10, TimeUnit.SECONDS));
		assertEquals(nEvents, c3.getCount());
		m_subscriber.disconnect();

	}

	
	@Test
	public void testSubscriptionGenericReceiving() throws Exception {

		int nEvents = 10;

		// Simple case: just the generic receiver, receiving 2 event types
		CountDownLatch c1 = new CountDownLatch(2*nEvents);
		m_subscriber.addGenericSubscription(new GenericEventReceiver(c1));
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		c1.await(10, TimeUnit.SECONDS);
		m_subscriber.disconnect();

		assertEquals(0, c1.getCount());

		// Overriding case: generic subscriber, then overridden by a different one
		newSharedSubscriber();
		c1 = new CountDownLatch(2*nEvents);
		CountDownLatch c2 = new CountDownLatch(2*nEvents);
		m_subscriber.addGenericSubscription(new GenericEventReceiver(c1));
		m_subscriber.addGenericSubscription(new GenericEventReceiver(c2));
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		c1.await(10, TimeUnit.SECONDS);
		c2.await(10, TimeUnit.SECONDS);
		m_subscriber.disconnect();

		assertEquals(2*nEvents, c1.getCount());
		assertEquals(0, c2.getCount());

		// Add/remove case: add generic subscription, then remove it, then listen: nothing should arrive
		newSharedSubscriber();
		c1 = new CountDownLatch(2*nEvents);
		m_subscriber.addGenericSubscription(new GenericEventReceiver(c1));
		m_subscriber.removeGenericSubscription();
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		c1.await(10, TimeUnit.SECONDS);
		m_subscriber.disconnect();

		assertEquals(2*nEvents, c1.getCount());

		// Mixed case: a generic receiver + receiver for event type 1
		newSharedSubscriber();
		c1 = new CountDownLatch(2*nEvents);
		c2 = new CountDownLatch(nEvents);
		m_subscriber.addGenericSubscription(new GenericEventReceiver(c1));
		m_subscriber.addSubscription(new EventReceiver2(c2));
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		c1.await(10, TimeUnit.SECONDS);
		c2.await(10, TimeUnit.SECONDS);
		m_subscriber.disconnect();

		assertEquals(10, c1.getCount()); // we set 2*nEvents as the initial count for c1
		assertEquals(0, c2.getCount());

	}

	
	/**
	 * Asserts that only subscribed events arrive at the subscriber client, 
	 * while others get filtered out already on the server.
	 * See also http://jira.alma.cl/browse/COMP-9076
	 */
	@Test
	public void testServerSideEventTypeFiltering() throws Exception {
		final int nEvents = 10;
		
		// Verify that our test finds a filter leak when it exists.
		// For that, we use one typed subscription but publish two event types.
		CountDownLatch cLeak = new CountDownLatch(nEvents);
		newSharedSubscriberWithFilterLeakCheck(null, cLeak);
		// Note that calling 'm_subscriber.proxySupplier.remove_all_filters()'
		// does not work here. Apparently after adding and removing filters, we do not return
		// to the initial "all events pass" behavior. 
		m_subscriber.addFilter("*");
		CountDownLatch c1 = new CountDownLatch(nEvents);
		m_subscriber.addSubscription(new EventReceiver1(c1));
		m_subscriber.startReceivingEvents();
		publish(nEvents, EventType.statusBlock2); // they should leak
		publish(nEvents, EventType.statusBlock1);
		assertTrue(c1.await(10, TimeUnit.SECONDS));
		assertTrue("Expecting a leak in the server-side event filter", cLeak.await(10, TimeUnit.SECONDS));
		
		// Again 1 typed subscription and 2 event types published, but without artificial filter leak
		List<String> leakedEvents = new ArrayList<String>();
		newSharedSubscriberWithFilterLeakCheck(leakedEvents, null);
		c1 = new CountDownLatch(nEvents);
		m_subscriber.addSubscription(new EventReceiver1(c1));
		m_subscriber.startReceivingEvents();
		publish(nEvents, EventType.statusBlock2); // should be filtered out
		publish(nEvents, EventType.statusBlock1);
		assertTrue(c1.await(10, TimeUnit.SECONDS));
		assertThat(leakedEvents, empty());

		// add generic subscription
		newSharedSubscriberWithFilterLeakCheck(leakedEvents, null);
		c1 = new CountDownLatch(nEvents);
		CountDownLatch c2 = new CountDownLatch(nEvents);
		m_subscriber.addSubscription(new EventReceiver1(c1));
		m_subscriber.addGenericSubscription(new GenericEventReceiver(c2));
		m_subscriber.startReceivingEvents();
		publish(nEvents, EventType.statusBlock2); 
		publish(nEvents, EventType.statusBlock1);
		assertTrue(c1.await(10, TimeUnit.SECONDS));
		assertTrue(c2.await(10, TimeUnit.SECONDS));
		assertThat(leakedEvents, empty());
		
		// remove generic subscription
		newSharedSubscriberWithFilterLeakCheck(leakedEvents, null);
		c1 = new CountDownLatch(nEvents);
		m_subscriber.addSubscription(new EventReceiver1(c1));
		m_subscriber.startReceivingEvents();
		publish(nEvents, EventType.statusBlock2); // should be filtered out
		publish(nEvents, EventType.statusBlock1);
		assertTrue(c1.await(10, TimeUnit.SECONDS));
		assertThat(leakedEvents, empty());
	}
	
	/**
	 * Sets up a hacked subscriber that reports stray events (normally they only get logged).
	 * @see #testServerSideEventTypeFiltering()
	 */
	private void newSharedSubscriberWithFilterLeakCheck(final List<String> leakedEvents, final CountDownLatch cLeak) throws Exception {
		newSharedSubscriber();
		NoEventReceiverListener filterLeakListener = new NoEventReceiverListener() {
			@Override
			public void noEventReceiver(String eventName) {
				m_logger.info("Leaked event: " + eventName);
				if (leakedEvents != null) {
					leakedEvents.add(eventName);
				}
				if (cLeak != null) {
					cLeak.countDown();
				}
			}
		};
		m_subscriber.setNoEventReceiverListener(filterLeakListener);
	}

	
	@Test
	public void testLifecycle() throws Exception {

		// We're totally disconnected, try to do illegal stuff
		try {
			m_subscriber.suspend();
			fail("suspend() should fail, as we're not yet connected");
		} catch(AcsJIllegalStateEventEx e) { }

		try {
			m_subscriber.resume();
			fail("resume() should fail, as we're not yet connected");
		} catch(AcsJIllegalStateEventEx e) { }

		// disconnect() should work, since it's supposed to be called even if we never connect.
		// After that we need a new subscriber though.
		m_subscriber.disconnect();
		newSharedSubscriber();

		m_subscriber.startReceivingEvents();
		assertFalse(m_subscriber.isDisconnected());

		// Now we're connected, try to do illegal stuff
		try {
			m_subscriber.startReceivingEvents();
			fail("startReceivingEvents() should fail, as we're already connected");
		} catch(AcsJIllegalStateEventEx e) { }

		try {
			m_subscriber.resume();
			fail("resume() should fail, as we're not suspended yet");
		} catch(AcsJIllegalStateEventEx e) { }

		m_subscriber.suspend();

		// Now we're suspended, try to do illegal stuff
		try {
			m_subscriber.suspend();
			fail("suspend() should fail, as we're already suspended");
		} catch (AcsJIllegalStateEventEx e) { }

		try {
			m_subscriber.startReceivingEvents();
			fail("startReceivingEvents() should fail, as we're already connected");
		} catch (AcsJIllegalStateEventEx e) { }

		m_subscriber.resume();
		m_subscriber.disconnect();
		assertTrue(m_subscriber.isDisconnected());

		// Now we're disconnected, try to do illegal stuff
		try {
			m_subscriber.disconnect();
			fail("disconnect() should fail, as we're already disconnected");
		} catch (AcsJIllegalStateEventEx e) { }

		try {
			m_subscriber.suspend();
			fail("suspend() should fail, as we're already disconnected");
		} catch (AcsJIllegalStateEventEx e) { }

		try {
			m_subscriber.resume();
			fail("resume() should fail, as we're already disconnected");
		} catch (AcsJIllegalStateEventEx e) { }

	}

	/*===================================*/
	/* Support methods                   */
	/*===================================*/
	private void publish(int nEvents, EventType type) {

		IDLEntity event = null;
		if( type.equals(EventType.statusBlock1) ) {
			event = new statusBlockEvent1();
			((statusBlockEvent1)event).counter1 = 0;
			((statusBlockEvent1)event).counter2 = 0;
			((statusBlockEvent1)event).flipFlop = true;
			((statusBlockEvent1)event).onOff = alma.ADMINTEST1.OnOffStates.ON;
		}
		else if( type.equals(EventType.statusBlock2) ) {
			event = new statusBlockEvent2();
			((statusBlockEvent2)event).counter1 = 0;
			((statusBlockEvent2)event).counter2 = 0;
			((statusBlockEvent2)event).flipFlop = true;
			((statusBlockEvent2)event).onOff = alma.ADMINTEST2.OnOffStates.ON;
		}

		try {
			for(int i=0; i!=nEvents; i++)
				m_publisher.publishEvent(event);
		} catch (AcsJException e) {
			e.printStackTrace();
		}
	}

	/*===================================*/
	/* Support classes                   */
	/*===================================*/

	private abstract class EventReceiverWithCounter {

		private CountDownLatch m_countDownLatch;

		public EventReceiverWithCounter(CountDownLatch c) {
			m_countDownLatch = c;
		}

		public void receive(EventDescription event) {
			if( m_countDownLatch != null )
				m_countDownLatch.countDown();
		}
	}

	/**
	 * Receives <code>statusBlockEvent1</code> events.
	 */
	private class EventReceiver1 extends EventReceiverWithCounter implements Callback<statusBlockEvent1> {

		public EventReceiver1() {
			super(null);
		}

		public EventReceiver1(CountDownLatch c) {
			super(c);
		}

		public void receive(statusBlockEvent1 event, EventDescription eventDescrip) {
			super.receive(eventDescrip);
		}

		public Class<statusBlockEvent1> getEventType() {
			return statusBlockEvent1.class;
		}
	}

	/**
	 * Receives <code>statusBlockEvent2</code> events.
	 */
	private class EventReceiver2 extends EventReceiverWithCounter implements Callback<statusBlockEvent2> {

		public EventReceiver2() {
			super(null);
		}

		public EventReceiver2(CountDownLatch c) {
			super(c);
		}

		public void receive(statusBlockEvent2 event, EventDescription eventDescrip) {
			super.receive(eventDescrip);
		}

		public Class<statusBlockEvent2> getEventType() {
			return statusBlockEvent2.class;
		}
		
	}

	private class GenericEventReceiver extends EventReceiverWithCounter implements GenericCallback {

		public GenericEventReceiver() {
			super(null);
		}

		public GenericEventReceiver(CountDownLatch c) {
			super(c);
		}

		public void receiveGeneric(Object event, EventDescription eventDescrip) {
			super.receive(eventDescrip);
		}
		
	}
}