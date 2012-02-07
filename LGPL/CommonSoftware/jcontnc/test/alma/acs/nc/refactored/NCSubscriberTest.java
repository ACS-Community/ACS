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

package alma.acs.nc.refactored;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.omg.CORBA.portable.IDLEntity;

import alma.ADMINTEST1.statusBlockEvent1;
import alma.ADMINTEST2.statusBlockEvent2;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber.Callback;
import alma.acs.nc.AcsEventSubscriber.GenericCallback;
import alma.acs.nc.CannotAddSubscriptionException;
import alma.acs.nc.SubscriptionNotFoundException;
import alma.acsnc.EventDescription;

/**
 * Test class for the new generation of NC Subscribers.
 * 
 * @author rtobar
 */
@SuppressWarnings("deprecation")
public class NCSubscriberTest extends ComponentClientTestCase {

	private static String CHANNEL_NAME = "pink-floyd";
	private NCSubscriber m_subscriber;
	
	/**
	 * We'll use this publisher to publish events of different types
	 * (statusBlockEvent1 and statusBlockEvent2).
	 * Thus we cannot parametrize it to any one of these types, but have to use the 
	 * generic base type IDLEntity or Object.
	 */
	private AcsEventPublisher<IDLEntity> m_publisher;

	private enum EventType {
		statusBlock1,
		statusBlock2
	};

	public NCSubscriberTest() throws Exception {
		super("NCSubscriberTest");
	}

	public void setUp() throws Exception {
		super.setUp();
		m_publisher = getContainerServices().createNotificationChannelPublisher(CHANNEL_NAME, IDLEntity.class);
		m_subscriber = (NCSubscriber)getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);

		// This is the all-exclusive filter
		assertEquals(1, m_subscriber.proxySupplier.get_all_filters().length);
	}


	public void tearDown() throws Exception {
		m_publisher.disconnect();
		super.tearDown();
	}

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
		} catch(CannotAddSubscriptionException e) { }

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
		} catch(CannotAddSubscriptionException e) { }

		// Several receiver subscriptions for the same type overwrite the previous one
		m_subscriber.addSubscription(new EventReceiver1());
		m_subscriber.addSubscription(new EventReceiver1());
		m_subscriber.addSubscription(new EventReceiver1());
		m_subscriber.addSubscription(new EventReceiver1());
		assertEquals(1, m_subscriber.subscriptionsFilters.size());
		assertEquals(1, m_subscriber.receivers.size());
		assertNull(m_subscriber.genericReceiver);
		assertEquals(2, m_subscriber.proxySupplier.get_all_filters().length);

		m_subscriber.addSubscription(new EventReceiver2());
		m_subscriber.addSubscription(new EventReceiver2());
		m_subscriber.addSubscription(new EventReceiver2());
		m_subscriber.addSubscription(new EventReceiver2());
		assertEquals(2, m_subscriber.subscriptionsFilters.size());
		assertEquals(2, m_subscriber.receivers.size());
		assertNull(m_subscriber.genericReceiver);
		assertEquals(3, m_subscriber.proxySupplier.get_all_filters().length);

		m_subscriber.addGenericSubscription(new GenericEventReceiver());
		m_subscriber.addGenericSubscription(new GenericEventReceiver());
		m_subscriber.addGenericSubscription(new GenericEventReceiver());
		m_subscriber.addGenericSubscription(new GenericEventReceiver());
		assertEquals(3, m_subscriber.subscriptionsFilters.size());
		assertEquals(2, m_subscriber.receivers.size());
		assertNotNull(m_subscriber.genericReceiver);
		assertEquals(4, m_subscriber.proxySupplier.get_all_filters().length);

	}

	public void testRemoveSubscription() throws Exception {

		// Invalid removals, then subscription //

		// event 1
		try {
			m_subscriber.removeSubscription(statusBlockEvent1.class);
			fail("Should fail because we don't have a subscription yet to statusBlockEvent1");
		} catch (SubscriptionNotFoundException e) { }
		m_subscriber.addSubscription(new EventReceiver1());

		// event 2
		try {
			m_subscriber.removeSubscription(statusBlockEvent2.class);
			fail("Should fail because we don't have a subscription yet to statusBlockEvent2");
		} catch (SubscriptionNotFoundException e) { }
		m_subscriber.addSubscription(new EventReceiver2());

		// generic
		try {
			m_subscriber.removeGenericSubscription();
			fail("Should fail because we don't have a generic subscription");
		} catch(SubscriptionNotFoundException e) {}
		m_subscriber.addGenericSubscription(new GenericEventReceiver());

		// Now we can safely remove all subscriptions
		m_subscriber.removeGenericSubscription();
		m_subscriber.removeSubscription(statusBlockEvent1.class);
		m_subscriber.removeSubscription(statusBlockEvent2.class);
		assertEquals(0, m_subscriber.subscriptionsFilters.size());
		assertEquals(0, m_subscriber.receivers.size());
		assertNull(m_subscriber.genericReceiver);

		// After we remove all subscriptions, only the all-exclusive filter is set in the server
		assertEquals(1, m_subscriber.proxySupplier.get_all_filters().length);

		// Trying to remove subscriptions should fail now
		try {
			m_subscriber.removeSubscription(statusBlockEvent1.class);
			fail("Should fail because we don't have a subscription yet to statusBlockEvent1");
		} catch (SubscriptionNotFoundException e) { }
		try {
			m_subscriber.removeSubscription(statusBlockEvent2.class);
			fail("Should fail because we don't have a subscription yet to statusBlockEvent2");
		} catch (SubscriptionNotFoundException e) { }

	}

	public void testSubscriptionbyEventTypeReceiving() throws Exception {

		int nEvents = 10;

		// Simple case: 1 receiver per event type, publisher publishes same amount of events for each type
		CountDownLatch c1 = new CountDownLatch(nEvents);
		CountDownLatch c2 = new CountDownLatch(nEvents);
		m_subscriber.addSubscription(new EventReceiver1(c1));
		m_subscriber.addSubscription(new EventReceiver2(c2));
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		c1.await(10, TimeUnit.SECONDS);
		c2.await(10, TimeUnit.SECONDS);
		m_subscriber.disconnect();

		assertEquals(0, c1.getCount());
		assertEquals(0, c2.getCount());

		// Overriding case: 1 receiver per event type, 2nd receiver is overridden
		m_subscriber = (NCSubscriber)getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);
		c1 = new CountDownLatch(nEvents);
		c2 = new CountDownLatch(nEvents);
		CountDownLatch c3 = new CountDownLatch(nEvents);
		m_subscriber.addSubscription(new EventReceiver1(c1));
		m_subscriber.addSubscription(new EventReceiver2(c2));
		m_subscriber.addSubscription(new EventReceiver2(c3));
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		c1.await(10, TimeUnit.SECONDS);
		c2.await(10, TimeUnit.SECONDS);
		c3.await(10, TimeUnit.SECONDS);
		m_subscriber.disconnect();

		assertEquals(0,  c1.getCount());
		assertEquals(10, c2.getCount());
		assertEquals(0,  c3.getCount());

		// Overriding case 2: 1 receiver per event type, 2nd receiver is overridden two times, but second time is invalid
		m_subscriber = (NCSubscriber)getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);
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
		} catch(CannotAddSubscriptionException e) { }
		m_subscriber.startReceivingEvents();

		publish(nEvents, EventType.statusBlock1);
		publish(nEvents, EventType.statusBlock2);
		c1.await(10, TimeUnit.SECONDS);
		c2.await(10, TimeUnit.SECONDS);
		c3.await(10, TimeUnit.SECONDS);
		m_subscriber.disconnect();

		assertEquals(0,  c1.getCount());
		assertEquals(0,  c2.getCount());
		assertEquals(nEvents, c3.getCount());

	}

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
		m_subscriber = (NCSubscriber)getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);
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
		m_subscriber = (NCSubscriber)getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);
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
		m_subscriber = (NCSubscriber)getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);
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

	public void testLifecycle() throws Exception {

		// We're totally disconnected, try to do illegal stuff
		try {
			m_subscriber.suspend();
			fail("suspend() should fail, as we're not yet connected");
		} catch(IllegalStateException e) { }

		try {
			m_subscriber.resume();
			fail("resume() should fail, as we're not yet connected");
		} catch(IllegalStateException e) { }

		try {
			m_subscriber.disconnect();
			fail("disconnect() should fail, as we're not yet connected");
		} catch(IllegalStateException e) { }

		// We need to create it again, since after disconnect() the object is not usable anymore
		m_subscriber = (NCSubscriber)getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);
		m_subscriber.startReceivingEvents();
		assertFalse(m_subscriber.isDisconnected());

		// Now we're connected, try to do illegal stuff
		try {
			m_subscriber.startReceivingEvents();
			fail("startReceivingEvents() should fail, as we're already connected");
		} catch(IllegalStateException e) { }

		try {
			m_subscriber.resume();
			fail("resume() should fail, as we're not suspended yet");
		} catch(IllegalStateException e) { }

		m_subscriber.suspend();

		// Now we're suspended, try to do illegal stuff
		try {
			m_subscriber.suspend();
			fail("suspend() should fail, as we're already suspended");
		} catch (IllegalStateException e) { }

		try {
			m_subscriber.startReceivingEvents();
			fail("startReceivingEvents() should fail, as we're already connected");
		} catch (IllegalStateException e) { }

		m_subscriber.resume();
		m_subscriber.disconnect();
		assertTrue(m_subscriber.isDisconnected());

		// Now we're disconnected, try to do illegal stuff
		try {
			m_subscriber.disconnect();
			fail("disconnect() should fail, as we're already disconnected");
		} catch (IllegalStateException e) { }

		try {
			m_subscriber.suspend();
			fail("suspend() should fail, as we're already disconnected");
		} catch (IllegalStateException e) { }

		try {
			m_subscriber.resume();
			fail("resume() should fail, as we're already disconnected");
		} catch (IllegalStateException e) { }

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

	private class EventReceiver1 extends EventReceiverWithCounter implements Callback {

		public EventReceiver1() {
			super(null);
		}

		public EventReceiver1(CountDownLatch c) {
			super(c);
		}

		public void receive(Object event, EventDescription eventDescrip) {
			super.receive(eventDescrip);
		}

		public Class<statusBlockEvent1> getEventType() {
			return statusBlockEvent1.class;
		}

	}

	private class EventReceiver2 extends EventReceiverWithCounter implements Callback {

		public EventReceiver2() {
			super(null);
		}

		public EventReceiver2(CountDownLatch c) {
			super(c);
		}

		public void receive(Object event, EventDescription eventDescrip) {
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

		public void receive(Object event, EventDescription eventDescrip) {
			super.receive(eventDescrip);
		}
		
	}
}