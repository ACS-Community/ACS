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
import java.util.logging.Logger;

import org.omg.CORBA.portable.IDLEntity;

import alma.ADMINTEST1.statusBlockEvent1;
import alma.ADMINTEST2.statusBlockEvent2;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acsnc.EventDescription;

/**
 * This test class aims to <b>investigate</b> how reuse the Admin Object. Also
 * the filtering capabilities are checked.
 * 
 * @author jslopez
 * 
 */
public class NCSubscriberTest extends ComponentClientTestCase {
	private NCSubscriberDirect subscriber = null;
	private NCSubscriberDirect otherSubscriber = null;

	private ContainerServices services = null;

	private Logger logger = null;

	public NCSubscriberTest(String name) throws Exception {
		super(name);
	}
	
	/**
	 * This test creates a Suscriber and add all event types giving a proper
	 * generic receiver callback.
	 */
	public void testGenericCallback() throws Exception {
		subscriber = new NCSubscriberDirect("testingChannel", services);
		assertNotNull("Construction of NCSubscriber failed.", subscriber);

		int numExpectedEvents = 1;
		CountDownLatch counterGeneric = new CountDownLatch(numExpectedEvents);
		
		GenericReceiver genericReceiver = new GenericReceiver(counterGeneric);

		subscriber.addGenericSubscription(genericReceiver);
		subscriber.startReceivingEvents();
		
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterGeneric.await(10, TimeUnit.SECONDS));
	}

	/**
	 * This test creates Suscriber and add all event types giving a proper
	 * generic receiver callback and then add a particular subscription. The
	 * goal of this test is to check the priority on the receiver handler.
	 */
	public void testGenericCallbackWithPriority() throws Exception {
		subscriber = new NCSubscriberDirect("testingChannel", services);
		assertNotNull("Construction of NCSubscriber failed.", subscriber);

		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterGeneric = new CountDownLatch(numExpectedEvents);
		
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		GenericReceiver genericReceiver = new GenericReceiver(counterGeneric);

		subscriber.addSubscription(statusBlockEvent1.class, e1Receiver);
		subscriber.addGenericSubscription(genericReceiver);
		subscriber.startReceivingEvents();
		
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE1.await(10, TimeUnit.SECONDS));
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterGeneric.await(10, TimeUnit.SECONDS));
	}

	/**
	 * This test aims to test the add/remove of the generic handler.
	 */
	public void testWithGenericSubscription() throws Exception {
		subscriber = new NCSubscriberDirect("testingChannel", services);
		assertNotNull("Construction of NCSubscriber failed.", subscriber);

		int numExpectedEvents = 1;
		CountDownLatch counterGeneric = new CountDownLatch(numExpectedEvents);
		
		GenericReceiver genericReceiver = new GenericReceiver(counterGeneric);

		subscriber.addGenericSubscription(genericReceiver);
		subscriber.startReceivingEvents();
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterGeneric.await(10, TimeUnit.SECONDS));
		subscriber.removeGenericSubscription();
		subscriber.addGenericSubscription(genericReceiver);
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterGeneric.await(10, TimeUnit.SECONDS));
	}

	/**
	 * This test aims to determinate the behavior of using specific
	 * subscriptions and a generic one combined.
	 */
	public void testCombinedSubscriptions() throws Exception {
		subscriber = new NCSubscriberDirect("testingChannel", services);
		assertNotNull("Construction of NCSubscriber failed.", subscriber);

		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterE2 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterGeneric = new CountDownLatch(numExpectedEvents);
		
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		E2Receiver e2Receiver = new E2Receiver(counterE2);
		GenericReceiver genericReceiver = new GenericReceiver(counterGeneric);
		
		subscriber.addSubscription(statusBlockEvent1.class, e1Receiver);
		subscriber.addSubscription(statusBlockEvent2.class, e2Receiver);
		subscriber.addGenericSubscription(genericReceiver);
		subscriber.startReceivingEvents();
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE1.await(10, TimeUnit.SECONDS));
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE2.await(10, TimeUnit.SECONDS));
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterGeneric.await(10, TimeUnit.SECONDS));
		subscriber.removeGenericSubscription();
		subscriber.removeSubscription(statusBlockEvent1.class);
		subscriber.addSubscription(statusBlockEvent1.class, e1Receiver);
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE1.await(10, TimeUnit.SECONDS));
	}

	/**
	 * This tests check the behavior of calling the removeSubscription with null
	 * as parameter.
	 */

	public void testRemoveAllSubscription() throws Exception {
		subscriber = new NCSubscriberDirect("testingChannel", services);
		assertNotNull("Construction of NCSubscriber failed.", subscriber);
		
		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterE2 = new CountDownLatch(numExpectedEvents);
		
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		E2Receiver e2Receiver = new E2Receiver(counterE2);

		subscriber.addSubscription(statusBlockEvent1.class, e1Receiver);
		subscriber.addSubscription(statusBlockEvent2.class, e2Receiver);
		subscriber.startReceivingEvents();
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE1.await(10, TimeUnit.SECONDS));
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE2.await(10, TimeUnit.SECONDS));
		subscriber.removeSubscription(null);
		subscriber.addSubscription(statusBlockEvent1.class, e1Receiver);
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE1.await(10, TimeUnit.SECONDS));

	}
	
	/**
	 * This test checks the removeGenericSubscription method.
	 */
	public void testDisconnect() throws Exception {
		subscriber = new NCSubscriberDirect("testingChannel", services);
		assertNotNull("Construction of NCSubscriber failed.", subscriber);
		
		int numExpectedEvents = 5;
		CountDownLatch counterGeneric = new CountDownLatch(numExpectedEvents);
		GenericReceiver genericReceiver = new GenericReceiver(counterGeneric);
		
		try {
			subscriber.removeGenericSubscription();
			fail("Expected IllegalStateException because no generic subscription added before.");
		} catch(IllegalStateException expected) {
			
		}
		
		subscriber.addGenericSubscription(genericReceiver);
		subscriber.startReceivingEvents();
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterGeneric.await(10, TimeUnit.SECONDS));
		
		subscriber.disconnect();
		
	}
	
	/** 
	 * This test checks the behavior of concurrent subscriber clients.
	 */
	public void testMultipleSubscribers() throws Exception {
		subscriber = new NCSubscriberDirect("testingChannel", services);
		assertNotNull("Construction of NCSubscriber failed.", subscriber);
		otherSubscriber = new NCSubscriberDirect("testingChannel", services);
		assertNotNull("Construction of NCSubscriber failed.", otherSubscriber);

		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterE2 = new CountDownLatch(numExpectedEvents);
		
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		E2Receiver e2Receiver = new E2Receiver(counterE2);
		
		subscriber.addSubscription(statusBlockEvent1.class, e1Receiver);
		otherSubscriber.addSubscription(statusBlockEvent2.class, e2Receiver);
		subscriber.startReceivingEvents();
		otherSubscriber.startReceivingEvents();
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE1.await(10, TimeUnit.SECONDS));
		subscriber.disconnect();
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE2.await(10, TimeUnit.SECONDS));
		otherSubscriber.disconnect();
	}

	/**
	 * This test checks the behavior of the current subscriber client
	 * concurrently with the old API.
	 */
	public void testOldSubscriber() throws Exception {
		subscriber = new NCSubscriberDirect("testingChannel", services);
		assertNotNull("Construction of NCSubscriber failed.", subscriber);
		
		Consumer oldConsumer = null;
		oldConsumer = new Consumer("testingChannel", services);
		assertNotNull("Construction of Consumer failed.", oldConsumer);
		
		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		
		subscriber.addSubscription(statusBlockEvent1.class, e1Receiver);
		subscriber.startReceivingEvents();
		
		oldConsumer.addSubscription(statusBlockEvent1.class);
		oldConsumer.consumerReady();
	}
	
	/**
	 * Getting common references.
	 * 
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		services = getContainerServices();
		logger = services.getLogger();
		logger.info("Creating subscriber");
	}
}

/**
 * This class overload some methods for testing purposes.
 * 
 * @author jslopez
 */
final class NCSubscriberDirect extends NCSubscriber {
	NCSubscriberDirect(String channelName, ContainerServices services)
			throws AcsJException {
		super(channelName, services);
	}

	/**
	 * Overloaded version that doesn't require the callback. Commented, since is
	 * no longer used. (Can be used again for testing purposes).
	 * 
	 * @see alma.acs.nc.refactored.NCSubscriber#push_structured_event(org.omg.CosNotification.StructuredEvent)
	 */
//	public void push_structured_event(StructuredEvent structuredEvent)
//	throws Disconnected {
//		EventDescription eDescrip = EventDescriptionHelper
//		.extract(structuredEvent.remainder_of_body);
//
//		System.out.println("Channel:" + channelName + ", Publisher:"
//				+ eDescrip.name + ", Event Type:"
//				+ structuredEvent.header.fixed_header.event_type.type_name);
//	}
}

/**
 * This class implements the receiver handler for statusBlockEvent1.
 */
class E1Receiver implements AcsEventSubscriber.Callback<statusBlockEvent1> {	
	private final CountDownLatch counter;
	
	E1Receiver(CountDownLatch counter) {
		this.counter = counter;
	}
	public void receive(statusBlockEvent1 event, EventDescription eDescrip) {
		System.out.println("Inside receive method 1: " + event.myString
				+ " : " + eDescrip.name);
		
		counter.countDown();
		
		// Little hack to force the maxProcessTime warning
		try {
			Thread.sleep(1000 * 0);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
};

/**
 * This class implements the receiver handler for statusBlockEvent2.
 */
class E2Receiver implements AcsEventSubscriber.Callback<statusBlockEvent2> {	
	private final CountDownLatch counter;
	
	E2Receiver(CountDownLatch counter) {
		this.counter = counter;
	}
	public void receive(statusBlockEvent2 event, EventDescription eDescrip) {
		System.out.println("Inside receive method 2: " + event.myString
				+ " : " + eDescrip.name);
		
		counter.countDown();
		
		// Little hack to force the maxProcessTime warning
		try {
			Thread.sleep(1000 * 0);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
};

/**
 * This class implements the generic receiver handler.
 */
class GenericReceiver implements AcsEventSubscriber.GenericCallback {
	private final CountDownLatch counter;
	
	GenericReceiver(CountDownLatch counter) {
		this.counter = counter;
	}

	public void receive(IDLEntity event, EventDescription eventDescrip) {
		System.out.println("Inside GENERIC receive method: " + " : "
				+ eventDescrip.name);
		
		counter.countDown();
		
		// Little hack to force the maxProcessTime warning
		try {
			Thread.sleep(1000 * 0);
		} catch (InterruptedException e) {
			e.printStackTrace();		
		}
	}
}