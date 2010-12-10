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

import org.omg.CORBA.ORB;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;

import alma.ADMINTEST1.statusBlockEvent1;
import alma.ADMINTEST2.statusBlockEvent2;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.Consumer;
import alma.acs.nc.SimpleSupplier;
import alma.acsnc.EventDescription;

/**
 * This test class aims to <b>investigate</b> how to reuse the NC Admin Object. 
 * Also the filtering capabilities are checked.
 * 
 * @author jslopez
 */
@SuppressWarnings("deprecation") // We're using Consumer on purpose
public class NCSubscriberTest extends ComponentClientTestCase {

	/**
	 * Naming service reference must be given to NCPublisher
	 */
	private NamingContext nctx;
	private SimpleSupplier supplier;
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		
		// store the naming service reference
		ORB orb = getContainerServices().getAdvancedContainerServices().getORB();
		org.omg.CORBA.Object nameServiceObj = orb.resolve_initial_references("NameService");
		assertNotNull("NameService must not be null", nameServiceObj);
		nctx = NamingContextHelper.narrow(nameServiceObj);
		
		// set up an event supplier for the test channel
		supplier = new SimpleSupplier(TEST_CHANNEL_NAME, getContainerServices());
	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}

	private static final String TEST_CHANNEL_NAME = "refactoredLibsTestChannel";

	public NCSubscriberTest() throws Exception {
		super(NCSubscriberTest.class.getSimpleName());
	}
	
	/**
	 * Tests the generic subscription (all event types) mechanism,
	 * without any competing type-specific subscriptions present.
	 */
	public void testGenericCallback() throws Exception {
		NCSubscriber subscriber = null;
		try {
			subscriber = createNCSubscriber(TEST_CHANNEL_NAME);
			
			int numExpectedEvents = 1;
			CountDownLatch counterGeneric = new CountDownLatch(numExpectedEvents);
			
			// set up receiver callback
			GenericReceiver genericReceiver = new GenericReceiver(counterGeneric);
			subscriber.addGenericSubscription(genericReceiver);
			
			// activate event listening
			subscriber.startReceivingEvents();
			
			// publish event and wait to receive it
			supplier.publishEvent(new EventDescription("abused idl struct", 32L, 64L));
			assertTrue("Got a timeout while waiting for " + numExpectedEvents
					+ " events.", counterGeneric.await(10, TimeUnit.SECONDS));
			
			// register the same generic receiver again, which should not change anything
			subscriber.addGenericSubscription(genericReceiver);
			
			
			// register another generic receiver, which should replace the previous one
			
		} 
		finally {
			if (subscriber != null) {
				subscriber.disconnect();
			}
		}
	}

	/**
	 * This test creates Subscriber and add all event types giving a proper
	 * generic receiver callback and then add a particular subscription. The
	 * goal of this test is to check the priority on the receiver handler.
	 */
	public void testGenericCallbackWithPriority() throws Exception {
		NCSubscriber subscriber = createNCSubscriber(TEST_CHANNEL_NAME);

		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterGeneric = new CountDownLatch(numExpectedEvents);
		
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		GenericReceiver genericReceiver = new GenericReceiver(counterGeneric);

		subscriber.addSubscription(e1Receiver);
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
		NCSubscriber subscriber = createNCSubscriber(TEST_CHANNEL_NAME);

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
		NCSubscriber subscriber = createNCSubscriber(TEST_CHANNEL_NAME);

		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterE2 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterGeneric = new CountDownLatch(numExpectedEvents);
		
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		E2Receiver e2Receiver = new E2Receiver(counterE2);
		GenericReceiver genericReceiver = new GenericReceiver(counterGeneric);
		
		subscriber.addSubscription(e1Receiver);
		subscriber.addSubscription(e2Receiver);
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
		subscriber.addSubscription(e1Receiver);
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE1.await(10, TimeUnit.SECONDS));
	}

	/**
	 * This tests check the behavior of calling the removeSubscription with null
	 * as parameter.
	 */

	public void testRemoveAllSubscription() throws Exception {
		NCSubscriber subscriber = createNCSubscriber(TEST_CHANNEL_NAME);
		
		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterE2 = new CountDownLatch(numExpectedEvents);
		
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		E2Receiver e2Receiver = new E2Receiver(counterE2);

		subscriber.addSubscription(e1Receiver);
		subscriber.addSubscription(e2Receiver);
		subscriber.startReceivingEvents();
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE1.await(10, TimeUnit.SECONDS));
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE2.await(10, TimeUnit.SECONDS));
		subscriber.removeSubscription(null);
		subscriber.addSubscription(e1Receiver);
		assertTrue("Got a timeout while waiting for " + numExpectedEvents
				+ " events.", counterE1.await(10, TimeUnit.SECONDS));

	}
	
	/**
	 * This test checks the removeGenericSubscription method.
	 */
	public void testDisconnect() throws Exception {
		NCSubscriber subscriber = createNCSubscriber(TEST_CHANNEL_NAME);
		
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
		NCSubscriber subscriber = createNCSubscriber(TEST_CHANNEL_NAME);
		NCSubscriber otherSubscriber = createNCSubscriber(TEST_CHANNEL_NAME);

		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		CountDownLatch counterE2 = new CountDownLatch(numExpectedEvents);
		
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		E2Receiver e2Receiver = new E2Receiver(counterE2);
		
		subscriber.addSubscription(e1Receiver);
		otherSubscriber.addSubscription(e2Receiver);
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
		NCSubscriber subscriber = createNCSubscriber(TEST_CHANNEL_NAME);
		
		Consumer oldConsumer = null;
		oldConsumer = new Consumer(TEST_CHANNEL_NAME, getContainerServices());
		assertNotNull("Construction of Consumer failed.", oldConsumer);
		
		int numExpectedEvents = 1;
		CountDownLatch counterE1 = new CountDownLatch(numExpectedEvents);
		E1Receiver e1Receiver = new E1Receiver(counterE1);
		
		subscriber.addSubscription(e1Receiver);
		subscriber.startReceivingEvents();
		
		oldConsumer.addSubscription(statusBlockEvent1.class);
		oldConsumer.consumerReady();
	}
	
	/**
	 * Factory method for NCSubscriber
	 */
	private NCSubscriber createNCSubscriber(String channelName) throws AcsJException {
		return new NCSubscriber(channelName, null, getContainerServices(), nctx, getName());
	}
}


///**
// * This class overload some methods for testing purposes.
// */
//final class NCSubscriberDirect<T extends IDLEntity> extends NCSubscriber<T> {
//	NCSubscriberDirect(String channelName, ContainerServices services)
//			throws AcsJException {
//		super(channelName, services);
//	}
//
//	/**
//	 * Overloaded version that doesn't require the callback. Commented, since is
//	 * no longer used. (Can be used again for testing purposes).
//	 * 
//	 * @see alma.acs.nc.refactored.NCSubscriber#push_structured_event(org.omg.CosNotification.StructuredEvent)
//	 */
//	public void push_structured_event(StructuredEvent structuredEvent)
//	throws Disconnected {
//		EventDescription eDescrip = EventDescriptionHelper
//		.extract(structuredEvent.remainder_of_body);
//
//		System.out.println("Channel:" + channelName + ", Publisher:"
//				+ eDescrip.name + ", Event Type:"
//				+ structuredEvent.header.fixed_header.event_type.type_name);
//	}
//}

/**
 * Base class of the various custom receivers, to limit code duplication.
 */
abstract class ReceiverBase {
	protected final CountDownLatch counter;
	
	ReceiverBase(CountDownLatch counter) {
		this.counter = counter;
	}
	
	protected void handleMyEvent(String eventData, EventDescription eDescrip) {
		System.out.println("Got a call to " + getClass().getSimpleName()+ "#receive: " + eventData + " : " + eDescrip.name);
		counter.countDown();
		// Little hack to force the maxProcessTime warning
		try {
			Thread.sleep(1000 * 0);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}

/**
 * This class implements the receiver handler for statusBlockEvent1.
 */
class E1Receiver extends ReceiverBase implements AcsEventSubscriber.Callback<statusBlockEvent1> {
	
	E1Receiver(CountDownLatch counter) {
		super(counter);
	}
	
	public void receive(statusBlockEvent1 event, EventDescription eDescrip) {
		handleMyEvent(event.myString, eDescrip);
	}
	
	@Override
	public Class<statusBlockEvent1> getEventType() {
		return statusBlockEvent1.class;
	}
}

/**
 * This class implements the receiver handler for statusBlockEvent2.
 */
class E2Receiver extends ReceiverBase implements AcsEventSubscriber.Callback<statusBlockEvent2> {
	
	E2Receiver(CountDownLatch counter) {
		super(counter);
	}
	public void receive(statusBlockEvent2 event, EventDescription eDescrip) {
		handleMyEvent(event.myString, eDescrip);
	}
	
	@Override
	public Class<statusBlockEvent2> getEventType() {
		return statusBlockEvent2.class;
	}
}

/**
 * This class implements the generic receiver handler.
 */
class GenericReceiver extends ReceiverBase implements AcsEventSubscriber.GenericCallback {
	
	GenericReceiver(CountDownLatch counter) {
		super(counter);
	}

	public void receive(IDLEntity event, EventDescription eDescrip) {
		handleMyEvent("<generic>", eDescrip);
	}
}