package alma.acs.nc;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNotifyChannelAdmin.AdminNotFound;
import org.omg.CosNotifyChannelAdmin.ConsumerAdmin;
import org.omg.CosNotifyChannelAdmin.ProxySupplier;
import org.omg.CosNotifyChannelAdmin.ProxyType;

import gov.sandia.NotifyMonitoringExt.EventChannel;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.concurrent.ThreadBurstExecutorService;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.NCSubscriber;
import alma.acs.nc.Helper;
import alma.acscommon.NC_KIND;

public class NCSubscriberAdminReuseTest extends ComponentClientTestCase {

	private static String CHANNEL_NAME = "bob-dylan";
	private EventChannel channel;

	public NCSubscriberAdminReuseTest() throws Exception {
		super(NCSubscriberAdminReuseTest.class.getSimpleName());
	}

	public void setUp() throws Exception {
		super.setUp();
		m_logger.info("------------ setUp " + getName() + " --------------");
		Helper helper = new Helper(CHANNEL_NAME, getContainerServices(), Helper.getNamingServiceInitial(getContainerServices()));
		channel = helper.getNotificationChannel(helper.getNotificationFactoryNameForChannel());
		assertNotNull(channel);
		assertEquals(0, channel.get_all_consumeradmins().length);
	}

	public void tearDown() throws Exception {
		m_logger.info("------------ tearDown " + getName() + " --------------");
		// Make sure we don't have any admin for the next round
		destroyConsumers();
		super.tearDown();
	}

	private void destroyConsumers() throws AdminNotFound {
		int[] adminIDs = channel.get_all_consumeradmins();
		if (adminIDs.length > 0) {
			m_logger.info("Will destroy all " + adminIDs.length + " consumer admin objects...");
			for(int adminID : adminIDs) {
				channel.get_consumeradmin(adminID).destroy();
			}
			m_logger.info("Done destroying all consumer admin objects.");
		}
		else {
			m_logger.info("There are no consumer admin objects, nothing to destroy.");
		}
	}

	public void testSharedAdminReuse() throws Exception {
		final int intendedNumberOfAdminObjects = 10;
		
		List<AcsEventSubscriber<IDLEntity>> subscriberList = new ArrayList<AcsEventSubscriber<IDLEntity>>();
		for(int adminCount = 1; adminCount <= intendedNumberOfAdminObjects; adminCount++) {
			// Create the maximum number of proxies per admin 
			for(int j=0; j!=NCSubscriber.PROXIES_PER_ADMIN; j++) {
				subscriberList.add(getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME, IDLEntity.class));
			}
			// verify that all "j loop" subscribers caused only the automatic creation of one admin object
			assertEquals(adminCount, channel.get_all_consumeradmins().length);
		}
		m_logger.info("Created " + subscriberList.size() + " subscribers for channel '" + CHANNEL_NAME);

		// Now, all admins should be full of proxies (-1 because of the dummy proxy).
		// There should not be any fluctuation in the number of proxies per admin as we have it in #testConcurrentSubscribersCreation
		// because here we create all subscribers sequentially.
		for(int adminID : channel.get_all_consumeradmins()) {
			assertEquals(NCSubscriber.PROXIES_PER_ADMIN, channel.get_consumeradmin(adminID).push_suppliers().length - 1);
		}
		m_logger.info("Verified that each admin object has " + NCSubscriber.PROXIES_PER_ADMIN + " subscribers. Will now disconnect the subscibers...");
		
		// disconnect all subscribers, which should remove the proxies from the admin objects
		int disconnectCount=1;
		for(AcsEventSubscriber<IDLEntity> subscriber : subscriberList) {
			subscriber.disconnect();
			m_logger.info("Disconnected subscriber #" + disconnectCount++);
		}
		m_logger.info("All subscribers are disconnected. Will now verify this on the server-side admin objects...");
		
		// Now, all consumer admins should have 0 proxies (+1, the dummy proxy)
		int[] adminIDs = channel.get_all_consumeradmins();
		assertEquals("Currently we do not release admin objects, which means that there should still be " + intendedNumberOfAdminObjects + " of them, even when empty.", 
				intendedNumberOfAdminObjects, adminIDs.length);
		int adminCount=1;
		for(int adminID : channel.get_all_consumeradmins()) {
			m_logger.info("About to check the supplier proxies on admin #" + adminCount);
			assertEquals(1, channel.get_consumeradmin(adminID).push_suppliers().length);
			adminCount++;
		}
	}

	
	/**
	 * TODO: Write a similar test with an old-style C++ Consumer, 
	 *       once we remove the deprecated Java NC Consumer.
	 */
	public void testNewAndOldNCsTogether() throws Exception {

		List<Consumer> consumers = new ArrayList<Consumer>();
		for(int i=1; i<=10; i++) {

			// Create the maximum number of proxies per admin
			// Also, per every NCSubscriber, create an old Consumer
			AcsEventSubscriber[] subscribers = new AcsEventSubscriber[NCSubscriber.PROXIES_PER_ADMIN];
			for(int j=0; j!=NCSubscriber.PROXIES_PER_ADMIN; j++) {
				subscribers[j] = getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME, IDLEntity.class);
				Consumer c = new Consumer(CHANNEL_NAME, getContainerServices());
				consumers.add(c);
			}
			assertEquals(i*(1 + NCSubscriber.PROXIES_PER_ADMIN), channel.get_all_consumeradmins().length);
		}

		// Now, let's examine the consumer admins, and see whether they are shared or not
		int sharedAdmins = 0;
		int lonelyAdmins = 0;
		for(int adminID: channel.get_all_consumeradmins()) {
			ConsumerAdmin admin = channel.get_consumeradmin(adminID);
			boolean isSharedAdmin = false;
			for(int proxyID: admin.push_suppliers()) {
				ProxySupplier proxy = admin.get_proxy_supplier(proxyID);
				if(ProxyType.PUSH_ANY.equals(proxy.MyType())) {
					isSharedAdmin = true;
					break;
				}
			}
			if( isSharedAdmin ) {
				assertEquals(NCSubscriber.PROXIES_PER_ADMIN, admin.push_suppliers().length - 1);
				sharedAdmins++;
			}
			else
				lonelyAdmins++;
		}

		assertEquals(10, sharedAdmins);
		assertEquals(10*NCSubscriber.PROXIES_PER_ADMIN, lonelyAdmins);

		// Manually free these old filthy consumers
		for(Consumer c: consumers)
			c.disconnect();
	}

	public void testConcurrentSubscribersCreation() throws Exception {
		// We test the concurrent creation of subscribers with different loads
		runConcurrentSubscribersCreation(10);
		runConcurrentSubscribersCreation(15);
		runConcurrentSubscribersCreation(53);
		runConcurrentSubscribersCreation(42);
	}

	private void runConcurrentSubscribersCreation(int numRealSubscribersDefinedTotal) throws Exception {

		m_logger.info("Setting up " + numRealSubscribersDefinedTotal + " concurrent subscriber creations...");
		
		final List<AcsEventSubscriber<IDLEntity>> subscribers = Collections.synchronizedList(
				new ArrayList<AcsEventSubscriber<IDLEntity>>());

		// Create all the tasks first
		ThreadBurstExecutorService executor = new ThreadBurstExecutorService(getContainerServices().getThreadFactory());
		for (int i=0; i<numRealSubscribersDefinedTotal; i++) {

			Runnable r = new Runnable() {
				public void run() {
					try {
						// create subscriber, and add it to the list
						subscribers.add( getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME, IDLEntity.class) );
					} catch (Exception e) {
						m_logger.log(Level.WARNING, "Failed to create a subscriber.", e);
					}
				}
			};

			try {
				executor.submit(r, 100, TimeUnit.SECONDS);
			} catch (InterruptedException e1) {
				fail("Failed to submit the subscriber creator thread to the executor service");
			}
		}

		// and now run'em all at the same time! (concurrently)
		m_logger.info("Will run " + numRealSubscribersDefinedTotal + " concurrent subscriber creations...");
		try {
			boolean startOK = executor.executeAllAndWait(100, TimeUnit.SECONDS);
			assertTrue("Not all subscribers started within the alotted 100 seconds window.", startOK);
		} catch (InterruptedException e) {
			fail("Got InterruptedException while running all my threads");
		}

		// After all the show, we should have all requested subscribers in the local list
		assertEquals(numRealSubscribersDefinedTotal, subscribers.size());
		m_logger.info("Successfully created " + numRealSubscribersDefinedTotal + " subscribers semi-concurrently. Will now check their NC admin links...");

		// Check if these subscribers are distributed correctly over several admin objects,
		// allowing for some overbooking due to concurrent requests (see comment about concurrency in c'tor of NCSubscriber)
		
		final int allowedAdminOverbooking = 2;
		int numRealSubscribersFoundTotal = 0;
		int[] adminIDs = channel.get_all_consumeradmins();
		
		for(int i=0; i < adminIDs.length; i++) {
			int adminID = adminIDs[i];
			String msgBase = "Subscriber admin #" + (i+1) + " (of " + adminIDs.length + ") ";
			try {
				int subs = channel.get_consumeradmin(adminID).push_suppliers().length;
				int numRealSubscribersThisAdmin = subs - 1; // minus 1 for the 'management' subscriber
				m_logger.info(msgBase + "has " + numRealSubscribersThisAdmin + " proxy objects (subscribers) attached.");
				
				if (i < adminIDs.length - 1) {
					// This is not the last of the admin objects. It should be  filled to the brim with proxies.
					assertThat(msgBase + "should be full with " + NCSubscriber.PROXIES_PER_ADMIN + " proxies.",
							numRealSubscribersThisAdmin,
							greaterThanOrEqualTo(NCSubscriber.PROXIES_PER_ADMIN) );
					assertThat(msgBase + "has more proxies than allowed by 'allowedAdminOverbooking'=" + allowedAdminOverbooking + ", which may be OK.",
							numRealSubscribersThisAdmin,
							lessThanOrEqualTo(NCSubscriber.PROXIES_PER_ADMIN + allowedAdminOverbooking) );
				}
				else {
					// We should be at the last of the admin objects, which may be only partially filled
					assertThat(msgBase + "has more proxies than allowed by 'allowedAdminOverbooking'=" + allowedAdminOverbooking + ", which may be OK.",
							numRealSubscribersThisAdmin,
							lessThanOrEqualTo(NCSubscriber.PROXIES_PER_ADMIN + allowedAdminOverbooking) );
					assertThat(msgBase + "should contain all remaining proxies.",
							numRealSubscribersThisAdmin,
							equalTo(numRealSubscribersDefinedTotal - numRealSubscribersFoundTotal) );
				}
				numRealSubscribersFoundTotal += numRealSubscribersThisAdmin;
			} 
			catch (AdminNotFound ex) {
				fail("Can't get information about consumer admin #" + (i+1) + " (ID=" + adminID + "): " + ex.toString());
			}
		}
		destroyConsumers();
	}
}