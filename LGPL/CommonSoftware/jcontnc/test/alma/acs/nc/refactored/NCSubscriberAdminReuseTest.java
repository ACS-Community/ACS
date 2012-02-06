package alma.acs.nc.refactored;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import org.omg.CosNotifyChannelAdmin.AdminNotFound;
import org.omg.CosNotifyChannelAdmin.ConsumerAdmin;
import org.omg.CosNotifyChannelAdmin.ProxySupplier;
import org.omg.CosNotifyChannelAdmin.ProxyType;

import gov.sandia.NotifyMonitoringExt.EventChannel;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.concurrent.ThreadBurstExecutorService;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.Consumer;
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
		Helper helper = new Helper(getContainerServices(), Helper.getNamingServiceInitial(getContainerServices()));
		channel = helper.getNotificationChannel(CHANNEL_NAME, NC_KIND.value, helper.getNotificationFactoryNameForChannel(CHANNEL_NAME));
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
		for(int adminID: channel.get_all_consumeradmins())
			channel.get_consumeradmin(adminID).destroy();
	}

	@SuppressWarnings("deprecation")
	public void testSharedAdminReuse() throws Exception {

		List<AcsEventSubscriber> subscriberList = new ArrayList<AcsEventSubscriber>();
		for(int i=1; i<=10; i++) {
			// Create the maximum number of proxies per admin
			AcsEventSubscriber[] subscribers = new AcsEventSubscriber[NCSubscriber.PROXIES_PER_ADMIN];
			for(int j=0; j!=NCSubscriber.PROXIES_PER_ADMIN; j++) {
				subscribers[j] = getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);
				subscriberList.add(subscribers[j]);
			}

			assertEquals(i, channel.get_all_consumeradmins().length);
		}

		// Now, all admins should be full of proxies (-1 because of the dummy proxy)
		for(int adminID: channel.get_all_consumeradmins())
			assertEquals(NCSubscriber.PROXIES_PER_ADMIN, channel.get_consumeradmin(adminID).push_suppliers().length - 1);

		for(AcsEventSubscriber subscriber: subscriberList)
			try { subscriber.disconnect(); } catch(IllegalStateException e) {}

		// Now, all consumer admins should have 0 proxies (+1, the dummy proxy)
		for(int adminID: channel.get_all_consumeradmins())
			assertEquals(1, channel.get_consumeradmin(adminID).push_suppliers().length);

	}

	@SuppressWarnings("deprecation")
	public void testNewAndOldNCsTogether() throws Exception {

		List<Consumer> consumers = new ArrayList<Consumer>();
		for(int i=1; i<=10; i++) {

			// Create the maximum number of proxies per admin
			// Also, per every NCSubscriber, create an old Consumer
			AcsEventSubscriber[] subscribers = new AcsEventSubscriber[NCSubscriber.PROXIES_PER_ADMIN];
			for(int j=0; j!=NCSubscriber.PROXIES_PER_ADMIN; j++) {
				subscribers[j] = getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);
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

	private void runConcurrentSubscribersCreation(int subscribersNum) throws Exception {

		m_logger.info("Setting up " + subscribersNum + " concurrent subscriber creations...");
		
		final List<AcsEventSubscriber> subscribers = Collections.synchronizedList(new ArrayList<AcsEventSubscriber>());

		// Create all the tasks first
		ThreadBurstExecutorService executor = new ThreadBurstExecutorService(getContainerServices().getThreadFactory());
		for(int i=0; i!=subscribersNum; i++) {

			Runnable r = new Runnable() {
				@SuppressWarnings("deprecation")
				public void run() {
					try {
						// create subscriber, and add it to the list
						subscribers.add( getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME) );
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
		m_logger.info("Running " + subscribersNum + " concurrent subscriber creations...");
		try {
			assertTrue(executor.executeAllAndWait(100, TimeUnit.SECONDS));
		} catch (InterruptedException e) {
			fail("Got InterruptedException while running all my threads");
		}

		// After all the show, we should have all requested subscribers in the  list
		assertEquals(subscribersNum, subscribers.size());

		// ...and all the admins should be full, except the last one (depending if CONCURRENT_SUBSCRIBERS is multiple of PROXIES_PER_ADMIN)
		int subscribersForLastAdmin = subscribersNum % NCSubscriber.PROXIES_PER_ADMIN;
		int expectedAdmins          = subscribersNum / NCSubscriber.PROXIES_PER_ADMIN;
		if( subscribersForLastAdmin != 0 )
			expectedAdmins++;

		int[] admins = channel.get_all_consumeradmins();
		assertEquals(expectedAdmins, admins.length);

		// And all admins should get example PROXIES_PER_ADMIN (+1, the dummy one), except the last one that gets the remains, if any
		for(int i=0; i != admins.length; i++) {
			int admin = admins[i];
			try {
				int subs = channel.get_consumeradmin(admin).push_suppliers().length;
				if( i == admins.length - 1 && subscribersForLastAdmin != 0 )
					assertEquals(subscribersForLastAdmin + 1, subs);
				else
					assertEquals(NCSubscriber.PROXIES_PER_ADMIN + 1, subs);
			} catch (AdminNotFound e) {
				fail("Can't get information about consumer admin " + admin);
			}
		}
		
		destroyConsumers();
	}
}