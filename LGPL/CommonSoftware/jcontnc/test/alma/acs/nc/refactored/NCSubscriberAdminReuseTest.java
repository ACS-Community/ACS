package alma.acs.nc.refactored;

import gov.sandia.NotifyMonitoringExt.EventChannel;

import java.util.ArrayList;
import java.util.List;

import org.omg.CosNotifyChannelAdmin.ConsumerAdmin;
import org.omg.CosNotifyChannelAdmin.ProxySupplier;
import org.omg.CosNotifyChannelAdmin.ProxyType;

import alma.acs.component.client.ComponentClientTestCase;
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
		Helper helper = new Helper(getContainerServices());
		channel = helper.getNotificationChannel(CHANNEL_NAME, NC_KIND.value, helper.getNotificationFactoryNameForChannel(CHANNEL_NAME));
		assertNotNull(channel);
	}

	public void tearDown() throws Exception {
		// Make sure we don't have any admin for the next round
		for(int adminID: channel.get_all_consumeradmins())
			channel.get_consumeradmin(adminID).destroy();
		super.tearDown();
	}

	@SuppressWarnings("deprecation")
	public void testSharedAdminReuse() throws Exception {

		assertEquals(0, channel.get_all_consumeradmins().length);
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

		assertEquals(0, channel.get_all_consumeradmins().length);

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
}
