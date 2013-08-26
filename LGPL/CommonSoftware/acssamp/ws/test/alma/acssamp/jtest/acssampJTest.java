////////////////////////////////////////////////////////////////////////////////
package alma.acssamp.jtest;

////////////////////////////////////////////////////////////////////////////////

import org.omg.CosNaming.NamingContext;

import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.nc.Helper;

/**
 * Runner for {@link acssampConsumer}.
 * Creates and activates the consumer, waits 50 seconds, then disconnects and returns.
 */
public class acssampJTest extends ComponentClient {
	private acssampConsumer consumer;

	public acssampJTest(String managerLoc, String clientName) throws Exception {
		super(null, managerLoc, clientName);
		ContainerServices csrv = getContainerServices();

		String ncChannel = "NC_LAMP1_brightness_1000000_10000000";

		NamingContext namingService = Helper.getNamingServiceInitial(csrv);

		consumer = new acssampConsumer(ncChannel, csrv, namingService);
		try {
			//After consumerReady() is invoked, push_structured_event(...) is invoked
			//by the notification channel.  That is, we have no control over when
			//that method is called.
			consumer.startReceivingEvents();
			System.out.println("Waiting for events, for 50 seconds ");
			Thread.sleep(50000);
			
			System.out.println("Sampling callback interval (ms) statistics: " + consumer.callbackIntervalStats.toString());
			System.out.println("Sampling interval (ms) statistics: " + consumer.samplingIntervalStats.toString());
			System.out.println("Sampling sequence length statistics: " + consumer.sequenceLengthStats.toString());
		} 
		catch (Exception e) {
			System.err.println(e);
		}
		finally {
			consumer.disconnect();
			tearDown();
		}
	}

	public static void main(String[] args) {
		String managerLoc = System.getProperty("ACS.manager");
		if (managerLoc == null) {
			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}

		try {
			new acssampJTest(managerLoc, "acssampJTest1");
		} catch (Exception e) {
			e.printStackTrace(System.err);
		}
	}
}
