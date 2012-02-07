package alma.acs.nc.refactored;

import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.portable.IDLEntity;

import alma.ADMINTEST1.OnOffStates;
import alma.ADMINTEST1.statusBlockEvent1;
import alma.ADMINTEST2.statusBlockEvent2;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.AcsEventSubscriber.Callback;
import alma.acsnc.EventDescription;

/**
 * Client program that sets up a supplier and a consumer in the same process, and lets them
 * interact for a few iterations. <code>nEvents</code> events are sent every <code>interval</code> seconds.
 * This is repeated <code>times</code> times.
 * This is currently used in conjunction with an intermediate
 * killing and restart of the notify service, to check that its persistence layer works
 * fine.
 *
 * @author rtobar, Nov 16th, 2010
 *
 */
public class SimpleSupplierConsumerClient implements Callback<EventDescription> {

	// For creating the clients
	private Logger m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("SimpleSupplierConsumerClient", false);
	private static final String CHANNEL_NAME = "chinoy";

	private volatile int received = 0;
	private int m_interval;
	private int m_times;
	private int m_nEvents;
	
	/**
	 * We'll use this publisher to publish events of different types
	 * (statusBlockEvent1, statusBlockEvent2, EventDescription).
	 * Thus we cannot parametrize it to any of these types, but have to use the 
	 * generic base type IDLEntity or Object.
	 */
	private AcsEventPublisher<IDLEntity> m_publisher;
	
	private AcsEventSubscriber m_subscriber;
	
	private ComponentClient m_client;

	public SimpleSupplierConsumerClient(int nEvents, int interval, int times) {
		m_nEvents = nEvents;
		m_interval = interval;
		m_times = times;
		m_logger.setLevel(Level.INFO);
	}

	public void createPublisherAndSubscriber() {

			try {
				m_client = new ComponentClient(m_logger, System.getProperty("ACS.manager"), "SimpleSupplierConsumerClient");

				m_subscriber = m_client.getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME);
				m_subscriber.addSubscription(this);
				m_publisher = m_client.getContainerServices().createNotificationChannelPublisher(CHANNEL_NAME, IDLEntity.class);
			} catch (AcsJContainerServicesEx e) {
				// Silently ignore the errors
			} catch (AcsJException e) {
				// Shouldn't happen
			} catch (Exception e) {
				e.printStackTrace();
			}

	}

	public void startReceiving() throws Exception {

		statusBlockEvent1 event1 = new statusBlockEvent1();
		event1.counter1 = 0;
		event1.counter2 = 0;
		event1.counter3 = 0;
		event1.flipFlop = true;
		event1.myString = "myValue";
		event1.onOff = OnOffStates.ON;
		event1.period = 0.2f;

		statusBlockEvent2 event2 = new statusBlockEvent2();
		event2.counter1 = 0;
		event2.counter2 = 0;
		event2.counter3 = 0;
		event2.flipFlop = true;
		event2.myString = "myValue";
		event2.onOff = alma.ADMINTEST2.OnOffStates.ON;
		event2.period = 0.2f;

		EventDescription ed = new EventDescription();
		ed.count = 0;
		ed.name  = "description";
		ed.timestamp = new Date().getTime();

		// subscriber is always listening
		m_subscriber.startReceivingEvents();

		for(int i=0; i!=m_times; i++) {

			m_logger.info( "Current number of filters in the proxySupplier: " + ((NCSubscriber)m_subscriber).proxySupplier.get_all_filters().length);

			// publish events of 2 different types
			for(int j=0; j!=m_nEvents; j++) {
				m_publisher.publishEvent(event1);
				m_publisher.publishEvent(event2);
				m_publisher.publishEvent(ed);
			}

			// Sleep and get events
			try {
				Thread.sleep(m_interval * 1000);
			} catch (InterruptedException e) { }

			try {
				m_subscriber.startReceivingEvents();
			} catch(IllegalStateException e) {
				m_logger.info("IllegalStateException thrown, perfect :D");
			}
		}

	}

	public void disconnectAndReport() throws Exception {
		m_publisher.disconnect();
		m_subscriber.disconnect();
		m_client.tearDown();
		m_logger.info("Received " + received + " events");
	}

	@Override
	public void receive(EventDescription event, EventDescription eventDescrip) {
		received++;
		m_logger.info("Received: " + received);
	}

	@Override
	public Class<EventDescription> getEventType() {
		return EventDescription.class;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) throws Exception {

		if( args.length < 3  ) {
			System.err.println("Usage: SimpleSupplierConsumerClient <# events> <# seconds between transfers> <# transfers>");
			System.exit(1);
		}

		SimpleSupplierConsumerClient client = null;
		try {
			client = new SimpleSupplierConsumerClient(
			   Integer.parseInt(args[0]),
			   Integer.parseInt(args[1]),
			   Integer.parseInt(args[2])
			);
		} catch(NumberFormatException e) {
			System.err.println("Invalid arguments, must all be integers");
			System.out.println("Usage: SimpleSupplierConsumerClient <# events> <# seconds between transfers> <# transfers>");
			System.exit(1);
		}
		
		client.createPublisherAndSubscriber();
		client.startReceiving();
		client.disconnectAndReport();
	}

}
