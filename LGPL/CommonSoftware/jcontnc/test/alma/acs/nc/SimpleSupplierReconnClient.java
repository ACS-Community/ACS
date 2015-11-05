package alma.acs.nc;

import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.portable.IDLEntity;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ADMINTEST1.OnOffStates;
import alma.ADMINTEST1.statusBlockEvent1;
import alma.ADMINTEST2.statusBlockEvent2;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.NCSubscriber;
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
public class SimpleSupplierReconnClient implements Callback<EventDescription> {

	// For creating the clients
	private Logger m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("SimpleSupplierReconnClient", false);
	private static final String CHANNEL_NAME = "test_reconn";

	private volatile int received = 0;
	private int m_interval;
	private int m_nEvents;
	
	/**
	 * We'll use this publisher to publish events of different types
	 * (statusBlockEvent1, statusBlockEvent2, EventDescription).
	 * Thus we cannot parametrize it to any of these types, but have to use the 
	 * generic base type IDLEntity or Object.
	 */
	private AcsEventPublisher<IDLEntity> m_publisher;
	
	private ComponentClient m_client;

	public SimpleSupplierReconnClient(int nEvents, int interval) {
		m_nEvents = nEvents;
		m_interval = interval;
		m_logger.setLevel(Level.INFO);
	}

	public void createPublisherAndSubscriber() {

			try {
				m_client = new ComponentClient(m_logger, System.getProperty("ACS.manager"), "SimpleSupplierReconnClient");
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

		int n_errors = 0;
		int n_changes = 0;
		boolean publishing = true;

		// publish events
		for(int j=0; j!=m_nEvents; j++) {
			try {
				//m_logger.info("Publishing event " + String.valueOf(j));
				m_publisher.publishEvent(event1);
		
				if(false == publishing) {
					publishing = true;
					n_changes++;
				}
				
				// Sleep and get events
				try {
					Thread.sleep(m_interval * 100);
				} catch (InterruptedException e) { }
			} catch(Exception e) {
				n_errors++;
				if(true == publishing) {
					publishing = false;
					n_changes++;
				}
			}
		}

		m_logger.info(String.valueOf(n_errors) + " exceptions caught");
		m_logger.info(String.valueOf(n_changes) + " changes found");
	}

	public void disconnectAndReport() throws Exception {
		m_publisher.disconnect();
		m_client.tearDown();
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

		if( args.length < 2  ) {
			System.err.println("Usage: SimpleSupplierReconnClient <# events> <# seconds between publications>");
			System.exit(1);
		}

		SimpleSupplierReconnClient client = null;
		try {
			client = new SimpleSupplierReconnClient(
			   Integer.parseInt(args[0]),
			   Integer.parseInt(args[1])
			);
		} catch(NumberFormatException e) {
			System.err.println("Invalid arguments, must all be integers");
			System.out.println("Usage: SimpleSupplierReconnClient <# events> <# seconds between transfers>");
			System.exit(1);
		}
		
		client.createPublisherAndSubscriber();
		client.startReceiving();
		client.disconnectAndReport();
	}

}
