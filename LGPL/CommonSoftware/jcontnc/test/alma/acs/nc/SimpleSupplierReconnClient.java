package alma.acs.nc;

import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

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
import alma.acs.nc.AcsEventPublisher.EventProcessingHandler;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.NCSubscriber;
import alma.acs.nc.NCPublisher;
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

    public static final String NS_RESTARTED = "NS_RESTARTED";
    public static final String NS_STOPPED = "NS_STOPPED";

	private volatile int received = 0;
	private int m_interval;
	private int m_nEvents;
	private boolean m_autoreconnect;
    private String m_nsAction;
	
	/**
	 * We'll use this publisher to publish events of different types
	 * (statusBlockEvent1, statusBlockEvent2, EventDescription).
	 * Thus we cannot parametrize it to any of these types, but have to use the 
	 * generic base type IDLEntity or Object.
	 */
	private AcsEventPublisher<IDLEntity> m_publisher;
	
	private ComponentClient m_client;

    public class CallbackObject implements EventProcessingHandler<IDLEntity> {

        public List<Integer> m_transitions;
        public int m_currentEvent;
        public int m_nEventsSent;
        public int m_nEventsStored;
        public int m_nEventsDropped;
        public int m_nExceptions;
        protected boolean m_sent;

        public CallbackObject() {
            reset();
        }

        public void reset() {
            this.m_transitions = new ArrayList<Integer>();
            this.m_currentEvent = 0;
            this.m_nEventsSent = 0;
            this.m_nEventsStored = 0;
            this.m_nEventsDropped = 0;
            this.m_nExceptions = 0;
            this.m_sent = true;
        }

        public void eventSent(IDLEntity event) {
            this.m_currentEvent++;
            this.m_nEventsSent++;
            if(false == this.m_sent) {
                this.m_transitions.add(this.m_currentEvent);
                this.m_sent = true;
            }
        }        

        public void eventStoredInQueue(IDLEntity event) {
            this.m_currentEvent++;
            this.m_nEventsStored++;
            if(true == this.m_sent) {
                this.m_transitions.add(this.m_currentEvent);
                this.m_sent = false;
            }
        }
        
        public void eventDropped(IDLEntity event) {
            this.m_currentEvent++;
            this.m_nEventsDropped++;
            if(true == this.m_sent) {
                this.m_transitions.add(this.m_currentEvent);
                this.m_sent = false;
            }
        }

        public void exceptionThrown() {
            this.m_currentEvent++;
            this.m_nExceptions++;
            if(true == this.m_sent) {
                this.m_transitions.add(this.m_currentEvent);
                this.m_sent = false;
            }
        }

    }

	private CallbackObject m_cbObj;

	public SimpleSupplierReconnClient(int nEvents, int interval,boolean autoreconnect,String nsAction) {
		m_nEvents = nEvents;
		m_interval = interval;
		m_autoreconnect = autoreconnect;
        m_nsAction = nsAction;
        m_cbObj = new CallbackObject();
		m_logger.setLevel(Level.INFO);
	}

	public void createPublisherAndSubscriber() {

			try {
				m_client = new ComponentClient(m_logger, System.getProperty("ACS.manager"), "SimpleSupplierReconnClient");
				m_publisher = m_client.getContainerServices().createNotificationChannelPublisher(CHANNEL_NAME, IDLEntity.class);
				((NCPublisher)m_publisher).setAutoreconnect(m_autoreconnect);
                m_publisher.enableEventQueue(100, m_cbObj); 
				//m_publisher.setAutoreconnect(m_autoreconnect);
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

        m_logger.info("NS action: " + m_nsAction);
        if(m_autoreconnect) {
            m_logger.info("Autoreconnection: ON");
        } else {
            m_logger.info("Autoreconnection: OFF");
        }
        m_logger.info("Start publishing " + String.valueOf(m_nEvents) + " events");

		// publish events
		for(int j=0; j!=m_nEvents; j++) {
			try {
				m_publisher.publishEvent(event1);
				
				// Sleep
				try {
					Thread.sleep(m_interval * 100);
				} catch (InterruptedException e) { }

			} catch(Throwable e) {
                m_cbObj.exceptionThrown();
			}
            m_logger.info("Published events at iteration " + String.valueOf(j));
		}

        String transitions = "";
        Iterator<Integer> it = m_cbObj.m_transitions.iterator();
        while(it.hasNext()) {
            transitions += String.valueOf(it.next()) + ",";
        }

        m_logger.info("===   Number of events sent: " + String.valueOf(m_cbObj.m_nEventsSent));
        m_logger.info("===   Number of events queued: " + String.valueOf(m_cbObj.m_nEventsStored));
        m_logger.info("===   Number of events dropped: " + String.valueOf(m_cbObj.m_nEventsDropped));
        m_logger.info("===   Number of exceptions: " + String.valueOf(m_cbObj.m_nExceptions));
        m_logger.info("===   Transitions: " + transitions);


        if(m_autoreconnect) {
            if(m_nsAction.equals(NS_RESTARTED)) {
                if(m_cbObj.m_transitions.size() != 2) {
                    m_logger.info("===   Wrong number of transitions: " 
                        + String.valueOf(m_cbObj.m_transitions.size()) + ". We expected 0 or 2");
                }
            } else if(m_nsAction.equals(NS_STOPPED)) {
                if(m_cbObj.m_transitions.size() != 1) {
                    m_logger.info("===   Wrong number of transitions: " 
                        + String.valueOf(m_cbObj.m_transitions.size()) + ". We expected 1");
                }
            } else {
                m_logger.info("===   Wrong Notify Service action: " + m_nsAction);
            }
        } else {
            if(m_nsAction.equals(NS_RESTARTED)) {
                if(m_cbObj.m_transitions.size() != 1) {
                    m_logger.info("===   Wrong number of transitions: " 
                        + String.valueOf(m_cbObj.m_transitions.size()) + ". We expected 1");
                }
            } else if(m_nsAction.equals(NS_STOPPED)) {
                if(m_cbObj.m_transitions.size() != 1) {
                    m_logger.info("===   Wrong number of transitions: " 
                        + String.valueOf(m_cbObj.m_transitions.size()) + ". We expected 1");
                }
            } else {
                m_logger.info("===   Wrong Notify Service action: " + m_nsAction);
            }
        }
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

		if( args.length < 4  ) {
			System.err.println("Usage: SimpleSupplierReconnClient <# events> <# seconds between publications> <autoreconnect> <NS action>");
			System.exit(1);
		}

        String nsAction = args[3];
        if(nsAction.equals(SimpleSupplierReconnClient.NS_RESTARTED) == false 
            && nsAction.equals(SimpleSupplierReconnClient.NS_STOPPED) == false) {
            System.err.println("Wrong NS action: " + nsAction + ". Must be: "
                + SimpleSupplierReconnClient.NS_RESTARTED + " or "
                + SimpleSupplierReconnClient.NS_STOPPED);
            System.exit(1);
        } 

		SimpleSupplierReconnClient client = null;
		try {
			client = new SimpleSupplierReconnClient(
			   Integer.parseInt(args[0]),
			   Integer.parseInt(args[1]),
			   args[2].equals("autoreconnect") || args[2].equals("AUTORECONNECT"),
               nsAction
			);
		} catch(NumberFormatException e) {
			System.err.println("Invalid arguments, must all be integers");
			System.out.println("Usage: SimpleSupplierReconnClient <# events> <# seconds between transfers> <autoreconnect>");
			System.exit(1);
		}
		
		client.createPublisherAndSubscriber();
		client.startReceiving();
		client.disconnectAndReport();
        System.out.println("Test finished");
	}

}
