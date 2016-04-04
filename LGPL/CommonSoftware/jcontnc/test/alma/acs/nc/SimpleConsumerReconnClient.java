package alma.acs.nc;

import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;

import org.omg.CORBA.portable.IDLEntity;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
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

import java.rmi.dgc.VMID;

/**
 * Client program that sets up a supplier and a consumer in the same process, and lets them
 * interact for a few iterations. <code>nEvents</code> events are sent every <code>interval</code> seconds.
 * This is repeated <code>times</code> times.
 * This is currently used in conjunction with an intermediate
 * killing and restart of the notify service, to check that autoreconnection works fine.
 *
 * @author rtobar, Nov 16th, 2010
 *
 */
public class SimpleConsumerReconnClient implements Callback<EventDescription> {

	// For creating the clients
    private static final VMID vmid = new VMID();
    private static final int CL_ID = vmid.hashCode();
	private Logger m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("SimpleConsumerReconnClient-"+String.valueOf(CL_ID), false);
	private static final String CHANNEL_NAME = "test_reconn";

    public static final String NS_RESTARTED = "NS_RESTARTED";
    public static final String NS_STOPPED = "NS_STOPPED";

	private volatile int received = 0;
    private volatile long lastReceived = -1;
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
	private AcsEventSubscriber<EventDescription> m_subscriber;
	private List<AcsEventSubscriber<EventDescription>> m_subscribers;
	
	private ComponentClient m_client;

    public class CallbackObject implements EventProcessingHandler<IDLEntity> {

        public List<Integer> m_transitions;
        public int m_currentEvent;
        public int m_nEventsSent;
        public int m_nEventsStored;
        public int m_nEventsDropped;
        public int m_nExceptions;
        public List<Throwable> m_exceptions;
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
            this.m_exceptions = new ArrayList<Throwable>();
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

        public void exceptionThrown(Throwable ex) {
            this.m_currentEvent++;
            this.m_nExceptions++;
            this.m_exceptions.add(ex);
            if(true == this.m_sent) {
                this.m_transitions.add(this.m_currentEvent);
                this.m_sent = false;
            }
        }

        public String exceptionsFound() {
            String str = "";
            Iterator<Throwable> it = m_cbObj.m_exceptions.iterator();
            while(it.hasNext()) {
                Throwable ex = (Throwable)it.next();
                if(ex instanceof AcsJCORBAProblemEx) {
                    str += ex.getClass().getName() +"(" + ((AcsJCORBAProblemEx)ex).getInfo()  + "),";
                } else if(ex instanceof AcsJException) {
                    str += ex.getClass().getName() +"(" + ((AcsJException)ex).getShortDescription()  + "),";
                } else {
                    str += ex.getClass().getName() +"(" + ex.getMessage()  + "),";
                }
            } 
            return str;
        }
    }

	private CallbackObject m_cbObj;

	public SimpleConsumerReconnClient() throws Exception {
        m_cbObj = new CallbackObject();
		m_logger.setLevel(Level.INFO);
        m_client = new ComponentClient(m_logger, System.getProperty("ACS.manager"), "SimpleConsumerReconnClient-"+String.valueOf(CL_ID));
	}

    public void createPublisher(boolean autoreconnect) {
        try {
            m_publisher = m_client.getContainerServices().createNotificationChannelPublisher(CHANNEL_NAME, IDLEntity.class);
            ((NCPublisher)m_publisher).setAutoreconnect(autoreconnect);
            m_publisher.enableEventQueue(100, m_cbObj); 
        } catch (AcsJContainerServicesEx e) {
            // Silently ignore the errors
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

	public void createSubscriber(boolean autoreconnect) {
        try {
            m_subscriber = m_client.getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME, EventDescription.class);
            m_subscriber.addSubscription(this);
            ((NCSubscriber)m_subscriber).setAutoreconnect(autoreconnect);
            m_logger.info("Main subscriber has been created");
        } catch (AcsJContainerServicesEx e) {
            // Silently ignore the errors
            e.printStackTrace();
        } catch (AcsJException e) {
            // Shouldn't happen
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
	}

    public void createOtherSubscribers(int num,boolean autoreconnect) {
        m_subscribers = new ArrayList<AcsEventSubscriber<EventDescription>>();
        for(int i = 0;i < num;++i) {
            try {
                AcsEventSubscriber<EventDescription> subs 
                    = m_client.getContainerServices().createNotificationChannelSubscriber(CHANNEL_NAME, EventDescription.class);
                subs.addSubscription(this);
                ((NCSubscriber)subs).setAutoreconnect(autoreconnect);
                m_subscribers.add(subs);
            } catch (AcsJContainerServicesEx e) {
                // Silently ignore the errors
                e.printStackTrace();
            } catch (AcsJException e) {
                // Shouldn't happen
                e.printStackTrace();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        m_logger.info("Created " + String.valueOf(m_subscribers.size()) + " extra subscribers");
    }

    public void startReceivingEventsOtherSubscribers() throws Exception {
        Iterator<AcsEventSubscriber<EventDescription>> it = m_subscribers.iterator();
        while(it.hasNext()) {
            it.next().startReceivingEvents();
        }
    }

    public void resumeOtherSubscribers() throws Exception {
        Iterator<AcsEventSubscriber<EventDescription>> it = m_subscribers.iterator();
        while(it.hasNext()) {
            it.next().resume();
        }       
    }

    public void suspendOtherSubscribers() throws Exception {
        Iterator<AcsEventSubscriber<EventDescription>> it = m_subscribers.iterator();
        while(it.hasNext()) {
            it.next().suspend();
        }       
    }

    public void sendEvents(int numEvents,int sleepTimeMs) {
		EventDescription ed = new EventDescription();
		ed.count = 0;
		ed.name  = "description";
		ed.timestamp = new Date().getTime();
        m_logger.info("Start publishing " + String.valueOf(numEvents) + " events");

		// publish events
		for(int j = 0; j < numEvents; j++) {
			try {
                ed.count = j;
				m_publisher.publishEvent(ed);
				try {
					Thread.sleep(sleepTimeMs);
				} catch (InterruptedException e) { }

			} catch(Throwable e) {
                m_cbObj.exceptionThrown(e);
			}
            m_logger.info(":::[TestSubscriberReconn] Published events at iteration " + String.valueOf(j));
		}
    }

/*
	public void startReceiving() throws Exception {
		EventDescription ed = new EventDescription();
		ed.count = 0;
		ed.name  = "description";
		ed.timestamp = new Date().getTime();

        m_logger.info("NS action: " + m_nsAction);
        if(m_autoreconnect) {
            m_logger.info("Autoreconnection: ON");
        } else {
            m_logger.info("Autoreconnection: OFF");
        }
        m_logger.info("Start publishing " + String.valueOf(m_nEvents) + " events");

		// subscriber is always listening
		m_subscriber.startReceivingEvents();
        try {
		    m_subscriber.suspend();
        } catch(Throwable ex) { m_logger.info("Suspend failed"); }
        try {
		    m_subscriber.resume();
        } catch(Throwable ex) { m_logger.info("Resume failed"); }

		// publish events
		for(int j=0; j!=m_nEvents; j++) {
			try {
				m_publisher.publishEvent(ed);
				
				// Sleep
				try {
					Thread.sleep(m_interval * 100);
				} catch (InterruptedException e) { }

			} catch(Throwable e) {
                m_cbObj.exceptionThrown(e);
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
        m_logger.info("===   Exceptions found: " + m_cbObj.exceptionsFound());
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
*/

    public void disconnectOtherSubscribers() throws Exception {
        Iterator<AcsEventSubscriber<EventDescription>> it = m_subscribers.iterator();
        while(it.hasNext()) {
            try {
                it.next().disconnect();
            } catch(Throwable e) {
                e.printStackTrace();
            }
        }       
    }

	public void disconnectAndReport() throws Exception {
        try {
            m_publisher.disconnect();
        } catch(Throwable e) {
            e.printStackTrace();
        }
        try {
            m_subscriber.disconnect();
        } catch(Throwable e) {
            e.printStackTrace();
        }
	}

    public void disconnectSubscriber() throws Exception {
        try {
            m_subscriber.disconnect();
        } catch(Throwable e) {
            e.printStackTrace();
        }
    }

    public void disconnectPublisher() throws Exception {
        try {
            m_publisher.disconnect();
        } catch(Throwable e) {
            e.printStackTrace();
        }
    }

	@Override
	public void receive(EventDescription event, EventDescription eventDescrip) {
		received++;
        if(lastReceived < event.count) {
            lastReceived = event.count;
        }
		//m_logger.info(":::[TestSubscriberReconn] Received: " + received);
	}

	@Override
	public Class<EventDescription> getEventType() {
		return EventDescription.class;
	}

    public boolean waitSec(int nSec) {
        try {
            Thread.sleep(nSec * 1000);
        } catch(InterruptedException ex) { return false; }
        return true;
    }

    /**
     * Test that creates a subscriber with autoreconnect enabled. Then the Notify Service is
     * restarted and the subscriber should reconnect to it. After 15sec, a publisher is created
     * and it starts to send events.
     */
    public void test1() throws Exception {
        m_logger.info("=== Test1");
        m_logger.info("=========================  Create the subscriber and await events");
        createSubscriber(true);
		m_subscriber.startReceivingEvents();
        m_logger.info("=========================  From this point we expect the Notify Service to restart");
        waitSec(15); // Here we expect the restart of the Notify Service
        m_logger.info("=========================  At this point the Notify Service should have been restarted");
        m_logger.info("=========================  Create the publisher and start sending events");
        createPublisher(true);
        sendEvents(5,250);
        disconnectAndReport();    
		m_client.tearDown();
        if(received == 5) {
            m_logger.info("Great! All events have been received");
        } else {
            m_logger.info("Error! Expected 5 events but was " + String.valueOf(received));
        }
    }

    /**
     * Test that creates a subscriber with autoreconnect enabled and every second is resumed and suspended.
     * The Notify Service is restarted and after 15sec a publisher is created and starts to send events.
     */
    public void test2() throws Exception {
        m_logger.info("=== Test2");
        m_logger.info("=========================  Create the subscriber and await events");
        createSubscriber(true);
		m_subscriber.startReceivingEvents();
        m_logger.info("=========================  From this point we expect the Notify Service to restart");
        m_logger.info("=========================  Suspend and resume the subscriber every 1 second");
        for(int i = 0;i < 12;++i) {
            try {
                m_subscriber.suspend();
            } catch(Throwable ex) { m_logger.info(":::[TestSubscriberReconn] Subscriber could not be suspended"); }
            try {
                m_subscriber.resume();
            } catch(Throwable ex) { m_logger.info(":::[TestSubscriberReconn] Subscriber could not be resumed"); }
            waitSec(1);
        }
        m_logger.info("=========================  At this point the Notify Service should have been restarted");
        createPublisher(true);
        sendEvents(5,250);
        disconnectAndReport();
		m_client.tearDown();
        if(received == 5) {
            m_logger.info("Great! All events have been received");
        } else {
            m_logger.info("Error! Expected 5 events but was " + String.valueOf(received));
        }    
    }

    /**
     * Test that creates a suspended subscriber with autoreconnect enabled. The NS is restarted and after 15s
     * a publisher is created and starts to send events.
     */ 
    public void test3() throws Exception {
        m_logger.info("=== Test3");
        m_logger.info("=========================  Create the subscriber suspended");
        createSubscriber(true);
        m_subscriber.startReceivingEvents();
        m_subscriber.suspend();
        m_logger.info("=========================  From this point we expect the Notify Service to restart");
        waitSec(12); // Here we expect the restart of the Notify Service
        m_logger.info("=========================  At this point the Notify Service should have been restarted");
        m_logger.info("=========================  Resume the subscriber. It should reconnect to the channel");
        m_subscriber.resume();
        waitSec(4);
        m_logger.info("=========================  Create the publisher and send events");
        createPublisher(true);
        sendEvents(5,250); // Send 5 events. We expect to receive all of them in the subscriber
        disconnectAndReport();
		m_client.tearDown();
        if(received == 5) {
            m_logger.info("Great! All events have been received");
        } else {
            m_logger.info("Error! Expected 5 events but was " + String.valueOf(received));
        }
    }

    /**
     * Create a subscriber with autoreconnect enabled and suspend it.
     * Create a publisher with autoreconnect enabled.
     * Restart the Notify Service
     * Publish events. The publisher should recreate the channel
     * Resume the subscriber
     * Publish more events
     */
    public void test4() throws Exception {
        m_logger.info("=== Test4");
        m_logger.info("=========================  Create a subscriber suspended");
        createSubscriber(true);
        m_subscriber.startReceivingEvents();
        m_subscriber.suspend();
        m_logger.info("=========================  Create a publisher and start sending events");
        createPublisher(true);
        sendEvents(10,1000);
        m_logger.info("=========================  From this point we expect the Notify Service to restart");
        waitSec(12); // Here we expect the restart of the Notify Service
        m_logger.info("=========================  At this point the Notify Service should have been restarted");
        m_logger.info("=========================  Resume the subscriber and the publisher starts sending events");
        m_subscriber.resume();
        sendEvents(5,250);
        disconnectAndReport();
		m_client.tearDown();
        if(received == 5) {
            m_logger.info("Great! All events have been received");
        } else {
            m_logger.info("Error! Expected 5 events but was " + String.valueOf(received));
        }
    }

    public void test5() throws Exception {
        int numSubs = 5;
        m_logger.info("=== Test5");
        m_logger.info("=========================  Create few subscribers and await events");
        createSubscriber(true);
        createOtherSubscribers(numSubs, true);
        m_subscriber.startReceivingEvents();
        startReceivingEventsOtherSubscribers();
        m_logger.info("=========================  From this point we expect the Notify Service to restart");
        waitSec(15); // Here we expect the restart of the Notify Service
        m_logger.info("=========================  At this point the Notify Service should have been restarted");
        m_logger.info("=========================  Creating the publisher and start sending events");
        createPublisher(true);
        sendEvents(5,250);
        waitSec(2);
        disconnectOtherSubscribers();
        disconnectAndReport();
		m_client.tearDown();
        if(received == (5 * (1+numSubs))) {
            m_logger.info("Great! All events have been received");
        } else {
            m_logger.info("Error! Expected " + String.valueOf(5 * (1+numSubs)) + " events but was " + String.valueOf(received));
        }
    }

    /**
     * Test to be run twice in different processes at the same time
     */
    public void test6() throws Exception {
        int nProc = 2;
        int numSubs = 5;
        int nEvents = 20;
        m_logger.info("=== Test6");
        m_logger.info("=========================  Create few subscribers and await events");
        createSubscriber(true);
        createOtherSubscribers(numSubs, true);
        m_subscriber.startReceivingEvents();
        startReceivingEventsOtherSubscribers();
        m_logger.info("=========================  From this point we expect the Notify Service to restart");
        waitSec(25); // Here we expect the restart of the Notify Service
        m_logger.info("=========================  At this point the Notify Service should have been restarted");
        m_logger.info("=========================  Creating the publisher and start sending events");
        createPublisher(true);
        sendEvents(nEvents,100);
        m_logger.info("=========================  Waiting 10 sec to receive events comming from the other process");
        waitSec(10);
        m_logger.info("=========================  Disconnect subscribers and publisher");
        disconnectOtherSubscribers();
        disconnectAndReport();
		m_client.tearDown();
        m_logger.info("=========================  Ssubscribers and publisher have been disconnected!");
        
        int numEventsExpected = nProc * nEvents * (1+numSubs);
        if(received == numEventsExpected) {
            m_logger.info("Great! All events have been received");
        } else {
            m_logger.info("Error! Expected " + String.valueOf(numEventsExpected) + " events but was " + String.valueOf(received));
        }       
    }

    /**
     * Test to be run twice in different processes at the same time
     */
    public void test7() throws Exception {
        int nProc = 3;
        int numSubs = 5;
        int nEvents = 20;
        m_logger.info("=== Test7");
        m_logger.info("=========================  Create few subscribers and await events");
        //createSubscriber(true);
        createOtherSubscribers(numSubs, true);
        //m_subscriber.startReceivingEvents();
        startReceivingEventsOtherSubscribers();
        m_logger.info("=========================  From this point we expect the Notify Service to restart");
        waitSec(50); // Here we expect the restart of the Notify Service
        m_logger.info("=========================  At this point the Notify Service should have been restarted");
        m_logger.info("=========================  Creating the publisher and start sending events");
        createPublisher(true);
        sendEvents(nEvents, 100);
        m_logger.info("=========================  Waiting 20 sec to receive events comming from other processes");
        waitSec(20);
/*        m_logger.info("=========================  Suspend susbcribers and expect to restart the Notify Service");
        suspendOtherSubscribers();
        waitSec(10); // Here we expect the restart of the Notify Service
        m_logger.info("=========================  At this point the Notify Service should have been restarted");
        m_logger.info("=========================  Resume the subscribers");
        resumeOtherSubscribers();
        m_logger.info("=========================  Send events again");
        sendEvents(nEvents, 100);
        m_logger.info("=========================  Waiting 20 sec to receive events comming from other processes");
        waitSec(20);*/
        m_logger.info("=========================  Disconnect subscribers and publisher");
        //disconnectSubscriber();
        disconnectOtherSubscribers();
        disconnectPublisher();
        m_logger.info("=========================  Subscribers and publisher have been disconnected!");
        waitSec(4);
        m_logger.info("=========================  Tear down the client");
		m_client.tearDown();
        
        int numEventsExpected = nProc * nEvents * (numSubs);
        if(received == numEventsExpected) {
            m_logger.info("Great! All events have been received");
        } else {
            m_logger.info("Error! Expected " + String.valueOf(numEventsExpected) + " events but was " + String.valueOf(received) 
                    + ". Last received event was " + String.valueOf(lastReceived));
        }       
    }


    /**
     * Test to be run twice in different processes at the same time
     */
    /*public void test8() throws Exception {
        int nProc = 3;
        int numSubs = 5;
        int nEvents = 20;
        m_logger.info("=== Test7");
        m_logger.info("=========================  Create few subscribers and await events");
        //createSubscriber(true);
        createOtherSubscribers(numSubs, true);
        //m_subscriber.startReceivingEvents();
        startReceivingEventsOtherSubscribers();
        m_logger.info("=========================  From this point we expect the Notify Service to restart");
        waitSec(30); // Here we expect the restart of the Notify Service
        m_logger.info("=========================  At this point the Notify Service should have been restarted");
        m_logger.info("=========================  Creating the publisher and start sending events");
        createPublisher(true);
        sendEvents(nEvents, 100);
        m_logger.info("=========================  Waiting 20 sec to receive events comming from other processes");
        waitSec(20);
        m_logger.info("=========================  Disconnect subscribers and publisher");
        //disconnectSubscriber();
        disconnectOtherSubscribers();
        disconnectPublisher();
        m_logger.info("=========================  Subscribers and publisher have been disconnected!");
        waitSec(4);
        m_logger.info("=========================  Tear down the client");
		m_client.tearDown();
        
        int numEventsExpected = nProc * nEvents * (numSubs);
        if(received == numEventsExpected) {
            m_logger.info("Great! All events have been received");
        } else {
            m_logger.info("Error! Expected " + String.valueOf(numEventsExpected) + " events but was " + String.valueOf(received) 
                    + ". Last received event was " + String.valueOf(lastReceived));
        }       
    }*/


	/**
	 * @param args
	 */
	public static void main(String[] args) throws Exception {

		if( args.length != 1 ) {
			System.err.println("Usage: SimpleConsumerReconnClient <# test>");
			System.exit(1);
		}

		SimpleConsumerReconnClient client = new SimpleConsumerReconnClient();

        int numTest = 0;
        try {
            numTest = Integer.parseInt(args[0]);
        } catch(NumberFormatException e) {
        }

        if(numTest < 1 || 7 < numTest) {
            System.out.println("Wrong test number: " + args[0]);
            System.exit(1);
        }

        switch(numTest) {
            case 1:
                client.test1();
                break;
            case 2:
                client.test2();
                break;
            case 3:
                client.test3();
                break;
            case 4:
                client.test4();
                break;
            case 5:
                client.test5();
                break;
            case 6:
                client.test6();
                break;
            case 7:
                client.test7();
                break;
        }
		
        System.out.println("Test finished");
	}

}
