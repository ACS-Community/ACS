/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.alarmsystem.clients;

import java.util.Collection;
import java.util.HashSet;
import java.util.logging.Logger;

import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;

import com.cosylab.acs.jms.ACSJMSMessageEntity;

import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.nc.AcsEventSubscriber;
import alma.acsnc.EventDescription;
import alma.alarmsystem.clients.source.SourceListener;

public class SourceClient implements AcsEventSubscriber.Callback<ACSJMSMessageEntity> { 
	
	// The listeners of alarms
	private HashSet<SourceListener> listeners = new HashSet<SourceListener>();
	
	private AcsEventSubscriber<ACSJMSMessageEntity> m_consumer;
	
	// TODO: Is there no IDL-defined constant for this NC name?
	private static final String m_channelName = "CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES";
	
	// Container services
	private final ContainerServices contSvcs;
	
	// To avoid to release the resources twice
	private volatile boolean closed=false;
	
	// Logger
	private Logger logger;

	public SourceClient(ContainerServices svc) throws Exception {
		if (svc==null) {
			throw new IllegalArgumentException("Invalid ContainerServices");
		}
		contSvcs=svc;
		logger=contSvcs.getLogger();
	}
	
	/**
	 * Connect to the sources NC
	 * 
	 * @throws Exception
	 */
	public void connect() throws Exception {
		if (closed) {
			throw new IllegalStateException("SourceClient is closed!");
		}
		logger.log(AcsLogLevel.DEBUG,"Connecting to source channel "+m_channelName);
		m_consumer = contSvcs.createNotificationChannelSubscriber(m_channelName, alma.acsnc.ALARMSYSTEM_DOMAIN_NAME.value, ACSJMSMessageEntity.class);
		m_consumer.addSubscription(this);
		m_consumer.startReceivingEvents();
		logger.log(AcsLogLevel.DEBUG,"Source channel "+m_channelName+" connected");
	}
	
	/**
	 * The method receives all the messages published in the NC
	 * For each message received it check if its content is right
	 * i.e. the name of the class, the member and the code contains the 
	 * number of the message in the sequence. In this way it also checks
	 * if the messages are received in the same order they were sent.
	 * The method also checks if all the messages have been received
	 * and prints a message if receives more messages then the messages 
	 * pushed
	 *  
	 * @param msg The message received from the NC
	 * @see alma.acs.nc.Consumer
	 */
	@Override
	public void receive(ACSJMSMessageEntity msg, EventDescription eventDescrip) {
		ASIMessage asiMsg;
		Collection<FaultState> faultStates;
		try {
			asiMsg=XMLMessageHelper.unmarshal(msg.text);
			faultStates = ASIMessageHelper.unmarshal(asiMsg);
		} catch (Exception e) {
			System.out.println("Exception caught while unmarshalling the msg "+e.getMessage());
			e.printStackTrace();
			return;
		}
		dispatchXMLASIMessage(msg.text);
		for (FaultState fs: faultStates) {
			dispatchFaultState(fs);
		}
	}
	
	@Override
	public Class<ACSJMSMessageEntity> getEventType() {
		return ACSJMSMessageEntity.class;
	}

	/**
	 * Add a listener for the alarms.
	 * 
	 * Add the listeners to the set of listeners to be notified when
	 * a new alarms is received from the categories.
	 * 
	 * @param newListener The listener for alarms from categories
	 */
	public void addAlarmListener(SourceListener newListener) {
		if (newListener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		if (closed) {
			throw new IllegalStateException("SourceClient is closed!");
		}
		synchronized(listeners) {
			listeners.add(newListener);
		}
	}
	
	/**
	 * Remove a listener from the list of listeners to be notified
	 * when a new alarm is received 
	 * 
	 * @param listener The not null listener to remove
	 * @return true if the list of listeners contained the specified listener
	 */
	public boolean removeListener(SourceListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		if (closed) {
			throw new IllegalStateException("SourceClient is closed!");
		}
		boolean ret;
		synchronized(listeners) {
			ret=listeners.remove(listener);
		}
		return ret;
	}
	
	/**
	 * This method, called when a new XML message arrives, dispatches
	 * string to the listeners.
	 * 
	 * @param msg The XML string to send to the listeners
	 */
	private synchronized void dispatchXMLASIMessage(String msg) {
		if (msg==null) {
			throw new IllegalArgumentException("The message to dispatch can't be null");
		}
		synchronized(listeners) {
			for (SourceListener listener: listeners) {
				listener.sourceXMLMsgReceived(msg);
			}
		}
	}
	
	/**
	 * This method, called when a new message arrives, dispatches
	 * the alarm to the listeners.
	 * 
	 * @param newAlarm The alarm to send to the listeners
	 */
	private synchronized void dispatchFaultState(FaultState faultState) {
		if (faultState==null) {
			throw new IllegalArgumentException("Invalid null alarm to dispatch");
		}
		synchronized(listeners) {
			for (SourceListener listener: listeners) {
				listener.faultStateReceived(faultState);
			}
		}
	}
	
	/**
	 * Frees all the resources
	 */
	public void close() {
		if (closed) {
			return;
		}
		closed=true;
		if (m_consumer!=null) {
			try {
				m_consumer.disconnect();
			} catch (Exception ex) {
				// Should we throw something? Who all calls this method?
				ex.printStackTrace();
			}
		}
	}
	
	/**
	 * Ensure that the resources have been released before destroying the object
	 */
	protected void finalize() throws Throwable {
		if (!closed) {
			close();
		}
		super.finalize();
	}

}
