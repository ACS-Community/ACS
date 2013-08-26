/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
package com.cosylab.acs.laser;

import java.util.Collection;
import java.util.Collections;
import java.util.Vector;
import java.util.logging.Logger;

import com.cosylab.acs.jms.ACSJMSMessageEntity;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogLevel;
import alma.acs.nc.AcsEventSubscriber;
import alma.acsErrTypeLifeCycle.wrappers.AcsJEventSubscriptionEx;
import alma.acscommon.ACS_NC_DOMAIN_ALARMSYSTEM;
import alma.acsnc.EventDescription;

/**
 * Connects to all the alarm source NC and listen for alarms.
 * <P>
 * {@link #shutdown()} must be called to disconnect from the NC when terminated
 * using objects of this class.
 * 
 * @author  acaproni
 * @version $Id: AlarmSourcesListener.java,v 1.5 2013/02/05 17:15:15 acaproni Exp $
 * @since ACS 11.0  
 */
public class AlarmSourcesListener implements AcsEventSubscriber.Callback<ACSJMSMessageEntity> {
	
	/**
	 * The listener of messages from sources
	 * 
	 * @author acaproni
	 *
	 */
	public interface SourceListener {
		
		/**
		 * The xml recieved from the source (to be unmarshalled
		 * into e <code>ASIMessage</code>).
		 * @param xml
		 */
		public void onMessage(String xml);
	}
	
	/**
	 * Container Services
	 */
	protected final ContainerServicesBase contSvcs;
	
	/**
	 * The logger
	 */
	protected final Logger logger;
	
	/**
	 * The alarm system prepend the name of each source NCs with the same string
	 */
	private final String channelGroupName="CMW.ALARM_SYSTEM.ALARMS.SOURCES.";
	
	/**
	 * The listeners to send messages to
	 */
	private final Collection<SourceListener> listeners = Collections.synchronizedCollection(new Vector<SourceListener>());
	
	/**
	 * Signal that the object has been shutdown
	 */
	private volatile boolean closed=false;
	
	/**
	 * The consumers each of which listens to a source NC
	 */
	private final Collection<AcsEventSubscriber<ACSJMSMessageEntity>> consumers = 
			Collections.synchronizedCollection(new Vector<AcsEventSubscriber<ACSJMSMessageEntity>>());
	
	/**
	 * Constructor. 
	 * 
	 * @param contSvcs Alarm service container services
	 * @param logger The logger
	 */
	public AlarmSourcesListener(ContainerServicesBase contSvcs, Logger logger) {
		if (contSvcs==null) {
			throw new IllegalArgumentException("Alarm container services can't be null!");
		}
		this.contSvcs=contSvcs;
		if (logger==null) {
			throw new IllegalArgumentException("The logger can't be null!");
		}
		this.logger=logger;
	}
	
	/**
	 * Constructor.
	 * 
	 * @param contSvcs Alarm service container services
	 * @param logger The logger
	 * @param listener the listener to notify messages to
	 */
	public AlarmSourcesListener(ContainerServicesBase contSvcs, Logger logger, SourceListener listener) {
		this(contSvcs,logger);
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null!");
		}
		addListener(listener);
	}
	
	/**
	 * Add a listener to notify messages to
	 * 
	 * @return <code>true</code> if the collection of message listeners changed as a result of the call
	 */
	public boolean addListener(SourceListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null!");
		}
		if (closed) {
			return false;
		}
		return listeners.add(listener);
	}
	
	/**
	 * Add a listener to notify messages to
	 * 
	 * @return <code>true</code> if the listener was removed as a result of this call
	 */
	public boolean removeListener(SourceListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null!");
		}
		if (closed) {
			return false;
		}
		return listeners.remove(listener);
	}
	
	/**
	 * Connect to all the passed sources NC
	 * 
	 * @param sourceNcNames The names of the source NCs to connect to
	 * @throws AcsJContainerServicesEx In case of error subscribing to the NC
	 * @throws AcsJIllegalStateEventEx If already subscribed to one of the consumers
	 * @throws AcsJEventSubscriptionEx
	 * @throws AcsJCouldntPerformActionEx
	 */
	public void connectSources(String[] sourceNcNames) 
			throws AcsJContainerServicesEx, AcsJIllegalStateEventEx, AcsJEventSubscriptionEx, AcsJCouldntPerformActionEx {
		if (sourceNcNames==null || sourceNcNames.length==0) {
			throw new IllegalArgumentException("No source channel names to connect to!");
		}
		for (String name: sourceNcNames) {
			connectSource(name);
		}
	}
	
	/**
	 * Connect to all the passed sources NC
	 * 
	 * @param sourceNcNames
	 * @throws AcsJContainerServicesEx In case of error subscribing to the NC
	 * @throws AcsJIllegalStateEventEx If already subscribed to the consumer
	 * @throws AcsJEventSubscriptionEx
	 * @throws AcsJCouldntPerformActionEx
	 */
	public void connectSource(String ncName) 
			throws AcsJContainerServicesEx, AcsJIllegalStateEventEx, AcsJEventSubscriptionEx, AcsJCouldntPerformActionEx {
		if (ncName==null || ncName.isEmpty()) {
			throw new IllegalArgumentException("Invalid name of source channel!");
		}
		String name=channelGroupName+ncName;
		logger.log(AcsLogLevel.DEBUG,"Connecting to source NC: "+name);
		AcsEventSubscriber<ACSJMSMessageEntity> consumer;
		consumer = contSvcs.createNotificationChannelSubscriber(name, ACS_NC_DOMAIN_ALARMSYSTEM.value, ACSJMSMessageEntity.class);
		consumer.addSubscription(this);
		logger.log(AcsLogLevel.DEBUG,"Start receiving alarms from source NC: "+name);
		consumer.startReceivingEvents();
		consumers.add(consumer);
	}
	
	/**
	 * Closes all the NC and remove all the listeners.
	 * 
	 */
	public void shutdown() {
		closed=true;
		// Disconnects the NCs
		logger.log(AcsLogLevel.DEBUG,"Sources listener is about to disconnect from "+consumers.size()+" source channels.");
		synchronized (consumers) {
			for (AcsEventSubscriber<ACSJMSMessageEntity> consumer: consumers) {
				try {
					disconnect(consumer);
				} catch (Throwable t) {
					logger.log(AcsLogLevel.ERROR,"Error disconnecting from the source NC: "+t.getMessage());
					t.printStackTrace(System.err);
				}
			}
			consumers.clear();
		}
		listeners.clear();
		logger.log(AcsLogLevel.DEBUG,"Sources listener is shutdown.");
	}
	
	/**
	 * Disconnect a consumer.
	 * 
	 * @throws AcsJIllegalStateEventEx If the subscriber has been already disconnected
	 * @throws AcsJCouldntPerformActionEx
	 */
	private void disconnect(AcsEventSubscriber<ACSJMSMessageEntity> consumer) 
			throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		consumer.disconnect();
	}
	
	/**
	 * The NC subscriber callback method.
	 */
	@Override
	public void receive(ACSJMSMessageEntity message, EventDescription eventDescrip) {
		if (message==null) {
			throw new NullPointerException("The message received is null");
		}
		// Discard messages if closed
		if (closed) {
			return;
		}
		if (message.type.compareTo("com.cosylab.acs.jms.ACSJMSObjectMessage")==0){
			logger.log(AcsLogLevel.WARNING,"Unknown message type received from source "+message.type);
		} else {
			notifyListeners(message.text);
		}
	}
	
	
	
	/**
	 * Notify the listeners of a new message
	 * 
	 * @param xml The xml (ASIMessage) to notify
	 */
	protected void notifyListeners(String xml) {
		if (xml==null) {
			throw new NullPointerException("The XML source message to notify is null");
		}
		if (closed) {
			return;
		}
		synchronized (listeners) {
			for (SourceListener listener: listeners) {
				try {
					listener.onMessage(xml);
				} catch (Throwable t) {
					logger.log(AcsLogLevel.ERROR,"Error notifying the source listener: "+t.getMessage(),t);
				}
			}
		}
	}
	
	@Override
	public Class<ACSJMSMessageEntity> getEventType() {
		return ACSJMSMessageEntity.class;
	}
}