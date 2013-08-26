/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/*
 * Created on Jun 27, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import javax.jms.JMSException;
import javax.jms.Topic;
import javax.jms.TopicSubscriber;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventSubscriber;
import alma.acscommon.ACS_NC_DOMAIN_ALARMSYSTEM;
import alma.acsnc.EventDescription;

/**
 * @author kzagar
 */
public class ACSJMSTopicSubscriber
	extends ACSJMSConsumer implements TopicSubscriber, AcsEventSubscriber.Callback<ACSJMSMessageEntity> {

	private AcsEventSubscriber<ACSJMSMessageEntity> consumer;
	
	/**
	 * @param destination
	 * @param containerServices
	 * @param selector
	 * @throws AcsJException
	 * @throws JMSException
	 */
	public ACSJMSTopicSubscriber(Topic topic, ContainerServicesBase containerServices, String selector) throws JMSException {
		super(topic, containerServices,selector);
		try {
			consumer = containerServices.createNotificationChannelSubscriber(
					topic.getTopicName(), ACS_NC_DOMAIN_ALARMSYSTEM.value, ACSJMSMessageEntity.class);
			consumer.addSubscription(this);
			consumer.startReceivingEvents();
		} catch (AcsJException e) {
			System.err.println("Error creating the subscriber: "+e.getMessage());
			e.printStackTrace(System.err);
			throw new JMSException("Error creating topic subscriber: " + e.getMessage());
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicSubscriber#getTopic()
	 */
	public Topic getTopic() throws JMSException {
		return (Topic)this.destination;
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicSubscriber#getNoLocal()
	 */
	public boolean getNoLocal() throws JMSException {
		throw new UnsupportedOperationException();
	}

	/**
	 * The NC subscriber callback method.
	 */
	@Override
	public void receive(ACSJMSMessageEntity eventData, EventDescription eventDescrip) {
		// let base class handle it
		receive(eventData);
	}

	@Override
	public Class<ACSJMSMessageEntity> getEventType() {
		return ACSJMSMessageEntity.class;
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#close()
	 */
	public void close() throws JMSException {
		if(consumer != null) {
			try {
				this.consumer.disconnect();
			} catch (Exception ex) {
				JMSException ex2 = new JMSException("Failed to disconnect NC subscriber for topic " + getTopic().getTopicName());
				ex2.setLinkedException(ex);
				throw ex2;
			}
		}
	}
}
