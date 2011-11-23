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
import alma.acs.nc.Consumer;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ACSJMSTopicSubscriber
	extends ACSJMSConsumer
	implements TopicSubscriber, ACSJMSMessageEntityConsumerListener {

	private Consumer consumer;
	
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
			//this.consumer = new Consumer(topic.getTopicName(), alma.acsnc.ALARMSYSTEM_DOMAIN_NAME.value, containerServices);
			//this.consumer.addSubscription(ACSJMSMessageEntity.class, this);
			this.consumer = new ACSJMSMessageEntityConsumer(topic.getTopicName(), containerServices, this);
			this.consumer.consumerReady();
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

	/* (non-Javadoc)
	 * @see javax.jms.MessageConsumer#close()
	 */
	public void close() throws JMSException {
		if(this.consumer != null) {
			this.consumer.disconnect();
		}
	}
}
