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

import alma.acs.container.ContainerServices;
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
	public ACSJMSTopicSubscriber(Topic topic, ContainerServices containerServices, String selector) throws JMSException {
		super(topic, containerServices,selector);
		try {
			//this.consumer = new Consumer(topic.getTopicName(), containerServices);
			//this.consumer.addSubscription(ACSJMSMessageEntity.class, this);
			this.consumer = new ACSJMSMessageEntityConsumer(topic.getTopicName(), containerServices, this);
			this.consumer.consumerReady();
		} catch (AcsJException e) {
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
