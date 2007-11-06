/*
 * Created on Jun 27, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Topic;
import javax.jms.TopicPublisher;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.CorbaPublisher;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ACSJMSTopicPublisher
	extends ACSJMSProducer
	implements TopicPublisher {
		
	private CorbaPublisher publisher; 

	/**
	 * @param destination
	 * @param containerServices
	 * @throws AcsJException
	 * @throws JMSException
	 */
	public ACSJMSTopicPublisher(Topic topic, ContainerServicesBase containerServices) throws JMSException {
		super(topic, containerServices);

		if(topic != null) {
			this.publisher = createPublisher(topic, containerServices);
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicPublisher#getTopic()
	 */
	public Topic getTopic() throws JMSException {
		return (Topic)this.destination;
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicPublisher#publish(javax.jms.Message)
	 */
	public void publish(Message message) throws JMSException {
		if(this.publisher == null) {
			// This method can not be used on an identified message producer.
			throw new UnsupportedOperationException();
		}

		sendMessage(this.publisher, message);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicPublisher#publish(javax.jms.Message, int, int, long)
	 */
	public void publish(Message message, int deliveryMode, int priority, long timeToLive) throws JMSException {
		if(this.publisher == null) {
			// This method can not be used on an identified message producer.
			throw new UnsupportedOperationException();
		}

		message.setJMSDeliveryMode(deliveryMode);
		message.setJMSPriority(priority);
		message.setJMSExpiration(timeToLive + System.currentTimeMillis());

		sendMessage(this.publisher, message);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicPublisher#publish(javax.jms.Topic, javax.jms.Message)
	 */
	public void publish(Topic topic, Message message) throws JMSException {
		if(this.publisher != null) {
			// This method can not be used on an unidentified message producer.
			throw new UnsupportedOperationException();
		}
		if (message==null) {
			throw new NullPointerException("The message is null");
		}
		
		CorbaPublisher tempPublisher = createPublisher(topic, this.containerServices);
		sendMessage(tempPublisher, message);
		tempPublisher.disconnect();
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicPublisher#publish(javax.jms.Topic, javax.jms.Message, int, int, long)
	 */
	public void publish(Topic topic, Message message, int deliveryMode, int priority, long timeToLive) throws JMSException {
		if(this.publisher != null) {
			// This method can not be used on an unidentified message producer.
			throw new UnsupportedOperationException();
		}
		
		message.setJMSDeliveryMode(deliveryMode);
		message.setJMSPriority(priority);
		message.setJMSExpiration(timeToLive + System.currentTimeMillis());
		
		CorbaPublisher tempPublisher = createPublisher(topic, this.containerServices); 
		sendMessage(createPublisher(topic, this.containerServices), message);
		tempPublisher.disconnect();
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#close()
	 */
	public void close() throws JMSException {
		if (this.publisher != null) {
			this.publisher.disconnect();
		}
		this.publisher = null;
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#send(javax.jms.Message)
	 */
	public void send(Message message) throws JMSException {
		if(this.publisher == null) {
			// This method can not be used on an identified message producer.
			throw new UnsupportedOperationException();
		}

		sendMessage(this.publisher, message);
	}

	private static void sendMessage(CorbaPublisher publisher, Message message)
	{
		if(message instanceof ACSJMSMessage) {
			
			// Debug print the props of the message
			/*System.out.println("===> Properties of the message to send: ");
			for(int i = 0; i < ((ACSJMSMessage)message).entity.properties.length; ++i) {
				System.out.print("Props: "+
						((ACSJMSMessage)message).entity.properties[i].property_name);
				System.out.print("=====> "+((ACSJMSMessage)message).entity.properties[i].property_value);
				System.out.println(" class ["+((ACSJMSMessage)message).entity.properties[i].property_value.getClass().getName()+"]");
			}*/
			
			
			
			((ACSJMSMessage)message).getEntity().type=message.getClass().getName();
			publisher.publish(((ACSJMSMessage)message).getEntity());			
			//this.publisher.publish(new EventDescription("a", 32, 64));
		} else {
			throw new IllegalArgumentException("ACSJMSProducer can only send ACSJMSMessage messages!");
		}
	}
	
	private static CorbaPublisher createPublisher(Topic topic, ContainerServicesBase containerServices) throws JMSException {
		try {
			return new CorbaPublisher(topic.getTopicName(), containerServices);
		} catch (AcsJException e) {
			throw new JMSException("Error creating topic publisher: " + e.getMessage());
		}
	}
}
