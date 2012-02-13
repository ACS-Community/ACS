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

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Topic;
import javax.jms.TopicPublisher;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventPublisher;

/**
 * To avoid creating an <code>AcsEventPublisher</code> for each message to publish,
 * an pool of <code>AcsEventPublisher</code> has been introduced by means
 * of a <code>HashMap</code>.
 * The key of the <code>HasMap</code> is the name of the NC.
 * To make the publisher reusable for all the topics, the <code>HashMap</code>
 * is static.
 * 
 * The <code>DefaultPublisherImpl</code> instantiates an object of this class and 
 * publishes messages passing the topic i.e. it never calls the method without topic.
 * In particular it calls:
 *   - <code>publish(Topic,Message)</code> for Heartbeat, Category, clients
 *   - <code>publish(Topic topic, Message message, int deliveryMode, int priority, long timeToLive)</code>
 *       for Sources
 *       
 * For our point of view the two methods have the same functioning because we
 * do not use the deliveryMode, priority and timeToLive at least for now ;-)
 * 
 * @see {@link cern.cmw.mom.pubsub.impl.DefaultPublisherImpl}
 * 
 * @author kzagar
 *
 */
public class ACSJMSTopicPublisher extends ACSJMSProducer implements TopicPublisher {
	
	/**
	 * Objects from this class associate a time to the <code>AcsEventPublisher</code>
	 * in order to remember when the NC has been accessed the last time.
	 * In this way it is possible to implement a policy to free not used NC.
	 * 
	 * @author acaproni
	 *
	 */
	private class PublisherPoolItem {
		
		private final String ncName;
		/**
		 * The publisher
		 */ 
		private AcsEventPublisher<ACSJMSMessageEntity> publisher;
		
		/**
		 * The last time the publisher has been accessed
		 */
		private long lastAccessTime;
		
		/**
		 * Constructor
		 * 
		 * Create a </code>PoolMenuItem</code> building a new
		 * <code>AcsEventPublisher</code> and updating the access time
		 * 
		 * @param name The name of the topic (i.e. of the NC)
		 * @throws AcsJException In case of error building the <code>AcsEventPublisher</code>
		 */
		public PublisherPoolItem(String name, ContainerServicesBase contSvcs) throws AcsJException {
			ncName = name;
			publisher = contSvcs.createNotificationChannelPublisher(ncName, alma.acsnc.ALARMSYSTEM_DOMAIN_NAME.value, ACSJMSMessageEntity.class);
			lastAccessTime=System.currentTimeMillis();
		}
		
		/**
		 * Publish a message on this publisher
		 * @param message
		 */
		public void sendMessage(Message message) throws JMSException
		{
			if (publisher==null) {
				throw new IllegalStateException("Trying to publish on a null publisher");
			}
			if(message instanceof ACSJMSMessage) {
				((ACSJMSMessage)message).getEntity().type = message.getClass().getName();
				try {
					publisher.publishEvent(((ACSJMSMessage)message).getEntity());
				} catch (AcsJException ex) {
					JMSException ex2 = new JMSException("Failed to publish on NC " + ncName);
					ex2.setLinkedException(ex);
					throw ex2;
				}
				lastAccessTime=System.currentTimeMillis();
			} else {
				throw new IllegalArgumentException("ACSJMSProducer can only send ACSJMSMessage messages!");
			}
		}
		
		/**
		 * Release the publisher
		 * 
		 * @see {@link AcsEventPublisher}
		 */
		public void close() {
			if (publisher==null) {
				throw new IllegalStateException("Trying to close a null publisher");
			}
			publisher.disconnect();
			publisher=null;
		}

		/**
		 * Return the last access time for this item.
		 * 
		 * The value returned is the number of milliseconds 
		 * as returned by <code>System.currentTimeMillis()</code>
		 * 
		 * @return The instant (msec) when this item has been accessed the last time
		 */
		public long getLastAccessTime() {
			return lastAccessTime;
		}
	}
	
	/**
	 * The name of the topic passed in the constructor
	 */
	private String topicName;
	
	/** The publishers
	 * The key is the name of the NC (i.e. the name of the topic)
	 * The value is the PublisherPoolItem
	 */
	private static HashMap<String, PublisherPoolItem> publishersPool = new HashMap<String, PublisherPoolItem>(); 

	/**
	 * Constructor 
	 * 
	 * @param topic The topic to publish messages into
	 * @param containerServices The container service

	 * @throws JMSException 
	 */
	public ACSJMSTopicPublisher(Topic topic, ContainerServicesBase containerServices) throws JMSException {
		super(topic, containerServices);
		
		if(topic != null) {
			if (topic.getTopicName()==null || topic.getTopicName().isEmpty()) {
				throw new IllegalArgumentException("Invalid topic name");
			}
			topicName=topic.getTopicName();
			synchronized(publishersPool) {
				if (publishersPool.containsKey(topic.getTopicName())) {
					return;
				}
			}
			try {
				PublisherPoolItem item = new PublisherPoolItem(topic.getTopicName(), containerServices);
				synchronized(publishersPool) {
					publishersPool.put(topic.getTopicName(),item);	
				}
			} catch (Exception e) {
				e.printStackTrace(System.err);
				throw new JMSException("Exception building a PublisherPoolItem "+e.getMessage());
			}
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicPublisher#getTopic()
	 */
	public Topic getTopic() throws JMSException {
		return (Topic)this.destination;
	}

	/**
	 * Publish a message in the topic
	 * 
	 * The topic is the topic whose name has been passed in the constructor.
	 * However for this implementation there is no difference when the topic 
	 * has been built because all the topics are in the pool.
	 * The name of the topic built in the constructor can be accessed through
	 * the <code>topicName</code> field.
	 * 
	 * @see javax.jms.TopicPublisher#publish(javax.jms.Message)
	 */
	public void publish(Message message) throws JMSException {
		if (message==null) {
			throw new IllegalArgumentException("The message can't be null");
		}
		PublisherPoolItem item;
		synchronized (publishersPool) {
			item = publishersPool.get(topicName);	
		}
		
		if (item==null) {
			throw new IllegalStateException("Impossible to use this method without building the publisher passing valid topic");
		}
		item.sendMessage(message);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicPublisher#publish(javax.jms.Message, int, int, long)
	 */
	public void publish(Message message, int deliveryMode, int priority, long timeToLive) throws JMSException {
		message.setJMSDeliveryMode(deliveryMode);
		message.setJMSPriority(priority);
		message.setJMSExpiration(timeToLive + System.currentTimeMillis());

		publish(message);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicPublisher#publish(javax.jms.Topic, javax.jms.Message)
	 */
	public void publish(Topic topic, Message message) throws JMSException {
		if (message==null) {
			throw new NullPointerException("The message can't be null");
		}
		if (topic==null) {
			throw new NullPointerException("The topic can't be null");
		}
		if (topic.getTopicName()==null || topic.getTopicName().isEmpty()) {
			throw new IllegalArgumentException("Invalid topic name");
		}
		PublisherPoolItem item;
		synchronized (publishersPool) {
			item= publishersPool.get(topic.getTopicName());
		}
		if (item==null) {
			try {
				item = new PublisherPoolItem(topic.getTopicName(),containerServices);
			} catch (Exception e) {
				throw new JMSException("Error creating an AcsEventPublisher");
			}
			synchronized (publishersPool) {
				publishersPool.put(topic.getTopicName(), item);
			}
		}
		item.sendMessage(message);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicPublisher#publish(javax.jms.Topic, javax.jms.Message, int, int, long)
	 */
	public void publish(Topic topic, Message message, int deliveryMode, int priority, long timeToLive) throws JMSException {
		message.setJMSDeliveryMode(deliveryMode);
		message.setJMSPriority(priority);
		message.setJMSExpiration(timeToLive + System.currentTimeMillis());
		
		publish(topic,message);
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#close()
	 */
	public void close() throws JMSException {
		Collection<PublisherPoolItem> values;
		Iterator<PublisherPoolItem> iter;
		
		synchronized (publishersPool) {
			values= publishersPool.values();
			iter = values.iterator();
			while (iter.hasNext()) {
				PublisherPoolItem item = iter.next();
				item.close();
				iter.remove();
			}
		}
	}

	/* (non-Javadoc)
	 * @see javax.jms.MessageProducer#send(javax.jms.Message)
	 */
	public void send(Message message) throws JMSException {
		publish(message);
	}
}
