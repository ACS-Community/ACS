/*
 * Created on Jun 27, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import java.util.Collection;
import java.util.HashMap;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Topic;
import javax.jms.TopicPublisher;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.CorbaPublisher;

/**
 * 
 * To avoid creating a <code>CorbaPublisher</code> for each message to publish,
 * an pool of <code>CorbaPublisher</code> has been introduced by means
 * of a <code>HashMap</code>.
 * The key of the <code>HasMap</code> is the name of the NC.
 * To made the publisher reusable for all the topics, the <code>HashMap</code>
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
	 * Objects from this class associate a time to the <code>CorbaPublisher</code>
	 * in order to remember when the NC has been accessed the last time.
	 * In this way it is possible to implement a policy to free not used NC.
	 * 
	 * @author acaproni
	 *
	 */
	private class PublisherPoolItem {
		
		/**
		 * The publisher
		 */ 
		private CorbaPublisher publisher;
		
		/**
		 * The last time the publisher has been accessed
		 */
		private long lastAccessTime;
		
		/**
		 * Constructor
		 * 
		 * Create a </code>PoolMenuItem</code> building a new
		 * <code>CorbaPublisher</code> and updating the access time
		 * 
		 * @param name The name of the topic (i.e. of the NC)
		 * @throws AcsJException In case of error building the <code>CorbaPublisher</code>
		 */
		public PublisherPoolItem(String name, ContainerServicesBase contSvcs) throws AcsJException {
			publisher= new CorbaPublisher(
					name, 
					alma.acsnc.ALARMSYSTEM_DOMAIN_NAME.value, 
					contSvcs);
			lastAccessTime=System.currentTimeMillis();
		}
		
		/**
		 * Publish a message on this publisher
		 * @param message
		 */
		public void sendMessage(Message message)
		{
			if (publisher==null) {
				throw new IllegalStateException("Trying to publish on a null publisher");
			}
			if(message instanceof ACSJMSMessage) {
				((ACSJMSMessage)message).getEntity().type=message.getClass().getName();
				publisher.publish(((ACSJMSMessage)message).getEntity());
				lastAccessTime=System.currentTimeMillis();
			} else {
				throw new IllegalArgumentException("ACSJMSProducer can only send ACSJMSMessage messages!");
			}
		}
		
		/**
		 * Release the publisher
		 * 
		 * @see {@link CorbaPublisher}
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
	 * The value is the CorbaPublisher
	 */
	private static HashMap<String, PublisherPoolItem> publishersPool = new HashMap<String, PublisherPoolItem>(); 

	/**
	 * Constructor 
	 * 
	 * @param topic The topic to publish messages into
	 * @param containerServices The container service

	 * @throws JMSException In case of error building the <code>CorbaPublisher</code>
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
					// A CorbaPublisher for this topic is already present
					//
					// Of course it can never happen ;-)
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
				throw new JMSException("Excepion building a PublisherPoolItem "+e.getMessage());
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
				throw new JMSException("Error building a CorbaPublisher");
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
		synchronized (publishersPool) {
			values= publishersPool.values();
			for (PublisherPoolItem item: values) {
				item.close();
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
