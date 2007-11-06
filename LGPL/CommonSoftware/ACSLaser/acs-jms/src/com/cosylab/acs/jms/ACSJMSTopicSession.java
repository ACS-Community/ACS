/*
 * Created on Jun 27, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import javax.jms.JMSException;
import javax.jms.Topic;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;

import alma.acs.container.ContainerServicesBase;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ACSJMSTopicSession extends ACSJMSSession implements TopicSession {

	/**
	 * @param containerServices
	 */
	public ACSJMSTopicSession(ContainerServicesBase containerServices) {
		super(containerServices);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicSession#createSubscriber(javax.jms.Topic)
	 */
	public TopicSubscriber createSubscriber(Topic topic) throws JMSException {
		return new ACSJMSTopicSubscriber(topic, this.containerServices,null);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicSession#createSubscriber(javax.jms.Topic, java.lang.String, boolean)
	 */
	public TopicSubscriber createSubscriber(
		Topic topic,
		String messageSelector,
		boolean noLocal)
		throws JMSException {
		// TODO messageSelector and noLocal arguments should be used somehow
		return new ACSJMSTopicSubscriber(topic, this.containerServices,messageSelector);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicSession#createPublisher(javax.jms.Topic)
	 */
	public TopicPublisher createPublisher(Topic topic) throws JMSException {
		ACSJMSTopicPublisher newPublisher;
		try {
			newPublisher = new ACSJMSTopicPublisher(topic, this.containerServices);
		} catch (JMSException e) {
			System.err.println("######################################");
			System.err.println("Exception caught "+e.getMessage());
			e.printStackTrace(System.err);
			System.err.println("######################################");
			throw e;
		}
		return newPublisher;
	}

	/* (non-Javadoc)
	 * @see javax.jms.Session#close()
	 */
	public void close() throws JMSException {
	}
}
