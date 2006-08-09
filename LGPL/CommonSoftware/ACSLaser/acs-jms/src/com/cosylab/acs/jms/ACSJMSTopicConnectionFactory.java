/*
 * Created on Jun 27, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import java.io.Serializable;

import javax.jms.JMSException;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;

import alma.acs.container.ContainerServices;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ACSJMSTopicConnectionFactory
	extends ACSJMSConnectionFactory
	implements TopicConnectionFactory, Serializable {

	public ACSJMSTopicConnectionFactory(ContainerServices containerServices)
	{
		super(containerServices);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicConnectionFactory#createTopicConnection()
	 */
	public TopicConnection createTopicConnection() throws JMSException {
		return new ACSJMSTopicConnection(this.containerServices);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicConnectionFactory#createTopicConnection(java.lang.String, java.lang.String)
	 */
	public TopicConnection createTopicConnection(String arg0, String arg1)
		throws JMSException {
			throw new UnsupportedOperationException();
	}

}
