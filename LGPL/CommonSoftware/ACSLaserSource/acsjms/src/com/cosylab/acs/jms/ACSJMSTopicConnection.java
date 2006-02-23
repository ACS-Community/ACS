/*
 * Created on Jun 27, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import javax.jms.ConnectionConsumer;
import javax.jms.JMSException;
import javax.jms.ServerSessionPool;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicSession;

import alma.acs.container.ContainerServices;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ACSJMSTopicConnection
	extends ACSJMSConnection
	implements TopicConnection {

	public ACSJMSTopicConnection(ContainerServices containerServices)
	{
		super(containerServices);	
	}
	
	/* (non-Javadoc)
	 * @see javax.jms.TopicConnection#createTopicSession(boolean, int)
	 */
	public TopicSession createTopicSession(boolean arg0, int arg1)
		throws JMSException {
		return new ACSJMSTopicSession(this.containerServices);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TopicConnection#createConnectionConsumer(javax.jms.Topic, java.lang.String, javax.jms.ServerSessionPool, int)
	 */
	public ConnectionConsumer createConnectionConsumer(
		Topic arg0,
		String arg1,
		ServerSessionPool arg2,
		int arg3)
		throws JMSException {
		throw new UnsupportedOperationException();
	}
	
	/* (non-Javadoc)
	 * @see javax.jms.Connection#stop()
	 */
	public void stop() throws JMSException {
	}
	
	/* (non-Javadoc)
	 * @see javax.jms.Connection#close()
	 */
	public void close() throws JMSException {
	}
}
