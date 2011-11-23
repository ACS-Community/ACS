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

import javax.jms.ConnectionConsumer;
import javax.jms.JMSException;
import javax.jms.ServerSessionPool;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicSession;

import alma.acs.container.ContainerServicesBase;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ACSJMSTopicConnection
	extends ACSJMSConnection
	implements TopicConnection {

	public ACSJMSTopicConnection(ContainerServicesBase containerServices)
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
