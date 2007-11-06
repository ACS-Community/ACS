/*
 * Created on Jun 24, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;

import alma.acs.container.ContainerServicesBase;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ACSJMSConnectionFactory implements ConnectionFactory {
	protected ContainerServicesBase containerServices;

	public ACSJMSConnectionFactory(ContainerServicesBase containerServices)
	{
		this.containerServices = containerServices;
	}

	/* (non-Javadoc)
	 * @see javax.jms.ConnectionFactory#createConnection()
	 */
	public Connection createConnection() throws JMSException {
		return new ACSJMSConnection(this.containerServices);
	}

	/* (non-Javadoc)
	 * @see javax.jms.ConnectionFactory#createConnection(java.lang.String, java.lang.String)
	 */
	public Connection createConnection(String arg0, String arg1) throws JMSException {
		throw new UnsupportedOperationException();
	}

}
