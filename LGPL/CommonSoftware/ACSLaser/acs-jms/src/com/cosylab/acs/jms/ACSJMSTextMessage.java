/*
 * Created on Jun 24, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.acs.jms;

import javax.jms.JMSException;
import javax.jms.TextMessage;

import alma.acs.container.ContainerServicesBase;

/**
 * @author kzagar
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class ACSJMSTextMessage extends ACSJMSMessage implements TextMessage {
	/**
	 * @param message
	 */
	public ACSJMSTextMessage(ACSJMSMessageEntity message, ContainerServicesBase cs) {
		super(message,cs);
	}

	public ACSJMSTextMessage(ContainerServicesBase cs) {
		super(cs);
	}

	/* (non-Javadoc)
	 * @see javax.jms.TextMessage#setText(java.lang.String)
	 */
	public void setText(String text) throws JMSException {
		this.entity.text = text;
	}

	/* (non-Javadoc)
	 * @see javax.jms.TextMessage#getText()
	 */
	public String getText() throws JMSException {
		return this.entity.text;
	}
}
