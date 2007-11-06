package com.cosylab.acs.jms;

import java.io.Serializable;
import java.util.HashMap;

import javax.jms.JMSException;
import javax.jms.ObjectMessage;

import alma.acs.container.ContainerServicesBase;

public class ACSJMSObjectMessage extends ACSJMSMessage implements ObjectMessage
{
	Serializable obj;
	
	public ACSJMSObjectMessage(Serializable obj, ContainerServicesBase cs) {
		super(cs);
		this.obj=obj;
	}
	
	
	public ACSJMSObjectMessage(ACSJMSMessageEntity message, ContainerServicesBase cs) {
		super(message,cs);
	}
	
	public ACSJMSObjectMessage(ContainerServicesBase cs) {
		super(cs);
	}
	
	public void setObject(Serializable obj) throws JMSException
	{
		this.obj=obj;
	}

	public Serializable getObject() throws JMSException
	{
		return obj;
	}
	
}
