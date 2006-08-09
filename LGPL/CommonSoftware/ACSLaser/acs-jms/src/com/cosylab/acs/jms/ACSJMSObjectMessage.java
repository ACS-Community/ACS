package com.cosylab.acs.jms;

import java.io.Serializable;
import java.util.HashMap;

import javax.jms.JMSException;
import javax.jms.ObjectMessage;

import alma.acs.container.ContainerServices;

public class ACSJMSObjectMessage extends ACSJMSMessage implements ObjectMessage
{
	Serializable obj;
	
	public ACSJMSObjectMessage(Serializable obj, ContainerServices cs) {
		super(cs);
		this.obj=obj;
	}
	
	
	public ACSJMSObjectMessage(ACSJMSMessageEntity message, ContainerServices cs) {
		super(message,cs);
	}
	
	public ACSJMSObjectMessage(ContainerServices cs) {
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
