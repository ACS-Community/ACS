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
