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
 * Created on Aug 23, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.acs.jms;

import org.omg.CosNotification.StructuredEvent;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.Consumer;

/**
 * @author msekoranja
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class ACSJMSMessageEntityConsumer extends Consumer {

	ACSJMSMessageEntityConsumerListener listener;
	/**
	 * @param arg0
	 * @param arg1
	 * @throws alma.acs.exceptions.AcsJException
	 */
	public ACSJMSMessageEntityConsumer(String channelName, ContainerServicesBase containerServices,
			ACSJMSMessageEntityConsumerListener listener)
			throws AcsJException {
		super(channelName, alma.acsnc.ALARMSYSTEM_DOMAIN_NAME.value, containerServices);
		this.listener = listener;
		addSubscription(ACSJMSMessageEntity.class);
	}

	   /**
	    * Overriden to avoid Java Reflection usage (classloader problems with NetBeans).
	    * @param structuredEvent
	    *           The structured event sent by a supplier subclass.
	    * @throws org.omg.CosEventComm.Disconnected
	    */
	   public void push_structured_event(StructuredEvent structuredEvent)
	         throws org.omg.CosEventComm.Disconnected
	   {
	      try
	      {
	      	// only ACSJMSMessageEntity is expected
	      	ACSJMSMessageEntity message =
	      		ACSJMSMessageEntityHelper.extract(structuredEvent.filterable_data[0].value);
	      	
	      	listener.receive(message);

	      }
	      catch (Throwable th)
	      {
	         // should never happen...
	         String msg = "Failed to process an event on the '" + m_channelName
	               + "' channel because: ";
	         msg = msg + th.getMessage();
	         m_logger.warning(msg);
	         th.printStackTrace(System.out);
	      }

	   }
}
