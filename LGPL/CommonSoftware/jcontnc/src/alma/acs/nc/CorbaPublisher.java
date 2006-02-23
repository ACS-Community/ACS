/*
 * ALMA - Atacama Large Millimeter Array
 * (c) European Southern Observatory, 2002
 * (c) Associated Universities Inc., 2002
 * Copyright by ESO (in the framework of the ALMA collaboration),
 * Copyright by AUI (in the framework of the ALMA collaboration),
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY, without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307  USA
 *
 * File CorbaPublisher.java
 */
 
//package alma.scheduling.define.nc;
package alma.acs.nc;

import alma.acs.exceptions.AcsJException;
import alma.acs.container.ContainerServices;

import org.omg.CORBA.portable.IDLEntity;
/*
import java.lang.reflect.*;

import org.omg.CORBA.Any;
import org.omg.CosNotification.*;
import org.omg.CosNotifyComm.InvalidEventType;
import org.omg.CosEventComm.Disconnected;
*/
/**
 * The CorbaPublisher class implements those methods needed to craft a publisher
 * that publishes events to a CORBA notification channel.
 * 
 * @version 1.00 Apr 10, 2003
 * @author Allen Farris
 */
public class CorbaPublisher extends alma.acs.nc.SimpleSupplier {
	
	//private StructuredEvent corbaEvent;
	
	/**
	 * The parameters are:
	 * @param channelName	the name of the channel -- e.g., Progress
	 * @param cs container services
    * @throws AcsJException 
	 */
	public CorbaPublisher (String channelName, ContainerServices cs) 
      throws AcsJException {

		super (channelName, cs);
	}
	
	/**
	 * This is the main method for publishing an event.  The IDLEntity must be
	 * the IDL structure that defines the event data.  It must match the names of
	 * the events in the list when the channel was created.
	 * @param event an IDLEntity to publish
	 */
	public final void publish(IDLEntity event) {
        try {
            super.publishEvent(event);
        } catch(AcsJException e) {
        }
	}
	

	/**
    * Returns the name of the channel.
	 * @return Name of the channel.
	 */
	public String getChannelName() {
		return m_channelName;
	}

}
