/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
/**
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) &
 *      Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/ 

package cl.utfsm.samplingSystemUI.core;
import org.omg.CosNotification.*;

import alma.acssamp.SampObjPackage.*;
import alma.acs.container.ContainerServices;
import java.util.concurrent.LinkedBlockingQueue;

final class NotificationChannelSuscription extends alma.acs.nc.Consumer {

	private LinkedBlockingQueue<DataItem> cChannel = null;
	private ContainerServices cServices;
	
	/**
	* Notifies the subscription to the channel service.  
	*
	* @param channelName The name of the channel.
	* @param cServices Services of the container.
	**/

	public NotificationChannelSuscription(String channelName, ContainerServices cServices) throws alma.acs.exceptions.AcsJException{
		super(channelName, cServices);
		
		// Create a channel, or get if it already exists
		try {
			cChannel =  ThreadCommunicator.getInstance().createChannel(channelName);
		} catch(IllegalArgumentException e) {
			cChannel = ThreadCommunicator.getInstance().getChannel(channelName);
		}
		cServices.getLogger().info("Starting notification channel " + channelName);
		this.cServices = cServices;
		// Comented lines are the workaround for ACS6
		//initializeChannel();
		try{
			//Subscribe to events
		//	EventType[] added = {new EventType(alma.acscommon.ALMADOMAIN.value, "SampDataBlockSeq")};
		//	EventType[] removed = {};
			//really subscribe to the events
		//	m_consumerAdmin.subscription_change(added, removed);
		addSubscription(null);
		
		}catch(Exception e){
			String msg = "'SampDataBlockSeq' event type is invalid for the '" + m_channelName + "' channel!";
			cServices.getLogger().warning(msg + e.getMessage());

		}
	}

	/**
	* Saves a log with information about time and value of the sampled data.
	*
	* @param stEvent A structured event.
	**/
	public void push_structured_event(StructuredEvent stEvent) throws org.omg.CosEventComm.Disconnected {

		try{
       			SampDataBlock[] sampledData = SampDataBlockSeqHelper.extract(stEvent.filterable_data[0].value);
			for (int i =0; i< sampledData.length; i++){
				
				// extract the time stamp
				long time= sampledData[i].sampTime;
				
				// extract the value
				// Check if this is an int, double or float. Anyways, we returnt it as double
				org.omg.CORBA.Any sampVal = sampledData[i].sampVal;
				org.omg.CORBA.TCKind kind = sampledData[i].sampVal.type().kind();
				double value = 0;
				switch(kind.value()) {
					case org.omg.CORBA.TCKind._tk_double:
						value=sampVal.extract_double();
						break;
					case org.omg.CORBA.TCKind._tk_float:
						value=(double)sampVal.extract_float();
						break;
					case org.omg.CORBA.TCKind._tk_long:
						value=(double)sampVal.extract_long();
						break;
					default:
						m_logger.warning("No matching type for incoming data");	
				}
				
				saveSampledData(time,value);
			}
		}catch(Exception e){
			cServices.getLogger().warning(e.getMessage());
		}
	}
	
	/**
	* Returns the name of the channel.
	**/
	public String getChannelName(){
		return m_channelName;
	}	

	private void saveSampledData(long time, double value){
		if(cChannel==null){
			cServices.getLogger().info("The channel name is not set");
			return;
		}

		if(!cChannel.offer(new DataItem(time,value)))
			cServices.getLogger().info("Droped data due to full queue");
	}

}


