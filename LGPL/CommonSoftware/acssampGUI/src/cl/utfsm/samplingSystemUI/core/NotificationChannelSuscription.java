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
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;

import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNotification.StructuredEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJGenericErrorEx;
import alma.acs.container.ContainerServices;
import alma.acs.nc.refactored.NCSubscriber;
import alma.acsnc.EventDescription;
import alma.acssamp.SampObjPackage.SampDataBlock;
import alma.acssamp.SampObjPackage.SampDataBlockSeqHelper;


final class NotificationChannelSuscription extends NCSubscriber<IDLEntity> {

	/**
	 * Used for further processing the data received by this NC subscriber.
	 */
	private LinkedBlockingQueue<DataItem> cChannel = null;
	
	/**
	 * Notifies the subscription to the channel service.
	 * 
	 * @param channelName
	 *            The name of the channel.
	 * @param cServices
	 *            Services of the container.
	 * @param namingService
	 * @throws alma.acs.exceptions.AcsJException
	 */
	public NotificationChannelSuscription(String channelName, ContainerServices services, NamingContext namingService) 
				throws alma.acs.exceptions.AcsJException{
		super(channelName, null, services, namingService, NotificationChannelSuscription.class.getSimpleName(), IDLEntity.class);

		try {
			// We subscribe to *all* events on this channel by using a generic subscription,
			// to make sure that server-side filtering does not remove any events.
			// Due to the non-standard processing in MyNCSubscriber, the generic callback should never be invoked.
			addGenericSubscription(new GenericCallback() {
				@Override
				public void receiveGeneric(Object event, EventDescription eventDescrip) {
					logger.warning("Unexpected event delivery to 'receiveGeneric' method.");
				}
			});
		} catch (Exception e) {
			String msg = "Failed to subscribe to sampling events: ";
			AcsJGenericErrorEx ex2 = new AcsJGenericErrorEx(e);
			ex2.setErrorDesc(msg);
			throw ex2;
		}

		
		// Create a channel, or get if it already exists
		try {
			cChannel = ThreadCommunicator.getInstance().createChannel(channelName);
		} catch(IllegalArgumentException e) {
			cChannel = ThreadCommunicator.getInstance().getChannel(channelName);
		}
		logger.info("Starting notification channel " + channelName);
	}

	/**
	* Saves a log with information about time and value of the sampled data.
	*
	* @param stEvent A structured event.
	**/
	public boolean push_structured_event_called(StructuredEvent stEvent) {

		try{
			SampDataBlock[] sampledData = SampDataBlockSeqHelper.extract(stEvent.filterable_data[0].value);
			for (int i =0; i< sampledData.length; i++){
				
				// extract the time stamp
				long timeOmg= sampledData[i].sampTime;
				
				// extract the value
				// Check if this is an int, double or float. Anyways, we return it as double
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
						throw new IllegalArgumentException("No matching type for incoming data: " + kind.value());
				}
				
				saveSampledData(timeOmg, value);
			}
		} catch(Exception ex){
			logger.log(Level.WARNING, "Failed to process SampDataBlockSeq event.", ex);
		}
		
		// Veto against further processing of this event, since we've done it already here in this non-standard way.
		return false;
	}
	
	/**
	* Returns the name of the channel.
	**/
	public String getChannelName(){
		return channelName;
	}	

	private void saveSampledData(long time, double value){
		if(cChannel==null){
			logger.info("The channel name is not set");
			return;
		}

		if(!cChannel.offer(new DataItem(time,value)))
			logger.info("Dropped data due to full queue");
	}

}
