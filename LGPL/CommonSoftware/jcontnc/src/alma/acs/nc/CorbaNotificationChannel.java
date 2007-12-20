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
 * File CorbaNotificationChannel.java
 */
 
package alma.acs.nc;

import org.omg.CORBA.portable.IDLEntity;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;

/**
 * The CorbaNotificationChannel class implements the notification
 * channel concepts using a CORBA-based approach that employs the CORBA
 * notification services.
 * 
 * @version 1.00 Apr 10, 2003
 * @author Allen Farris
 */
public class CorbaNotificationChannel extends AbstractNotificationChannel {

	/**
	 * Get the Receiver interface to a currently created CORBA channel.
	 * 
	 * @param channelName	The name of the requested channel.
	 * @param cs container services
	 * @return A Receiver interface to the specified channel or 
	 * 			null if the channel does not exist.
	 */
	static public Receiver getCorbaReceiver(String channelName, ContainerServicesBase cs) {
        try { 
    		CorbaReceiver r = new CorbaReceiver(channelName, cs);
	    	return r;
        } catch(AcsJException e) {
            return null;
        }
	}
	
	/**
	 * The ALMA domain name, which is not explicitly used and is hidden
	 * from the application.
	 */
	static public final String ALMA_DOMAIN = "ALMA";

	/**
	 * The CORBA publisher object that is used to create, access, and publish 
	 * events on the CORBA channel.
	 */
	private CorbaPublisher corbaPublisher;
	
	/**
	 * The CORBA receiver object that is used to attach and detach receivers 
	 * of events.
	 */
	private CorbaReceiver corbaReceiver;


	/**
	 * Create a CORBA Notification Channel.
	 * @param inChannelName	The name of this channel.
	 * @param cs container services
	 */
	public CorbaNotificationChannel (String inChannelName, ContainerServicesBase cs) {
		super(inChannelName, cs);
        try {
    		corbaPublisher = new CorbaPublisher(inChannelName, cs);
	    	corbaReceiver = new CorbaReceiver(inChannelName, cs);
        } catch(AcsJException e) {
           //OK to swallow???
        }
	}
	
	/**
	 * Create a CORBA Notification Channel and specify the CorbaPublisher 
	 * being used.
	 * @param inCorbaPublisher	The CORBA publisher object.
	 * @param cs Container services reference
	 */
	public CorbaNotificationChannel (CorbaPublisher inCorbaPublisher, ContainerServicesBase cs) {
    		super(inCorbaPublisher.getChannelName(), cs);
	    	this.corbaPublisher = inCorbaPublisher;
        try {
		    this.corbaReceiver = new CorbaReceiver(channelName, cs);
        }catch(AcsJException e) {
           //OK to swallow???
        }

	}
	
	/**
	 * Get the Publisher interface to a currently created CORBA channel.
	 * Only the creator of the channel can provide a Publisher interface.
	 * @return A Publisher interface to thisd channel.
	 */
	public Publisher getPublisher() {
		return this;
	}

	/**
	 * Attach a Receiver, that receives one type of event, to this notification 
	 * channel.  The receiver is required to have a public method called
	 * "receive(EventType)", that receives and processes the event.  The 
	 * EventType parameter in the method signature is the name of an IDL 
	 * structure that defines the event.
	 * @param eventTypeName 	The name of the event type that this receiver 
	 * 							wishes to receive.
	 * @param receiver			An object that receives and processes this event.
	 * 							It must have a public method of the form 
	 * 							"receive(EventType)", where the EventType 
	 * 							parameter in the method signature is the name 
	 * 							of an IDL structure that defines the event.
	 */
	public void attach (String eventTypeName, Object receiver) {
		corbaReceiver.attach(eventTypeName,receiver);
	}
	
	/**
	 * Detach an eventType/Receiver from this notification channel.  Only the 
	 * specified event type is detached for the specified receiver.
	 * @param eventTypeName 	The name of the event type that this receiver 
	 * 							receives.
	 * @param receiver			The object that receives and processes this event.
	 */
	public void detach (String eventTypeName, Object receiver) {
		corbaReceiver.detach(eventTypeName,receiver);
	}

	/**
	 * Connect this corbaReceiver to the notification channel. 
	 * At this point the objects that have been attached begin receiving events.
	 * This method must be called or no events will be recieved.
	 */
	public void begin() {
		corbaReceiver.begin();
	}
	
	/**
	 * Disconnect this reciever from the notification channel.
	 * All objects that have been recieving events are removed and no further
	 * events are received.
	 */
	public void end() {
		corbaReceiver.end();
	}

	/**
	 * Publish an event on this notification channel.
	 * @param event The event must be an IDL structure.
	 */
	public void publish(IDLEntity event) {
		// Publish the event.		
		corbaPublisher.publish(event);
	}
	
	/**
	 * Deactivate this notification channel. The publisher gets disconnected 
     * and then the channel is destroyed.
	 */
	public void deactivate() {
		corbaPublisher.disconnect();
        try {
            corbaPublisher.destroyNotificationChannel();
        } catch(Exception e) {
           //OK to swallow???
        }
	}
	
}

