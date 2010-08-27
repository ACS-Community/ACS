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
 * File Receiver.java
 */
 
//package alma.scheduling.define.nc;
package alma.acs.nc;

/**
 * The Receiver interface allows one to attach and detach
 * objects to a notification channel that receive 
 * events published on that channel.  No events are actually
 * processed until the "begin()" method is called.  Likewise,
 * the "end()" methods stops all events from being processed
 * by those objects.
 * 
 * @version 1.00 Apr 10, 2003
 * @author Allen Farris
 * @deprecated since ACS 9.0 Use CorbaReceiver instead, or better even {@link Consumer}.

 */
public interface Receiver {

	/**
	 * Attach an event receiver object to this notification 
	 * channel.  The receiver is required to have a public method called
	 * "receive(EventType)", that receives and processes the event.  The 
	 * EventType parameter in the method signature is the name of an IDL 
	 * structure that defines the event.
	 * @param eventTypeName 	The full path name of the event type that 
	 * 							this receiver wishes to receive.
	 * @param receiver			An object that receives and processes this event.
	 * 							It must have a public method of the form 
	 * 							"receive(EventType)", where the EventType 
	 * 							parameter in the method signature is the name 
	 * 							of an IDL structure that defines the event.
	 */
	public void attach (String eventTypeName, Object receiver);
	
	/**
	 * Detach an eventType/Receiver from this notification channel.  Only the 
	 * specified event type is detached for the specified receiver.
	 * @param eventTypeName 	The name of the event type that this receiver 
	 * 							receives.
	 * @param receiver			The object that receives and processes this event.
	 */
	public void detach (String eventTypeName, Object receiver);
	
	/**
	 * The begin() method must be called to initiate the process of receiving 
	 * events.  At this point the objects that have been attached begin 
	 * receiving events.  This method must be called or no events will be 
	 * recieved.
	 */
	public void begin();
	
	/**
	 * Stop all events from being processed by the attached Receiver objects.
	 * All objects that have been recieving events are removed and no further
	 * events are received.
	 */
	public void end();
	

}
