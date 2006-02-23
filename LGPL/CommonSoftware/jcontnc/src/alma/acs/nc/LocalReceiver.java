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
 * File LocalReceiver.java
 */
 
//package alma.scheduling.define.nc;
package alma.acs.nc;

import java.util.ArrayList;
import java.util.ListIterator;

/**
 * The LocalReceiver class is an internal class used by the local
 * notification channel.  Only the Receiver methods are public.
 * Such an object is created by static methods in the LocalNotificationChannel
 * class.
 * 
 * @version 1.00 Apr 24, 2003
 * @author Allen Farris
 *
 */
class LocalReceiver implements Receiver {
	
	/**
	 * The local notification channel to which this receiver belongs.
	 */
	private LocalNotificationChannel channel;
	
	/**
	 * The list of receiver objects that process received events.
	 * The items on this list are all of type EventReceiver.
	 */
	private ArrayList receivers;
	
	/**
	 * Designates whether a begin() method has been called or not.
	 */
	private boolean isBegin;
	
	/**
	 * Create a local receiver object.  Only the LocalNotificationChannel
	 * static method can create such an object.
	 * @param inChannel
	 */
	LocalReceiver(LocalNotificationChannel inChannel) {
		this.channel = inChannel;
		receivers = new ArrayList ();
		isBegin = false;
	}

	/**
	 * Get the list of event receiver objects.  This method is used by the
	 * LocalNotificationChannel class "publish" method.
	 * @return Returns a list of receivers.
	 */	
	ArrayList getReceivers() {
		return receivers;
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

		// CheckEventType
		// This check has been disabled for now.
		// Make sure this event name is legal.
		/*
		if (!channel.checkEventName(eventTypeName))
			throw new IllegalArgumentException(
			"Invalid receiver!  Method receive(" + eventTypeName + ")" + 
			" in Class " + receiver.getClass().getName() + " is not accessible.");
		*/
				
		// Make sure the receiver object has the proper method.
		String err = AbstractNotificationChannel.checkReceiver(eventTypeName,receiver);
		if (err != null)
			throw new IllegalArgumentException(err);

		// Add this eventTypeName/receiver to the list of receivers.
		synchronized (receivers) {
			// Make sure the eventTypeName/receiver is not already in the list.
			ListIterator iter = receivers.listIterator();
			EventReceiver item = null;
			while (iter.hasNext()) {
				item = (EventReceiver)iter.next();
				if (item.eventTypeName.equals(eventTypeName) &&
					item.receiver == receiver)
					return;
			}
			// OK, then add it.
			EventReceiver x = new EventReceiver(eventTypeName,receiver);
			receivers.add(x);
		}
	}
	
	/**
	 * Detach an eventType/Receiver from this notification channel.  Only the 
	 * specified event type is detached for the specified receiver.
	 * @param eventTypeName 	The name of the event type that this receiver 
	 * 							receives.
	 * @param receiver			The object that receives and processes this event.
	 */
	public void detach (String eventTypeName, Object receiver) {
		synchronized (receivers) {
			// Find the eventTypeName/receiver in the list and remove it.
			int n = 0;
			ListIterator iter = receivers.listIterator();
			EventReceiver item = null;
			while (iter.hasNext()) {
				item = (EventReceiver)iter.next();
				if (item.eventTypeName.equals(eventTypeName) &&
					item.receiver == receiver) {
					receivers.remove(n);
					break;
				}
				++n;
			}
			return;
		}
	}
	
	public void begin() {
		if (!isBegin) {
			// Add this local receiver to the list of channel receivers.
			channel.addLocalReceiver(this);
			isBegin = true;
		}
	}
	
	public void end() {
		if (isBegin) {
			// Remove this local receiver from the list of channel receivers.
			channel.removeLocalReceiver(this);
			// clear the list of event recievers.
			receivers.clear();
			isBegin = false;
		}
	}

}
