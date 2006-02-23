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
 * File LocalNotificationChannel.java
 */
 
//package alma.scheduling.define.nc;
package alma.acs.nc;

import java.util.ArrayList;
import java.util.ListIterator;
import java.lang.reflect.*;

import org.omg.CORBA.portable.IDLEntity;

/**
 * <p>
 * The LocalNotificationChannel class implements the notification channel
 * concepts for the case in which multiple threads within the same
 * Java virtual machine wish to publish and receive events.
 * 
 * @version 1.00 Apr 10, 2003
 * @author Allen Farris
 */
public class LocalNotificationChannel extends AbstractNotificationChannel {
	
	/**
	 * This is a list of currently created channels.  Each item in the list
	 * is a LocalNotificationChannel.
	 */
	static private ArrayList channels = new ArrayList ();
	
	/**
	 * Find a channel in the list of channels, given its channel name 
	 * and subsystem name.  If there is no such channel, null is returned.
	 * @param channelName Name of the channel
	 * @return A channel
	 */
	static private LocalNotificationChannel findChannel(String channelName) {
		synchronized (channels) {
			ListIterator iter = channels.listIterator();
			int n = 0;
			LocalNotificationChannel item = null;
			while (iter.hasNext()) {
				item = (LocalNotificationChannel)iter.next();
				if (item.getChannelName().equals(channelName))
					break;
				++n;
			}
			if (n == channels.size())
				return null;
			return item;
		}
	}

	/**
	 * Get the Receiver interface to a currently created channel.
	 * @param channelName	The name of the requested channel.
	 * @return A Receiver interface to the specified channel or 
	 * 			null if the channel does not exist.
	 */
	static public Receiver getLocalReceiver(String channelName) {
		LocalNotificationChannel c = findChannel(channelName);
		if (c == null)
			throw new IllegalArgumentException("There is no such channel as " +
				channelName);
		Receiver r = new LocalReceiver (c);
		return r;
	}

	/**
	 * The list of receivers that are registered to recieve events
	 * on this channel.  The items in this list are all objects
	 * of type LocalReceiver.
	 */
	private ArrayList receivers;

	/**
	 * the receiver used by the owner of this channel.
	 */
	private LocalReceiver owner;

	/**
	 * Create a Local Notification Channel.
	 * @param inChannelName	The name of this channel.
	 */
	public LocalNotificationChannel (String inChannelName) {
		super(inChannelName, null);
		this.receivers = new ArrayList ();
		this.owner = new LocalReceiver(this);
		// Make sure this channnel does not already exist.
		LocalNotificationChannel x = findChannel(inChannelName);
		if (x != null)
			throw new IllegalArgumentException("channel " + inChannelName + 
			" cannot be created.  It already exists.");
		synchronized (channels) {
			// Add this channel to the list of created channels.
			channels.add(this);
		}
	}
	
	// CheckEventType
	// The alternative constructor, adds the eventType -- not included at this time.
	// * @param eventType	The names of the events that are published on 
	// * 						this channel.
	/*
	public LocalNotificationChannel (String channelName, String[] eventType) {
		super(channelName,eventType);
		this.receivers = new ArrayList ();
		// Make sure this channnel/subsystem does not already exist.
		LocalNotificationChannel x = findChannel(channelName);
		if (x != null)
			throw new IllegalArgumentException("channel " + channelName + 
			" cannot be created.  It already exists.");
		owner = new LocalReceiver(this);
		synchronized (channels) {
			// Add this channel to the list of created channels.
			channels.add(this);
		}
	}
	*/
	
	/**
	 * Add a local receiver to the list of receivers on this channel.
	 * This method is used internally by the LocalReceiver.
	 * @param r the local reciever to be added.
	 */
	void addLocalReceiver(LocalReceiver r) {
		synchronized (receivers) {
			receivers.add(r);
		}
	}

	/**
	 * Remove a local receiver from the list of receivers on this channel.
	 * This method is used internally by the LocalReceiver.
	 * @param r the local reciever to be removed.
	 */
	void removeLocalReceiver(LocalReceiver r) {
		synchronized (receivers) {
			receivers.remove(r);
		}
	}

	/**
	 * Get the Publisher interface to this channel.
	 * @return A Publisher interface to the specified channel or 
	 * 			null if the channel does not exist.
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
		owner.attach(eventTypeName,receiver);
	}
	
	/**
	 * Detach an eventType/Receiver from this notification channel.  Only the 
	 * specified event type is detached for the specified receiver.
	 * @param eventTypeName 	The name of the event type that this receiver 
	 * 							receives.
	 * @param receiver			The object that receives and processes this event.
	 */
	public void detach (String eventTypeName, Object receiver) {
		owner.detach(eventTypeName,receiver);
	}
	
	/**
	 * Begin receiving events. 
	 * At this point the objects that have been attached begin receiving events.
	 * This method must be called or no events will be recieved.
	 */
	public void begin() {
		owner.begin();
	}
	
	/**
	 * Stop receiving events.
	 * All objects that have been recieving events are removed and no further
	 * events are received.
	 */
	public void end() {
		owner.end();
	}


	/**
	 * Publish an event on this notification channel.
	 * @param event The event must be an IDL structure.
	 */
	public synchronized void publish(IDLEntity event) {
		// If there are no receivers, there's no point.
		if (receivers.size() == 0)
			return;
		
		Class eventClass = event.getClass();
		String className = eventClass.getName();

		// CheckEventType
		// The following check is disabled for now.
		/*
		if (!checkEvent(event))
			throw new IllegalArgumentException(
			"Invalid event type!  No such event as " + className);
		*/
		
		// Execute the "receive(<EventType>)" methods of all receivers
		// registered to receive this event.
		
		LocalReceiver local = null;
		ArrayList localList = null;
		ListIterator iter = null;
		EventReceiver item = null;
		Class receiverClass = null;
		Method receiveMethod = null;
		Class[] parm = new Class [1];
		Object[] arg = new Object [1];
		synchronized (receivers) {
			ListIterator localIter = receivers.listIterator();
			while (localIter.hasNext()) {
				local = (LocalReceiver)localIter.next();
				localList = local.getReceivers();
				iter = localList.listIterator();
				while (iter.hasNext()) {
					item = (EventReceiver)iter.next();
					if (item.eventTypeName.equals(className)) {
						try {
							receiverClass = item.receiver.getClass();
							parm[0] = eventClass;
								receiveMethod = receiverClass.getMethod("receive",parm);
							arg[0] = event;
							receiveMethod.invoke(item.receiver,arg);
						// If we've done the type checking properly, we should get
						// no exceptions at this point.
						} catch (NoSuchMethodException ex) {
							throw new IllegalArgumentException (
							"Internal Error! Cannot find method receive(" + 
							item.eventTypeName + ") in class " + receiverClass.getName());
						} catch (InvocationTargetException ex) {
							throw new IllegalArgumentException (
							"Internal Error! Cannot invoke method receive(" + 
							item.eventTypeName + ") in class " + receiverClass.getName());
						} catch (IllegalAccessException ex) {
							throw new IllegalArgumentException (
							"Internal Error! Cannot access method receive(" + 
							item.eventTypeName + ") in class " + receiverClass.getName());
						}
					}
				}
			}
		}
	}
	
	/**
	 * Deactivate this notifcation channel.
	 */
	public void deactivate() {
		synchronized (channels) {
			LocalNotificationChannel x = findChannel(channelName);
			if (x != null)
				channels.remove(this);
		}
	}


}
