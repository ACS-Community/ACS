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
 * File AbstractNotificationChannel.java
 */
 
//package alma.scheduling.define.nc;
package alma.acs.nc;

import java.lang.reflect.*;
import alma.acs.container.ContainerServices;
/**
 * The AbstractNotificationChannel class forms the base class
 * from which Local and CORBA Notification Channel classes are
 * extended.
 * 
 * @version 1.00 Apr 17, 2003
 * @author Allen Farris
 */
public abstract class AbstractNotificationChannel implements NotificationChannel {
	
	/**
	 * The designation for a local notification channel.
	 */
	public static final int LOCAL = 0;
	/**
	 * The designation for a CORBA notification channel.
	 */
	public static final int CORBA = 1;
	
	/**
	 * Create a Notification Channel.
	 * @param type Either LOCAL or CORBA.
	 * @param channelName	The name of this channel.
	 * @param cs container services
	 * @return notification channel
	 */
	static public AbstractNotificationChannel createNotificationChannel(int type,String channelName, ContainerServices cs) {
		switch (type) {
			case LOCAL: return new LocalNotificationChannel(channelName);
			case CORBA: return new CorbaNotificationChannel(channelName,cs);
		}
		throw new IllegalArgumentException("Illeagl type value (" + type + 
			").  Must be LOCAL or CORBA.");
	}
	
	/**
	 * Get the Receiver interface to a currently created channel.
	 * @param type Either LOCAL or CORBA.
	 * @param channelName	The name of the requested channel.
	 * @param cs 
	 * @return The reciever interface.
	 */
	static public Receiver getReceiver(int type, String channelName, ContainerServices cs) {
		switch (type) {
			case LOCAL: return LocalNotificationChannel.getLocalReceiver(channelName);
			case CORBA: return CorbaNotificationChannel.getCorbaReceiver(channelName,cs);
		}
		throw new IllegalArgumentException("Illeagl type value (" + type + 
			").  Must be LOCAL or CORBA.");
	}

	/**
	 * The name of this channel.
	 */
	protected String channelName;

	// CheckEventType
	// * The type of events that are published on this channel.
	// protected String[] eventType;
	
	
	/**
	 * Create an AbstractNotification Channel.
	 * @param inChannelName	The name of this channel.
	 * @param cs	container services
	 */
	protected AbstractNotificationChannel (String inChannelName, ContainerServices cs) {
		// Make sure the argument is legal.
		if (inChannelName == null || inChannelName.length() == 0)
			throw new IllegalArgumentException("channelName cannot be null");
		// Save the argument.
		this.channelName = inChannelName;
	}
	
	/**
	 * Get the Publisher interface to a currently created channel.
	 * @return A Publisher interface to the specified channel.
	 */
	public abstract Publisher getPublisher();

	/**
	 * Deactivate this notification channel.
	 */
	public abstract void deactivate();

	/**
	 * Get the channelName.
	 * @return String
	 */
	public String getChannelName() {
		return channelName;
	}
	
	// The following are helper methods used by the implementing classes. 
	
	/**
	 * Return an error message if the receiver object does not contain
	 * a method of the type "receive(EventType)"; otherwise return null.
	 * @param eventTypeName 	The name of the event type that this receiver 
	 * 							wishes to receive.
	 * @param receiver			An object that receives and processes this event.
	 * 							It must have a public method of the form 
	 * 							"receive(EventType)", where the EventType 
	 * 							parameter in the method signature is the name 
	 * 							of an IDL structure that defines the event.
	 * @return Error message string if there's a problem.
	 */
	static String checkReceiver(String eventTypeName, Object receiver) {
		// Make sure the receiver object has the proper method.
		Class receiverClass = receiver.getClass();
		Method receiveMethod = null;
		Class[] parm = new Class [1];
		try {
			parm[0] = Class.forName(eventTypeName);
			receiveMethod = receiverClass.getMethod("receive",parm);
		} catch (ClassNotFoundException err) { 
			return
			"Invalid event type!  There is no class defining " + eventTypeName;
		} catch (NoSuchMethodException err) { 
			return
			"Invalid receiver!  Class " + receiverClass.getName() + 
			" has no such public method as receive(" + eventTypeName + ")";
		} catch (SecurityException err) { 
			return
			"Invalid receiver!  Method receive(" + eventTypeName + ")" + 
			" in Class " + receiverClass.getName() + " is not accessible.";
		}
		return null;
	}
	
}
