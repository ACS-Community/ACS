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
 * File EventReceiver.java
 */
 
//package alma.scheduling.define.nc;
package alma.acs.nc;

/**
 * The EventReceiver object is a helper class used internally in
 * implementations of CORBA and Local Receivers.  It is merely
 * a pair -- an event type name and the receiver object used to
 * process that event. 
 * 
 * @version 1.00 Apr 29, 2003
 * @author Allen Farris
 */
class EventReceiver {
	/**
	 * The event type name.
	 */
	String eventTypeName;
	/**
	 * The receiver object used to process the event.
	 */
	Object receiver;

	/**
	 * Create an EventReceiver object.
	 * @param e The event type name.
	 * @param r The receiver object used to process the event.
	 */
	EventReceiver(String e, Object r) {
		eventTypeName = e;
		receiver = r;
	}
}
