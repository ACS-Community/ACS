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
 * File NotificationChannel.java
 */
 
//package alma.scheduling.define.nc;
package alma.acs.nc;

/**
 * The NotificationChannel interface allows one to both publish
 * events as well as attach and detach an object that receives 
 * events from a notification channel that already exists.  It
 * is merely a combination of the Publisher and Receiver interfaces
 * 
 * @version 1.00 Apr 10, 2003
 * @author Allen Farris

 */
public interface NotificationChannel extends Publisher, Receiver {

	/**
	 * Use the current notification channel object to get a Publisher
	 * interface.
	 * @return A publisher used to send events.
	 */
	public Publisher getPublisher();

}
