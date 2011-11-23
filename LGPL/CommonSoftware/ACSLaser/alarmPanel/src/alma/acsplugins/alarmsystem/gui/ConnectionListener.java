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
package alma.acsplugins.alarmsystem.gui;

/**
 * The listener of the state of the connection 
 * 
 * @author acaproni
 *
 */
public interface ConnectionListener {

	/**
	 * The client is connected
	 * i.e. the category client is connected to all the categories
	 */
	public void connected();
	
	/**
	 * The client is diconnected
	 */
	public void disconnected();
	
	/**
	 * The client is connecting.
	 */
	public void connecting();
	
	/**
	 * The heartbeat with the ASC has been lost.
	 * 
	 * The reconnection is signaled by <code>connected()</code>
	 */
	public void heartbeatLost();
}
