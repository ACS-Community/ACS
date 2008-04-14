/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package com.cosylab.logging.engine;

import org.omg.CORBA.ORB;

import si.ijs.maci.Manager;

/**
 * Interface implemented by ACSRemoteAccess and simulatorRemoteAccess. 
 * Its methods serve the basic purpose of creating, using and destroying 
 * the engine to which messages are directed.
 * Creation date: (10/30/2001 3:42:14 PM)
 * @author: 
 */
public interface RemoteAccess {
	public void destroy();
	public void initialize(ORB theORB, Manager manager);
	public boolean isInitialized();
	public boolean isConnected();
	
	/**
	 * Close the threads and free all the resources
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync);
}
