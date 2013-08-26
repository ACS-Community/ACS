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
package alma.acs.gui.loglevel.tree.node;

import si.ijs.maci.ContainerInfo;

/**
 * A container class for the ContainerInfo adding some useful method 
 * for tree management
 * 
 * @author acaproni
 *
 */
public class TreeContainerInfo implements TreeContentInfo {
	
	// The ContainerInfo
	private ContainerInfo client;
	
	public TreeContainerInfo(ContainerInfo cli) {
		if (cli==null) {
			throw new IllegalArgumentException("Invalid null ClientInfo in constructor");
		}
		client=cli;
	}
	
	/**
	 * Override toString to show the name on the tree
	 */
	public String toString() {
		return client.name;
	}
	
	/**
	 * Getter 
	 * 
	 * @return The ClientInfo encapsulated in this object
	 */
	public ContainerInfo getClientInfo() {
		return client;
	}
	
	/**
	 * Check if the encapsulated ClientInfo's handle is equal
	 * to the passed one
	 * 
	 * @param handle The handle to compare with the handle of the 
	 *               client
	 * @return true if the handle and the parameter are equal
	 */
	public boolean compareHandle(int handle) {
		return client.h==handle;
	}
	
	/**
	 * Check if the encapsulated ClientInfo's name is equal
	 * to the passed one
	 * 
	 * @param handle The name to compare with the name of the 
	 *               client
	 * @return true if the name and the parameter are equal
	 */
	public boolean compareName(String name) {
		if (client.name==null) {
			return name==null;
		}
		return client.name.equals(name);
	}
	
	/**
	 * Return the handle of the encsapsulated object 
	 * 
	 * @return The handle of the object
	 */
	public int getHandle() {
		return client.h;
	}
	
	/**
	 * Return the name of the encsapsulated object 
	 * 
	 * @return The name of the object
	 */
	public String getName() {
		return client.name;
	}
}
