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
package alma.acs.gui.loglevel.tree;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;

/**
 * The listener to components/container and clients events 
 * like login-logout
 * 
 * @author acaproni
 *
 */
public interface LogLevelListener {
	
	/**
	 * Called if container logged in
	 * 
	 * @param contInfo The container 
	 */
	public void containerLoggedIn(ContainerInfo contInfo);
	
	/**
	 * Called if container logged out
	 * 
	 * @param contHandle The handle of the container 
Ha chiama	 */
	public void containerLoggedOut(int conthandle);
	
	/**
	 * Called if a component logged in or when a client requested
	 * a component
	 * 
	 * @param contInfo The component
	 */
	public void componentLoggedIn(ComponentInfo compInfo);
	
	/**
	 * Called when a client released a component.
	 * If this method is called it means that the component
	 * is still alive (used by some client)
	 * 
	 * @param compInfo The released component
	 */
	public void componentReleased(ComponentInfo compInfo);
	
	/**
	 * Called if component logged out
	 * 
	 * @param comphandle The handle of the component
	 */
	public void componentLoggedOut(int comphandle);
	
	/**
	 * Called if a client logged in
	 * 
	 * @param contInfo The container 
	 */
	public void clientLoggedIn(ClientInfo clientInfo);
	
	/**
	 * Called if a client logged in
	 * 
	 * @param clientHandle The handle of the client 
	 */
	public void clientLoggedOut(int clientHandle);
}
