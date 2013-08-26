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
package alma.acs.gui.util.panel;

import alma.acs.container.ContainerServices;

/**
 * The interface that the component shown into the main window must implement
 * 
 * The panel must extends a JComponent (it can also be a JRootPane in order
 * to be able to define its own menubar).
 * 
 * @author acaproni
 *
 */
public interface IPanel {
	/**
	 * The method is called  just before the main window closes (i.e. the
	 * applicationterminates).
	 * The panel has to release all its resources to allow the application to exit.
	 * This method is executed on a separate daemon thread.
	 *
	 */
	public void stop() throws Exception;
	
	/**
	 * Signal the panel that can start its computation 
	 *
	 */
	public void start() throws Exception;
	
	/**
	 * Returns true if the panel is executed inside
	 * OMC i.e. it is a plugin.
	 * 
	 * @return If the panel is executed iS AN omc PLUGIN
	 *         false if it is executed stannd alone
	 */
	public boolean isOMCPlugin();
	
	/**
	 * Set the ACS Container Services
	 * 
	 * @param cs The container services
	 */
	public void setACSContainerServices(ContainerServices cs);
}
