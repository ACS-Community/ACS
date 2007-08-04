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

import javax.swing.JFrame;

import alma.acs.container.ContainerServices;

/**
 * Default implementatio for a IPanel.
 * It is intended to help the developer writing only the code
 * he/she really needs.
 * 
 * The usual way to create a panel is to extend this class.
 * 
 * If you want to create your implementation of a panel from scratch
 * you should pay particular attention to the closing handshake
 * with the main window. The panel in fact can't close the application
 * but it must send a message to the main window asking to close.
 * This must be done to avoid circular dependency for panels that can be
 * executes in standalone or inside the OMC gui (plugin).
 * 
 * @author acaproni
 *
 */
public class DefaultPanel implements IPanel {
	
	// The frame that own this panel
	// It is not null if the application is executen in standalone mode
	// (i.e. not an OMC plugin)
	protected JFrame frame=null;
	
	// The containerservices.
	// It is set before calling start()
	protected ContainerServices containerServices=null;
	
	/**
	 * Constructor invoked when the panel is executed in stand alone mode
	 * 
	 * @param frame The frame showings this panel
	 */
	public DefaultPanel(JFrame frame) {
		if (frame==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices in constructor");
		}
		this.frame=frame;
	}
	
	/**
	 * Empty constructor used when executing inside OMC
	 *
	 */
	public DefaultPanel() {	
	}
	
	/**
	 * Set the ContainerServices
	 * 
	 * @param contSvc The COntainerServices
	 */
	public void setContainerServices(ContainerServices contSvc) {
		if (contSvc==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices");
		}
	}
	
	/**
	 * Check if the panel is executed as OMC plugin
	 * or as standalone application
	 * 
	 * @return true if the panel is running inside the OMC
	 */
	public boolean isOMCPlugin() {
		return frame!=null;
	}

	/**
	 * @see IPanel
	 */
	public void start() {
	}

	/**
	 * @see IPanel
	 */
	public void stop() {
	}
	
	/**
	 * @see IPanel
	 */
	public void setACSContainerServices(ContainerServices cs) {
		if (cs==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices");
		}
		containerServices=cs;	
	}

}
