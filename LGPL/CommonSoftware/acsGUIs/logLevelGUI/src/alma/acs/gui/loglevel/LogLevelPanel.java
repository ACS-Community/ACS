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
package alma.acs.gui.loglevel;

import javax.swing.JScrollPane;

import alma.acs.container.ContainerServices;
import alma.acs.gui.loglevel.tree.LogLvlTree;

/**
 * The panel of the application containing the tree with 
 * containers and components
 * 
 * @author acaproni
 *
 */
public class LogLevelPanel extends JScrollPane {
	
	// The container services
    private ContainerServices contSvc=null;
    
	/**
	 * Constructor
	 *
	 */
	public LogLevelPanel() {
		super(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		initialize();
	}
	
	/**
	 * Init the component
	 *
	 */
	private void initialize() {
	}
	
	/**
	 * Method used by the plugin interface in EXEC:
	 * it connects the application to the NC
	 * @see alma.exec.extension.subsystemplugin.SubsystemPlugin
	 * 
	 * @throws Exception
	 */
	public void start() throws Exception {
		if (contSvc==null) {
			throw new IllegalStateException("Trying to start the panel with null ContainerServices");
		}
		
		String managerLoc = System.getProperty("ACS.manager");
	    if (managerLoc!=null) {
	    	managerLoc=managerLoc.trim();
	    } else {
	    	throw new IllegalStateException("ACS.magager property not set!");
	    }
	    
	    LogLvlTree tree = new LogLvlTree(contSvc.getAdvancedContainerServices().getORB(),contSvc.getLogger());
		setViewportView(tree);
		tree.start();
	}
	
	/**
	 * Method used by the plugin interface in EXEC.
	 * Stop the application disconnecting from the NC
	 * @see alma.exec.extension.subsystemplugin.SubsystemPlugin
	 * 
	 * @throws Exception
	 */
	public void stop() throws Exception {
		contSvc=null;
	}
	
	/**
	 * Method used by the plugin interface in EXEC.
	 * Pause the application (scroll lock enabled)
	 * @see alma.exec.extension.subsystemplugin.IPauseResume
	 * 
	 * @throws Exception
	 */
	public void pause() throws Exception {
	}
	
	/**
	 * Method used by the plugin interface in EXEC.
	 * Unpause the application (scroll lock disabled)
	 * @see alma.exec.extension.subsystemplugin.IPauseResume
	 * 
	 * @throws Exception
	 */
	public void resume() throws Exception {
	}
	
	/**
	 * Set the plugin container services
	 * 
	 * @see alma.exec.extension.subsystemplugin.SubsystemPlugin
	 */
	public void setServices (ContainerServices ctrl) {
		setACSContainerServices(ctrl);
	}
	
	/**
	 * Run in restricted mode
	 * 
	 * @see alma.exec.extension.subsystemplugin.SubsystemPlugin
	 */
	public boolean runRestricted (boolean restricted) throws Exception {
		return restricted;
	}
	
	/**
	 * Set the ACS Container Services
	 * 
	 * @param cs The container services
	 */
	public void setACSContainerServices(ContainerServices cs) {
		if (cs==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices");
		}
		contSvc=cs;
	}

}
