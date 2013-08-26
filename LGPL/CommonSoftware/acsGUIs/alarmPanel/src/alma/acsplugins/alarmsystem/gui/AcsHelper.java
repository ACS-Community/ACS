/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2011, All rights reserved
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
package alma.acsplugins.alarmsystem.gui;

import org.omg.CORBA.ORB;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;

/**
 * An helper class for the ACS component client
 * needed when the panel runs in stand alone mode
 * 
 * @author acaproni
 */
public class AcsHelper {
	
	/**
	 * The ACS client
	 */
	private final AdvancedComponentClient acsClient;
	
	/**
	 * Constructor
	 * 
	 * @param clientName The name of the client
	 * @throws Exception in case of error connecting the ACS client
	 */
	public AcsHelper(String clientName) throws Exception{
		if (clientName==null || clientName.isEmpty()) {
			throw new IllegalArgumentException("Invalide client name");
		}
	    String managerLoc = System.getProperty("ACS.manager");
	    if (managerLoc!=null) {
	    	managerLoc=managerLoc.trim();
	    } else {
	    	throw new IllegalStateException("ACS.magager property not set!");
	    }
	
        acsClient = new AdvancedComponentClient(null, managerLoc, clientName);
        getLogger().log(AcsLogLevel.DEBUG,"Connected to ACS");
	}
	
	public ContainerServices getContainerServices() {
		return acsClient.getContainerServices();
	}
	
	public AcsLogger getLogger() {
		return acsClient.getContainerServices().getLogger();
	}
	
	public ORB getOrb() {
		return acsClient.getContainerServices().getAdvancedContainerServices().getORB();
	}
	
	/**
	 * Close the ACS client.
	 */
	public void done() {
		try {
			acsClient.tearDown();
		} catch (Throwable t) {}
	}
	
}
