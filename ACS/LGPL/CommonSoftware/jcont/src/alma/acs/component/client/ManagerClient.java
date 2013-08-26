/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.component.client;

import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

import si.ijs.maci.AuthenticationData;
import si.ijs.maci.ClientPOA;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ImplLangType;

import alma.acs.component.ComponentDescriptor;
import alma.acs.container.ContainerServicesImpl;
import alma.acs.util.UTCUtility;

/**
 * Class to be used when logging in to the ACS Manager.
 * 
 * @author hsommer Apr 2, 2003 2:09:24 PM
 */
class ManagerClient extends ClientPOA
{
	private final String m_clientName;

	private final Logger m_logger;

    private final long startTimeUTClong;

    private long executionId = -1; 
	
	/**
	 * Optional container services, used for notification for components_available etc.
	 * To be set by the client application, and later used by the methods which the manager calls 
	 * (in other threads, thus this field should be volatile to ensure that a non-null value becomes visible)
	 */
	private volatile ContainerServicesImpl containerServices;

	
	ManagerClient(String clientName, Logger logger) {
    	startTimeUTClong = UTCUtility.utcJavaToOmg(System.currentTimeMillis());		
		m_clientName = clientName;
		m_logger = logger;
	}
		
	public void setContainerServices(ContainerServicesImpl containerServices) {
		this.containerServices = containerServices;
	}
	
	/**
	 * @see si.ijs.maci.ClientOperations#authenticate(java.lang.String)
	 */
	public AuthenticationData authenticate(long execution_id, String question)
	{
        // keep old executionId if it exists
        if (executionId < 0) {
        	executionId = execution_id;
        }
        
		AuthenticationData ret = new AuthenticationData(
				 "C", 
				 ClientType.CLIENT_TYPE,
				 ImplLangType.JAVA,
				 false, 
				 startTimeUTClong,
				 executionId);
		return ret;
	}
	
	/**
	 * @see si.ijs.maci.ClientOperations#components_available(si.ijs.maci.ComponentInfo[])
	 */
	public void components_available(ComponentInfo[] components) {
//		m_logger.fine("+++++++++++++++++++ comp available +++++++++++++++++++");
		if (containerServices != null) {
	        List<ComponentDescriptor> compDescs = ComponentDescriptor.fromComponentInfoArray(components);
			containerServices.fireComponentsAvailable(compDescs);
		}
	}
	
	/**
	 * @see si.ijs.maci.ClientOperations#components_unavailable(java.lang.String[])
	 */
	public void components_unavailable(String[] component_names) {
//		m_logger.fine("-------------------- comp unavailable --------------------");
		if (containerServices != null) {
	        List<String> compNames = Arrays.asList(component_names); 
			containerServices.fireComponentsUnavailable(compNames);
		}
	}
	
	/**
	 * @see si.ijs.maci.ClientOperations#disconnect()
	 */
	public void disconnect()
	{
		m_logger.info("disconnected from manager");
	}
	
	/**
	 * @see si.ijs.maci.ClientOperations#message(short, java.lang.String)
	 */
	public void message(short type, String message)
	{
		m_logger.info("message: " + message);
	}
	
	/**
	 * @see si.ijs.maci.ClientOperations#taggedmessage(short, short, java.lang.String)
	 */
	public void taggedmessage(short type, short tag, String message)
	{
		m_logger.info("message: " + message);
	}
	
	/**
	 * @see si.ijs.maci.ClientOperations#name()
	 */
	public String name()
	{
		return m_clientName;
	}
	
	/**
	 * @see si.ijs.maci.ClientOperations#ping()
	 */
	public boolean ping()
	{
		return true;
	}
}
