/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory,
 * 2002 Copyright by ESO (in the framework of the ALMA collaboration), All
 * rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.test.corbareftest.component;

import java.util.logging.Logger;

import alma.ACS.ComponentStates;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.CorbaRefTest.HelloWorldOperations;

/**
 * A component for testing the persistence of CORBA
 * references 
 * 
 * @author acaproni
 */
public class HelloWorldImpl extends ComponentImplBase implements ComponentLifecycle, HelloWorldOperations
{
	private ContainerServices m_containerServices;
	private Logger m_logger;

	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle
	/////////////////////////////////////////////////////////////
	
	public void initialize(ContainerServices containerServices) {
		m_containerServices = containerServices;
		m_logger = m_containerServices.getLogger();
		m_logger.log(AcsLogLevel.DEBUG,"initialize");
	}
    
	public void execute() {
		m_logger.log(AcsLogLevel.DEBUG,"execute");
	}
    
	public void cleanUp() {
		m_logger.log(AcsLogLevel.DEBUG,"cleanUp");
	}
    
	public void aboutToAbort() {
		cleanUp();
		m_logger.log(AcsLogLevel.DEBUG,"aboutToAbort");
	}
	
	/////////////////////////////////////////////////////////////
	// Implementation of ACSComponent
	/////////////////////////////////////////////////////////////
	
	public ComponentStates componentState() {
		return m_containerServices.getComponentStateManager().getCurrentState();
	}
	public String name() {
		return m_containerServices.getName();
	}
	
	/////////////////////////////////////////////////////////////
	// Implementation of HelloDemoOperations
	/////////////////////////////////////////////////////////////
	
	public void displayMessage() {
		System.out.println("displayMessage called...");
	}
	
}
