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
package alma.demo.HelloDemoImpl;
import java.util.logging.Logger;

import org.omg.CORBA.DoubleHolder;
import org.omg.CORBA.IntHolder;

import alma.ACS.ComponentStates;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ContainerServices;
import alma.demo.HelloDemoOperations;

/**
 * A very simple component that does not make use of 
 * {@link alma.acs.component.ComponentImplBase}.
 * 
 * Javadoc comments have been removed to keep the
 * listing for the tutorial shorter.
 * 
 * @author hsommer
 */
public class HelloDemoImpl implements ComponentLifecycle, HelloDemoOperations
{
	private ContainerServices m_containerServices;
	private Logger m_logger;

	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle
	/////////////////////////////////////////////////////////////
	
	public void initialize(ContainerServices containerServices) {
		m_containerServices = containerServices;
		m_logger = m_containerServices.getLogger();
		m_logger.info("initialize() called...");
	}
    
	public void execute() {
		m_logger.info("execute() called...");
	}
    
	public void cleanUp() {
		m_logger.info("cleanUp() called..., nothing to clean up.");
	}
    
	public void aboutToAbort() {
		cleanUp();
		m_logger.info("managed to abort...");
        System.out.println("HelloDemo component managed to abort... you should know this even if the logger did not flush correctly!");
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
	
	public String sayHello() {
		m_logger.info("sayHello called...");
		return "hello";
	}
	
	public String sayHelloWithParameters(String inString,
			DoubleHolder inoutDouble, IntHolder outInt) {
		m_logger.info("sayHello called with arguments inString=" + inString
				+ "; inoutDouble=" + inoutDouble.value
				+ ". Will return 'hello'...");
		outInt.value = (int) Math.round(Math.E * 10000000);
		return "hello";
	}
}
