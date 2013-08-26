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
package alma.acs.component;

import alma.ACS.ACSComponentOperations;
import alma.ACS.ComponentStates;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogger;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

/**
 * Convenience base class for components. 
 * Provides default implementation of the required interface methods.
 * 
 * @author hsommer May 6, 2003 11:50:18 AM
 */
public class ComponentImplBase implements ComponentLifecycle, ACSComponentOperations
{
    /** 
     * name of the component instance, which is either given statically in the CDB deployment configuration 
     * or determined right before the instantiation of a dynamic component.
     * To be used by subclass as a shortcut for <code>m_containerServices.getName()</code>.
     */
	protected String m_instanceName;
	
    /** 
     * API through which the container explicitly provides various services to its components.
     */
	protected ContainerServices m_containerServices;
	
    /** 
     * Logger to be used by subclass for all application code logging.
     */
	protected AcsLogger m_logger;


	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle
	/////////////////////////////////////////////////////////////

	/**
	 * Subclass must call <code>super.initialize(containerServices)</code> 
	 * unless it overrides as well all other methods which access the member vars of this class.
	 *  
	 * @see alma.acs.component.ComponentLifecycle#initialize(ContainerServices)
	 */
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException
	{
		m_containerServices = containerServices;
	    checkProperInit();
		m_instanceName = containerServices.getName();
		m_logger = m_containerServices.getLogger();
	}

	/* (non-Javadoc)
	 * @see alma.acs.component.ComponentLifecycle#execute()
	 */
	public void execute() throws ComponentLifecycleException
	{
	    checkProperInit();
	}

	/* (non-Javadoc)
	 * @see alma.acs.component.ComponentLifecycle#cleanUp()
	 */
	public void cleanUp() throws AcsJComponentCleanUpEx
	{
		// by default nothing to do
	}

	/**
	 * Calls {@link #cleanUp() cleanUp)}.
	 * Override this method if the emergency situation of a container/component abort 
	 * with unknown remaining lifetime requires a different strategy than the regular clean-up. 
	 *  
	 * @see alma.acs.component.ComponentLifecycle#aboutToAbort()
	 */
	public void aboutToAbort()
	{
		m_logger.fine("calling cleanUp() from the abort thread...");
		try {
			cleanUp();
		} catch (AcsJComponentCleanUpEx ex) {
			ex.printStackTrace();
		}
	}



	/////////////////////////////////////////////////////////////
	// Implementation of ACSComponentOperations
	/////////////////////////////////////////////////////////////

	/**
	 * @see alma.ACS.ACSComponentOperations#componentState()
	 */
	public ComponentStates componentState()
	{
	    checkProperInit();
		ComponentStates state = m_containerServices.getComponentStateManager().getCurrentState(); 
		return state;
	}

	/**
	 * @see alma.ACS.ACSComponentOperations#name()
	 */
	public String name()
	{
	    checkProperInit();
		return m_instanceName;
	}


	/////////////////////////////////////////////////////////////
	// other
	/////////////////////////////////////////////////////////////

	/**
	 * Asserts that m_containerServices != null.
	 * @throws IllegalStateException with error message if {@link #initialize(ContainerServices) initialize} 
	 * was not called correctly.
	 */
	private void checkProperInit() {
	    if (m_containerServices == null) {
	        String msg = "Component base class 'ComponentImplBase' is not properly initialized: 'initialize(containerServices)' was not called at all, or with a null arg.";
		    throw new IllegalStateException(msg);	        
	    }
	}
}
