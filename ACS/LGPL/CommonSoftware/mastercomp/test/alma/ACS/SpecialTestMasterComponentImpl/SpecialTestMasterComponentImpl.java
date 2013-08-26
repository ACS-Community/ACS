/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.ACS.SpecialTestMasterComponentImpl;

import alma.ACS.RWdouble;
import alma.ACS.RWdoubleHelper;
import alma.ACS.RWdoublePOATie;
import alma.ACS.SpecialTestMasterComponentOperations;
import alma.ACS.MasterComponentImpl.MasterComponentImplBase;
import alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemActions;
import alma.ACS.impl.RWdoubleImpl;
import alma.ACS.jbaci.DataAccess;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

/**
 * Custom subsystem master component for testing purposes, with
 * an additional subsystem-specific method and  
 * log-only dummy implementation of the action methods.
 *  
 * @author hsommer
 * created Apr 14, 2004 1:47:40 PM
 */
public class SpecialTestMasterComponentImpl extends MasterComponentImplBase 
		implements SpecialTestMasterComponentOperations
{

	private RWdouble m_someOtherProperty;
	private DataAccess m_someOtherPropertyDataAccess;
	

	/**
	 * As part of the initialization of this test master component, 
	 * the baci property 'someOtherProperty' is set up.
	 * 
	 * @see alma.acs.component.ComponentLifecycle#initialize(alma.acs.container.ContainerServices)
	 */
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {

		super.initialize(containerServices);
		
		try
		{
			RWdoubleImpl someOtherPropertyImpl = new RWdoubleImpl("someOtherProperty", this);
			m_someOtherPropertyDataAccess = someOtherPropertyImpl.getDataAccess();
			
			RWdoublePOATie someOtherPropertyTie = new RWdoublePOATie(someOtherPropertyImpl);
			m_someOtherProperty = RWdoubleHelper.narrow(this.registerProperty(someOtherPropertyImpl, someOtherPropertyTie));
		}
		catch (Throwable th)
		{
			throw new ComponentLifecycleException("Failed to create ACS property 'someOtherProperty'.", th); 
		}
	}
	
	
	/**
	 * @see alma.ACS.MasterComponentImpl.MasterComponentImplBase#getActionHandler()
	 */
	protected AlmaSubsystemActions getActionHandler() {	
		return new SpecialTestSubsystemActions(m_logger);
	}

	/**
	 * @see alma.ACS.SpecialTestMasterComponentOperations#componentNeedsAttention(java.lang.String, java.lang.String)
	 */
	public void componentNeedsAttention(String componentName, String troubleCode)
	{
		m_logger.info("received SOS call from component '" + componentName + "'...");
		
		// now in a real impl, we should check if the component is one from our subsystem
		// (we should keep its reference in a map), and take appropriate action,
		// such as starting another component of that type and killing this one,
		// or, if things can't be cured, going into subsytem error state
		// by calling "doTransition(SubsystemStateEvent.SUBSYSEVENT_ERROR);"
	}
	
	
	public RWdouble someOtherProperty()
	{
		return m_someOtherProperty;
	}
	

}
