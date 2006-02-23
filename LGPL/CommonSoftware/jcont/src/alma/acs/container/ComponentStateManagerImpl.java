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
package alma.acs.container;

import java.util.logging.Logger;

import alma.ACS.ComponentStates;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.component.ComponentStateManager;

/**
 * Manages the state of a component, enforcing state transition rules.
 * An instance of <code>ComponentStateManager</code> is logically part of the container,
 * but the component can access it and request a state change.
 * 
 * @author hsommer
 * created Oct 22, 2003 5:03:07 PM
 */
public class ComponentStateManagerImpl implements ComponentStateManager
{
	private ComponentStates m_currentState;
	private Logger m_logger;
	private String m_componentName;
	
	/**
	 * 
	 */
	public ComponentStateManagerImpl(String componentName, Logger logger)
	{
		m_componentName = componentName;
		m_logger = logger;
		m_currentState = ComponentStates.COMPSTATE_NEW;
	}

	
	/**
	 * {@inheritDoc}
	 * 
	 * @see alma.acs.component.ComponentStateManager#getCurrentState()
	 */
	public synchronized ComponentStates getCurrentState()
	{
		return m_currentState;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see alma.acs.component.ComponentStateManager#getName(alma.ACS.ComponentStates)
	 */
	public String getName(ComponentStates corbaStateObj)
	{
		switch (corbaStateObj.value())
		{
			case ComponentStates._COMPSTATE_NEW : return "NEW";
			case ComponentStates._COMPSTATE_INITIALIZING : return "INITIALIZING";
			case ComponentStates._COMPSTATE_INITIALIZED : return "INITIALIZED";
			case ComponentStates._COMPSTATE_OPERATIONAL : return "OPERATIONAL";
			case ComponentStates._COMPSTATE_ERROR : return "ERROR";
			case ComponentStates._COMPSTATE_DESTROYING : return "DESTROYING";
			case ComponentStates._COMPSTATE_ABORTING : return "ABORTING";
			case ComponentStates._COMPSTATE_DEFUNCT : return "DEFUNCT";
			default : 
				m_logger.warning("unknown component state " + corbaStateObj.value() + 
								"encountered - need to update " + this.getClass().getName());
				return "unknown";
		}
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see alma.acs.component.ComponentStateManager#setState(alma.ACS.ComponentStates)
	 */
	public synchronized void setState(ComponentStates newState)
		throws ComponentLifecycleException
	{
		if (newState == getCurrentState())
		{
			m_logger.fine("ignoring no-op transition from and to " + getName(newState));
		}
		else
		{
			// todo!: add validation
			
			m_logger.finer("switching state of component " + m_componentName + 
							" from " + getName(m_currentState) + 
							" to " + getName(newState));

			m_currentState = newState;			
		}
	}

	/**
	 * To be called by the container only (it's not part of the interface).
	 * <p>
	 * TODO: figure out state transition rules. Perhaps allow the currently discussed set of 
	 * rules only in this method, and restrict component-initiated transitions 
	 * (through method <code>setState</code>) to those involving the ERROR state.
	 * 
	 * @see alma.acs.component.ComponentStateManager#setState(alma.ACS.ComponentStates)
	 */
	synchronized void setStateByContainer(ComponentStates newState)
		throws ComponentLifecycleException
	{
		// just delegate as long as the rules are not clearly defined
		setState(newState);
	}


}
