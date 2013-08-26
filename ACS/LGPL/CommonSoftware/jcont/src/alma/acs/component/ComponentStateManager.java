/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.component;

import alma.ACS.ComponentStates;


/**
 * @author hsommer
 * created Oct 22, 2003 5:07:57 PM
 */
public interface ComponentStateManager
{
	/**
	 * Returns the current state of the component
	 * @return the component's current state 
	 */
	public ComponentStates getCurrentState();
	
	/**
	 * Requests to change the state. 
	 * To be called only by the component 
	 * (the container will use a separate method in the implementation class
	 * to allow for different transition validation rules)
	 * 
	 * @param newState  the new component state
	 * @throws ComponentLifecycleException  if the transition from the current state to 
	 * 			the new state is not allowed.
	 */
	public void setState(ComponentStates newState) throws ComponentLifecycleException;
	
	
	/**
	 * It seemed not worth it to create Java enum classes following the standard patterns,
	 * since the component has to return the CORBA state class anyway.
	 * Nonetheless, getting the state names for logging etc. might be useful. 
	 *  
	 * @param corbaStateObj  the int-based corba enum class
	 * @return the state name as specified in IDL, e.g. "COMPSTATE_NEW".
	 */
	public String getName(ComponentStates corbaStateObj);
}
