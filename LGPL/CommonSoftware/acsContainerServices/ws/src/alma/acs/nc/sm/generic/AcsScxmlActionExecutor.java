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
package alma.acs.nc.sm.generic;

import java.util.Collection;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;

public interface AcsScxmlActionExecutor<A extends Enum<A>>
{
	/**
	 * @param action
	 * @param evtDispatcher
	 * @param errRep
	 * @param scInstance
	 * @param derivedEvents
	 * @return true if the <code>action</code> was recognized.
	 * @throws AcsJStateMachineActionEx
	 */
	public boolean execute(A action, EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx;

}
