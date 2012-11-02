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
package alma.acs.nc.sm.generated;

import java.util.Collection;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;

/**
 * A convenience interface that bundles all state machine actions as methods.
 * <p>
 * Note that the state machine design leaves full choice about how to implement
 * actions. Just in case you want to do it all in one class then this interface can be useful,
 * mostly to be alerted of SM changes by compile errors (especially when using 'Override' tags).
 * 
 * @see EventSubscriberActionDispatcher
 */
public interface EventSubscriberAllActionsHandler
{
	void createEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;

	void destroyEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;
	
	void createConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;
	
	void destroyConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;
	
	void suspendAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;
	
	void resumeAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;

}
