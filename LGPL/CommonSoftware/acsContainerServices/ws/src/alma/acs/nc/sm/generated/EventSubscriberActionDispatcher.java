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
import java.util.logging.Logger;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;
import alma.acs.nc.sm.generic.AcsScxmlActionExecutor;

/**
 * Convenience class for dispatching from enum-based {@link AcsScxmlActionExecutor} 
 * to action handler {@link EventSubscriberAllActionsHandler}.
 * <p>
 * This class is only useful if you chose to implement all actions in one class
 * that implements {@link EventSubscriberAllActionsHandler},
 * which is typically the case for small state machines or for unit testing.
 * For other cases you must implement the dispatching yourself.
 * 
 * @author hsommer
 */
public class EventSubscriberActionDispatcher implements AcsScxmlActionExecutor<EventSubscriberAction>
{
	private final EventSubscriberAllActionsHandler actionHandler;

	public EventSubscriberActionDispatcher(EventSubscriberAllActionsHandler actionHandler, Logger logger) {
		this.actionHandler = actionHandler;
	}

	public boolean execute(EventSubscriberAction action, EventDispatcher evtDispatcher, ErrorReporter errRep,
			SCInstance scInstance, Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {

		boolean ret = true;

		switch (action) {

		case createEnvironment:
			actionHandler.createEnvironmentAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case destroyEnvironment:
			actionHandler.destroyEnvironmentAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case createConnection:
			actionHandler.createConnectionAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case destroyConnection:
			actionHandler.destroyConnectionAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case suspendConnection:
			actionHandler.suspendAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case resumeConnection:
			actionHandler.resumeAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		default:
			ret = false;
		}

		return ret;
	}
}
