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

import static alma.acs.nc.sm.generated.EventSubscriberSignal.cleanUpEnvironment;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.resume;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.setUpEnvironment;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.startReceivingEvents;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.stopReceivingEvents;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.suspend;

import org.apache.commons.scxml.model.ModelException;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;
import alma.acs.nc.sm.generic.AcsScxmlEngine;

/**
 * Dispatches SM signals (events) by calling {@link AcsScxmlEngine#fireSignalWithErrorFeedback(Enum)}
 * on the provided state machine. 
 * <p>
 * This is a convenience class that can be used as a base class for the ACS component
 * or other classes that use the state machine.
 * <p>
 * TODO: A code-generator option should decide whether the named
 * event methods delegate to {@link #fireSignal(Enum)}
 * or to {@link #fireSignalWithErrorFeedback(Enum)}.
 * Currently we use fireSignalWithErrorFeedback.
 *  
 * @author hsommer
 */
public abstract class EventSubscriberSignalDispatcher implements EventSubscriberSignalHandler
{
	protected abstract AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction> getScxmlEngine();
	
	@Override
	public boolean setUpEnvironment() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx {
		return fireSignalWithErrorFeedback(setUpEnvironment);
	}

	@Override
	public boolean startReceivingEvents() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx {
		return fireSignalWithErrorFeedback(startReceivingEvents);
	}

	@Override
	public boolean suspend() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx {
		return fireSignalWithErrorFeedback(suspend);
	}

	@Override
	public boolean resume() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx {
		return fireSignalWithErrorFeedback(resume);
	}

	@Override
	public boolean stopReceivingEvents() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx {
		return fireSignalWithErrorFeedback(stopReceivingEvents);
	}

	@Override
	public boolean cleanUpEnvironment() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx {
		return fireSignalWithErrorFeedback(cleanUpEnvironment);
	}
	
	@Override
	public boolean fireSignal(EventSubscriberSignal signal) {
		return getScxmlEngine().fireSignal(signal);
	}
	
	@Override
	public boolean fireSignalWithErrorFeedback(EventSubscriberSignal signal) 
			throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx {
		try {
			return getScxmlEngine().fireSignalWithErrorFeedback(signal);
		} catch (ModelException ex) {
			// Since we do not modify the SM model at runtime, this seems
			// so unlikely that we don't want to declare ModelException
			// as a checked exception.
			throw new IllegalStateException(ex);
		}
	}
}
