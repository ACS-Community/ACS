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

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;

/**
 * Interface to send events to the SM, similar
 * to the IDL-generated component "Operations" interface 
 * <p>
 * TODO: A code-generator option should decide whether the named
 * event methods delegate to {@link #fireSignal(Enum)}
 * or to {@link #fireSignalWithErrorFeedback(Enum)}, 
 * and add the declared exceptions accordingly.
 * Currently we use fireSignalWithErrorFeedback.
 * @author hsommer
 */
public interface EventSubscriberSignalHandler
{
	boolean setUpEnvironment() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean startReceivingEvents() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean suspend() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean resume() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean stopReceivingEvents() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean cleanUpEnvironment() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;
	

	boolean fireSignal(EventSubscriberSignal signal);

	boolean fireSignalWithErrorFeedback(EventSubscriberSignal signal) 
			throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

}
