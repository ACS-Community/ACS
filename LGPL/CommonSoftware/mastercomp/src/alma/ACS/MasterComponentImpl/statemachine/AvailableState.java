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
package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_AVAILABLE;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.genfw.runtime.sm.AcsState;

public class AvailableState extends AlmaSubsystemStateAbstract
{
	public AvailableSubStateAbstract m_subState;

	public AvailableState(AlmaSubsystemContext superContext) {
		super(superContext);
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsState#stateName()
	 */
	public String stateName() {
		return SUBSYSSTATE_AVAILABLE.value;
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsState#getStateHierarchy()
	 */
	public synchronized AcsState[] getStateHierarchy() {
		AcsState[] substates = m_subState.getStateHierarchy();
		AcsState[] hierarchy = new AcsState[substates.length + 1];
		hierarchy[0] = this;
		System.arraycopy(substates, 0, hierarchy, 1, substates.length);
		return hierarchy;
	}

	public void setSubstate(AvailableSubStateAbstract newSubState, String eventName) {
		AvailableSubStateAbstract oldSubState = m_subState;
		if (oldSubState != newSubState) {
			m_superContext.logTransition(oldSubState, newSubState, eventName);
			m_subState = newSubState;
		}
		
		// always propagate state change upwards
		m_superContext.setState(this, eventName);		

		// from UML spec (1.5):
		// If the transition goes to a substate of the composite state, then that
		// substate becomes active and its entry code is executed after the execution of the
		// entry code of the composite state. This rule applies recursively if the transition
		// terminates on a transitively nested substate.
		if (oldSubState != newSubState) {
			m_subState.entry();
		}
	}


	// events to be handled by substate classes

	public void entry() {
	}

	public void initPass1() throws AcsJIllegalStateEventEx {
		m_subState.initPass1();
	}

	public void initPass2() throws AcsJIllegalStateEventEx {
		m_subState.initPass2();
	}

	public void start() throws AcsJIllegalStateEventEx {
		m_subState.start();
	}

	public void stop() throws AcsJIllegalStateEventEx {
		m_subState.stop();
	}

	public void shutdownPass2() throws AcsJIllegalStateEventEx {
		m_subState.shutdownPass2();
	}

	
	public void reinit() {
		m_subState.reinit();
	}

	public void error() {
		m_subState.error();
	}
	
	public void shutdownPass1() {
		m_subState.shutdownPass1();
	}
	
}
