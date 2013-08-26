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

import alma.ACS.SUBSYSSTATE_OFFLINE;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.genfw.runtime.sm.AcsState;

public class OfflineState extends AvailableSubStateAbstract
{
	public OfflineSubStateAbstract m_subState;
	public boolean newEntryToThis = false;

	public OfflineState(AlmaSubsystemContext superContext, AvailableState context) {
		super(superContext, context);
	}

	public String stateName() {
		return SUBSYSSTATE_OFFLINE.value;
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsState#getStateHierarchy()
	 */
	public AcsState[] getStateHierarchy() {
		AcsState[] substates = m_subState.getStateHierarchy();
		AcsState[] hierarchy = new AcsState[substates.length + 1];
		hierarchy[0] = this;
		System.arraycopy(substates, 0, hierarchy, 1, substates.length);
		return hierarchy;
	}

	public void setSubstate(OfflineSubStateAbstract newSubState, String eventName) {
		OfflineSubStateAbstract oldSubState = m_subState;
		if (oldSubState != newSubState) {
			m_superContext.logTransition(oldSubState, newSubState, eventName);
			m_subState = newSubState;
		}
		
		// always propagate state change upwards. May result in a call to entry() if the previous state was not a substate of this state.
		m_context.setSubstate(this, eventName);
		
		if (newEntryToThis || oldSubState != newSubState) {
			m_subState.entry();
		}
		
		newEntryToThis = false;
	}


	public void entry() {
		newEntryToThis = true;
	}
	
	public void initPass1() throws AcsJIllegalStateEventEx {
		m_subState.initPass1();
	}

	public void initPass2() throws AcsJIllegalStateEventEx {
		m_subState.initPass2();
	}

	public void shutdownPass2() throws AcsJIllegalStateEventEx {
		m_subState.shutdownPass2();
	}

}
