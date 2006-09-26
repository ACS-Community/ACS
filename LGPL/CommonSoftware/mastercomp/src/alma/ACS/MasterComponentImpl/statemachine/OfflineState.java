package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_OFFLINE;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.genfw.runtime.sm.AcsState;

public class OfflineState extends AvailableSubStateAbstract
{
	public OfflineSubStateAbstract m_subState;

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
		
		// always propagate state change upwards
		m_context.setSubstate(this, eventName);
		
		if (oldSubState != newSubState) {
			m_subState.entry();
		}
	}


	public void entry() {
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
