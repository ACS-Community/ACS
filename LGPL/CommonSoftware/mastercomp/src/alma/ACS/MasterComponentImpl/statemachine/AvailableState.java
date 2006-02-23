package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_AVAILABLE;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateIllegalEventException;

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

	public void initPass1() throws AcsStateIllegalEventException {
		m_subState.initPass1();
	}

	public void initPass2() throws AcsStateIllegalEventException {
		m_subState.initPass2();
	}

	public void start() throws AcsStateIllegalEventException {
		m_subState.start();
	}

	public void stop() throws AcsStateIllegalEventException {
		m_subState.stop();
	}

	public void shutdownPass2() throws AcsStateIllegalEventException {
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
