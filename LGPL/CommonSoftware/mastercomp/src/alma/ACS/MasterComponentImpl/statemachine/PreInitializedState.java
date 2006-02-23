package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_PREINITIALIZED;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;

public class PreInitializedState extends OfflineSubStateAbstract implements AcsSimpleState {

    public PreInitializedState(AlmaSubsystemContext superContext, OfflineState context) {
        super(superContext, context);

    }

	public String stateName() {
		return SUBSYSSTATE_PREINITIALIZED.value;
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsState#getStateHierarchy()
	 */
	public AcsState[] getStateHierarchy() {
		return new AcsState[] {this};
	}

	public void activate(String eventName) {
		synchronized (m_superContext) {		
			m_offlineContext.setSubstate(this, eventName);
		}
	}
	

	public void entry() {
	}


    public void initPass2() {
        m_superContext.m_stateInitializingPass2.activate("initPass2");
    }

}
