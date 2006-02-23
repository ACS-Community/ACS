package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_PRESHUTDOWN;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;

public class PreShutdownState extends OfflineSubStateAbstract implements AcsSimpleState
{

    public PreShutdownState(AlmaSubsystemContext superContext, OfflineState context) {
        super(superContext, context);
    }

	public String stateName() {
		return SUBSYSSTATE_PRESHUTDOWN.value;
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

    public void shutdownPass2() {
        m_superContext.m_stateShuttingdownPass2.activate("shutdownPass2");
    }

}
