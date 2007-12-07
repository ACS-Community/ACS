package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_SHUTDOWN;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.logging.AcsLogger;

public class ShutdownState extends OfflineSubStateAbstract implements AcsSimpleState
{

    public ShutdownState(AlmaSubsystemContext superContext, OfflineState context, AcsLogger logger) {
        super(superContext, context, logger);

    }

	public String stateName() {
		return SUBSYSSTATE_SHUTDOWN.value;
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

    public void initPass1() {
        m_superContext.m_stateInitializingPass1.activate("initPass1");
    }

}
