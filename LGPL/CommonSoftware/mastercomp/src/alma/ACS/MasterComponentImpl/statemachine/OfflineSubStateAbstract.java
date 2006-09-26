package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.genfw.runtime.sm.AcsState;

/**
 * Abstract class for substates of composite state 'Offline'.
 */
public abstract class OfflineSubStateAbstract implements AcsState 
{

	protected AlmaSubsystemContext m_superContext;
	protected OfflineState m_offlineContext;

    public OfflineSubStateAbstract(AlmaSubsystemContext superContext, OfflineState offlineContext) {
        m_superContext = superContext;
        m_offlineContext = offlineContext;
    }

	public abstract AcsState[] getStateHierarchy();
	public abstract String stateName();

    public abstract void entry();
    
    public void initPass1() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "initPass1");
    }

    public void initPass2() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "initPass2");
    }

    public void shutdownPass2() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "shutdownPass2");
    }

}
