package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.genfw.runtime.sm.AcsState;

/**
 * Abstract class for substates of composite state 'Available'.
 */
public abstract class AvailableSubStateAbstract implements AcsState 
{
	protected AlmaSubsystemContext m_superContext;

    // the nested state AvailableState serves as both state class and context class for its substates
    protected AvailableState m_context;

    public AvailableSubStateAbstract(AlmaSubsystemContext superContext, AvailableState context) {
        m_superContext = superContext;
        m_context = context;
    }

	public abstract AcsState[] getStateHierarchy();
	public abstract String stateName();


    public abstract void entry();

    // events to be dealt with by subclasses
    
    public void initPass1() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "initPass1");
    }

    public void initPass2() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "initPass2");
    }

    public void start() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "start");
    }

    public void stop() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "stop");
    }

    public void shutdownPass2() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "shutdownPass1");
    }

    
    // events handled by this base class 
    
    public void shutdownPass1() {
    	m_superContext.m_stateShuttingdownPass1.activate("shutdownPass1");
    }

    public void reinit() {		
    	m_superContext.m_stateReinitializing.activate("reinit");
    }

    public void error() {
		m_context.setSubstate(m_superContext.m_stateError, "error");
    }

}
