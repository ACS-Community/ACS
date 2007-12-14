package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_REINITIALIZING;
import alma.acs.genfw.runtime.sm.AcsDoActivity;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateActionException;
import alma.acs.logging.AcsLogger;

public class ReinitializingState extends OfflineSubStateAbstract implements AcsSimpleState
{
	/**
	 * Would be nice to mark this field final, but the AcsDoActivity requires the nextState
	 * which may not be constructed yet when this class's ctor is called.
	 */
	private AcsDoActivity m_doActivity;
	
    public ReinitializingState(AlmaSubsystemContext superContext, OfflineState offlineContext, AcsLogger logger) {
        super(superContext, offlineContext, logger);
    }
    
	public String stateName() {
		return SUBSYSSTATE_REINITIALIZING.value;
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
		if (m_doActivity == null) {
			m_doActivity = new AcsDoActivity(
					"Reinitializing", 
					m_superContext.m_stateOnline, m_superContext.m_stateError, 
					logger, m_superContext.getSharedActivityExecutor() ) 
					{
						public void runActions() throws AcsStateActionException  {
							m_superContext.reinitSubsystem();
						}
					};        			
		}
		// perform do/ action asynchronously
		m_doActivity.execute();
	}

	public void exit() {
		m_doActivity.terminateActions();
	}

}
