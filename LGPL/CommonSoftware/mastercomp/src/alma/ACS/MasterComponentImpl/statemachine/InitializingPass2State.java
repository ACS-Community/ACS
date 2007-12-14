package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_INITIALIZING_PASS2;
import alma.acs.genfw.runtime.sm.AcsDoActivity;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateActionException;
import alma.acs.logging.AcsLogger;

public class InitializingPass2State extends OfflineSubStateAbstract implements AcsSimpleState
{
	private AcsDoActivity m_doActivity;

    public InitializingPass2State(AlmaSubsystemContext superContext, OfflineState offlineContext, AcsLogger logger) {
        super(superContext, offlineContext, logger);
    }
    
	public String stateName() {
		return SUBSYSSTATE_INITIALIZING_PASS2.value;
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
					"InitializingPass2", 
					m_superContext.m_stateOnline, m_superContext.m_stateError, 
					logger, m_superContext.getSharedActivityExecutor() ) 
					{
						public void runActions() throws AcsStateActionException  {
							m_superContext.initSubsysPass2();
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
