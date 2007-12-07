package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_SHUTTINGDOWN_PASS2;
import alma.acs.genfw.runtime.sm.AcsDoActivity;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateActionException;
import alma.acs.logging.AcsLogger;

public class ShuttingdownPass2State extends OfflineSubStateAbstract implements AcsSimpleState
{

	private AcsDoActivity m_doActivity;
	
    public ShuttingdownPass2State(AlmaSubsystemContext superContext, OfflineState context, AcsLogger logger) {
        super(superContext, context, logger);
    }
    
	public String stateName() {
		return SUBSYSSTATE_SHUTTINGDOWN_PASS2.value;
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
		// perform do/ action asynchronously
		if (m_doActivity == null) {
			m_doActivity = new AcsDoActivity("ShuttingdownPass2", m_superContext.m_stateShutdown, m_superContext.m_stateError, logger) {
                public void runActions() throws AcsStateActionException 
				{
					m_superContext.shutDownSubsysPass2();
				}
			};
		}
		m_doActivity.execute();
	}

}
