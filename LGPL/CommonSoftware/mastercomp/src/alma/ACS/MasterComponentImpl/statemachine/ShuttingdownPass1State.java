package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_SHUTTINGDOWN_PASS1;
import alma.acs.genfw.runtime.sm.AcsDoActivity;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateActionException;

public class ShuttingdownPass1State extends OfflineSubStateAbstract implements AcsSimpleState
{

	private AcsDoActivity m_doActivity;
	
    public ShuttingdownPass1State(AlmaSubsystemContext superContext, OfflineState context) {
        super(superContext, context);
    }
    
	public String stateName() {
		return SUBSYSSTATE_SHUTTINGDOWN_PASS1.value;
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
			m_doActivity = new AcsDoActivity("ShuttingdownPass1", m_superContext.m_statePreShutdown, m_superContext.m_stateError) {
                public void runActions() throws AcsStateActionException 
				{
					m_superContext.shutDownSubsysPass1();
				}
			};
		}
		m_doActivity.execute();
	}

}
