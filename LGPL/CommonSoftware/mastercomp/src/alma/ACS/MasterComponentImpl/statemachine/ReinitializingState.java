package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_REINITIALIZING;
import alma.acs.genfw.runtime.sm.AcsDoActivity;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateActionException;

public class ReinitializingState extends OfflineSubStateAbstract implements AcsSimpleState
{

	private AcsDoActivity m_doActivity;
	
    public ReinitializingState(AlmaSubsystemContext superContext, OfflineState offlineContext) {
        super(superContext, offlineContext);
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
		// perform do/ action asynchronously
		if (m_doActivity == null) {
			m_doActivity = new AcsDoActivity("Reinitializing", m_superContext.m_stateOnline, m_superContext.m_stateError) {
                public void runActions() throws AcsStateActionException 
				{
					m_superContext.reinitSubsystem();
				}
			};
		}
		m_doActivity.execute();
	}

}
