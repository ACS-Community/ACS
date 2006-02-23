package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_INITIALIZING_PASS1;
import alma.acs.genfw.runtime.sm.AcsDoActivity;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateActionException;


public class InitializingPass1State extends OfflineSubStateAbstract implements AcsSimpleState
{
	private AcsDoActivity m_doActivity;
	
    public InitializingPass1State(AlmaSubsystemContext superContext, OfflineState context) 
    {
        super(superContext, context);
    }

	public String stateName() 
	{
		return SUBSYSSTATE_INITIALIZING_PASS1.value;
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsState#getStateHierarchy()
	 */
	public AcsState[] getStateHierarchy() 
	{
		return new AcsState[] {this};
	}

	public void activate(String eventName) 
	{
		synchronized (m_superContext) {		
			m_offlineContext.setSubstate(this, eventName);
		}
	}
	

	public void entry() 
	{
		// perform do/ action asynchronously
		if (m_doActivity == null) {
			m_doActivity = new AcsDoActivity("InitializingPass1", m_superContext.m_statePreInitialized, m_superContext.m_stateError) {
                public void runActions() throws AcsStateActionException 
				{
					m_superContext.initSubsysPass1();
				}
			};
		}
		m_doActivity.execute();
	}

	public void exit()
	{
		m_doActivity.terminateActions();
	}
}


