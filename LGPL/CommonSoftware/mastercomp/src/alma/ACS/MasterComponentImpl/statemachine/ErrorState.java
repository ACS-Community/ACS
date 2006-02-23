package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_ERROR;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;

public class ErrorState extends AvailableSubStateAbstract implements AcsSimpleState
{
	public ErrorState(AlmaSubsystemContext superContext, AvailableState context) {
		super(superContext, context);
	}

	public String stateName() {
		return SUBSYSSTATE_ERROR.value;
	}

	public void activate(String eventName) {
		synchronized (m_superContext) {		
			m_context.setSubstate(this, eventName);
		}
	}
	
	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsState#getStateHierarchy()
	 */
	public AcsState[] getStateHierarchy() {
		return new AcsState[] {this};
	}


	public void entry() {
	}

}
