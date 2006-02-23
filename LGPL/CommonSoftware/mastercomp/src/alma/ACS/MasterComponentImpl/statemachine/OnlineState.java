package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_ONLINE;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;

public class OnlineState extends AvailableSubStateAbstract implements AcsSimpleState
{

	public OnlineState(AlmaSubsystemContext superContext, AvailableState context) {
		super(superContext, context);
	}

	public String stateName() {
		return SUBSYSSTATE_ONLINE.value;
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsState#getStateHierarchy()
	 */
	public AcsState[] getStateHierarchy() {
		return new AcsState[] {this};
	}

	public void activate(String eventName) {
		synchronized (m_superContext) {		
			m_context.setSubstate(this, eventName);
		}
	}
	

	
	public void entry() {
	}

	public void stop() {
		m_context.setSubstate(m_superContext.m_stateOnline, "stop");
	}

	public void start() {
		m_context.setSubstate(m_superContext.m_stateOperational, "start");

	}

}
