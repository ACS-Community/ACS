package alma.ACS.MasterComponentImpl.statemachine;

import alma.ACS.SUBSYSSTATE_OPERATIONAL;
import alma.acs.genfw.runtime.sm.AcsSimpleState;
import alma.acs.genfw.runtime.sm.AcsState;

public class OperationalState extends AvailableSubStateAbstract implements AcsSimpleState
{

	public OperationalState(AlmaSubsystemContext superContext,
			AvailableState context) {
		super(superContext, context);

	}

	public String stateName() {
		return SUBSYSSTATE_OPERATIONAL.value;
	}

	public void activate(String eventName) {
		synchronized (m_superContext) {		
			m_context.setSubstate(this, eventName);
		}
	}
	

	public void entry() {
	}

	public void start() {
		m_context.setSubstate(m_superContext.m_stateOperational, "start");
	}

	public void stop() {
		m_context.setSubstate(m_superContext.m_stateOnline, "stop");
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AcsState#getStateHierarchy()
	 */
	public AcsState[] getStateHierarchy() {
		return new AcsState[] {this};
	}

}
