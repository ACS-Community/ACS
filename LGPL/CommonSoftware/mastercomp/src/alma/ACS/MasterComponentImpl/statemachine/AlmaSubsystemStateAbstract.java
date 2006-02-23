package alma.ACS.MasterComponentImpl.statemachine;

import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.genfw.runtime.sm.AcsStateIllegalEventException;

/**
 * Abstract state class. Concrete subclass receives delegation calls from the
 * SM's super context class.
 */
public abstract class AlmaSubsystemStateAbstract implements AcsState 
{
	protected AlmaSubsystemContext m_superContext;

	public AlmaSubsystemStateAbstract(AlmaSubsystemContext superContext) {
		m_superContext = superContext;
	}

	public abstract AcsState[] getStateHierarchy();
	public abstract String stateName();

	// all events that the master component can handle
	// make all events illegal here, and let subclasses allow certain events 

	public void stop() throws AcsStateIllegalEventException {
		m_superContext.illegalEvent(stateName(), "stop");
	}

	public void shutdownPass1() throws AcsStateIllegalEventException {
		m_superContext.illegalEvent(stateName(), "shutdownPass1");
	}

	public void initPass2() throws AcsStateIllegalEventException {
		m_superContext.illegalEvent(stateName(), "initPass2");
	}

	public void start() throws AcsStateIllegalEventException {
		m_superContext.illegalEvent(stateName(), "start");
	}

	public void initPass1() throws AcsStateIllegalEventException {
		m_superContext.illegalEvent(stateName(), "initPass1");
	}

	public void shutdownPass2() throws AcsStateIllegalEventException {
		m_superContext.illegalEvent(stateName(), "shutdownPass2");
	}

	public void reinit() throws AcsStateIllegalEventException {
		m_superContext.illegalEvent(stateName(), "reinit");
	}

	public void error() throws AcsStateIllegalEventException {
		m_superContext.illegalEvent(stateName(), "error");
	}

	public abstract void entry();

	public void exit() {
	}

}
