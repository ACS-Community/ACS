package alma.acs.nc.sm.generic;

import java.util.Collection;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;

public interface AcsScxmlActionExecutor<A extends Enum<A>>
{
	/**
	 * @param action
	 * @param evtDispatcher
	 * @param errRep
	 * @param scInstance
	 * @param derivedEvents
	 * @return true if the <code>action</code> was recognized.
	 * @throws AcsJStateMachineActionEx
	 */
	public boolean execute(A action, EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx;

}
