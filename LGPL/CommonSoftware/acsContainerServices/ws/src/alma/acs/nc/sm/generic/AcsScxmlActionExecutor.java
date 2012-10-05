package alma.acs.nc.sm.generic;

import java.util.Collection;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.SCXMLExpressionException;
import org.apache.commons.scxml.TriggerEvent;
import org.apache.commons.scxml.model.ModelException;

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
	 * @throws ModelException
	 * @throws SCXMLExpressionException
	 * @throws AcsJStateMachineActionEx
	 */
	public boolean execute(A action, EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) 
					throws ModelException, SCXMLExpressionException, AcsJStateMachineActionEx;

}
