package alma.acs.nc.sm.generic;

import java.util.Collection;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.SCXMLExpressionException;
import org.apache.commons.scxml.TriggerEvent;
import org.apache.commons.scxml.model.ModelException;

public interface AcsScxmlActionExecutor<E extends Enum<E>>
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
	 */
	public boolean execute(E action, EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) 
					throws ModelException, SCXMLExpressionException;

}
