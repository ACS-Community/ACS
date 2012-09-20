package alma.acs.nc.sm;

import java.util.Collection;
import java.util.logging.Logger;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.SCXMLExpressionException;
import org.apache.commons.scxml.TriggerEvent;
import org.apache.commons.scxml.model.ModelException;

import alma.acs.nc.sm.generated.EventSubscriberAction;
import alma.acs.nc.sm.generic.AcsScxmlActionExecutor;

/**
 * Handler for actions ConnectionSuspender and ConnectionResumer
 * 
 * @author hsommer
 */
public class SuspendResumeActionHandler implements AcsScxmlActionExecutor<EventSubscriberAction>
{
	private final Logger logger;

	public SuspendResumeActionHandler(Logger logger) {
		this.logger = logger;
	}

	@Override
	public boolean execute(EventSubscriberAction action, EventDispatcher evtDispatcher, ErrorReporter errRep,
			SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
					throws ModelException, SCXMLExpressionException {
		switch (action) {
		case ConnectionSuspender:
			suspend(evtDispatcher, errRep, scInstance, derivedEvents);
			return true;

		case ConnectionResumer:
			resume(evtDispatcher, errRep, scInstance, derivedEvents);
			return true;

		default:
			return false;
		}
	}
	
	public void suspend(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) {
		logger.info("Suspend action called, in the user-supplied action dispatcher");
	}

	public void resume(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) {
		logger.info("Resume action called, in the user-supplied action dispatcher");
	}
	
}
