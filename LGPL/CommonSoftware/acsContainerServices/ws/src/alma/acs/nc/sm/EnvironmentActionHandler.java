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
 * Handler for actions EnvironmentCreator and EnvironmentDestructor
 * 
 * @author hsommer
 */
public class EnvironmentActionHandler implements AcsScxmlActionExecutor<EventSubscriberAction>
{
	private final Logger logger;

	public EnvironmentActionHandler(Logger logger) {
		this.logger = logger;
	}

	@Override
	public boolean execute(EventSubscriberAction action, EventDispatcher evtDispatcher, ErrorReporter errRep,
			SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
					throws ModelException, SCXMLExpressionException {
		switch (action) {
		case EnvironmentCreator:
			create(evtDispatcher, errRep, scInstance, derivedEvents);
			return true;

		case EnvironmentDestructor:
			destroy(evtDispatcher, errRep, scInstance, derivedEvents);
			return true;

		default:
			return false;
		}
		
	}
	
	public void create(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) {
		logger.info("EnvironmentCreator action called, in the user-supplied action dispatcher");
	}
	
	public void destroy(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) {
		logger.info("EnvironmentDestructor action called, in the user-supplied action dispatcher");
	}

}
