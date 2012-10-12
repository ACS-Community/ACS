package alma.acs.nc.sm;

import java.util.Collection;
import java.util.logging.Logger;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;
import alma.acs.nc.sm.generated.EventSubscriberAction;
import alma.acs.nc.sm.generic.AcsScxmlActionExecutor;

/**
 * Handler for actions createConnection and destroyConnection.
 * <p>
 * Note that grouping these two actions into one class is an arbitrary choice,
 * not demanded by the state machine framework.
 * <p>
 * In this class we give a fairly empty base implementation which 
 * can be overridden by subclasses of {@link alma.acs.nc.AcsEventSubscriberImplBase}. 
 * 
 * @author hsommer
 */
public class ConnectionActionHandler implements AcsScxmlActionExecutor<EventSubscriberAction>
{
	private final Logger logger;

	public ConnectionActionHandler(Logger logger) {
		this.logger = logger;
	}

	@Override
	public boolean execute(EventSubscriberAction action, EventDispatcher evtDispatcher, ErrorReporter errRep,
			SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
					throws AcsJStateMachineActionEx {
		
		switch (action) {
		case createConnection:
			createConnection(evtDispatcher, errRep, scInstance, derivedEvents);
			return true;

		case destroyConnection:
			destroyConnection(evtDispatcher, errRep, scInstance, derivedEvents);
			return true;

		default:
			return false;
		}
		
	}
	
	public void createConnection(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx {
		logger.info("createConnection action called (event 'startReceivingEvents'), in the user-supplied action dispatcher");
	}

	public void destroyConnection(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx {
		logger.info("destroyConnection action called (event 'stopReceivingEvents'), in the user-supplied action dispatcher");
	}

}
