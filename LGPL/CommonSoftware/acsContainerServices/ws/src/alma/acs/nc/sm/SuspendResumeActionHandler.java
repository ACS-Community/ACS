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
 * Handler for actions suspendConnection and resumeConnection.
 * It is a bit arbitrary that we use a separate class to implement these two actions, 
 * but it gives a feel for how the SCXML state machines can be used.
 * 
 * @author hsommer
 */
public abstract class SuspendResumeActionHandler implements AcsScxmlActionExecutor<EventSubscriberAction>
{
	private final Logger logger;

	public SuspendResumeActionHandler(Logger logger) {
		this.logger = logger;
	}

	@Override
	public boolean execute(EventSubscriberAction action, EventDispatcher evtDispatcher, ErrorReporter errRep,
			SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
				throws AcsJStateMachineActionEx {

		switch (action) {
		case suspendConnection:
			suspend(evtDispatcher, errRep, scInstance, derivedEvents);
			return true;

		case resumeConnection:
			resume(evtDispatcher, errRep, scInstance, derivedEvents);
			return true;

		default:
			return false;
		}
	}
	
	public abstract void suspend(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents)
			throws AcsJStateMachineActionEx;

	public abstract void resume(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents)
			throws AcsJStateMachineActionEx;
	
}
