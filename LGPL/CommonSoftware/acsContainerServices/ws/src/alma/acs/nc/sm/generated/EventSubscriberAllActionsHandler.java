package alma.acs.nc.sm.generated;

import java.util.Collection;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;

/**
 * A convenience interface that bundles all state machine actions as methods.
 * <p>
 * Note that the state machine design leaves full choice about how to implement
 * actions. Just in case you want to do it all in one class then this interface can be useful,
 * mostly to be alerted of SM changes by compile errors (especially when using 'Override' tags).
 * 
 * @see EventSubscriberActionDispatcher
 */
public interface EventSubscriberAllActionsHandler
{
	void createEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;

	void destroyEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;
	
	void createConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;
	
	void destroyConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;
	
	void suspendAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;
	
	void resumeAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx;

}
