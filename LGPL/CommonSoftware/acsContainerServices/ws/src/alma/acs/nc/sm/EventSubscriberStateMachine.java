package alma.acs.nc.sm;

import static alma.acs.nc.sm.generated.EventSubscriberAction.createConnection;
import static alma.acs.nc.sm.generated.EventSubscriberAction.destroyConnection;
import static alma.acs.nc.sm.generated.EventSubscriberAction.resumeConnection;
import static alma.acs.nc.sm.generated.EventSubscriberAction.suspendConnection;
import static alma.acs.nc.sm.generated.EventSubscriberAction.createEnvironment;
import static alma.acs.nc.sm.generated.EventSubscriberAction.destroyEnvironment;

import java.util.logging.Logger;

import alma.acs.nc.AcsEventSubscriberImplBase;
import alma.acs.nc.sm.generated.EventSubscriberAction;
import alma.acs.nc.sm.generated.EventSubscriberSignal;
import alma.acs.nc.sm.generated.EventSubscriberSignalDispatcher;
import alma.acs.nc.sm.generic.AcsScxmlActionDispatcher;
import alma.acs.nc.sm.generic.AcsScxmlEngine;

/**
 * Adapts {@link AcsScxmlEngine} for the concrete SM type 'EventSubscriber',
 * including the action handlers.
 * <p>
 * This code could also be embedded somewhere else, 
 * e.g. directly in component code {@link AcsEventSubscriberImplBase}.
 * 
 * @author hsommer
 */
public class EventSubscriberStateMachine extends EventSubscriberSignalDispatcher
{
	private static final String scxmlFileName = "/alma/acs/nc/sm/generated/EventSubscriberSCXML.xml";
	private final Logger logger;
	
	private final AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction> stateMachine;

	public EventSubscriberStateMachine(Logger logger, 
										EnvironmentActionHandler environmentActionHandler, 
										ConnectionActionHandler connectionActionHandler,
										SuspendResumeActionHandler suspendResumeActionHandler ) {
		this.logger = logger;
	
		AcsScxmlActionDispatcher<EventSubscriberAction> actionDispatcher = 
				new AcsScxmlActionDispatcher<EventSubscriberAction>(logger, EventSubscriberAction.class);
		
		actionDispatcher.registerActionHandler(createEnvironment, environmentActionHandler);
		actionDispatcher.registerActionHandler(destroyEnvironment, environmentActionHandler);
		
		actionDispatcher.registerActionHandler(createConnection, connectionActionHandler);
		actionDispatcher.registerActionHandler(destroyConnection, connectionActionHandler);
		
		actionDispatcher.registerActionHandler(suspendConnection, suspendResumeActionHandler);
		actionDispatcher.registerActionHandler(resumeConnection, suspendResumeActionHandler);
		
		// Here the AcsScxmlEngine constructor will load the scxml file and start the state machine
		stateMachine = new AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction>(
				scxmlFileName, logger, actionDispatcher, EventSubscriberSignal.class);
	}

	@Override
	protected AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction> getScxmlEngine() {
		return stateMachine;
	}
	
	/**
	 * Convenience method, delegates to the underlying AcsScxmlEngine.
	 * @return The state name. Hierarchical states are separated by ":", with outer state first, e.g. "EnvironmentCreated::Connected::Suspended"
	 */
	public String getCurrentState() {
		return stateMachine.getCurrentState();
	}
	
	/**
	 * 
	 * @param stateName Name of a (outer) state, as defined in <code>alma/acs/nc/sm/generated/EventSubscriberSCXML.xml</code>.
	 */
	public boolean isStateActive(String stateName) {
		return getScxmlEngine().isStateActive(stateName);
	}

}
