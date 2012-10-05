package alma.acs.nc.sm;

import static alma.acs.nc.sm.generated.EventSubscriberAction.ConnectionCreator;
import static alma.acs.nc.sm.generated.EventSubscriberAction.ConnectionDestructor;
import static alma.acs.nc.sm.generated.EventSubscriberAction.ConnectionResumer;
import static alma.acs.nc.sm.generated.EventSubscriberAction.ConnectionSuspender;
import static alma.acs.nc.sm.generated.EventSubscriberAction.EnvironmentCreator;
import static alma.acs.nc.sm.generated.EventSubscriberAction.EnvironmentDestructor;

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
		
		actionDispatcher.registerActionHandler(EnvironmentCreator, environmentActionHandler);
		actionDispatcher.registerActionHandler(EnvironmentDestructor, environmentActionHandler);
		
		actionDispatcher.registerActionHandler(ConnectionCreator, connectionActionHandler);
		actionDispatcher.registerActionHandler(ConnectionDestructor, connectionActionHandler);
		
		actionDispatcher.registerActionHandler(ConnectionSuspender, suspendResumeActionHandler);
		actionDispatcher.registerActionHandler(ConnectionResumer, suspendResumeActionHandler);
		
		// The AcsScxmlEngine constructor loads the scxml file and starts the state machine
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
	
}
