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

	public EventSubscriberStateMachine(Logger logger) {
		this.logger = logger;
		
		AcsScxmlActionDispatcher<EventSubscriberAction> actionDispatcher = createActionDispatcher();
		
		// The AcsScxmlEngine constructor loads the scxml file and starts the state machine
		stateMachine = new AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction>(scxmlFileName, logger, actionDispatcher);
	}

	/**
	 * Sets up action handlers.
	 * p:
	 * TODO: Modify action ctors to require notify service objects
	 * 
	 * @return
	 */
	protected AcsScxmlActionDispatcher<EventSubscriberAction> createActionDispatcher() {
		AcsScxmlActionDispatcher<EventSubscriberAction> actionDispatcher = 
				new AcsScxmlActionDispatcher<EventSubscriberAction>(logger, EventSubscriberAction.class);
		
		EnvironmentActionHandler environmentActionHandler = new EnvironmentActionHandler(logger);
		actionDispatcher.registerActionHandler(EnvironmentCreator, environmentActionHandler);
		actionDispatcher.registerActionHandler(EnvironmentDestructor, environmentActionHandler);
		
		ConnectionActionHandler connectionActionHandler = new ConnectionActionHandler(logger);
		actionDispatcher.registerActionHandler(ConnectionCreator, connectionActionHandler);
		actionDispatcher.registerActionHandler(ConnectionDestructor, connectionActionHandler);
		
		SuspendResumeActionHandler suspendResumeActionHandler = new SuspendResumeActionHandler(logger);
		actionDispatcher.registerActionHandler(ConnectionSuspender, suspendResumeActionHandler);
		actionDispatcher.registerActionHandler(ConnectionResumer, suspendResumeActionHandler);
		return actionDispatcher;
	}

	@Override
	protected AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction> getScxmlEngine() {
		return stateMachine;
	}
	
	/**
	 * Convenience method, delegates to
	 */
	public String getCurrentState() {
		return stateMachine.getCurrentState();
	}
}
