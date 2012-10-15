package alma.acs.nc.sm;

import java.util.logging.Logger;

import alma.acs.nc.sm.generated.EventSubscriberAction;
import alma.acs.nc.sm.generated.EventSubscriberActionDispatcher;
import alma.acs.nc.sm.generated.EventSubscriberSignal;
import alma.acs.nc.sm.generated.EventSubscriberSignalDispatcher;
import alma.acs.nc.sm.generic.AcsScxmlActionDispatcher;
import alma.acs.nc.sm.generic.AcsScxmlEngine;

/**
 * Adapts {@link AcsScxmlEngine} for the concrete SM type 'EventSubscriber',
 * and a single all-action handler.
 * <p>
 * Currently this class is used only in tests, while the real code
 * uses {@link AcsScxmlEngine} directly, similarly to this class.
 * 
 * @author hsommer
 */
public class EventSubscriberStateMachine extends EventSubscriberSignalDispatcher
{
	private static final String scxmlFileName = "/alma/acs/nc/sm/generated/EventSubscriberSCXML.xml";
	private final Logger logger;
	
	private final AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction> stateMachine;

	public EventSubscriberStateMachine(Logger logger, EventSubscriberActionDispatcher actionExecutor) {
		this.logger = logger;
	
		AcsScxmlActionDispatcher<EventSubscriberAction> genericActionDispatcher = 
				new AcsScxmlActionDispatcher<EventSubscriberAction>(logger, EventSubscriberAction.class);
		
		// register our action executor for all possible actions
		for (EventSubscriberAction action : EventSubscriberAction.values()) {
			genericActionDispatcher.registerActionHandler(action, actionExecutor);
		}
		
		// Here the AcsScxmlEngine constructor will load the scxml file and start the state machine
		stateMachine = new AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction>(
				scxmlFileName, logger, genericActionDispatcher, EventSubscriberSignal.class);
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
