package alma.acs.nc.sm.generated;

import alma.acs.nc.sm.generic.AcsScxmlDispatchingAction;

/**
 * All SM actions are mapped to the generic {@link AcsScxmlDispatchingAction}, 
 * so that SCXML will invoke {@link EventSubscriberActionDispatcher}.
 * <p>
 * This enum class replaces the config file config/SMActionMap.txt, 
 * together with the option of setting arbitrary action handlers.
 * <p>
 * TODO: Rename to uppercase and change the name to start with a verb, e.g. createEnvironment.
 *       The noun style goes back to when a handler class was generated for every action,
 *       and the action name had to provide the class name. Then also rename it in the scxml file.
 * @author hsommer
 */
public enum EventSubscriberAction {
	EnvironmentCreator,
	ConnectionCreator,
	ConnectionSuspender,
	ConnectionResumer,
	ConnectionDestructor,
	EnvironmentDestructor
}
