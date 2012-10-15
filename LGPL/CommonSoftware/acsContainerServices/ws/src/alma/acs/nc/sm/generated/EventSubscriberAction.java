package alma.acs.nc.sm.generated;

import alma.acs.nc.sm.generic.AcsScxmlDispatchingAction;

/**
 * All SM actions get mapped to the generic {@link AcsScxmlDispatchingAction}
 * that is parametrized with an action enum such as this one. 
 * <p>
 * Note that the SM framework mandates that all actions can be listed in an
 * enum class. 
 * However, the action implementation can be done in one or many classes. 
 * {@link EventSubscriberAllActionsHandler} is useful in case you want to implement all actions
 * in one class. 
 * <p>
 * This enum class replaces the config file config/SMActionMap.txt used in the ESO SM framework, 
 * together with the option of setting arbitrary action handlers.
 * 
 * @author hsommer
 */
public enum EventSubscriberAction {
	createEnvironment,
	createConnection,
	suspendConnection,
	resumeConnection,
	destroyConnection,
	destroyEnvironment
}
