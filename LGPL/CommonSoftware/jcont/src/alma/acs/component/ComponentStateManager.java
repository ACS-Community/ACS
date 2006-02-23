package alma.acs.component;

import alma.ACS.ComponentStates;


/**
 * @author hsommer
 * created Oct 22, 2003 5:07:57 PM
 */
public interface ComponentStateManager
{
	/**
	 * Returns the current state of the component
	 * @return the component's current state 
	 */
	public ComponentStates getCurrentState();
	
	/**
	 * Requests to change the state. 
	 * To be called only by the component 
	 * (the container will use a separate method in the implementation class
	 * to allow for different transition validation rules)
	 * 
	 * @param newState  the new component state
	 * @throws ComponentLifecycleException  if the transition from the current state to 
	 * 			the new state is not allowed.
	 */
	public void setState(ComponentStates newState) throws ComponentLifecycleException;
	
	
	/**
	 * It seemed not worth it to create Java enum classes following the standard patterns,
	 * since the component has to return the CORBA state class anyway.
	 * Nonetheless, getting the state names for logging etc. might be useful. 
	 *  
	 * @param corbaStateObj  the int-based corba enum class
	 * @return the state name as specified in IDL, e.g. "COMPSTATE_NEW".
	 */
	public String getName(ComponentStates corbaStateObj);
}
