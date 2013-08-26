/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

import alma.maciErrType.wrappers.AcsJCannotActivateComponentEx;
import alma.maciErrType.wrappers.AcsJCannotDeactivateComponentEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationFailedEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationUncleanEx;

/**
 * Container is an agent of MACI that is installed on every computer of the control system.
 * There can be more than one Container living on the same computer, but there can be only one Container per process.
 * It has the following responsibilities: 
 * <OL> 
 *		<LI>Constructs components when the Manager instructs it to (see activate_component and deactivate_component).</LI> 
 *		<LI>Provides the components that it hosts with basic MACI services, such as: 
 *			<UL> 
 *				<LI>access to the Manager</LI> 
 * 				<LI>access to the Local Database</LI> 
 * 				<LI>access to other components (indirectly through the Manager)</LI> 
 * 				<LI>access to the ORB and POA</LI> 
 * 			</UL> 
 * 		<LI>Handles intricacies of working with the ORB and the POA, such as connecting the newly created components to the POA.</LI> 
 * 		<LI>Is responsive to a shutdown directive from the Manager, which can shutdown only the Container's process, or reboot the computer (see shutdown).</LI> 
 * </OL> 
 *
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface Container extends Client
{
	
	/**
	 * Activate a Component whose type (class) and name (instance) are given. 
	 *
	 * In the process of activation, Component's code-base is loaded into memory if it is not there already. 
	 * The code-base resides in an executable file (usually a dynamic-link library or a shared library -- DLL). 
	 *
	 * On platforms that do not automatically load dependent executables (e.g., VxWorks),
	 * the Container identifies the dependancies by querying the executable and loads them automatically. 
	 *
	 * Once the code is loaded, it is asked to construct a servant of a given type. 
	 *
	 * The servant is then initialized with the Configuration Database (CDB) and
	 * Persistance Database (PDB) data. The servant is attached to the Component, and a
	 * reference to it is returned. 
	 *
	 * @param	handle	Handle of the Component that is being activated.
	 * 					This handle is used by the Component when it will present itself to the Manager.
	 * 					The Component is expected to remember this handle for its entire life-time.
	 * @param	executionId Execution ID.
	 * @param	name	Name of the Component to instantiate.
	 * @param	exe		Path to the executable file (a DLL or a shared library) in which the Component's code resides.
	 * 					The path is relative to the root directory in which all executable code is stored.
	 * 					The path must not contain dots, and uses slashes (not backslashes) to separate components of the path.
	 * 					The path must not include the extension, or any prefixes, so that it is platform independent.
	 * @param	type	The type of the Component to instantiate. The interpretation of this field depends on the executable.
	 * 					Type should uniquely identify the code-base which the Component will be executing.
	 * 					<B>Note:</B> Type name is a CORBA repository ID.
	 * @return			Returns the reference to the object that has just been activated.
	 *					If the Component could not the activated, a nil reference is returned. 
	 */
	public ComponentInfo activate_component(int handle, long executionId, String name, String exe, String type)
		throws AcsJCannotActivateComponentEx;

	public interface ComponentInfoCompletionCallback {
		void done(ComponentInfo result);
		void failed(ComponentInfo result, Throwable exception);
	}
	

	/**
	 * Asynchronous version of activate_component.
	 * @see #activate_component(int, long, String, String, String)
	 */
	public void activate_component_async(int handle, long executionId, String name, String exe, String type,
			ComponentInfoCompletionCallback callback);
	
	/**
	 * Deactivate a component whose handles is given.
	 *
	 * Deactivation is the inverse process of activation:
	 * Component is detached from the POA, and thus made unavailable through CORBA,
	 * and its resources are freed. If it's code-base is no longer used,
	 * it is unloaded from memory.
	 * 
	 * @param	handle	A handle identifying a component to be released.
	 */
	public void deactivate_component(int handle) 
		throws AcsJCannotDeactivateComponentEx,AcsJComponentDeactivationUncleanEx,
			   AcsJComponentDeactivationFailedEx;
	
	/**
	 * Returns information about a subset of components that are currently hosted by the Container. 
	 * <B>Note:</B> If the list of handles is empty, information about all components hosted by the container is returned.
	 * 
	 * @param	handles	Handles of the components whose information should be retrieved.
	 * @return			Information about the selected components.
	 */
	public ComponentInfo[] get_component_info(int[] handles) throws RemoteException;
	
	/**
	 * Restarts a component.
	 * 
	 * @param	h	handle of the component to be restarted.	
	 * @return	 a new reference of the restarted component.
	 */ 
	public Component restart_component(int handle) throws RemoteException;

	/**
	 * Shutdown the Container. 
	 * 
	 * @param	Action to take after shutting down.
	 * 			Bits 8 through 15 of this parameter denote the action,
	 * 			which can be one of: 
	 * 			<UL>
	 * 				<LI>0 -- reload the container</LI>
	 * 				<LI>1 -- reboot the computer</LI>
	 * 				<LI>2 -- exit the container</LI>
	 * 			</UL>
	 * 
	 * 			The bits 0 thru 7 (values 0 to 255) are the return value that the Container	will pass to the operating system.
	 */
	public void shutdown(int action) throws RemoteException;
	
	/**
	 * Set component shutdown order.
	 * @param handles	ordered list of components handles.
	 */
	public void set_component_shutdown_order(int[] handles) throws RemoteException;

}
