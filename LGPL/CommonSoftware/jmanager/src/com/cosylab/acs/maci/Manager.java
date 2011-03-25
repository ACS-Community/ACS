/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

import java.net.URI;

import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;
import alma.maciErrType.wrappers.AcsJComponentSpecIncompatibleWithActiveComponentEx;
import alma.maciErrType.wrappers.AcsJIncompleteComponentSpecEx;
import alma.maciErrType.wrappers.AcsJInvalidComponentSpecEx;
import alma.maciErrType.wrappers.AcsJNoPermissionEx;

/**
 * Manager is the central point of interaction between the components 
 * and the clients that request MACI services. A Manager is 
 * responsible for managing a domain.	
 * Manager has the following functionality:
 *	<UL>
 *	<LI>It is the communication entry point.</LI>
 *	<LI>It performs as a name service, resolving CURLs into Component references.</LI>
 *	<LI>It delegates the Component life cycle management to the Container.</LI>
 *	<LI>It provides information about the whole domain.</LI>
 *	</UL>
 * 
  * This interface defines a CORBA-independent interface for the Manager.
 * In ACS, the acs.maci.plugManagerProxyImpl class implements the CORBA
 * interface and delegates toan implementation of this Manager class
 *  the real work. 
 * 
 * NOTE: There is no <code>throws RemoteException</code> in the signature of the methods.
 * 
 * @see si.ijs.acs.maci.plug.ManagerProxyImpl, si.ijs.acs.maci.plug.ManagerProxy, acs.maci.manager.ManagerImpl
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface Manager extends ManagerConstants
{
	/**
	 * Return the fully qualified name of the domain, e.g., "antenna1.alma.nrao".
	 * 
	 * @return	the fully qualified name of the domain
	 */
	public String getDomain();

	/**
	 * Get all the information that the Manager has about its known containers.
	 * To invoke this method, the caller must have INTROSPECT_MANAGER access rights, or it must be the object whose info it is requesting (request by handle sequence of length one).
	 * Calling this function does not affect the internal state of the Manager.
	 * 
	 * @param	id		Identification of the caller.
	 * @param	handles	Handles of the containers whose information is requested. If this is an empty sequence, the name_wc parameter is used.
	 * @param	name_wc	Wildcard that the container's name must match in order for its information to be returned.
	 * @return			A sequence of ContainerInfo structures containing the entire Manager's knowledge about the containers. 
	 *					If access is denied to a subset of objects, the handles to those objects are set to 0.
	 */
	public ContainerInfo[] getContainerInfo(int id, int[] handles, String name_wc) throws AcsJNoPermissionEx;

	/**
	 * Get all the information that the Manager has about its known clients.
	 * To invoke this method, the caller must have INTROSPECT_MANAGER access rights, or it must be the object whose info it is requesting (request by handle sequence of length one).
	 * Calling this function does not affect the internal state of the Manager.
	 * 
	 * @param	id		Identification of the caller.
	 * @param	handles	Handles of the clients whose information is requested. If this is an empty sequence, the name_wc parameter is used.
	 * @param	name_wc	Wildcard that the clients's name must match in order for its information to be returned.
	 * @return			A sequence of ClientInfo structures containing the entire Manager's knowledge about the containers. 
	 *					If access is denied to a subset of objects, the handles to those objects are set to 0.
	 */
	public ClientInfo[] getClientInfo(int id, int[] handles, String name_wc) throws AcsJNoPermissionEx;

	/**
	 * Get all the information that the Manager has about components.
	 * To invoke this method, the caller must have INTROSPECT_MANAGER access rights, or it must have adequate privileges to access the Component (the same as with the get_component method).
	 * Information about all components is returned, unless the active_only parameter is set to true,
	 * in which case only information about those components that are currently registered with the Manager
	 * and activated is returned.
	 * Calling this function does not affect the internal state of the Manager.
	 * 
	 * @param	id		Identification of the caller.
	 * @param	handles	Handles of the components whose information is requested. If this is an empty sequence, the name_wc and type_wc parameters are used.
	 * @param	name_wc	Wildcard that the Component's name must match in order for its information to be returned.
	 * @param	type_wc	Wildcard that the Component's type must match in order for its information to be returned.
	 * @param	activeOnly	If <code>true</code> returns information only about components that are activated.
	 * @return			A sequence of ComponentInfo structures containing the entire Manager's knowledge about the components.
	 *					If access is denied to a subset of objects, the handles to those objects are set to 0.
	 */
	public ComponentInfo[] getComponentInfo(int id, int[] handles, String name_wc, String type_wc, boolean activeOnly) throws AcsJNoPermissionEx;

	/** 
	 * Get a component, activating it if necessary.
	 * The client represented by id (the handle)
	 * must have adequate access rights to access the component. This is untrue of components:
	 * components always have unlimited access rights to other components.
	 * 
	 * @param	id		Identification of the caller. If this is an invalid handle,
	 * 					or if the caller does not have enough access rights,
	 * 					a <code>AcsJNoPermissionEx</code> exception is raised.
	 * @param	curl	CURL of the component whose reference is to be retrieved.
	 * @param	activate	<code>true</code> if the Component is to be activated in case it does not exist.
	 * 						If set to <code>false</code>, and the Component exist,
	 * 						a <code>null</code> reference is returned and status is set to COMPONENT_NOT_ACTIVATED.
	 * @param	status	Status of the request. One of COMPONENT_ACTIVATED, COMPONENT_DOES_NO_EXIST and COMPONENT_NOT_ACTIVATED.
	 * @return			Reference to the Component. If the Component could not be activated, a nil reference is returned,
	 *					and the status contains an error code detailing the cause of failure (one of the component_* constants).
	 */
	public Component getComponent(int id, URI curl, boolean activate, StatusHolder status) throws AcsJCannotGetComponentEx, AcsJNoPermissionEx;

	/** 
	 * Restart a component.
	 * The client represented by id (the handle) must be a owner of a component.
	 * 
	 * @param	id		Identification of the caller. If this is an invalid handle,
	 * 					or if the caller does not have enough access rights,
	 * 					a <code>AcsJNoPermissionEx</code> exception is raised.
	 * @param	curl	CURL of the component whose reference is to be restarted.
	 * @return			Reference to the component. If the component could not be restarted, a nil reference is returned.
	 */
	public Component restartComponent(int id, URI curl) 
	    throws AcsJNoPermissionEx, AcsJBadParameterEx;

	/**
	 * Change mortality state of an component.
	 * Compnent must be already active, otherwise <code>NoResourcesException</code> exception will be thrown.
	 * The caller must be an owner of an component or have administator rights,
	 * otherwise <code>AcsJNoPermissionEx</code> exception will be thrown.
	 * 
	 * @param id Identification of the caller. The caller must be an owner of an component or have administator rights.
	 * @param curl The CURL of the component whose mortality to change.
	 * @param immortalState New mortality state.
	 **/
	public void makeComponentImmortal(int id, URI curl, boolean immortalState) 
	    throws AcsJCannotGetComponentEx, AcsJNoPermissionEx, AcsJBadParameterEx;

	/** 
	 * Get a service.
	 * NOTE: a component is also a service, i.e. a service activated by a container.
	 * @see #get_component
	 */
	public Component getService(int id, URI curl, boolean activate, StatusHolder status) throws AcsJCannotGetComponentEx, AcsJNoPermissionEx;

	/**
	 * Login to MACI. 
	 * Containers, Clients and Administrative clients call this function
	 * first to identify themselves with the Manager. The Manager authenticates them
	 * (through the authenticate function), and assigns them access rights and a handle,
	 * through which they will identify themselves at subsequent calls to the Manager.
	 * 
	 * @param	reference A reference to the Client.
	 * @return	A ClientInfo structure with handle (h) and access fields filled-in.
	 * 			If the client with this name did not logout prior to calling login,
	 *			the components sequence in ClientInfo contains the handles of all components that
	 *			the client was using. (For containers, the components sequence contains
	 *			handles of all components previously hosted by the Container.)
	 */
	public ClientInfo login(Client reference) throws AcsJNoPermissionEx;

	/**
	 * Logout from MACI.
	 * 
	 * @param	id Handle of the Client that is logging out
	 */
	public void logout(int id) throws AcsJNoPermissionEx;

	/** 
	 * Register a CORBA object as a component, assigning it a CURL and making it accessible through the Manager.
	 * The component is treated as an immortal Component.
	 * 
	 * @param	id		Identification of the caller.
	 * 					The caller must have the REGISTER_COMPONENT access right to perform this operation.
	 * @param	curl	CURL that will be assigned to the object. The CURL must be in the Manager's domain, otherwise a fundamental property of domains that one computer belongs to only one domain would be too easy to violate.
	 * @param	type	Type of the component
	 * @param	cob		Object to be registered as component.
	 * @return			Returns the handle of the newly registered component.
	 */
	public int registerComponent(int id, URI curl, String type, Component cob) 
	throws AcsJNoPermissionEx, AcsJBadParameterEx;

	/**
	 * Release a component.
	 * In order for this operation to be possible, the caller represented by the id
	 * must have previously successfuly requested the component via a call to get_component.
	 * Releasing a component more times than requesting it should be avoided, but it produces no errors.
	 * 
	 * @param	id		Identification of the caller. The caller must have previously got the Component through get_component.
	 * @param	curl	The CURL of the Component to be released.
	 * @return			Number of clients that are still using the Component after the operation completed.
	 *					This is a useful debugging tool.
	 */
	public int releaseComponent(int id, URI curl) throws AcsJNoPermissionEx, AcsJBadParameterEx;

	public interface LongCompletionCallback {
		void done(int result);
		void failed(int result, Throwable exception);
	}
	
	public void releaseComponentAsync(int id, URI curl, LongCompletionCallback callback) throws AcsJNoPermissionEx, AcsJBadParameterEx;

	/**
	 * Forcefully release a component.
	 * 
	 * @param	id		Identification of the caller. 
	 * @param	curl	The CURL of the Component to be released.
	 * @return			Number of clients that are still using the Component after the operation completed.
	 *					This is a useful debugging tool.
	 */
	public int forceReleaseComponent(int id, URI curl) throws AcsJNoPermissionEx, AcsJBadParameterEx;

	/**
	 * Shutdown the Manager.
	 * <B>Warning:</B> This call will also deactivate all components active in the system, including startup and immortal components.
	 * 
	 * @param	id		Identification of the caller. The caller must have the SHUTDOWN_SYSTEM access right.
	 * @param	containers	The code to send to shutdown methods of all containers.
	 * 						If <code>0</code>, the Container's shutdown methods are not called.
	 */
	public void shutdown(int id, int containers) throws AcsJNoPermissionEx;

	/**
	 * Shutdown a container.
	 * 
	 * @param	id		Identification of the caller. The caller must have the SHUTDOWN_SYSTEM access right.
	 * @param	container_name	name of the container to shutdown.
	 * @param	action	The code to send to shutdown method of the container. If <code>0</code>, the Container's disconnect methods is called instead.
	 */
	public void shutdownContainer(int id, String containerName, int action) throws AcsJNoPermissionEx;

	/**
	 * Unregister a component from the Manager.
	 * 
	 * @param	id		Identification of the caller.
	 * 					The caller must have the REGISTER_COMPONENT access right to perform this operation.
	 * @param	handle	Component's handle.
	 * 					The component must have been previously registered through the call to register_component.
	 * 					If there are clients still using this component, a <code>components_unavailable</code> notification is
	 * 					issued to all of them, and the component is unregistered.
	 */
	public void unregisterComponent(int id, int handle) throws AcsJNoPermissionEx, AcsJBadParameterEx;


	/** 
	 * Get default component of given type.
	 * The client represented by id (the handle) 
	 * must have adequate access rights to access the component.
	 * 
	 * @param	id		Identification of the caller. If this is an invalid handle,
	 * 					or if the caller does not have enough access rights,
	 * 					a <code>AcsJNoPermissionEx</code> exception is raised.
	 * @param	type	type of the component whose reference is to be restarted.
	 * @return			<code>ComponentInfo</code> of the component. If no defualt component is found
	 * 					<code>NoDefaultComponentException</code> exception is thrown.
	 */
	public ComponentInfo getDefaultComponent(int id, String type) throws AcsJCannotGetComponentEx, AcsJNoPermissionEx, NoDefaultComponentException;

	/**
	 * Activation of an dynamic component.
	 * @param	id 				Identification of the caller.
	 * @param	componentSpec	Component to be obtained.
	 * @param	markAsDefault	Mark component as default component of its type.
	 * @return	<code>ComponentInfo</code> of requested component.
	 * 			If <code>componentSpec</code> if found to be incomplete <code>IncompleteComponentSpecException</code> exception is thrown.
	 * 			If requested component collides with already activated component with the same name <code>ComponentSpecIncompatibleWithActiveComponentException</code> exception is thrown.
	 */
	public ComponentInfo getDynamicComponent(int id, ComponentSpec componentSpec, boolean markAsDefault)
	throws AcsJCannotGetComponentEx, AcsJNoPermissionEx, AcsJIncompleteComponentSpecEx,
	   AcsJInvalidComponentSpecEx, AcsJComponentSpecIncompatibleWithActiveComponentEx;

	/**
	 * Activation of an co-deployed component.
	 * @param	id 				Identification of the caller.
	 * @param	componentSpec	Component to be obtained.
	 * @param	markAsDefault	Mark component as default component of its type.
	 * @param	targetComponentURI	target co-deployed component.
	 * @return	<code>ComponentInfo</code> of requested component.
	 * 			If <code>componentSpec</code> if found to be incomplete <code>IncompleteComponentSpecException</code> exception is thrown.
	 * 			If requested component collides with already activated component with the same name <code>ComponentSpecIncompatibleWithActiveComponentException</code> exception is thrown.
	 */
	public ComponentInfo getCollocatedComponent(int id, ComponentSpec componentSpec,
			boolean markAsDefault, URI targetComponentURI)
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx, AcsJIncompleteComponentSpecEx,
		   AcsJInvalidComponentSpecEx, AcsJComponentSpecIncompatibleWithActiveComponentEx;

	/**
	 * Get a component, do not activate it and also do not do any reference counting.
	 * The client represented by id (the handle)
	 * must have adequate access rights to access the Component. This is untrue of components:
	 * components always have unlimited access rights to other components.
	 *
	 * @param id Identification of the caller. If this is an invalid handle, or if the caller does not have enough access rights, a <code>AcsJNoPermissionEx</code> exception is raised.
	 * @param component_url CURL of the Component whose reference is to be retrieved.
	 * @return Reference to the Component.
	 */
	public Component getComponentNonSticky(int id, URI curl) 
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx;

}

