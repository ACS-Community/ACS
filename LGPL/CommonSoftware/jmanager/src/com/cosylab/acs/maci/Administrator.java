/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Administrator is a special-purpose client that can monitor the functioning of the domain that it administers.
 * Monitoring includes obtaining the status of the MACI components (the Manager and Containers) as well
 * as notification about the availability of the Component components.
 * The administrator client is granted special access permissions (INTROSPECT_MANAGER).
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 * @see		Client
 */
public interface Administrator extends Client
{

	/**
	 * Notification that an container has just logged in.
	 * 
	 * @param	info	Information about the container that has just logged in.
	 * 					If the handle of the container (<code>info.h</code>) is 0, the container failed to authenticate.
	 */
	public void containerLoggedIn(ContainerInfo info, long timeStamp, long executionId) throws RemoteException;

	/**
	 * Notification that a container has just logged out.
	 * 
	 * @param	handle	Handle of the container that has logged out.
	 */
	public void containerLoggedOut(int handle, long timeStamp) throws RemoteException;

	/**
	 * Notification that a client has just logged in.
	 * 
	 * @param	info	Information about the client that has just logged in.
	 * 					If the handle of the client (<code>info.h</code>) is 0, the client failed to authenticate.
	 */
	public void clientLoggedIn(ClientInfo info, long timeStamp, long executionId) throws RemoteException;

	/**
	 * Notification that a client has just logged out.
	 * 
	 * @param	handle	Handle of the client that has logged out.
	 */
	public void clientLoggedOut(int handle, long timeStamp) throws RemoteException;

	/**
	 * Notification that some components have just been requested (through get_component or get_components).
	 * 
	 * @param	clients		Handles of clients requesting components.
	 * @param	components	Handles of requested components.
	 * 						Component at the i-th place in the sequence was requested by the i-th client of sequence clients.
	 */
	public void components_released(int[] clients, int[] components, long timeStamp) throws RemoteException;

	/**
	 * Notification that some components have just been released (through release_component or release_components).
	 * 
	 * @param	clients		Handles of clients requesting components.
	 * @param	components	Handles of requested components.
	 * 						Component at the i-th place in the sequence was requested by the i-th client of sequence clients.
	 */
	public void components_requested(int[] clients, int[] components, long timeStamp) throws RemoteException;

	/**
	 * Notification that some component has just been activated (through Container.activate_component).
	 * 
	 * @param	component	ComponentInfo of activated component.
	 */
	public void component_activated(ComponentInfo info, long timeStamp, long executionId) throws RemoteException;

	/**
	 * Notification that some component has just been deactivated (through Container.deactivate_component).
	 * 
	 * @param	handle	Handle of component being deactivated.
	 */
	public void component_deactivated(int handle, long timeStamp) throws RemoteException;
}
