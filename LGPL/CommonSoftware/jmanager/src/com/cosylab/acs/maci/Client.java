/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci;

/**
 * Every client of a Component service that is not itself a Component must implement an interface called Client.
 * The interface allows the client to act as a secure party in the communication with the components,
 * to receive general-purpose string messages from the MACI components and to be notified when
 * any change happens to the components that the client utilizes. Each client logs in to the MACI system
 * before any other requests are made, and in turn it obtains an handle,
 * which it must use in every subsequent request to the MACI.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public interface Client
{

	/**
	 * Authentication method.
	 * Method authenticate is the challenge issued to the client after it tries to login.
	 * The login will be successful if the client's authenticate() produces the expected result.
	 * Only in this case will the Manager's login method return a valid handle,
	 * which the client will later use as the id parameter with all calls to the Manager.
	 * 
	 * @param	question	The question posed by the Manager.
	 * @return Answer to the question. The first character of the answer identifies the type of the client,
	 * 			and can be one of:
	 * 			<UL>
	 * 				<LI><TT>C</TT> A regular client (implements just the Client interface).</LI>
	 * 				<LI><TT>A</TT> An container (implements the Container interface).</LI>
	 * 				<LI><TT>AR</TT> An container with recovery capability (implements the Container interface). </LI>
	 * 				<LI><TT>S</TT> Supervisor (implements the Administrator interface).</LI>
	 *			</UL>
	 */
	public String authenticate(String question) throws RemoteException;

	/**
	 * Notify client about the change (availability) of the components currently in use by this client.
	 * For administrative clients, notification is issued for the change of availability of any component in the domain.
	 * 
	 * @param cobs	A sequence of ComponentInfo structures identifying the affected components.
	 * 				Regular clients receive the name, the type, the handle and the reference of the newly activated component.
	 * 				Administrative clients also receive the handle of the container where the component was activated.
	 */
	public void components_available(ComponentInfo[] components) throws RemoteException;

	/**
	 * Notify client that some of the components currently in use by client have become unavailable.
	 * 
	 * @param	cobs	CURLs of the unavailable components.
	 */
	public void components_unavailable(String[] components) throws RemoteException;

	/**
	 * Disconnect notification.
	 * The disconnect method is called by the Manager to notify the client that
	 * it will be unavailable and that the client should log off.
	 */
	public void disconnect() throws RemoteException;

	/**
	 * The Manager and administrators use this method for sending textual messages to the client.
	 * 
	 * @param	type	Type of the message, instance of <code>MessageType</code>.
	 * @param	message	Contents of the message. The contents are human readable.
	 */
	public void message(MessageType type, String message) throws RemoteException;

	/**
	 * Client name.
	 * @return	name
	 */
	public String name() throws RemoteException;

	/**
	 * Manager pings its clients (both GUI clients, as well as Containers) repeatedly to verify that they still exist.
	 * The return value can be either "true", indicating that everything is OK with the client,
	 * or "false", indicating that client is malfunctioning.
	 * 
	 * If <code>RemoteTransientException</code> exception is thrown, the Manager should retry the ping several times,
	 * and only then shall the client be assumed to be malfunctioning.
	 * If another exception is thrown, the client may be immediately assumed to be malfunctioning.
	 * Once the client is found to be malfunctioning, the Manager makes an implicit logout of the client.
	 */
	public boolean ping() throws RemoteException;

}
