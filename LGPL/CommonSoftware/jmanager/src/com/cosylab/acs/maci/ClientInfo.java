/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

import java.io.Serializable;

/**
 * Structure in which the Manager stores information about a client.
 * Here, a client is any entity that accesses components, and therefore a Component can also act as a client.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ClientInfo implements Serializable
{

	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = 860176672630128907L;

	/**
	 * Client's handle.
	 * The range depends on the class of the client (a Component, regular client, administrator, ...).
	 */
	private int handle;

	/**
	 * Client's name.
	 * This is the same name as given by <code>Client#name</code> for non-Component clients, and Component's name for Component clients.
	 */
	private String name;

	/**
	 * Reference to the client's object.
	 * If the client is a Component, this is the reference to the Container which hosts the Component.
	 */
	private Client client;
	
	/**
	 * Specifies the components to which the client has requested and successfuly obtained a reference from the Manager.
	 * If a client has done so more than once for the same component, component's handle is repeated.
	 */
	private IntArray components;
	
	/**
	 * Specifies the access rights of the client.
	 * These access rights are used by the Manager in determining the client's access rights.
	 */
	private int accessRights;

	/**
	 * Creates an instance of ClientInfo with all necesarry data.
	 * @param handle	handle of the client.
	 * @param name		name of the client.
	 * @param client	client itself.
	 * 
	 * @see #handle
	 * @see #name 
	 * @see #client
	 */
	public ClientInfo(int handle, String name, Client client)
	{
		this.handle = handle;
		this.name = name;
		this.client = client;
		this.components = new IntArray();
	}

	/**
	 * Returns the accessRights.
	 * @return int
	 */
	public int getAccessRights()
	{
		return accessRights;
	}

	/**
	 * Returns the client.
	 * @return Client
	 */
	public Client getClient()
	{
		return client;
	}

	/**
	 * Returns the components.
	 * @return ArrayList
	 */
	public IntArray getComponents()
	{
		return components;
	}

	/**
	 * Returns the handle.
	 * @return int
	 */
	public int getHandle()
	{
		return handle;
	}

	/**
	 * Returns the name.
	 * @return String
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * Sets the accessRights.
	 * @param accessRights The accessRights to set
	 */
	public void setAccessRights(int accessRights)
	{
		this.accessRights = accessRights;
	}

	/**
	 * Sets the client.
	 * @param client The client to set
	 */
	public void setClient(Client client)
	{
		this.client = client;
	}

	/**
	 * Sets the handle.
	 * @param handle The handle to set
	 */
	public void setHandle(int handle)
	{
		this.handle = handle;
	}

	/**
	 * Sets the name.
	 * @param name The name to set
	 */
	public void setName(String name)
	{
		this.name = name;
	}

	/**
	 * Sets the components.
	 * @param components The components to set
	 */
	public void setComponents(IntArray components)
	{
		this.components = components;
	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ClientInfo = { ");
		sbuff.append("name = '");
		sbuff.append(name);
		sbuff.append("', ");
		sbuff.append(HandleHelper.toString(handle));
		sbuff.append(" }");
		return new String(sbuff);
	}
}
