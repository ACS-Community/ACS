/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.MessageType;
import com.cosylab.acs.maci.RemoteException;

/**
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ClientImpl implements Client
{

	/**
	 * Client name.
	 */
	String name;

	/**
	 * Client handle.
	 */
	int handle;

	/**
	 * Implementation of <code>Client</code>.
	 * @param	name	name of the client
	 */
	public ClientImpl(String name)
	{
		this.name = name;
	}

	/**
	 * @see com.cosylab.acs.maci.Client#authenticate(String)
	 */
	public String authenticate(String question) throws RemoteException
	{
		return "C";
	}

	/**
	 * @see com.cosylab.acs.maci.Client#components_available(ComponentInfo[])
	 */
	public void components_available(ComponentInfo[] cobs) throws RemoteException
	{
	}

	/**
	 * @see com.cosylab.acs.maci.Client#components_unavailable(String[])
	 */
	public void components_unavailable(String[] cobs) throws RemoteException
	{
	}

	/**
	 * @see com.cosylab.acs.maci.Client#disconnect()
	 */
	public void disconnect() throws RemoteException
	{
	}

	/**
	 * @see com.cosylab.acs.maci.Client#message(MessageType, String)
	 */
	public void message(MessageType type, String message)
		throws RemoteException
	{
	}

	/**
	 * @see com.cosylab.acs.maci.Client#name()
	 */
	public String name() throws RemoteException
	{
		return name;
	}

	/**
	 * @see com.cosylab.acs.maci.Client#ping()
	 */
	public boolean ping() throws RemoteException
	{
		return true;
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
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ClientImpl = { ");
		sbuff.append("name = '");
		sbuff.append(name);
		sbuff.append("', handle = '");
		sbuff.append(handle);
		sbuff.append("' }");
		return new String(sbuff);
	}

}
