/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.test;

import java.util.HashMap;

import com.cosylab.acs.maci.AuthenticationData;
import com.cosylab.acs.maci.ClientType;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.ImplLang;
import com.cosylab.acs.maci.MessageType;
import com.cosylab.acs.maci.RemoteException;

/**
 * Test client implementation.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class TestClient implements Client
{
	/**
	 * Client name.
	 */
	String name;

	/**
	 * Client type.
	 */
	ClientType type;

	/**
	 * Recover flag.
	 */
	boolean recover;

	/**
	 * Client operation status.
	 * This is the value which method ping() will return.
	 */
	boolean operative = true;

	/**
	 * Client handle.
	 */
	int handle;

	/**
	 * List of owned components.
	 */
	HashMap components = new HashMap();

	/**
	 * Constructor for TestClient.
	 * @param	name	name of the client, non-<code>null</code>.
	 */
	public TestClient(String name)
	{
		this (name, ClientType.CLIENT, false);
	}

	/**
	 * Constructor for TestClient.
	 * @param	type	reply to autheticate, non-<code>null</code>.
	 * @param	name	name of the client, non-<code>null</code>.
	 */
	public TestClient(String name, ClientType type)
	{
		this (name, type, false);
	}

	/**
	 * Constructor for TestClient.
	 * @param	type	reply to autheticate, non-<code>null</code>.
	 * @param	name	name of the client, non-<code>null</code>.
	 */
	public TestClient(String name, ClientType type, boolean recover)
	{
		assert (name != null);
		assert (type != null);
		
		this.name = name;
		this.type = type;
		this.recover = recover;
	}

	/**
	 * @see com.cosylab.acs.maci.Client#authenticate(long, String)
	 */
	public AuthenticationData authenticate(long executionId, String question) throws RemoteException
	{
		return new AuthenticationData("", type, ImplLang.cpp, recover, System.currentTimeMillis(), executionId);
	}

	/**
	 * @see com.cosylab.acs.maci.Client#components_available(ComponentInfo[])
	 */
	public void components_available(ComponentInfo[] cobs) throws RemoteException
	{
		/*
		for (int i = 0; i < cobs.length; i++)
		{
			System.out.println("Available: "+cobs[i].getName());
		}
		*/
	}

	/**
	 * @see com.cosylab.acs.maci.Client#components_unavailable(String[])
	 */
	public void components_unavailable(String[] cobs) throws RemoteException
	{
		/*
		for (int i = 0; i < cobs.length; i++)
		{
			System.out.println("Unavailable: "+cobs[i]);
		}
		*/
	}

	/**
	 * @see com.cosylab.acs.maci.Client#disconnect()
	 */
	public void disconnect() throws RemoteException
	{
		setOperative(false);
	}

	/**
	 * @see com.cosylab.acs.maci.Client#message(MessageType, String)
	 */
	public void message(MessageType type, String message)
		throws RemoteException
	{
		/*
		System.out.println("Message received: " + message + " " + type);
		*/
	}

	/**
	 * @see com.cosylab.acs.maci.Client#taggedmessage(MessageType, short, String)
	 */
	public void taggedmessage(MessageType type, short id, String message)
		throws RemoteException
	{
		/*
		System.out.println("Message received: " + message + " " + type);
		*/
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
		return operative;
	}

	/**
	 * Returns the operative.
	 * @return boolean
	 */
	public boolean isOperative()
	{
		return operative;
	}

	/**
	 * Sets the operative.
	 * @param operative The operative to set
	 */
	public void setOperative(boolean operative)
	{
		this.operative = operative;
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
	 * Returns the cobs.
	 * @return HashMap
	 */
	public HashMap getComponents()
	{
		return components;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.Client#getRemoteLocation()
	 */
	public String getRemoteLocation() throws RemoteException {
		return "local";
	}

	/* (non-Javadoc)
	 * @see com.cosylab.acs.maci.Client#release()
	 */
	public void release() {
		// noop
	}

}
