/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.test;

import java.util.HashMap;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.core.defaults.MessageLogEntry;
import abeans.pluggable.RemoteException;
import abeans.pluggable.acs.logging.LoggingLevel;

import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.MessageType;

/**
 * Test client implementation.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class TestClient implements Client, Identifiable
{
	/**
	 * Identifier.
	 */
	Identifier id = null;

	/**
	 * Client name.
	 */
	String name;

	/**
	 * Client reply.
	 */
	String reply;

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
		this (name, "C");
	}

	/**
	 * Constructor for TestClient.
	 * @param	reply	reply to autheticate, non-<code>null</code>.
	 * @param	name	name of the client, non-<code>null</code>.
	 */
	public TestClient(String name, String reply)
	{
		assert (name != null);
		assert (reply != null);
		
		this.name = name;
		this.reply = reply;
	}

	/**
	 * @see com.cosylab.acs.maci.Client#authenticate(String)
	 */
	public String authenticate(String question) throws RemoteException
	{
		return reply;
	}

	/**
	 * @see com.cosylab.acs.maci.Client#components_available(ComponentInfo[])
	 */
	public void components_available(ComponentInfo[] cobs) throws RemoteException
	{
		for (int i = 0; i < cobs.length; i++)
		{
			new MessageLogEntry(this, "components_available", "Available: "+cobs[i].getName(), LoggingLevel.INFO).dispatch();
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Client#components_unavailable(String[])
	 */
	public void components_unavailable(String[] cobs) throws RemoteException
	{
		for (int i = 0; i < cobs.length; i++)
		{
			new MessageLogEntry(this, "components_unavailable", "Unavailable: "+cobs[i], LoggingLevel.INFO).dispatch();
		}
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
		new MessageLogEntry(this, "message", "Message received: " + message + " " + type, LoggingLevel.DEBUG).dispatch();
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

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null)
			id = new IdentifierSupport(name, name, Identifier.APPLICATION);
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return false;
	}

}
