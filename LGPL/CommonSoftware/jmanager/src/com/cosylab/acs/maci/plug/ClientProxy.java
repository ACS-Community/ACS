/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import org.omg.CORBA.Object;
import org.omg.CORBA.TIMEOUT;
import org.omg.CORBA.TRANSIENT;

import alma.acs.util.IorParser;
import alma.acs.util.UTCUtility;

import com.cosylab.acs.maci.AuthenticationData;
import com.cosylab.acs.maci.ClientType;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.ImplLang;
import com.cosylab.acs.maci.MessageType;
import com.cosylab.acs.maci.RemoteException;
import com.cosylab.acs.maci.RemoteTimeoutException;
import com.cosylab.acs.maci.RemoteTransientException;

/**
 * CORBA Client Proxy.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ClientProxy extends CORBAReferenceSerializator implements Client, Serializable
{

	/**
	 * Serial version UID.
	 */
	private static final long serialVersionUID = 5300210586145192795L;

	/**
	 * CORBA reference.
	 */
	protected si.ijs.maci.Client client;

	/**
	 * Cached serialized reference.
	 */
	protected String ior;
	
	/**
	 * Constructor for ClientProxy.
	 * @param	client	CORBA reference, non-<code>null</code>.
	 */
	public ClientProxy(si.ijs.maci.Client client)
	{
		assert (client != null);
		
		this.client = client;
		
		this.ior = serialize(client);
	}

	/**
	 * Convert CORBA ClienType to manager type.
	 * @param type
	 * @return
	 */
	public static ClientType toClientType(si.ijs.maci.ClientType type)
	{
		if (type == si.ijs.maci.ClientType.CLIENT_TYPE)
			return ClientType.CLIENT;
		else if (type == si.ijs.maci.ClientType.ADMINISTRATOR_TYPE)
			return ClientType.ADMINISTRATOR;
		else if (type == si.ijs.maci.ClientType.CONTAINER_TYPE)
			return ClientType.CONTAINER;
		else
			throw new IllegalArgumentException("unsupported client type");
	}
	
	/**
	 * Convert CORBA ImplLang to manager type.
	 * @param type
	 * @return
	 */
	public static ImplLang toImplLang(si.ijs.maci.ImplLangType type)
	{
		if (type == si.ijs.maci.ImplLangType.JAVA)
			return ImplLang.java;
		else if (type == si.ijs.maci.ImplLangType.CPP)
			return ImplLang.cpp;
		else if (type == si.ijs.maci.ImplLangType.PYTHON)
			return ImplLang.py;
		else
			throw new IllegalArgumentException("unsupported implementation language type");
	}

	/**
	 * @see com.cosylab.acs.maci.Client#authenticate(long, String)
	 */
	public AuthenticationData authenticate(long executionId, String question) throws RemoteException
	{
		try
		{
			si.ijs.maci.AuthenticationData retVal = client.authenticate(executionId, question);
			return new AuthenticationData(
					retVal.answer,
					toClientType(retVal.client_type),
					toImplLang(retVal.impl_lang),
					retVal.recover,
					UTCUtility.utcOmgToJava(retVal.timestamp),
					retVal.execution_id);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'authenticate()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Client#components_available(ComponentInfo[])
	 */
	public void components_available(ComponentInfo[] cobs) throws RemoteException
	{
		try
		{
			// invalid info (replacement for null)
			final si.ijs.maci.ComponentInfo invalidInfo = new si.ijs.maci.ComponentInfo("<invalid>", "<invalid>", null, "<invalid>", new int[0], 0, "<invalid>", 0, 0, new String[0]);
			
			// transform to CORBA specific 
			si.ijs.maci.ComponentInfo[] infos = null;
			if (cobs != null)
			{
				infos = new si.ijs.maci.ComponentInfo[cobs.length];
				for (int i = 0; i < cobs.length; i++)
					if (cobs[i] == null)
						infos[i] = invalidInfo;
					else
					{
						Object obj = null;
						if (cobs[i].getComponent() != null)
							obj = (Object)cobs[i].getComponent().getObject();
						String[] interfaces;
						if (cobs[i].getInterfaces() != null)
							interfaces = cobs[i].getInterfaces();
						else
							interfaces = new String[0];
						infos[i] = new si.ijs.maci.ComponentInfo(
												 cobs[i].getType(),
												 cobs[i].getCode(),
											     obj,
												 cobs[i].getName(),
												 cobs[i].getClients().toArray(),
												 cobs[i].getContainer(),
												 cobs[i].getContainerName(),
												 cobs[i].getHandle(),
												 ManagerProxyImpl.mapAccessRights(cobs[i].getAccessRights()),
												 interfaces
												 );
					}
			}
			else
				infos = new si.ijs.maci.ComponentInfo[0];
			
			client.components_available(infos);
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'components_available()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'components_available()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'component_available()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Client#components_unavailable(String[])
	 */
	public void components_unavailable(String[] cobs) throws RemoteException
	{
		try
		{
			client.components_unavailable(cobs);
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'components_unavailable()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'components_unavailable()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'component_unavailable()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Client#disconnect()
	 */
	public void disconnect() throws RemoteException
	{
		try
		{
			client.disconnect();
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'disconnect()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'disconnect()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'disconnect()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Client#message(MessageType, String)
	 */
	public void message(MessageType type, String message)
		throws RemoteException
	{
		try
		{
			
			short msgType;
			if (type == MessageType.MSG_ERROR)
				msgType = si.ijs.maci.Client.MSG_ERROR;
			else
				msgType = si.ijs.maci.Client.MSG_INFORMATION;
			
			client.message(msgType, message);
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'message()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'message()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'message()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Client#message(MessageType, String)
	 */
	public void taggedmessage(MessageType type, short id, String message)
		throws RemoteException
	{
		try
		{
			
			short msgType;
			if (type == MessageType.MSG_ERROR)
				msgType = si.ijs.maci.Client.MSG_ERROR;
			else
				msgType = si.ijs.maci.Client.MSG_INFORMATION;
			
			client.taggedmessage(msgType, id, message);
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'tagged_message()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'tagged_message()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'tagged_message()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Client#name()
	 */
	public String name() throws RemoteException
	{
		try
		{
			return client.name();
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'name()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Client#ping()
	 */
	public boolean ping() throws RemoteException
	{
		// invalid reference check
		if (client == null)
			return false;

		try
		{
			return client.ping();
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'ping()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'ping()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'ping()' method.", ex);
		}
	}

	/**
	 * Returns the client.
	 * @return si.ijs.maci.Client
	 */
	public si.ijs.maci.Client getClient()
	{
		return client;
	}
	

    /**
     * Get CORBA host/port string.
	 * @see com.cosylab.acs.maci.Client#getRemoteLocation()
	 */
	public String getRemoteLocation() throws RemoteException {
		try
		{
			String[] data = IorParser.parse(ior);
			return data[0]+ ":" + data[1];
		} catch (Throwable th) {
			throw new RemoteException("Failed to extract remote location.", th);
		}
	}

	/**
	 * We explicitly release the client reference, so that the ClientGIOPConnection can be closed,
	 * where otherwise it will occupy one of the connector threads until it gets garbage collected,
	 * possibly leading to the manager lock up described in http://jira.alma.cl/browse/COMP-979
	 * 
	 * @see com.cosylab.acs.maci.Client#release()
	 */
	public void release() {
		client._release();
	}

	/**
     * Save the state of the <tt>ClientProxy</tt> instance to a stream (that
     * is, serialize it).
     */
    private void writeObject(ObjectOutputStream stream)
        throws IOException
    {
    	stream.writeObject(ior);
    }

    /**
     * Reconstitute the <tt>ClientProxy</tt> instance from a stream (that is,
     * deserialize it).
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException
    {
		try {
			ior = (String)stream.readObject();
        	client = si.ijs.maci.ClientHelper.narrow(deserialize(ior));
		}
		catch (Exception e) {
			// silent here and set reference to null.
			// An method after deserialization should clean such invalid reference
			client = null;
			ior = null;
		}
    }


	/**
	 * Returns a single-line rendition of this instance into text.
	 * 
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ClientProxy = { ");
		sbuff.append("client = '");
		sbuff.append(client);
		sbuff.append("' }");
		return new String(sbuff);
	}

	/**
	 * @see java.lang.Object#equals(Object)
	 */
	public boolean equals(Object obj)
	{
		if (client == null)
			return (obj == null);
		else if (obj instanceof si.ijs.maci.Client)
		{
			try
			{
				return client._is_equivalent((si.ijs.maci.Client)obj);
			}
			catch (Exception ex)
			{
				return false;
			}
		}
		else
			return false;
	}

}
