/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.net.URI;
import java.util.logging.Logger;

import si.ijs.maci.ComponentInfo;
import alma.acs.util.UTCUtility;

import com.cosylab.acs.maci.IntArray;
import com.cosylab.acs.maci.MessageType;
import com.cosylab.acs.maci.RemoteException;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;

import si.ijs.maci.AuthenticationData;
import si.ijs.maci.Client;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientPOA;
import si.ijs.maci.ClientType;
import si.ijs.maci.ImplLangType;
import si.ijs.maci.Manager;

/**
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ClientProxyImpl extends ClientPOA
{

	/**
	 * <code>Client</code> implementation.
	 */
	private ClientImpl client;

	/**
	 * CORBA reference of the <code>maci::Manager</code> this client is logged in.
	 */
	private Manager manager;

	/**
	 * Logger.
	 */
	private Logger logger;

	/**
	 * CORBA proxy implementation of <code>Client</code>.
	 * @param	name	name of the client
	 * @param	logger 	logger.
	 */
	public ClientProxyImpl(String name, Logger logger)
	{
		client = new ClientImpl(name);
		this.logger = logger;
	}
	

	/**
	 * @see si.ijs.maci.ClientOperations#name()
	 */
	public String name()
	{
		return client.getName();
	}

	/**
	 * @see si.ijs.maci.ClientOperations#disconnect()
	 */
	public void disconnect()
	{
		try
		{
			client.disconnect();
		}
		catch (RemoteException re)
		{
			// noop.
		}
	}

	/**
	 * Convert manager ClientType to CORBA type.
	 * @param type
	 * @return
	 */
	public static ClientType toClientType(com.cosylab.acs.maci.ClientType type)
	{
		switch (type)
		{
			case CLIENT:
				return ClientType.CLIENT_TYPE;
			case ADMINISTRATOR:
				return ClientType.ADMINISTRATOR_TYPE;
			case CONTAINER:
				return ClientType.CONTAINER_TYPE;
			default:
				throw new IllegalArgumentException("unsupported client type");
		}
	}
	
	/**
	 * Convert manager ImplLang to CORBA type.
	 * @param type
	 * @return
	 */
	public static ImplLangType toImplLangType(com.cosylab.acs.maci.ImplLang type)
	{
		switch (type)
		{
			case java:
				return ImplLangType.JAVA;
			case cpp:
				return ImplLangType.CPP;
			case py:
				return ImplLangType.PYTHON;
			default:
				throw new IllegalArgumentException("unsupported implementation language type");
		}
	}

	/**
	 * @see si.ijs.maci.ClientOperations#authenticate(long, String)
	 */
	public AuthenticationData authenticate(long executionId, String question)
	{
		try
		{
			com.cosylab.acs.maci.AuthenticationData retVal = client.authenticate(executionId, question);
			return new AuthenticationData(
					retVal.getAnswer(),
					toClientType(retVal.getClientType()),
					toImplLangType(retVal.getImplLang()),
					retVal.isRecover(),
					UTCUtility.utcJavaToOmg(retVal.getTimeStamp()),
					retVal.getExecutionId());
		}
		catch (RemoteException re)
		{
			return null;
		}
	}

	/**
	 * @see si.ijs.maci.ClientOperations#message(short, String)
	 */
	public void message(short type, String message)
	{
		try
		{
			MessageType msgType;
			if (type == Client.MSG_ERROR)
				msgType = MessageType.MSG_ERROR;
			else
				msgType = MessageType.MSG_INFORMATION;
				
			client.message(msgType, message);
		}
		catch (RemoteException re)
		{
			// noop.
		}
	}

	/**
	 * @see si.ijs.maci.ClientOperations#taggedmessage(short, String)
	 */
	public void taggedmessage(short type, short id, String message)
	{
		try
		{
			MessageType msgType;
			if (type == Client.MSG_ERROR)
				msgType = MessageType.MSG_ERROR;
			else
				msgType = MessageType.MSG_INFORMATION;
				
			client.taggedmessage(msgType, id, message);
		}
		catch (RemoteException re)
		{
			// noop.
		}
	}

	/**
	 * @see si.ijs.maci.ClientOperations#ping()
	 */
	public boolean ping()
	{
		try
		{
			return client.ping();
		}
		catch (RemoteException re)
		{
			return false;
		}
	}

	/**
	 * @see si.ijs.maci.ClientOperations#components_available(ComponentInfo[])
	 */
	public void components_available(ComponentInfo[] components)
	{
		try
		{
			com.cosylab.acs.maci.ComponentInfo[] info = new com.cosylab.acs.maci.ComponentInfo[components.length];
			
			for (int i=0; i < components.length; i++)
			{
				info[i] = new com.cosylab.acs.maci.ComponentInfo(components[i].h, components[i].name, components[i].type, components[i].code,
															new ComponentProxy(components[i].name, components[i].reference));
				info[i].setContainer(components[i].container);
				info[i].setContainerName(components[i].container_name);
				info[i].setClients(new IntArray(components[i].clients));
				info[i].setInterfaces(components[i].interfaces);
				info[i].setAccessRights(components[i].access);
			}
			
			client.components_available(info);
		}
		catch (RemoteException re)
		{
			// noop.
		}
	}

	/**
	 * @see si.ijs.maci.ClientOperations#components_unavailable(String[])
	 */
	public void components_unavailable(String[] component_names)
	{
		try
		{
			client.components_unavailable(component_names);
		}
		catch (RemoteException re)
		{
			// noop.
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
		sbuff.append("ClientProxyImpl = { ");
		sbuff.append("client = '");
		sbuff.append(client);
		sbuff.append("' }");
		return new String(sbuff);
	}

	/*********************** Helper methods ***********************/

	/**
	 * Login to the manager and obtain handle.
	 * 
	 * @param	manager	CORBA <code>maci::Manager</code> reference, non-<code>null</code>
	 * @return boolean
	 */
	public boolean login(ORB orb, Manager manager)
	{
		assert (orb != null);
		assert (manager != null);
		this.manager = manager;
				
		try
		{
			ClientInfo clientInfo = manager.login(this._this(orb));
			if (clientInfo != null && clientInfo.h != 0)
			{
				client.setHandle(clientInfo.h);
				
				logger.info("Successfully logged in to the Manager.");
				
				return true;
			}
		}
		catch (Exception ex)
		{
			//RemoteException re = new RemoteException("Failed to login to the Manager.", ex);
		      // TODO log this

			return false;
		}
		
		return false;
		
	}
	
	/**
	 * Logout from manager.
	 */
	public void logout()
	{
		if (manager == null || client.getHandle() == 0)
			return;
				
		try
		{
			manager.logout(client.getHandle());
			client.setHandle(0);

			logger.info("Successfully logged out from the Manager.");
		}
		catch (Exception ex)
		{
			//RemoteException re = new RemoteException("Failed to logout from the Manager.", ex);
		      // TODO log this
		}
		
	}
	
	/**
	 * Request and obtain Component from the manager.
	 * @param	curl	CURL of the Component.
	 * @param	activate	<code>true</code> if Component has to activated.
	 * @return	Object	reference of the Component
	 */
	public Object getComponent(URI curl, boolean activate)
	{
		if (manager == null || client.getHandle() == 0)
			return null;
				
		try
		{
			Object component = manager.get_service(client.getHandle(), curl.toString(), activate);
			
			if (component != null)
			{
				logger.info("Successfully obtained Component '" + curl + "'.");
					
				return component;
			}
		}
		catch (Exception ex)
		{
			//RemoteException re = new RemoteException("Failed to obtain Component '" + curl + "'.", ex);
		      // TODO log this
		}
		
		return null;
	}
	
  /**
   * Releases Component at the manager.
   * @param component  reference to the Component.
   */
  public void releaseComponent(URI curl)
  {
    if (manager == null || client.getHandle() == 0)
      return;
        
    try
    {
      manager.release_component(client.getHandle(), curl.toString());
    }
    catch (Exception ex)
    {
      //RemoteException re = new RemoteException("Failed to release component '" + curl + "'.", ex);
      // TODO log this
    }
  }
}
