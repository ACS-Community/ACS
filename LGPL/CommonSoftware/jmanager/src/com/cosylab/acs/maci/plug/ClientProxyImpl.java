/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.net.URI;

import si.ijs.maci.ComponentInfo;
import com.cosylab.acs.maci.IntArray;
import com.cosylab.acs.maci.MessageType;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.core.defaults.MessageLogEntry;
import abeans.pluggable.RemoteException;
import abeans.pluggable.acs.logging.LoggingLevel;

import org.omg.CORBA.IntHolder;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import si.ijs.maci.Client;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientPOA;
import si.ijs.maci.Manager;

/**
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ClientProxyImpl extends ClientPOA implements Identifiable
{

	/**
	 * Identifier.
	 */
	private Identifier id = null;

	/**
	 * <code>Client</code> implementation.
	 */
	ClientImpl client;

	/**
	 * CORBA reference of the <code>maci::Manager</code> this client is logged in.
	 */
	Manager manager;

	/**
	 * CORBA proxy implementation of <code>Client</code>.
	 * @param	name	name of the client
	 */
	public ClientProxyImpl(String name)
	{
		client = new ClientImpl(name);
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
	 * @see si.ijs.maci.ClientOperations#authenticate(String)
	 */
	public String authenticate(String question)
	{
		try
		{
			return client.authenticate(question);
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

	/*********************** Abeans methods ***********************/

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null)
			id = new IdentifierSupport(client.getName(), client.getName(), Identifier.PLUG);
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return true;
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
				
				if (isDebug())
					new MessageLogEntry(this, "login", "Successfully logged in to the Manager.", LoggingLevel.INFO).dispatch();
				
				return true;
			}
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to login to the Manager.", ex);
			re.caughtIn(this, "login");
			// Exception service will handle this
			//throw re;
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

			if (isDebug())
				new MessageLogEntry(this, "logout", "Successfully logged out from the Manager.", LoggingLevel.INFO).dispatch();
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to logout from the Manager.", ex);
			re.caughtIn(this, "logout");
			// Exception service will handle this
			//throw re;
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
			IntHolder status = new IntHolder();
			Object component = manager.get_service(client.getHandle(), curl.toString(), activate, status);
			
			if (component != null)
			{
				if (isDebug())
					new MessageLogEntry(this, "curl", "Successfully obtained Component '" + curl + "'.", LoggingLevel.INFO).dispatch();
					
				return component;
			}
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to obtain Component '" + curl + "'.", ex);
			re.caughtIn(this, "getCOB");
			re.putValue("curl", curl);
			// Exception service will handle this
			//throw re;
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
      RemoteException re = new RemoteException(this, "Failed to release Component '" + curl + "'.", ex);
      re.caughtIn(this, "releaseComponent");
      re.putValue("curl", curl);
      // Exception service will handle this
      //throw re;
    }
  }
}
