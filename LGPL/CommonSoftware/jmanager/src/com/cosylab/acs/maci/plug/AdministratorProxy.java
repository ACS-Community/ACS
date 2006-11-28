/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import si.ijs.maci.Container;

import com.cosylab.acs.maci.ContainerInfo;
import com.cosylab.acs.maci.Administrator;
import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.RemoteException;

/**
 * CORBA Administrator Proxy.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class AdministratorProxy extends ClientProxy implements Administrator
{

	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -4699536769730108070L;

	/**
	 * CORBA reference.
	 */
	protected si.ijs.maci.Administrator administrator;

	/**
	 * Constructor for AdministratorProxy.
	 * @param	administrator	CORBA reference, non-<code>null</code>.
	 */
	public AdministratorProxy(si.ijs.maci.Administrator administrator)
	{
		super(administrator);
		
		this.administrator = administrator;
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#containerLoggedIn(ContainerInfo)
	 */
	public void containerLoggedIn(ContainerInfo info) throws RemoteException
	{
		try
		{
			
			si.ijs.maci.ContainerInfo containerInfo = null;
			if (info != null)
				containerInfo = new si.ijs.maci.ContainerInfo(info.getName(),
															  info.getHandle(),
															  (Container)((ClientProxy)info.getContainer()).getClient(),
															  info.getComponents().toArray());
			
			administrator.container_logged_in(containerInfo);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'containerLoggedIn()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#containerLoggedOut(int)
	 */
	public void containerLoggedOut(int handle) throws RemoteException
	{
		try
		{
			administrator.container_logged_out(handle);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'containerLoggedOut()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#clientLoggedIn(ClientInfo)
	 */
	public void clientLoggedIn(ClientInfo info) throws RemoteException
	{
		try
		{
			si.ijs.maci.ClientInfo clientInfo = null;
			if (info != null)
				clientInfo = new si.ijs.maci.ClientInfo(info.getHandle(),
														((ClientProxy)(info.getClient())).getClient(),
														info.getComponents().toArray(),
														info.getName(),
														ManagerProxyImpl.mapAccessRights(info.getAccessRights()));
			
			administrator.client_logged_in(clientInfo);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'clientLoggedIn()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#clientLoggedOut(int)
	 */
	public void clientLoggedOut(int handle) throws RemoteException
	{
		try
		{
			administrator.client_logged_out(handle);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'clientLoggedOut()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#components_released(int[], int[])
	 */
	public void components_released(int[] clients, int[] components) throws RemoteException
	{
		try
		{
			administrator.components_released(clients, components);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'components_released()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#components_requested(int[], int[])
	 */
	public void components_requested(int[] clients, int[] components)
		throws RemoteException
	{
		try
		{
			administrator.components_requested(clients, components);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'components_requested()' method.", ex);
			throw re;
		}
	}


	/**
	 * Returns the client.
	 * @return si.ijs.maci.Client
	 */
	public si.ijs.maci.Client getClient()
	{
		return administrator;
	}

     /**
     * Save the state of the <tt>ContainerProxy</tt> instance to a stream (that
     * is, serialize it).
     */
    private void writeObject(ObjectOutputStream stream)
        throws IOException
    {
        stream.writeObject(serialize(administrator));
    }

    /**
     * Reconstitute the <tt>ContainerProxy</tt> instance from a stream (that is,
     * deserialize it).
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException
    {
		try {
			administrator = si.ijs.maci.AdministratorHelper.narrow(deserialize((String)stream.readObject()));
		}
		catch (Exception e) {
			// silent here and set reference to null.
			// An method after deserialization should clean such invalid reference
			administrator = null;
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
		sbuff.append("AdministratorProxy = { ");
		sbuff.append("administrator = '");
		sbuff.append(administrator);
		sbuff.append("' }");
		return new String(sbuff);
	}

	/**
	 * @see java.lang.Object#equals(Object)
	 */
	public boolean equals(Object obj)
	{
		if (administrator == null)
			return (obj == null);
		else if (obj instanceof si.ijs.maci.Administrator)
		{
			try
			{
				return administrator._is_equivalent((si.ijs.maci.Administrator)obj);
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
