/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import org.omg.CORBA.Object;
import org.omg.CORBA.TIMEOUT;
import org.omg.CORBA.TRANSIENT;

import si.ijs.maci.Container;
import alma.acs.util.UTCUtility;

import com.cosylab.acs.maci.Administrator;
import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.ContainerInfo;
import com.cosylab.acs.maci.RemoteException;
import com.cosylab.acs.maci.RemoteTimeoutException;
import com.cosylab.acs.maci.RemoteTransientException;

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
		
		this.ior = serialize(administrator);
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#containerLoggedIn(ContainerInfo, long, long)
	 */
	public void containerLoggedIn(ContainerInfo info, long timeStamp, long executionId) throws RemoteException
	{
		try
		{
			
			si.ijs.maci.ContainerInfo containerInfo = null;
			if (info != null)
				containerInfo = new si.ijs.maci.ContainerInfo(info.getName(),
															  info.getHandle(),
															  (Container)((ClientProxy)info.getContainer()).getClient(),
															  info.getComponents().toArray());
			
			administrator.container_logged_in(containerInfo, UTCUtility.utcJavaToOmg(timeStamp), executionId);
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'container_logged_in()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'container_logged_in()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'container_logged_in()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#containerLoggedOut(int, long)
	 */
	public void containerLoggedOut(int handle, long timeStamp) throws RemoteException
	{
		try
		{
			administrator.container_logged_out(handle, UTCUtility.utcJavaToOmg(timeStamp));
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'container_logged_out()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'container_logged_out()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'container_logged_out()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#clientLoggedIn(ClientInfo, long, long)
	 */
	public void clientLoggedIn(ClientInfo info, long timeStamp, long executionId) throws RemoteException
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
			
			administrator.client_logged_in(clientInfo, UTCUtility.utcJavaToOmg(timeStamp), executionId);
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'client_logged_in()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'client_logged_in()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'client_logged_in()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#clientLoggedOut(int, long)
	 */
	public void clientLoggedOut(int handle, long timeStamp) throws RemoteException
	{
		try
		{
			administrator.client_logged_out(handle, UTCUtility.utcJavaToOmg(timeStamp));
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'client_logged_out()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'client_logged_out()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'client_logged_out()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#components_released(int[], int[], long)
	 */
	public void components_released(int[] clients, int[] components, long timeStamp) throws RemoteException
	{
		try
		{
			administrator.components_released(clients, components, UTCUtility.utcJavaToOmg(timeStamp));
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'components_released()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'components_released()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'components_released()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#components_requested(int[], int[], long)
	 */
	public void components_requested(int[] clients, int[] components, long timeStamp)
		throws RemoteException
	{
		try
		{
			administrator.components_requested(clients, components, UTCUtility.utcJavaToOmg(timeStamp));
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'components_requested()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'components_requested()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'components_requested()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#component_activated(com.cosylab.acs.maci.ComponentInfo, long, long)
	 */
	public void component_activated(ComponentInfo info, long timeStamp, long executionId) throws RemoteException {
		try
		{
			// invalid info (replacement for null)
			final si.ijs.maci.ComponentInfo invalidInfo = new si.ijs.maci.ComponentInfo("<invalid>", "<invalid>", null, "<invalid>", new int[0], 0, "<invalid>", 0, 0, new String[0]);

			si.ijs.maci.ComponentInfo componentInfo = invalidInfo;
			if (info != null)
			{
				Object obj = null;
				if (info.getComponent() != null)
					obj = (Object)info.getComponent().getObject();
				String[] interfaces;
				if (info.getInterfaces() != null)
					interfaces = info.getInterfaces();
				else
					interfaces = new String[0];

				componentInfo = new si.ijs.maci.ComponentInfo(info.getType(),
										 info.getCode(),
									     obj,
										 info.getName(),
										 info.getClients().toArray(),
										 info.getContainer(),
										 info.getContainerName(),
										 info.getHandle(),
										 ManagerProxyImpl.mapAccessRights(info.getAccessRights()),
										 interfaces);
			}
			
			administrator.component_activated(componentInfo, UTCUtility.utcJavaToOmg(timeStamp), executionId);
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'component_activated()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'component_activated()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'component_activated()' method.", ex);
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Administrator#component_deactivated(int, long)
	 */
	public void component_deactivated(int handle, long timeStamp) throws RemoteException {
		try
		{
			administrator.component_deactivated(handle, UTCUtility.utcJavaToOmg(timeStamp));
		}
		catch (TIMEOUT te)
		{
			throw new RemoteTimeoutException("Failed to invoke 'component_deactivated()' method due to timeout.", te);
		}
		catch (TRANSIENT tre)
		{
			throw new RemoteTransientException("Failed to invoke 'component_deactivated()' method due to transient exception.", tre);
		}
		catch (Throwable ex)
		{
			throw new RemoteException("Failed to invoke 'component_deactivated()' method.", ex);
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
        stream.writeObject(ior);
    }

    /**
     * Reconstitute the <tt>ContainerProxy</tt> instance from a stream (that is,
     * deserialize it).
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException
    {
		try {
			ior = (String)stream.readObject();
			administrator = si.ijs.maci.AdministratorHelper.narrow(deserialize(ior));
		}
		catch (Exception e) {
			// silent here and set reference to null.
			// An method after deserialization should clean such invalid reference
			administrator = null;
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
