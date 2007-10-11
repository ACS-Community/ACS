/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import com.cosylab.acs.maci.AccessRights;
import com.cosylab.acs.maci.Component;
import com.cosylab.acs.maci.Container;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.IntArray;
import com.cosylab.acs.maci.RemoteException;


/**
 * CORBA Container Proxy.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ContainerProxy extends ClientProxy implements Container
{

	/**
	 * Serial version UID. 
	 */
	private static final long serialVersionUID = -7485420616224721204L;

	/**
	 * CORBA reference.
	 */
	protected si.ijs.maci.Container container;

	/**
	 * Constructor for ContainerProxy.
	 * @param	container	CORBA reference, non-<code>null</code>.
	 */
	public ContainerProxy(si.ijs.maci.Container container)
	{
		super(container);
		
		this.container = container;
	}


	/**
	 * @see com.cosylab.acs.maci.Container#activate_component(int, long, String, String, String)
	 */
	public ComponentInfo activate_component(int handle, long executionId, String name, String exe, String type)
		throws RemoteException
	{
		try
		{
			ComponentInfo retVal = null;
			si.ijs.maci.ComponentInfo info = container.activate_component(handle, executionId, name, exe, type);
			if (info != null)
			{
				retVal = new ComponentInfo(info.h, info.name, info.type, info.code,
					 				 info.reference != null ? new ComponentProxy(info.name, info.reference) : null);
				retVal.setContainer(info.container);
				retVal.setContainerName(info.container_name);
				retVal.setAccessRights(inverseMapAccessRights(info.access));
				retVal.setClients(new IntArray(info.clients));
				retVal.setInterfaces(info.interfaces);
			}
			
			return retVal;
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'activate_component()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Container#deactivate_components(int[])
	 */
	public void deactivate_components(int[] handles) throws RemoteException
	{
		try
		{
			container.deactivate_components(handles);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'deactivate_components()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Container#get_component_info(int[])
	 */
	public ComponentInfo[] get_component_info(int[] handles) throws RemoteException
	{
		try
		{
			
			// returned value
			ComponentInfo[] retVal = null;
			
			// transform to CORBA specific 
			si.ijs.maci.ComponentInfo[] infos = container.get_component_info(handles);
			if (infos != null)
			{
				retVal = new ComponentInfo[infos.length];
				for (int i = 0; i < infos.length; i++)
				{
					ComponentInfo componentInfo = new ComponentInfo(infos[i].h, infos[i].name, infos[i].type, infos[i].code,
													new ComponentProxy(infos[i].name, infos[i].reference));
					componentInfo.setContainer(infos[i].container);
					componentInfo.setContainerName(infos[i].container_name);
					componentInfo.setAccessRights(inverseMapAccessRights(infos[i].access));
					componentInfo.setClients(new IntArray(infos[i].clients));
					componentInfo.setInterfaces(infos[i].interfaces);
					retVal[i] = componentInfo;
				}
			}
			
			return retVal;
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'get_component_info()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Container#restart_component(int)
	 */
	public Component restart_component(int handle) throws RemoteException
	{
		try
		{
			org.omg.CORBA.Object component = container.restart_component(handle);
			final String componentName = "unknown"; 
			return new ComponentProxy(componentName, component);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'restart_component()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Container#shutdown(int)
	 */
	public void shutdown(int action) throws RemoteException
	{
		try
		{
			container.shutdown(action);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'shutdown()' method.", ex);
			throw re;
		}
	}

	/**
	 * @see com.cosylab.acs.maci.Container#deactivate_components(int[])
	 */
	public void set_component_shutdown_order(int[] handles) throws RemoteException
	{
		try
		{
			container.set_component_shutdown_order(handles);
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException("Failed to invoke 'set_component_shutdown_order()' method.", ex);
			throw re;
		}
	}
	
	/**
	 * Returns the client.
	 * @return si.ijs.maci.Client
	 */
	public si.ijs.maci.Client getClient()
	{
		return container;
	}

	/** 
	 * Map CORBA specific codes to <code>AccessRights</code> status codes.
	 * 
	 * @param	accessRights	CORBA specific codes
	 * @return	<code>AccessRights</code> status codes
	 */
	public static int inverseMapAccessRights(int accessRights)
	{
		int retVal = 0;
		
		if ((accessRights & si.ijs.maci.AccessRights.INTROSPECT_MANAGER.value) ==
			si.ijs.maci.AccessRights.INTROSPECT_MANAGER.value)
			retVal |= AccessRights.INTROSPECT_MANAGER;
			
		if ((accessRights & si.ijs.maci.AccessRights.REGISTER_COMPONENT.value) ==
			si.ijs.maci.AccessRights.REGISTER_COMPONENT.value)
			retVal |= AccessRights.REGISTER_COMPONENT;
			
		if ((accessRights & si.ijs.maci.AccessRights.SHUTDOWN_SYSTEM.value) == 
			si.ijs.maci.AccessRights.SHUTDOWN_SYSTEM.value)
			retVal |= AccessRights.SHUTDOWN_SYSTEM;

		return retVal;
	}


    /**
     * Save the state of the <tt>ContainerProxy</tt> instance to a stream (that
     * is, serialize it).
     */
    private void writeObject(ObjectOutputStream stream)
        throws IOException
    {
        stream.writeObject(serialize(container));
    }

    /**
     * Reconstitute the <tt>ContainerProxy</tt> instance from a stream (that is,
     * deserialize it).
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException
    {
		try {
			container = si.ijs.maci.ContainerHelper.narrow(deserialize((String)stream.readObject()));
		}
		catch (Exception e) {
			// silent here and set reference to null.
			// An method after deserialization should clean such invalid reference
			container = null;
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
		sbuff.append("ContainerProxy = { ");
		sbuff.append("container = '");
		sbuff.append(container);
		sbuff.append("' }");
		return new String(sbuff);
	}

	/**
	 * @see java.lang.Object#equals(Object)
	 */
	public boolean equals(Object obj)
	{
		if (container == null)
			return (obj == null);
		else if (obj instanceof si.ijs.maci.Container)
		{
			try
			{
				return container._is_equivalent((si.ijs.maci.Container)obj);
			}
			catch (Exception ex)
			{
				return false;
			}
		}
                else if (obj instanceof ClientProxy)
                {
                        try
                        {
                                return container._is_equivalent(((ClientProxy)obj).getClient());
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
