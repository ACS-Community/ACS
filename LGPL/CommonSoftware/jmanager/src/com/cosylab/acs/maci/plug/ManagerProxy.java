/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.plug;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.net.URI;

import org.omg.CORBA.Object;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.pluggable.RemoteException;
import alma.maciErrType.NoPermissionEx;

import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.Component;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.ComponentSpec;
import com.cosylab.acs.maci.ComponentSpecIncompatibleWithActiveComponentException;
import com.cosylab.acs.maci.ComponentStatus;
import com.cosylab.acs.maci.ContainerInfo;
import com.cosylab.acs.maci.IncompleteComponentSpecException;
import com.cosylab.acs.maci.IntArray;
import com.cosylab.acs.maci.InvalidComponentSpecException;
import com.cosylab.acs.maci.Manager;
import com.cosylab.acs.maci.NoDefaultComponentException;
import com.cosylab.acs.maci.NoPermissionException;
import com.cosylab.acs.maci.StatusHolder;
import com.cosylab.acs.maci.StatusSeqHolder;

/**
 * CORBA Manager Proxy.
 * This class is used by clients of remote Managers that want to use
 * the acs.maci.Manager interface.
 * In particular it used by the JManager application to implement 
 * Manager federation.
 * 
 *  In this way we have simmetry:
 *  - ManagerProxyImpl is used to convert the IDL interface of the Manager into acs.maci.Manager
 *    on the servant/skeleton side
 *  - ManagerProsy is used to convert from acs.maci.Manager into the IDL interface
 *    on the client/stub side
 *    
 * @todo not completely implemented
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ManagerProxy extends CORBAReferenceSerializator implements Manager, Identifiable, Serializable
{

	/**
	 * Identifier.
	 */
	protected Identifier id = null;

	/**
	 * CORBA reference.
	 */
	protected si.ijs.maci.Manager manager;
	
	/**
	 * Constructor for ManagerProxy.
	 * @param	manager	CORBA reference, non-<code>null</code>.
	 */
	public ManagerProxy(si.ijs.maci.Manager manager)
	{
		assert (manager != null);
		
		this.manager = manager;
	}

	/**
	 * Constructor for ManagerProxy.
	 * @param	obj	java.lang.Object which is CORBA reference, non-<code>null</code>.
	 */
	public ManagerProxy(java.lang.Object obj)
	{
	    this(si.ijs.maci.ManagerHelper.narrow((Object)obj));
	}
	
    /**
     * @see com.cosylab.acs.maci.Manager#getClientInfo(int, int[], java.lang.String)
     */
    public ClientInfo[] getClientInfo(int id, int[] handles, String name_wc)
            throws NoPermissionException
    {
        /// @todo Not implemented
        return null;
    }
    
	/**
	 * Map <code>componentStatus</code> status codes from CORBA specific.
	 *
	 * @param	status	CORBA specific int value (status).
	 * @return	status of type <code>componentStatus</code>, <code>null</code> if code is unknown.
	 * @deprecated Will be removed together with getServices and getComponents
	 */
	public static ComponentStatus mapStatusInverse(int status)
	{
		if (status == si.ijs.maci.Manager.COMPONENT_NONEXISTENT)
			return ComponentStatus.COMPONENT_DOES_NO_EXIST;
		else if (status == si.ijs.maci.Manager.COMPONENT_NOT_ACTIVATED)
			return ComponentStatus.COMPONENT_NOT_ACTIVATED;
		else if (status == si.ijs.maci.Manager.COMPONENT_ACTIVATED)
			return ComponentStatus.COMPONENT_ACTIVATED;
		else
			return null;
	}

	/**
     * @see com.cosylab.acs.maci.Manager#getComponent(int, java.net.URI, boolean, com.cosylab.acs.maci.StatusHolder)
     */
    public Component getComponent(int id, URI curl, boolean activate,
            StatusHolder status) throws NoPermissionException
    {
		try
		{
		    Component retVal = null;

		    org.omg.CORBA.Object object = manager.get_component(id, curl.toString(), activate);
		    
		    if (object != null)
		    {
		        retVal = new ComponentProxy(curl.toString(), object);
		    }
			
			return retVal;
		}
		catch (NoPermissionEx npex)
		{
		    NoPermissionException npe = new NoPermissionException(this, "Remote manager has thrown no permission exception.", npex);
			npe.caughtIn(this, "getComponent");
			npe.putValue("curl", curl);
			throw npe;
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to invoke 'get_component()' method.", ex);
			re.caughtIn(this, "getComponent");
			//throw re;
			return null;
		}
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getComponentInfo(int, int[], java.lang.String, java.lang.String, boolean)
     */
    public ComponentInfo[] getComponentInfo(int id, int[] handles,
            String name_wc, String type_wc, boolean activeOnly)
            throws NoPermissionException
    {
		try
		{
			
			// returned value
			ComponentInfo[] retVal = null;
			
			// transform to CORBA specific 
			si.ijs.maci.ComponentInfo[] infos = manager.get_component_info(id, handles, name_wc, type_wc, activeOnly);
			if (infos != null)
			{
				retVal = new ComponentInfo[infos.length];
				for (int i = 0; i < infos.length; i++)
				{
					ComponentInfo componentInfo = new ComponentInfo(infos[i].h, infos[i].name, infos[i].type, infos[i].code,
													new ComponentProxy(infos[i].name, infos[i].reference));
					componentInfo.setContainer(infos[i].container);
					componentInfo.setContainerName(infos[i].container_name);
					componentInfo.setAccessRights(ContainerProxy.inverseMapAccessRights(infos[i].access));
					componentInfo.setClients(new IntArray(infos[i].clients));
					componentInfo.setInterfaces(infos[i].interfaces);
					retVal[i] = componentInfo;
				}
			}
			
			return retVal;
		}
		catch (NoPermissionEx npex)
		{
		    NoPermissionException npe = new NoPermissionException(this, "Remote manager has thrown no permission exception.", npex);
			npe.caughtIn(this, "getComponentInfo");
			throw npe;
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to invoke 'get_component_info()' method.", ex);
			re.caughtIn(this, "getComponentInfo");
			//throw re;
			return null;
		}
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getComponents(int, java.net.URI[], boolean, com.cosylab.acs.maci.StatusSeqHolder)
     * @deprecated
     */
    public Component[] getComponents(int id, URI[] curls, boolean activate,
            StatusSeqHolder statuses) throws NoPermissionException
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getContainerInfo(int, int[], java.lang.String)
     */
    public ContainerInfo[] getContainerInfo(int id, int[] handles,
            String name_wc) throws NoPermissionException
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getDefaultComponent(int, java.lang.String)
     */
    public ComponentInfo getDefaultComponent(int id, String type)
            throws NoPermissionException, NoDefaultComponentException
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getDomain()
     */
    public String getDomain()
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getDynamicComponent(int, com.cosylab.acs.maci.ComponentSpec, boolean)
     */
    public ComponentInfo getDynamicComponent(int id,
            ComponentSpec componentSpec, boolean markAsDefault)
            throws NoPermissionException, IncompleteComponentSpecException,
            InvalidComponentSpecException,
            ComponentSpecIncompatibleWithActiveComponentException
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getDynamicComponents(int, com.cosylab.acs.maci.ComponentSpec[])
     * @deprecated
     */
    public ComponentInfo[] getDynamicComponents(int id,
            ComponentSpec[] components) throws NoPermissionException
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getCollocatedComponent(int, com.cosylab.acs.maci.ComponentSpec, boolean, URI)
     */
    public ComponentInfo getCollocatedComponent(int id,
            ComponentSpec componentSpec, boolean markAsDefault, URI targetComponentURI)
            throws NoPermissionException, IncompleteComponentSpecException,
            InvalidComponentSpecException,
            ComponentSpecIncompatibleWithActiveComponentException
    {
        /// @todo Not implemented
        return null;
    }

	/**
	 * @see com.cosylab.acs.maci.Manager#getComponentNonSticky(int, java.net.URI)
	 */
	public Component getComponentNonSticky(int id, URI curl) 
		throws NoPermissionException
    {
        /// @todo Not implemented
        return null;
    }
    

	
    /**
     * @see com.cosylab.acs.maci.Manager#getService(int, java.net.URI, boolean, com.cosylab.acs.maci.StatusHolder)
     */
    public Component getService(int id, URI curl, boolean activate,
            StatusHolder status) throws NoPermissionException
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getServices(int, java.net.URI[], boolean, com.cosylab.acs.maci.StatusSeqHolder)
     * @deprecated
     */
    public Component[] getServices(int id, URI[] curls, boolean activate,
            StatusSeqHolder statuses) throws NoPermissionException
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#login(com.cosylab.acs.maci.Client)
     */
    public ClientInfo login(Client reference) throws NoPermissionException
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#logout(int)
     */
    public void logout(int id) throws NoPermissionException
    {
        /// @todo Not implemented

    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#registerComponent(int, java.net.URI, java.lang.String, com.cosylab.acs.maci.Component)
     */
    public int registerComponent(int id, URI curl, String type, Component cob)
            throws NoPermissionException
    {
        /// @todo Not implemented
        return 0;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#releaseComponent(int, java.net.URI)
     */
    public int releaseComponent(int id, URI curl) throws NoPermissionException
    {
		try
		{
		    return manager.release_component(id, curl.toString());
		}
		catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to invoke 'release_component()' method.", ex);
			re.caughtIn(this, "releaseComponent");
			//throw re;
			return 0;
		}
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#releaseComponents(int, java.net.URI[])
     * @deprecated
     */
    public void releaseComponents(int id, URI[] curls)
            throws NoPermissionException
    {
        /// @todo Not implemented

    }
    
	/**
	 * @see com.cosylab.acs.maci.Manager#forceReleaseComponent(int, java.net.URI)
	 */
	public int forceReleaseComponent(int id, URI curl)
			throws NoPermissionException {
		/// @todo Not implemented
		return 0;
	}

	/**
     * @see com.cosylab.acs.maci.Manager#restartComponent(int, java.net.URI)
     */
    public Component restartComponent(int id, URI curl)
            throws NoPermissionException
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#shutdown(int, int)
     */
    public void shutdown(int id, int containers) throws NoPermissionException
    {
        /// @todo Not implemented

    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#unregisterComponent(int, int)
     */
    public void unregisterComponent(int id, int handle)
            throws NoPermissionException
    {
        /// @todo Not implemented

    }
    
	/**
	 * @see com.cosylab.acs.maci.Manager#shutdownContainer(int, java.lang.String, int)
	 */
	public void shutdownContainer(int id, String containerName, int action)
			throws NoPermissionException {
		/// @todo Not implemented
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#makeComponentImmortal(int, java.net.URI, boolean)
	 */
	public void makeComponentImmortal(int id, URI curl, boolean immortalState)
			throws NoPermissionException {
		/// @todo Not implemented
	}
	
	/**
	 * Returns the manager.
	 * @return si.ijs.maci.Manager
	 */
	public si.ijs.maci.Manager getManager()
	{
		return manager;
	}

 	/*****************************************************************************/
	/*************************** [ Abeans methods ] ******************************/
	/*****************************************************************************/

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null)
			id = new IdentifierSupport("Manager CORBA Proxy", "Manager", Identifier.PLUG);
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return false;
	}

    /**
     * Save the state of the <tt>ManagerProxy</tt> instance to a stream (that
     * is, serialize it).
     */
    private void writeObject(ObjectOutputStream stream)
        throws IOException
    {
        stream.writeObject(serialize(manager));
    }



    /**
     * Reconstitute the <tt>ManagerProxy</tt> instance from a stream (that is,
     * deserialize it).
     */
    private void readObject(ObjectInputStream stream)
        throws IOException, ClassNotFoundException
    {
		try {
        	manager = si.ijs.maci.ManagerHelper.narrow(deserialize((String)stream.readObject()));
		}
		catch (Exception e) {
			// silent here and set reference to null.
			// An method after deserialization should clean such invalid reference
			manager = null;
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
		sbuff.append("ManagerProxy = { ");
		sbuff.append("manager = '");
		sbuff.append(manager);
		sbuff.append("' }");
		return new String(sbuff);
	}

	/**
	 * @see java.lang.Object#equals(Object)
	 */
	public boolean equals(Object obj)
	{
		if (manager == null)
			return (obj == null);
		else if (obj instanceof si.ijs.maci.Manager)
		{
			try
			{
				return manager._is_equivalent((si.ijs.maci.Manager)obj);
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

