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

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBlongPOA;
import alma.ACSErr.Completion;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.acs.exceptions.AcsJCompletion;
import alma.maciErrType.NoPermissionEx;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;
import alma.maciErrType.wrappers.AcsJComponentSpecIncompatibleWithActiveComponentEx;
import alma.maciErrType.wrappers.AcsJIncompleteComponentSpecEx;
import alma.maciErrType.wrappers.AcsJInvalidComponentSpecEx;
import alma.maciErrType.wrappers.AcsJNoPermissionEx;

import com.cosylab.acs.maci.Client;
import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.Component;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.ComponentSpec;
import com.cosylab.acs.maci.ContainerInfo;
import com.cosylab.acs.maci.HandleHelper;
import com.cosylab.acs.maci.IntArray;
import com.cosylab.acs.maci.Manager;
import com.cosylab.acs.maci.NoDefaultComponentException;
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
public class ManagerProxy extends CORBAReferenceSerializator implements Manager, Serializable
{

	/**
	 * Serial version UID.
	 */
	private static final long serialVersionUID = 1616932223611613918L;

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
            throws AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return null;
    }
    
	/**
     * @see com.cosylab.acs.maci.Manager#getComponent(int, java.net.URI, boolean, com.cosylab.acs.maci.StatusHolder)
     */
    public Component getComponent(int id, URI curl, boolean activate,
            StatusHolder status) throws AcsJNoPermissionEx
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
		    AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
			npe.setReason("Remote manager has thrown no permission exception.");
			npe.setID(HandleHelper.toString(id));
			npe.setProtectedResource(curl.toString());
			throw npe;
		}
		catch (Exception ex)
		{
			//RemoteException re = new RemoteException("Failed to invoke 'get_component()' method.", ex);
			//throw re;
			return null;
		}
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getComponentInfo(int, int[], java.lang.String, java.lang.String, boolean)
     */
    public ComponentInfo[] getComponentInfo(int id, int[] handles,
            String name_wc, String type_wc, boolean activeOnly)
            throws AcsJNoPermissionEx
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
		    AcsJNoPermissionEx npe = new AcsJNoPermissionEx();
			npe.setReason("Remote manager has thrown no permission exception.");
			npe.setID(HandleHelper.toString(id));
			throw npe;
		}
		catch (Exception ex)
		{
			//RemoteException re = new RemoteException("Failed to invoke 'get_component_info()' method.", ex);
			//throw re;
			return null;
		}
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getComponents(int, java.net.URI[], boolean, com.cosylab.acs.maci.StatusSeqHolder)
     * @deprecated
     */
    public Component[] getComponents(int id, URI[] curls, boolean activate,
            StatusSeqHolder statuses) throws AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getContainerInfo(int, int[], java.lang.String)
     */
    public ContainerInfo[] getContainerInfo(int id, int[] handles,
            String name_wc) throws AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getDefaultComponent(int, java.lang.String)
     */
    public ComponentInfo getDefaultComponent(int id, String type)
            throws AcsJNoPermissionEx, NoDefaultComponentException
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
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx, AcsJIncompleteComponentSpecEx,
		   AcsJInvalidComponentSpecEx, AcsJComponentSpecIncompatibleWithActiveComponentEx
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getDynamicComponents(int, com.cosylab.acs.maci.ComponentSpec[])
     * @deprecated
     */
    public ComponentInfo[] getDynamicComponents(int id,
            ComponentSpec[] components) throws AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getCollocatedComponent(int, com.cosylab.acs.maci.ComponentSpec, boolean, URI)
     */
    public ComponentInfo getCollocatedComponent(int id,
            ComponentSpec componentSpec, boolean markAsDefault, URI targetComponentURI)
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx, AcsJIncompleteComponentSpecEx,
		   AcsJInvalidComponentSpecEx, AcsJComponentSpecIncompatibleWithActiveComponentEx
    {
        /// @todo Not implemented
        return null;
    }

	/**
	 * @see com.cosylab.acs.maci.Manager#getComponentNonSticky(int, java.net.URI)
	 */
	public Component getComponentNonSticky(int id, URI curl) 
		throws AcsJCannotGetComponentEx, AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return null;
    }
    

	
    /**
     * @see com.cosylab.acs.maci.Manager#getService(int, java.net.URI, boolean, com.cosylab.acs.maci.StatusHolder)
     */
    public Component getService(int id, URI curl, boolean activate,
            StatusHolder status) throws AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#getServices(int, java.net.URI[], boolean, com.cosylab.acs.maci.StatusSeqHolder)
     * @deprecated
     */
    public Component[] getServices(int id, URI[] curls, boolean activate,
            StatusSeqHolder statuses) throws AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#login(com.cosylab.acs.maci.Client)
     */
    public ClientInfo login(Client reference) throws AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#logout(int)
     */
    public void logout(int id) throws AcsJNoPermissionEx
    {
        /// @todo Not implemented

    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#registerComponent(int, java.net.URI, java.lang.String, com.cosylab.acs.maci.Component)
     */
    public int registerComponent(int id, URI curl, String type, Component cob)
            throws AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return 0;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#releaseComponent(int, java.net.URI)
     */
    public int releaseComponent(int id, URI curl) throws AcsJNoPermissionEx
    {
		try
		{
		    return manager.release_component(id, curl.toString());
		}
		catch (Exception ex)
		{
			//RemoteException re = new RemoteException("Failed to invoke 'release_component()' method.", ex);
			//throw re;
			return 0;
		}
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#releaseComponents(int, java.net.URI[])
     * @deprecated
     */
    public void releaseComponents(int id, URI[] curls)
            throws AcsJNoPermissionEx
    {
        /// @todo Not implemented

    }
    
	/**
	 * @see com.cosylab.acs.maci.Manager#forceReleaseComponent(int, java.net.URI)
	 */
	public int forceReleaseComponent(int id, URI curl)
			throws AcsJNoPermissionEx {
		/// @todo Not implemented
		return 0;
	}

	/**
     * @see com.cosylab.acs.maci.Manager#restartComponent(int, java.net.URI)
     */
    public Component restartComponent(int id, URI curl)
            throws AcsJNoPermissionEx
    {
        /// @todo Not implemented
        return null;
    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#shutdown(int, int)
     */
    public void shutdown(int id, int containers) throws AcsJNoPermissionEx
    {
        /// @todo Not implemented

    }
    
    /**
     * @see com.cosylab.acs.maci.Manager#unregisterComponent(int, int)
     */
    public void unregisterComponent(int id, int handle)
            throws AcsJNoPermissionEx
    {
        /// @todo Not implemented

    }
    
	/**
	 * @see com.cosylab.acs.maci.Manager#shutdownContainer(int, java.lang.String, int)
	 */
	public void shutdownContainer(int id, String containerName, int action)
			throws AcsJNoPermissionEx {
		/// @todo Not implemented
	}

	/**
	 * @see com.cosylab.acs.maci.Manager#makeComponentImmortal(int, java.net.URI, boolean)
	 */
	public void makeComponentImmortal(int id, URI curl, boolean immortalState)
			throws AcsJNoPermissionEx {
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

	public void releaseComponentAsync(int id, URI curl,
			LongCompletionCallback callback) throws AcsJNoPermissionEx, AcsJBadParameterEx {
		try
		{
			final LongCompletionCallback fcallback = callback;
			CBlongPOA cbo = new CBlongPOA() {
				
				public boolean negotiate(long time_to_transmit, CBDescOut desc) {
					return false;
				}
				
				public void working(int value, Completion c, CBDescOut desc) {
					// noop
				}
				
				public void done(int value, Completion c, CBDescOut desc) {
					if (c.code == 0 && c.type == 0)
						fcallback.done(value);
					else
						// TODO maybe convert to specific exceptions
						fcallback.failed(value, AcsJCompletion.fromCorbaCompletion(c).getAcsJException());
				}
			};
			
			CBDescIn desc = new CBDescIn(0, 0, 0);
		    manager.release_component_async(id, curl.toString(), cbo._this(), desc);
		}
		catch (NoPermissionEx nop)
		{
			throw new AcsJNoPermissionEx(nop);
		}
	}

}

