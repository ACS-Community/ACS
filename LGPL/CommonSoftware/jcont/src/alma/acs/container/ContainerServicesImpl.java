/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.container;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.IntHolder;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;

import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ComponentSpec;
import si.ijs.maci.ComponentSpecIncompatibleWithActiveComponent;
import si.ijs.maci.IncompleteComponentSpec;
import si.ijs.maci.InvalidComponentSpec;
import si.ijs.maci.ManagerOperations;
import si.ijs.maci.NoDefaultComponent;

import alma.ACS.OffShoot;
import alma.ACS.OffShootHelper;
import alma.ACS.OffShootOperations;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
import alma.acs.component.dynwrapper.DynWrapperException;
import alma.acs.component.dynwrapper.DynamicProxyFactory;
import alma.acs.container.archive.ArchiveProxy;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.ClientLogManager;
import alma.entities.commonentity.EntityT;

/**
 * Implementation of the <code>ContainerServices</code> interface.
 * To be used by ACS components, as well as any other clients that need access
 * to components.
 * <p>
 * This class is "cheap" to instantiate because many resources it uses are singletons
 * and or objects otherwise shared among instances.
 * It should thus be ok to create one instance per component or other client.
 * <p>
 * This class has to be thread-safe, because a component's functional methods can be called from
 * different ORB threads, or because the component could itself create threads, each of them
 * accessing this object.
 *  
 * @author hsommer Apr 1, 2003 2:28:01 PM
 */
public class ContainerServicesImpl implements ContainerServices
{
    private AdvancedContainerServicesImpl advancedContainerServices;
    
	private volatile ArchiveProxy m_archiveProxy;

	protected final AcsManagerProxy m_acsManagerProxy;

    // logger used by this class
	protected final Logger m_logger;
    
    // logger given to component
    private volatile Logger componentLogger;

	// sync'd map, key=curl, value=corbaStub
	private final Map<String, org.omg.CORBA.Object> m_usedComponentsMap;

	// sync'd map, key=curl, value=ComponentDescriptor
	private final Map<String, ComponentDescriptor> m_componentDescriptorMap;
	
	// the handle that the manager has assigned to the component to whom this ContainerServices object belongs
	private final int m_clientHandle;
    
    // the component name. "Client" refers to the component acting as a client to the manager 
	private final String m_clientName;

	private final AcsCorba acsCorba; 
	
	
	private final POA m_clientPOA;

	private final ComponentStateManager m_componentStateManager;
    private final ThreadFactory m_threadFactory;

    private volatile String[] methodsExcludedFromInvocationLogging;


	/**
	 * ctor.
	 * @param acsManagerProxy 
	 * @param componentPOA the POA for the component. Can be the root POA or some other specialized POA.
	 * @param acsCorba  Encapsulates the ORB and all POAs
	 * @param logger  logger to be used by this class
	 * @param clientHandle  handle to be used for identification when sending requests to the manager.
	 * 						For components, this should be the component handle assigned by the manager;
	 *                      for other clients, it can be the general handle assigned to the client at login.
	 * @param clientCurl
	 * @param componentStateManager  can be null if this class is instantiated 
	 * 									for a component client outside of a container
     * @param threadFactory to be used for <code>getThreadFactory</code>
	 */
	public ContainerServicesImpl(AcsManagerProxy acsManagerProxy, POA componentPOA, AcsCorba acsCorba,
									Logger logger, int clientHandle, String clientCurl, 
									ComponentStateManager componentStateManager,
                                    ThreadFactory threadFactory)
	{
		// The following fields are final. This guarantees that they will be copied to main thread memory,
		// and thus be seen by other threads after this ctor has terminated.
		m_acsManagerProxy = acsManagerProxy;
		m_clientPOA = componentPOA;
		this.acsCorba = acsCorba;
		m_logger = logger;
		m_clientHandle = clientHandle;
		m_clientName = clientCurl;
		
		m_componentStateManager = componentStateManager;
		 
		// should do for thread-safety as long as we don't iterate over it
		m_usedComponentsMap = Collections.synchronizedMap(new HashMap<String, org.omg.CORBA.Object>());
		m_componentDescriptorMap = Collections.synchronizedMap(new HashMap<String, ComponentDescriptor>());
        
        m_threadFactory = threadFactory;        
	}



	/////////////////////////////////////////////////////////////
	// Implementation of ContainerServices
	/////////////////////////////////////////////////////////////

	/**
	 * Gets the component name (which the component does not know statically)
	 * @see alma.acs.container.ContainerServices#getName()
	 */
	public String getName() {
		return m_clientName;
	}

    
	/**
	 * {@inheritDoc}
	 * 
	 * This method should only be called by a component that lives inside a container;
	 * a component client that is not a component itself should not call it,
	 * would result in a NPE!
	 * 
	 * @see alma.acs.container.ContainerServices#getComponentStateManager()
	 */
	public ComponentStateManager getComponentStateManager()
	{
		if (m_componentStateManager == null)
		{
			// to make debugging easier if this ever happened on the container side
			throw new NullPointerException("ComponentStateManager is null!");
		}
		
		return m_componentStateManager;
	}


	/**
	 * The component must retrieve its logger object from this interface
	 * (as opposed to using the <code>ClientLogManager</code> singleton) 
	 * so that the container is free
	 * to give away loggers that are somehow taylored to the particular component.
	 * <p>
	 * The goal is to have "componentName" and other fields in all ALMA log entries,
	 * and have tool support for filtering logs by component, subsystem, user, ...
	 * 
	 * @see alma.acs.container.ContainerServices#getLogger()
	 */
	public Logger getLogger()
	{
        if (componentLogger == null) {
    		componentLogger = ClientLogManager.getAcsLogManager().getLoggerForComponent(m_clientName);
        }
        return componentLogger;
	}


	/**
	 * @see alma.acs.container.ContainerServices#assignUniqueEntityId(EntityT)
	 */
	public void assignUniqueEntityId(EntityT entity) throws ContainerException
	{
		if (entity == null)
		{
			throw new ContainerException("The XML entity's child of type EntityT must not be null.");
		}
		
		String newId = null;
		try
		{ 	
			if (m_archiveProxy == null)
			{
				// must be lazy inst., can't do it in ctor 
				m_archiveProxy = ArchiveProxy.getArchiveProxy(this, m_logger);
			}
		
			newId = m_archiveProxy.nextUniqueId();
		}
		catch (Exception ex)
		{
			throw new ContainerException("failed to create id.", ex);
		}

		entity.setEntityId(newId);
		
		entity.setEntityIdEncrypted("-- id encryption not yet implemented --");
		
	}


	/**
	 * @see alma.acs.container.ContainerServices#findComponents(java.lang.String, java.lang.String)
	 */
	public String[] findComponents(String curlWildcard, String typeWildcard)  throws ContainerException
	{
		if (curlWildcard == null)
		{
			curlWildcard = "*";
		}
		if (typeWildcard == null)
		{
			typeWildcard = "*";
		}

		m_logger.finer("about to call Manager#get_component_info with curlWildcard='" +
		curlWildcard + "' and typeWildcard='" + typeWildcard + "'.");
		
		ComponentInfo[] components = m_acsManagerProxy.get_component_info(
			new int[0], curlWildcard, typeWildcard, false );
			
		ArrayList<String> curls = new ArrayList<String>();
		
		if (components != null)
		{
			for (int i = 0; i < components.length; i++)
			{
				curls.add(components[i].name);
			}
		}
		
		m_logger.finer("received " + curls.size() + " curls from get_component_info.");
		
		return curls.toArray(new String[curls.size()]);
	}


	/**
	 * 
	 * @see alma.acs.container.ContainerServices#getComponentDescriptor(java.lang.String)
	 */
	public ComponentDescriptor getComponentDescriptor(String componentUrl)
		throws ContainerException
	{
		ComponentDescriptor desc = m_componentDescriptorMap.get(componentUrl);
		
		if (desc == null)
		{
			// try to get it from the manager
			ComponentInfo[] compInfos = m_acsManagerProxy.get_component_info(new int[0], componentUrl, "*", false);
			if (compInfos != null && compInfos.length == 1)
			{
				desc = new ComponentDescriptor(compInfos[0]);
				m_componentDescriptorMap.put(componentUrl, desc);
			}
			else
			{
				String msg = "failed to retrieve the component descriptor for the component instance " 
								+ componentUrl;
				m_logger.warning(msg);
				throw new ContainerException(msg);
			}
		}
		return desc;
	}


	/**
	 * @see alma.acs.container.ContainerServices#getComponent(String)
	 */	
	public org.omg.CORBA.Object getComponent(String curl) throws ContainerException
	{
		if (curl == null) {
			throw new ContainerException("component name (curl) must not be null!");
		}
		
		// check if our component has requested the other component before
		org.omg.CORBA.Object stub = m_usedComponentsMap.get(curl);
		
		if (stub != null)
		{
			// reusing seems ok as long as there is one separate 
			// instance of ContainerServicesImpl for each component.
			// This reuse does not cut off the manager from component access,
			// since it only happens at second or further access attempts;
			// the first time, the component reference is obtained through the manager
			// (independently of whether the components are collocated inside the same container),
			// so that the manager is aware of component dependencies.    
			m_logger.info("client '" + m_clientName + "' attempts to retrieve component '" + 
									curl + "' more than once; will return existing reference.");
		}
		else
		{
			m_logger.fine("will retrieve remote component '" + curl + 
							"' using ACS Manager#get_component with client handle " + m_clientHandle);
		
			IntHolder status = new IntHolder();
			
			// todo: think about timeouts
			stub = m_acsManagerProxy.get_component(m_clientHandle, curl, true, status);
		
			if (status.value == ManagerOperations.COMPONENT_ACTIVATED)
			{
				m_logger.fine("component " + curl + " retrieved successfully.");
			}
			else if (status.value == ManagerOperations.COMPONENT_NONEXISTENT)
			{
				String msg = "component " + curl + " does not exist.";
				m_logger.severe(msg);
				throw new ContainerException(msg);
			}
			else if (status.value == ManagerOperations.COMPONENT_NOT_ACTIVATED)
			{
				String msg = "component " + curl + " could not be activated.";
				m_logger.severe(msg);
				throw new ContainerException(msg);
			}
		
			m_usedComponentsMap.put(curl, stub);
		}
		return stub;
	}
	

	/**
	 * @see alma.acs.container.ContainerServices#getDefaultComponent(java.lang.String)
	 */
	public org.omg.CORBA.Object getDefaultComponent(String componentIDLType)
		throws ContainerException
	{
		ComponentInfo cInfo = null;
		try
		{
			// the call
			cInfo = m_acsManagerProxy.get_default_component(m_clientHandle, componentIDLType);
		}
		catch (NoDefaultComponent e) {
			String msg = "failed to retrieve default component for type " + componentIDLType + 
						" because no default component has been defined.";
			m_logger.info(msg);
			throw new ContainerException(msg, e);
		}
		catch (Throwable thr) {
			String msg = "failed to retrieve default component for type " + componentIDLType +
							" for unexpected reasons!";
			m_logger.log(Level.WARNING, msg, thr);
			throw new ContainerException(msg, thr);
		}

		if (cInfo.reference == null) {
			String msg = "Default component for type '" + componentIDLType + "' could not be accessed. ";
			m_logger.info(msg);
			throw new ContainerException(msg);
		}
		
		m_usedComponentsMap.put(cInfo.name, cInfo.reference);
		m_componentDescriptorMap.put(cInfo.name, new ComponentDescriptor(cInfo));

		return cInfo.reference;
	}


	public org.omg.CORBA.Object getCollocatedComponent(String compUrl, String targetCompUrl) throws ContainerException {
		ComponentQueryDescriptor cqd = new ComponentQueryDescriptor(compUrl, null);		
		ComponentSpec cspec = cqd.toComponentSpec();
		ComponentInfo cInfo = null;
		
		try {
			// the call
			cInfo = m_acsManagerProxy.get_collocated_component(m_clientHandle, cspec, false, targetCompUrl);
		}
		catch (Throwable thr) {
			String msg = "Failed to retrieve component '" + compUrl + "' created such that it runs collocated with '"+ targetCompUrl + "'.";
			m_logger.log(Level.FINE, msg, thr); // it's serious, but the caller is supposed to log this. Container only logs just in case.
			throw new ContainerException(msg, thr);
		}

		if (cInfo.reference == null) {
			String msg = "Failed to retrieve component '" + compUrl + "' created such that it runs collocated with '"+ targetCompUrl + "'.";
			m_logger.log(Level.FINE, msg); // it's serious, but the caller is supposed to log this. Container only logs just in case.
			throw new ContainerException(msg);
		}
		
		m_usedComponentsMap.put(cInfo.name, cInfo.reference);
		m_componentDescriptorMap.put(cInfo.name, new ComponentDescriptor(cInfo));

		return cInfo.reference;
	}
	
	
	/**
	 * {@inheritDoc}
	 * 
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(si.ijs.maci.ComponentSpec, boolean)
	 */
	public org.omg.CORBA.Object getDynamicComponent(
		ComponentQueryDescriptor compDesc,
		boolean markAsDefault)
		throws ContainerException
	{
	    /*
		String entryMsg = "getDynamicComponent called with " +
		compDesc.toString() + 
		", markAsDefault=" + markAsDefault;
		m_logger.fine(entryMsg);
		
		ComponentInfo cInfo = null;
		try
		{
			// the call
			cInfo = m_acsManagerProxy.get_dynamic_component(compDesc.toComponentSpec(), markAsDefault);
			
			m_usedComponentsMap.put(cInfo.name, cInfo.reference);
			m_componentDescriptorMap.put(cInfo.name, new ComponentDescriptor(cInfo));
		}
		catch (IncompleteComponentSpec e)
		{
			String msg = "failed to create dynamic component. component spec is incomplete.";
			m_logger.warning(msg);
			throw new ContainerException(msg, e);
		}
		catch (InvalidComponentSpec e)
		{
			String msg = "failed to create dynamic component. component spec is invalid.";
			m_logger.warning(msg);
			throw new ContainerException(msg, e);
		}
		catch (ComponentSpecIncompatibleWithActiveComponent e)
		{
			String msg = "failed to create the dynamic component; " + 
						"the component spec uniquely describes an existing component.";
			m_logger.warning(msg);
			throw new ContainerException(msg, e);
		}
		catch (Throwable thr)
		{
			String msg = "failed to create dynamic component for unexpected reasons!";
			m_logger.log(Level.WARNING, msg, thr);
			throw new ContainerException(msg, thr);
		}
		
		return cInfo.reference;
	    */
	    return getDynamicComponent(compDesc.toComponentSpec(), markAsDefault);
	}

	
    	/**
	 * {@inheritDoc}
	 * 
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(si.ijs.maci.ComponentSpec, boolean)
	 */
	public org.omg.CORBA.Object getDynamicComponent(
		ComponentSpec compSpec,
		boolean markAsDefault)
		throws ContainerException
	{
		String entryMsg = "getDynamicComponent called with" + 
		" compName=" + compSpec.component_name +
		" compType=" + compSpec.component_type +
		" compCode=" + compSpec.component_code +
		" compContainer=" + compSpec.container_name +
		" markAsDefault=" + markAsDefault;
		m_logger.fine(entryMsg);
		
		ComponentInfo cInfo = null;
		try
		{
			// the call
			cInfo = m_acsManagerProxy.get_dynamic_component(m_clientHandle, compSpec, markAsDefault);
			
			m_usedComponentsMap.put(cInfo.name, cInfo.reference);
			m_componentDescriptorMap.put(cInfo.name, new ComponentDescriptor(cInfo));
		}
		catch (IncompleteComponentSpec e)
		{
			String msg = "failed to create dynamic component. component spec is incomplete.";
			m_logger.warning(msg);
			throw new ContainerException(msg, e);
		}
		catch (InvalidComponentSpec e)
		{
			String msg = "failed to create dynamic component. component spec is invalid.";
			m_logger.warning(msg);
			throw new ContainerException(msg, e);
		}
		catch (ComponentSpecIncompatibleWithActiveComponent e)
		{
			String msg = "failed to create the dynamic component; " + 
						"the component spec uniquely describes an existing component.";
			m_logger.warning(msg);
			throw new ContainerException(msg, e);
		}
		catch (Throwable thr)
		{
			String msg = "failed to create dynamic component for unexpected reasons!";
			m_logger.log(Level.WARNING, msg, thr);
			throw new ContainerException(msg, thr);
		}
		
		return cInfo.reference;
	}




	/**
	 * @see alma.acs.container.ContainerServices#getCDB()
	 */
	public DAL getCDB() throws ContainerException
	{
		DAL dal = null;

        String errMsg = "Failed to get the reference to the CDB component/service.";
        IntHolder status = new IntHolder();
        try
        {
            // manager's get_service contains get_component, so even if the CDB becomes a real component, we can leave this 
            org.omg.CORBA.Object dalObj = m_acsManagerProxy.get_service("CDB", true, status);
            dal = DALHelper.narrow(dalObj);
        }
        catch (Exception e)
        {
            throw new ContainerException(errMsg, e);
        }
        if (status.value != ManagerOperations.COMPONENT_ACTIVATED)
        {
            throw new ContainerException(errMsg + " Bad status '" + status.value + "' returned.");
        }
        
		return dal;
	}


	/**
	 * Releases the specified component reference. This involves notification of the manager,
	 * as well as calling <code>_release()</code> on the CORBA stub.
	 * If the curl is not known to the container, the request will be ignored.
	 * <p>
	 * Note that <i>references</i> to other components are released by this method, 
	 * where the components hosted inside this container act as clients.
	 * These referenced components may run inside this or some other container/container.
	 * <p>
	 * TODO optionally run in a separate thread to gain speed, especially needed for quick shutdown; 
	 * consider race condition at the manager with new activation request for the same component though. 
	 * 
	 * @see alma.acs.container.ContainerServices#releaseComponent(java.lang.String)
	 */
	public void releaseComponent(String curl) {
		releaseComponent(curl, false);
	}

	/**
	 * This method was introduced to unify the implementation of #releaseComponent and AdvancedContainerServices#forceReleaseComponent.
	 * TODO: check if this should be taken back with ACS 6.0.
	 */
	void releaseComponent(String curl, boolean forcefully) {
		if (!m_usedComponentsMap.containsKey(curl)) 
		{
			m_logger.info("ignoring request by client '" + m_clientName + 
									"' to release other component with unknown curl='" + curl + "'.");
		}
		else
		{
			org.omg.CORBA.Object stub = m_usedComponentsMap.get(curl);
			
			m_logger.fine("about to release component " + curl + (forcefully ? " forcefully" : ""));
			try {
				stub._release();
				if (forcefully) {
					m_acsManagerProxy.force_release_component(m_clientHandle, curl);
				}
				else {
					m_acsManagerProxy.release_component(m_clientHandle, curl);
				}
				m_logger.info("client '" + m_clientName + "' has successfully released " + 
						" a component with curl=" + curl);
			}
			catch (Throwable thr) { // mainly thinking of org.omg.CORBA.NO_PERMISSION
				m_logger.log(Level.WARNING, "client '" + m_clientName + "' (handle " + m_clientHandle + ") failed to release " + 
						" with the manager the component with curl=" + curl, thr);
			}
			finally {
				m_usedComponentsMap.remove(curl);				
			}
		}
	}

	/**
	 * @see alma.acs.container.ContainerServices#activateOffShoot(org.omg.PortableServer.Servant)
	 */
	public OffShoot activateOffShoot(Servant servant)
		throws ContainerException
	{
		checkOffShootServant(servant);
		String servantName = servant.getClass().getName();
		// check if the servant is the Tie variant, which allows proxy-based call interception by the container
		boolean isTie = false;
		if (servantName.endsWith("POATie")) {
			try {
				// the _delegate getter method is mandated by the IDL-to-Java mapping spec
				Method implGetter = servant.getClass().getMethod("_delegate", (Class[]) null);
				isTie = true;
				Class operationsIF = implGetter.getReturnType();
				Object offshootImpl = implGetter.invoke(servant, (Object[]) null);
				// now we insert the interceptor between the tie skeleton and the impl.
				// Offshoots have no name, so we construct one from the component name and the offshoot interface name
				// 
				String qualOffshootName = getName() + "/" + operationsIF.getName().substring(0, operationsIF.getName().length() - "Operations".length());
				Object interceptingOffshootImpl = ContainerSealant.createContainerSealant(
						operationsIF, offshootImpl, qualOffshootName, true, m_logger, 
						Thread.currentThread().getContextClassLoader(), methodsExcludedFromInvocationLogging);
				Method implSetter = servant.getClass().getMethod("_delegate", new Class[]{operationsIF});
				implSetter.invoke(servant, new Object[]{interceptingOffshootImpl});
				m_logger.fine("created sealant for offshoot " + qualOffshootName);
			} catch (NoSuchMethodException e) {
				// so this was not a Tie skeleton, even though its name ends misleadingly with "POATie"
			} catch (Exception e) {
				m_logger.log(Level.WARNING, "Failed to create interceptor for offshoot " + servantName, e);
			} 
		}		

		if (!isTie) {
// TODO: perhaps require tie offshoots with ACS 5.0, and enable this warning log			
//			m_logger.warning("Offshoot servant '" + servantName + "' from component '" + getName() + 
//					"' does not follow the tie approach. Calls can thus not be intercepted by the container.");
		}
				
		OffShoot shoot = null;
		try 
		{
			org.omg.CORBA.Object obj = acsCorba.activateOffShoot(servant, m_clientPOA);
			shoot = OffShootHelper.narrow(obj);
		}
		catch (Throwable ex)
		{
			String msg = "failed to activate offshoot object of type '" + servant.getClass().getName() +
							"' for client '" + m_clientName + "'. ";
			// flatten the exception chain by one level if possible
			if (ex instanceof ContainerException && ex.getCause() != null) {
				msg += "(" + ex.getMessage() + ")"; 
				ex = ex.getCause();
			}
			m_logger.log(Level.WARNING, msg, ex);
			throw new ContainerException(msg, ex);
		}	
//		m_logger.fine("successfully activated offshoot of type " + cbServant.getClass().getName());
		return shoot;
	}



	public void deactivateOffShoot(Servant cbServant)
	throws ContainerException
	{
		checkOffShootServant(cbServant);
		acsCorba.deactivateOffShoot(cbServant, m_clientPOA);
	}

	/**
	 * @param cbServant
	 * @throws ContainerException
	 */
	private void checkOffShootServant(Servant cbServant) throws ContainerException {
		if (!(cbServant instanceof OffShootOperations))
		{
			String msg = "invalid offshoot servant provided. Must implement " + OffShootOperations.class.getName();
			m_logger.warning(msg);
			throw new ContainerException(msg);
		}
	}


    /**
     * @see alma.acs.container.ContainerServices#getAdvancedContainerServices()
     */
    public synchronized AdvancedContainerServices getAdvancedContainerServices() {
        if (advancedContainerServices == null) {
            advancedContainerServices = new AdvancedContainerServicesImpl(this, m_logger);
            // todo: once the legitimate cases of calling this method are settled, remove the log message.
            m_logger.info("component '" + getName() + "' requested AdvancedContainerServices");
        }
        return advancedContainerServices;
    }
    


	/**
	 * {@inheritDoc}.
	 * <p>
	 * todo: implement:
	 * ask AcsContainer if it knows componentReference, and if it has transpXml-IF;
	 * if so, get component impl directly;
	 * check if respective component helper allows direct calls to transpXmlIF
	 * (by not implementing _getInterfaceTranslator, or some explicit flag);
	 * move intercepting layer (ContainerSealant) so that it's still in between the components.
	 *   
	 * @see alma.acs.container.ContainerServices#getTransparentXmlComponent(java.lang.Class, org.omg.CORBA.Object, java.lang.Class)
	 */
    public Object getTransparentXmlComponent(
            Class transparentXmlIF,
            org.omg.CORBA.Object componentReference,
            Class flatXmlIF)
    throws ContainerException
    {
        m_logger.finer("creating xml binding class aware wrapper around component " + 
                "implementing " + flatXmlIF.getName() + "..."); 
        Object wrapper = null;
        try
        {
            wrapper = DynamicProxyFactory.getDynamicProxyFactory(m_logger).createClientProxy(
                    transparentXmlIF,
                    componentReference,
                    flatXmlIF);
        }
        catch (DynWrapperException e)
        {
            throw new ContainerException("failed to create XML binding class wrapper " +
                    "for component implementing " + flatXmlIF.getName(), e); 
        }
        return wrapper;
    }
    

	/////////////////////////////////////////////////////////////
	// other
	/////////////////////////////////////////////////////////////

	public void releaseAllComponents()
	{
		// copy curls first to avoid deleting from m_usedComponentsMap
		// while iterating over it (fail-fast iterator throws ConcurrentModificationException)
		List<String> curls = new ArrayList<String>(m_usedComponentsMap.keySet());
		
		for (Iterator<String> iter = curls.iterator(); iter.hasNext();)
		{
			String curl = iter.next();
			releaseComponent(curl);
		}
	}


    /**
     * @see alma.acs.container.ContainerServices#getThreadFactory()
     */
    public ThreadFactory getThreadFactory() {
        return m_threadFactory;
    }

    
    AcsCorba getAcsCorba() {
    	return acsCorba;
    }

	/**
	 * With this optional call, automatic invocation logging for certain offshoot methods can be disabled.  
	 * @param methodsExcludedFromInvocationLogging 
	 * @see ComponentHelper#getComponentMethodsExcludedFromInvocationLogging()
	 */
	void setMethodsExcludedFromInvocationLogging(String[] methodsExcludedFromInvocationLogging) {
		this.methodsExcludedFromInvocationLogging = methodsExcludedFromInvocationLogging;
	}


}

