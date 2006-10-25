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
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Vector;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;

import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ComponentSpec;

import alma.ACS.OffShoot;
import alma.ACS.OffShootHelper;
import alma.ACS.OffShootOperations;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
import alma.acs.component.dynwrapper.DynamicProxyFactory;
import alma.acs.container.archive.Range;
import alma.acs.container.archive.UIDLibrary;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.ClientLogManager;
import alma.entities.commonentity.EntityT;
import alma.maciErrType.wrappers.AcsJNoPermissionEx;
import alma.maciErrType.wrappers.AcsJmaciErrTypeEx;
import alma.xmlstore.Identifier;
import alma.xmlstore.IdentifierHelper;
import alma.xmlstore.IdentifierJ;
import alma.xmlstore.IdentifierOperations;

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

    // identifier archive and UID lib will created lazily
	private volatile UIDLibrary uidLibrary;
	private volatile IdentifierJ identifierArchive;
	/** cheat property that allows testing without identifier archive present, because UIDs will be faked */
	public static final String PROPERTYNAME_FAKE_UID_FOR_TESTING = "acs.container.fakeUIDsForTesting";
	private final boolean fakeUIDsForTesting = Boolean.getBoolean(PROPERTYNAME_FAKE_UID_FOR_TESTING);
	
	protected final AcsManagerProxy m_acsManagerProxy;

    // logger used by this class
	protected final Logger m_logger;
    
    // logger given to component
    private volatile Logger componentLogger;

	// sync'd map, key=curl, value=corbaStub
	private final Map<String, org.omg.CORBA.Object> m_usedComponentsMap;
	private final Map<String, org.omg.CORBA.Object> m_usedNonStickyComponentsMap;

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
     * Optional callback object for component available/unavailable notification
     */
    private ComponentListener compListener;

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
		m_usedNonStickyComponentsMap = Collections.synchronizedMap(new HashMap<String, org.omg.CORBA.Object>());
		
		m_componentDescriptorMap = Collections.synchronizedMap(new HashMap<String, ComponentDescriptor>());
        
        m_threadFactory = threadFactory;        
        
        if (fakeUIDsForTesting) {
        	m_logger.warning("Running in test mode where UIDs will be constructed randomly instead of being retrieved from the archive!");
        }
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

	
	public void registerComponentListener(ComponentListener listener) {
		compListener = listener;
	}
	
	/**
	 */
	void fireComponentsAvailable (List<ComponentDescriptor> compDescs) {
		// find out which components are interesting for the client
		List<ComponentDescriptor> interesting = new Vector<ComponentDescriptor>();
		for (ComponentDescriptor cd : compDescs) {
			if (m_usedComponentsMap.containsKey(cd.getName()) || m_usedNonStickyComponentsMap.containsKey(cd.getName())) {
				interesting.add(cd);
			}
		}
		
     	if (interesting.size() > 0 && compListener != null) {
     		try {
				compListener.componentsAvailable(interesting);
			} catch (Throwable thr) {
				m_logger.log(Level.INFO, "componentsAvailable implementation of client " + m_clientName + " failed", thr);
			}
     	}
	}

	/**
	 */
	void fireComponentsUnavailable (List<String> compNames) {
		// find out which components are interesting for the client
		List<String> interesting = new Vector<String>();
		for (String cn : compNames) {
			if (m_usedComponentsMap.containsKey(cn) || m_usedNonStickyComponentsMap.containsKey(cn) ) {
				interesting.add(cn);
			}
		}
		
     	if (interesting.size() > 0 && compListener != null) {
     		try {
				compListener.componentsUnavailable(interesting);
			} catch (Throwable thr) {
				m_logger.log(Level.INFO, "componentsUnavailable implementation of client " + m_clientName + " failed", thr);
			}
     	}
	}
	

	
	/**
	 * @see alma.acs.container.ContainerServices#assignUniqueEntityId(EntityT)
	 */
	public void assignUniqueEntityId(EntityT entity) throws AcsJContainerServicesEx
	{
		if (entity == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("entity"); 
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}

		if (fakeUIDsForTesting) {
			long localId = (new Random(System.currentTimeMillis())).nextLong();
			String uid = Range.generateUID("testArchiveId", "testRangeId", localId);
			entity.setEntityId(uid);
			return;
		}
		
		try {
			if (identifierArchive == null) {
				Identifier identRaw = IdentifierHelper.narrow(getDefaultComponent("IDL:alma/xmlstore/Identifier:1.0"));
				identifierArchive = getTransparentXmlComponent(IdentifierJ.class, identRaw, IdentifierOperations.class);
			}
			if (uidLibrary == null) {
				uidLibrary = new UIDLibrary(m_logger);
			}
			uidLibrary.assignUniqueEntityId(entity, identifierArchive);
		}
		catch (Throwable thr) {
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo("failed to assign a UID to entity of type " + entity.getEntityTypeName());
			throw ex;
		}
	}


	/**
	 * @see alma.acs.container.ContainerServices#findComponents(java.lang.String, java.lang.String)
	 */
	public String[] findComponents(String curlWildcard, String typeWildcard) 
		throws AcsJContainerServicesEx
	{
		if (curlWildcard == null) {
			curlWildcard = "*";
		}
		if (typeWildcard == null) {
			typeWildcard = "*";
		}

		String msgSpec = "curlWildcard='" + curlWildcard + "' and typeWildcard='" + typeWildcard + "'.";
		if (m_logger.isLoggable(Level.FINER)) {
			m_logger.finer("about to call Manager#get_component_info with " + msgSpec);
		}
		
		ComponentInfo[] components = null;
		try {
			components = m_acsManagerProxy.get_component_info(new int[0], curlWildcard, typeWildcard, false );
		} 
		catch (AcsJNoPermissionEx ex) {
		    m_logger.log(Level.FINE, "No permission to find components with " + msgSpec, ex);  
		    AcsJContainerServicesEx ex2 = new AcsJContainerServicesEx(ex);
		    ex2.setContextInfo(msgSpec);
		    throw ex2;
		}
		catch (Throwable thr) {
			m_logger.log(Level.FINE, "Unexpected failure calling 'get_component_info' with " + msgSpec, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msgSpec);
			throw ex;
		}		
			
		ArrayList<String> curls = new ArrayList<String>();
		
		if (components != null) {
			for (int i = 0; i < components.length; i++) {
				curls.add(components[i].name);
			}
		}
		
		if (m_logger.isLoggable(Level.FINER)) {
			m_logger.finer("received " + curls.size() + " curls from get_component_info.");
		}
		
		return curls.toArray(new String[curls.size()]);
	}


	/**
	 * 
	 * @see alma.acs.container.ContainerServices#getComponentDescriptor(java.lang.String)
	 */
	public ComponentDescriptor getComponentDescriptor(String curl)
		throws AcsJContainerServicesEx
	{
		if (curl == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("curl"); 
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}

		ComponentDescriptor desc = m_componentDescriptorMap.get(curl);
		
		if (desc == null) {
			// try to get it from the manager
			ComponentInfo[] compInfos;
			try {
				compInfos = m_acsManagerProxy.get_component_info(new int[0], curl, "*", false);
			} catch (Throwable thr) { 
				m_logger.log(Level.FINE, "Unexpected failure calling 'get_component_info'.", thr);
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
				ex.setContextInfo("CURL=" + curl);
				throw ex;
			}
			if (compInfos.length == 1) {
				desc = new ComponentDescriptor(compInfos[0]);
				m_componentDescriptorMap.put(curl, desc);
			}
			else {
				String msg = "failed to retrieve a unique component descriptor for the component instance "  + curl;
				m_logger.fine(msg);
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
				ex.setContextInfo(msg);
				throw new AcsJContainerServicesEx();
			}
		}
		return desc;
	}


	/**
	 * @see alma.acs.container.ContainerServices#getComponent(String)
	 */	
	public org.omg.CORBA.Object getComponent(String curl) throws AcsJContainerServicesEx
	{
		if (curl == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("curl"); 
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
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
		
			/// @todo: think about timeouts
			
			try {
			    stub = m_acsManagerProxy.get_component(m_clientHandle, curl, true);
			    m_logger.fine("component " + curl + " retrieved successfully.");
			    m_usedComponentsMap.put(curl, stub);
			} catch (AcsJmaciErrTypeEx ex) {				
			    String msg = "Failed to retrieve component " + curl;
			    m_logger.log(Level.FINE, msg, ex); // only a low-level log because the client component is supposed to log the exception which contains all context data 
			    throw new AcsJContainerServicesEx(ex);
			} catch (Throwable thr) {
			    String msg = "Failed to retrieve component " + curl + " for unexpected reasons.";
			    m_logger.log(Level.FINE, msg, thr);  
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
				ex.setContextInfo(msg);
				throw ex;
			}			
		}
		return stub;
	}
	
	
	public org.omg.CORBA.Object getComponentNonSticky(String curl) 
		throws AcsJContainerServicesEx
	{
		if (curl == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("curl"); 
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}

		org.omg.CORBA.Object stub = null;
		try {
			stub = m_acsManagerProxy.get_component_non_sticky(m_clientHandle, curl);
		    m_logger.fine("Non-sticky reference to component '" + curl + "' retrieved successfully.");
		    m_usedNonStickyComponentsMap.put(curl, stub);
		} catch (AcsJmaciErrTypeEx ex) {				
		    String msg = "Failed to retrieve non-sticky reference to component " + curl;
		    m_logger.log(Level.FINE, msg, ex); // only a low-level log because the client component is supposed to log the exception which contains all context data 
		    throw new AcsJContainerServicesEx(ex);
		} catch (Throwable thr) {
		    String msg = "Failed to retrieve non-sticky reference to component '" + curl + "' for unexpected reasons.";
		    m_logger.log(Level.FINE, msg, thr);  
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}	
		return stub;
	}

	/**
	 * @see alma.acs.container.ContainerServices#getDefaultComponent(java.lang.String)
	 */
	public org.omg.CORBA.Object getDefaultComponent(String componentIDLType)
		throws AcsJContainerServicesEx
	{
		if (componentIDLType == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("componentIDLType"); 
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}

		ComponentInfo cInfo = null;
		try
		{
			// the call
			cInfo = m_acsManagerProxy.get_default_component(m_clientHandle, componentIDLType);
		}
		catch (AcsJmaciErrTypeEx ex) {
			String msg = "failed to retrieve default component for type " + componentIDLType;
			m_logger.log(Level.FINE, msg, ex); // higher-level log should be produced by the calling client from the exception later
			throw new AcsJContainerServicesEx(ex);
		}
		catch (Throwable thr) {
			String msg = "failed to retrieve default component for type " + componentIDLType + " for unexpected reasons!";
			m_logger.log(Level.FINE, msg, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}

		// cInfo.reference == null should no longer happen since the maci exception changes for ACS 6.0
		// @todo check and remove this
		if (cInfo.reference == null) {
			String msg = "Default component for type '" + componentIDLType + "' could not be accessed. ";
			m_logger.info(msg);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
			ex.setContextInfo(msg);
			throw ex;
		}
		
		m_usedComponentsMap.put(cInfo.name, cInfo.reference);
		m_componentDescriptorMap.put(cInfo.name, new ComponentDescriptor(cInfo));

		return cInfo.reference;
	}


	public org.omg.CORBA.Object getCollocatedComponent(String compUrl, String targetCompUrl) throws AcsJContainerServicesEx {
		
		if (compUrl == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("compUrl"); 
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}
		if (targetCompUrl == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("targetCompUrl");
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}

		ComponentQueryDescriptor cqd = new ComponentQueryDescriptor(compUrl, null);
		ComponentSpec cspec = cqd.toComponentSpec();
		ComponentInfo cInfo = null;
		
		try {
			// the call
			cInfo = m_acsManagerProxy.get_collocated_component(m_clientHandle, cspec, false, targetCompUrl);
			
		} catch (AcsJmaciErrTypeEx ex) {				
			String msg = "Failed to retrieve component '" + compUrl + "' created such that it runs collocated with '"+ targetCompUrl + "'.";
			m_logger.log(Level.FINE, msg, ex); // it's serious, but the caller is supposed to log this. Container only logs just in case.
		    throw new AcsJContainerServicesEx(ex);
		}
		catch (Throwable thr) {
			String msg = "Unexpectedly failed to retrieve component '" + compUrl + "' created such that it runs collocated with '"+ targetCompUrl + "'.";
			m_logger.log(Level.FINE, msg, thr); 
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}

		// cInfo.reference == null should no longer happen since the maci exception changes for ACS 6.0
		// @todo check and remove this
		if (cInfo.reference == null) {
			String msg = "Failed to retrieve component '" + compUrl + "' created such that it runs collocated with '"+ targetCompUrl + "'.";
			m_logger.info(msg);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
			ex.setContextInfo(msg);
			throw ex;
		}
		
		m_usedComponentsMap.put(cInfo.name, cInfo.reference);
		m_componentDescriptorMap.put(cInfo.name, new ComponentDescriptor(cInfo));

		return cInfo.reference;
	}
	
	
	/**
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(si.ijs.maci.ComponentSpec, boolean)
	 */
	public org.omg.CORBA.Object getDynamicComponent(ComponentQueryDescriptor compDesc, boolean markAsDefault)
		throws AcsJContainerServicesEx
	{
	    return getDynamicComponent(compDesc.toComponentSpec(), markAsDefault);
	}

	
	/**
	 * @see alma.acs.container.ContainerServices#getDynamicComponent(si.ijs.maci.ComponentSpec, boolean)
	 */
	public org.omg.CORBA.Object getDynamicComponent(ComponentSpec compSpec, boolean markAsDefault)
		throws AcsJContainerServicesEx
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
		} catch (AcsJmaciErrTypeEx ex) {
			m_logger.log(Level.FINE, "Failed to create dynamic component", ex);
			throw new AcsJContainerServicesEx(ex);
		}
		catch (Throwable thr) {
			String msg = "Unexpectedly failed to create dynamic component for unexpected reasons!";
			m_logger.log(Level.FINE, msg, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}
		
		return cInfo.reference;
	}




	/**
	 * @see alma.acs.container.ContainerServices#getCDB()
	 */
	public DAL getCDB() throws AcsJContainerServicesEx
	{
		DAL dal = null;

        String errMsg = "Failed to get the reference to the CDB component/service.";
        try
        {
            // manager's get_service contains get_component, so even if the CDB becomes a real component, we can leave this 
            org.omg.CORBA.Object dalObj = m_acsManagerProxy.get_service("CDB", true);
            dal = DALHelper.narrow(dalObj);
		} catch (AcsJmaciErrTypeEx ex) {
			m_logger.log(Level.FINE, errMsg, ex);
			throw new AcsJContainerServicesEx(ex);
		}
        catch (Throwable thr) {
			String msg = "Unexpectedly failed to get the CDB reference!";
			m_logger.log(Level.FINE, msg, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
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
		
		if (curl == null) {
			m_logger.info("Invalid curl 'null', nothing to release.");
			return;
		}
		
		if (!m_usedComponentsMap.containsKey(curl)) 
		{
			if (m_usedNonStickyComponentsMap.containsKey(curl)) {
				m_logger.info("ignoring request by client '" + m_clientName + 
						"' to release component '" + curl + "' because the reference is non-sticky and does not need to be released.");				
			}
			else {
				m_logger.info("ignoring request by client '" + m_clientName + 
									"' to release other component with unknown curl='" + curl + "'.");
			}
		}
		else
		{
			org.omg.CORBA.Object stub = m_usedComponentsMap.get(curl);
			
			m_logger.fine("about to release component " + curl + (forcefully ? " forcefully" : ""));
			try {
				if (forcefully) {
					m_acsManagerProxy.force_release_component(m_clientHandle, curl);
				}
				else {
					m_acsManagerProxy.release_component(m_clientHandle, curl);
				}
				m_logger.info("client '" + m_clientName + "' has successfully released " +  " a component with curl=" + curl);
				stub._release();
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
		throws AcsJContainerServicesEx
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
				java.lang.Object offshootImpl = implGetter.invoke(servant, (java.lang.Object[]) null);
				// now we insert the interceptor between the tie skeleton and the impl.
				// Offshoots have no name, so we construct one from the component name and the offshoot interface name
				// 
				String qualOffshootName = getName() + "/" + operationsIF.getName().substring(0, operationsIF.getName().length() - "Operations".length());
				java.lang.Object interceptingOffshootImpl = ContainerSealant.createContainerSealant(
						operationsIF, offshootImpl, qualOffshootName, true, m_logger, 
						Thread.currentThread().getContextClassLoader(), methodsExcludedFromInvocationLogging);
				Method implSetter = servant.getClass().getMethod("_delegate", new Class[]{operationsIF});
				implSetter.invoke(servant, new java.lang.Object[]{interceptingOffshootImpl});
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
		try  {
			org.omg.CORBA.Object obj = acsCorba.activateOffShoot(servant, m_clientPOA);
			shoot = OffShootHelper.narrow(obj);
		}
		catch (Throwable thr) {
			String msg = "failed to activate offshoot object of type '" + servant.getClass().getName() +
							"' for client '" + m_clientName + "'. ";
			// flatten the exception chain by one level if possible
			if (thr instanceof AcsJContainerServicesEx && thr.getCause() != null) {
				msg += "(" + thr.getMessage() + ")"; 
				thr = thr.getCause();
			}
			m_logger.log(Level.FINE, msg, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			throw ex;
		}	
//		m_logger.fine("successfully activated offshoot of type " + cbServant.getClass().getName());
		return shoot;
	}



	public void deactivateOffShoot(Servant cbServant)
	throws AcsJContainerServicesEx
	{
		checkOffShootServant(cbServant);
		try {
			acsCorba.deactivateOffShoot(cbServant, m_clientPOA);
		} catch (AcsJContainerEx e) {
			throw new AcsJContainerServicesEx(e);
		}
	}

	/**
	 * @param cbServant
	 * @throws ContainerException
	 */
	private void checkOffShootServant(Servant servant) throws AcsJContainerServicesEx {
		if (servant == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("servant");
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}		
		
		if (!(servant instanceof OffShootOperations)) {
			String msg = "invalid offshoot servant provided. Must implement " + OffShootOperations.class.getName();
			m_logger.fine(msg);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
			ex.setContextInfo(msg);
			throw ex;
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
    public <T> T getTransparentXmlComponent(Class<T> transparentXmlIF, org.omg.CORBA.Object componentReference, Class flatXmlIF)
    	throws AcsJContainerServicesEx
    {
    	if (m_logger.isLoggable(Level.FINEST)) {
	        m_logger.finest("creating xml binding class aware wrapper around component " + 
	                "implementing " + flatXmlIF.getName() + "...");
    	}
    	
        T wrapper = null;
        try
        {
            wrapper = DynamicProxyFactory.getDynamicProxyFactory(m_logger).createClientProxy(
                    transparentXmlIF,
                    componentReference,
                    flatXmlIF);
        }
        catch (Throwable thr)
        {
        	String msg = "failed to create XML binding class wrapper for component implementing " + flatXmlIF.getName();
        	m_logger.log(Level.FINE, msg, thr);
        	AcsJContainerServicesEx ex2 = new AcsJContainerServicesEx(thr);
        	ex2.setContextInfo(msg);
        	throw ex2;
        }
        return wrapper;
    }
    

	/////////////////////////////////////////////////////////////
	// other
	/////////////////////////////////////////////////////////////

	public void releaseAllComponents()
	{
		// copy curls first to avoid deleting from m_usedComponentsMap
		// while iterating over it (fail-fast iterator throws ConcurrentModificationException).
		// synchronized just in case...
		List<String> curls = new ArrayList<String>();
		synchronized (m_usedComponentsMap) {
			curls.addAll(m_usedComponentsMap.keySet());
		}
		
		for (String curl : curls ) {
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

