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

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Vector;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;

import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;

import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ComponentSpec;

import alma.ACS.CBlong;
import alma.ACS.OffShoot;
import alma.ACS.OffShootHelper;
import alma.ACS.OffShootOperations;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.alarmsystem.source.AlarmSource;
import alma.acs.alarmsystem.source.AlarmSourceImpl;
import alma.acs.callbacks.RequesterUtil;
import alma.acs.callbacks.ResponseReceiver;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
import alma.acs.component.dynwrapper.ComponentInvocationHandler;
import alma.acs.component.dynwrapper.DynamicProxyFactory;
import alma.acs.container.archive.Range;
import alma.acs.container.archive.UIDLibrary;
import alma.acs.container.corba.AcsCorba;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;
import alma.entities.commonentity.EntityT;
import alma.maciErrType.wrappers.AcsJComponentDeactivationFailedEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationUncleanEx;
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

	/**
	 * Holds and re-establishes the connection to the manager, and encapsulates the handle given by the manager at login.
	 */
	protected final AcsManagerProxy m_acsManagerProxy;

    // logger used by this class
	protected final AcsLogger m_logger;
    
    // logger given to component
    private volatile AcsLogger componentLogger;

    // alarm source used by components when raising/clearing alarms
    private final AlarmSource m_alarmSource;

	// sync'd map, key=curl, value=corbaStub
	private final Map<String, org.omg.CORBA.Object> m_usedComponentsMap;
	private final Map<String, org.omg.CORBA.Object> m_usedNonStickyComponentsMap;

	// sync'd map, key=curl, value=ComponentDescriptor
	private final Map<String, ComponentDescriptor> m_componentDescriptorMap;
	
	/** 
	 * The handle that the manager has assigned to the component to whom this ContainerServices object belongs, 
	 * or 0 if this ContainerServices object does not belong to a component, 
	 * in which case m_acsManagerProxy's handle should be used.
	 */
	private final int m_componentHandle;

	/**
	 * Name of the component or other client (client app or container etc)
	 */
	private final String m_clientName;

	/**
	 * The externally provided instance of AcsCorba
	 */
	private final AcsCorba acsCorba; 
	
	
	private final POA m_clientPOA;
	private Object m_componentXmlTranslatorProxy;

	// sync'd map, key=offshot implementation object, value=servant
	// (they can be the same, but help to keep track of the servants
	// when activating offshoots of xyzJ type)
	private Map<Object, Servant> m_activatedOffshootsMap;

	private final ComponentStateManager m_componentStateManager;
    private final ThreadFactory m_threadFactory;

    private volatile String[] methodsExcludedFromInvocationLogging;

    /**
     * Optional callback object for component available/unavailable notification
     */
    private ComponentListener compListener;

	private final List<CleanUpCallback> cleanUpCallbacks;

	private final Map<String, AcsEventSubscriber> m_subscribers;
	private final Map<String, AcsEventPublisher<?>> m_publishers;
	
	/**
	 * Subscriber instance gets created using reflection, since module jcontnc must come after jcont for other reasons.
	 */
	private final String CLASSNAME_NC_SUBSCRIBER = "alma.acs.nc.refactored.NCSubscriber";
	
	/**
	 * Publisher instance gets created using reflection, since module jcontnc must come after jcont for other reasons.
	 */
	private final String CLASSNAME_NC_PUBLISHER  = "alma.acs.nc.refactored.NCPublisher";

	/**
	 * Instead of using "synchronized" lazy factory methods, there is this separate sync object 
	 * for lazy instantiations, to not conflict with possible other uses of this object's monitor.
	 */
	private final Object lazyCreationSync = new Object();

	/**
	 * The cached CDB reference.
	 */
	private volatile DAL cdb;
	
	
	/**
	 * ctor.
	 * @param acsManagerProxy 
	 * @param componentPOA the POA for the component. Can be the root POA or some other specialized POA.
	 * @param acsCorba  Encapsulates the ORB and all POAs
	 * @param logger  logger to be used by this class
	 * @param componentHandle  handle to be used for identification when sending requests to the manager.
	 *                      For components, this should be the component handle assigned by the manager;
	 *                      for other clients, it should be 0 to indicate that the handle obtained at manager login should be used.
	 * @param clientCurl
	 * @param componentStateManager  can be null if this class is instantiated 
	 * 									for a component client outside of a container
	 * @param threadFactory to be used for <code>getThreadFactory</code>
	 */
	public ContainerServicesImpl(AcsManagerProxy acsManagerProxy, POA componentPOA, AcsCorba acsCorba,
									AcsLogger logger, int componentHandle, String clientCurl, 
									ComponentStateManager componentStateManager,
									ThreadFactory threadFactory)
	{
		// The following fields are final. This guarantees that they will be copied to main thread memory,
		// and thus be seen by other threads after this ctor has terminated.
		m_acsManagerProxy = acsManagerProxy;
		m_clientPOA = componentPOA;
		this.acsCorba = acsCorba;
		m_logger = logger;
		m_componentHandle = componentHandle;
		m_clientName = clientCurl;
		
		m_componentStateManager = componentStateManager;
		 
		// should do for thread-safety as long as we don't iterate over it
		m_usedComponentsMap = Collections.synchronizedMap(new HashMap<String, org.omg.CORBA.Object>());
		m_usedNonStickyComponentsMap = Collections.synchronizedMap(new HashMap<String, org.omg.CORBA.Object>());
		
		m_componentDescriptorMap = Collections.synchronizedMap(new HashMap<String, ComponentDescriptor>());
		m_activatedOffshootsMap = Collections.synchronizedMap(new HashMap<Object, Servant>());

		m_subscribers = new HashMap<String, AcsEventSubscriber>();
		m_publishers  = new HashMap<String, AcsEventPublisher<?>>();

		m_threadFactory = threadFactory;
		
		cleanUpCallbacks = new ArrayList<CleanUpCallback>();
		
		if (fakeUIDsForTesting) {
			m_logger.warning("Running in test mode where UIDs will be constructed randomly instead of being retrieved from the archive!");
		}

		m_alarmSource = new AlarmSourceImpl(this);
		m_alarmSource.start();
	}

	void setComponentXmlTranslatorProxy(Object xmlTranslatorProxy) {
		m_componentXmlTranslatorProxy = xmlTranslatorProxy;
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
	 * to give away loggers that are somehow tailored to the particular component.
	 * <p>
	 * The goal is to have "componentName" and other fields in all ALMA log entries,
	 * and have tool support for filtering logs by component, subsystem, user, ...
	 * 
	 * @see alma.acs.container.ContainerServices#getLogger()
	 */
	public AcsLogger getLogger()
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
	public void fireComponentsAvailable (List<ComponentDescriptor> compDescs) {
		if (compListener == null) {
			return;
		}
		
		// find out which components are interesting for the client
		List<ComponentDescriptor> interesting = null;
		if (compListener.includeForeignComponents()) {
			interesting = compDescs;
		}
		else {
			interesting = new Vector<ComponentDescriptor>();
			for (ComponentDescriptor cd : compDescs) {
				if (m_usedComponentsMap.containsKey(cd.getName()) || m_usedNonStickyComponentsMap.containsKey(cd.getName())) {
					interesting.add(cd);
				}
			}
		}
		
     	if (interesting.size() > 0) {
     		try {
				compListener.componentsAvailable(interesting);
			} catch (Throwable thr) {
				m_logger.log(Level.INFO, "componentsAvailable implementation of client " + m_clientName + " failed", thr);
			}
     	}
	}

	/**
	 */
	public void fireComponentsUnavailable (List<String> compNames) {
		if (compListener == null) {
			return;
		}
		
		// find out which components are interesting for the client
		List<String> interesting = null;
		if (compListener.includeForeignComponents()) {
			interesting = compNames;
		}
		else {
			interesting = new Vector<String>();
			for (String cn : compNames) {
				if (m_usedComponentsMap.containsKey(cn) || m_usedNonStickyComponentsMap.containsKey(cn) ) {
					interesting.add(cn);
				}
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
	@Override
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
				identifierArchive = getTransparentXmlWrapper(IdentifierJ.class, identRaw, IdentifierOperations.class);
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
	@Override
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
							"' using ACS Manager#get_component with client handle " + getEffectiveClientHandle());
		
			/// @todo: think about timeouts
			
			try {
			    stub = m_acsManagerProxy.get_component(getEffectiveClientHandle(), curl, true);
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
			stub = m_acsManagerProxy.get_component_non_sticky(getEffectiveClientHandle(), curl);
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
			cInfo = m_acsManagerProxy.get_default_component(getEffectiveClientHandle(), componentIDLType);
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
		return getCollocatedComponent(cqd, false, targetCompUrl);
	}
	
	public org.omg.CORBA.Object getCollocatedComponent(ComponentQueryDescriptor spec, boolean markAsDefaul, String targetCompUrl) throws AcsJContainerServicesEx {
		if (spec == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("ComponentQueryDescriptor");
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}
		if (targetCompUrl == null) {
			AcsJBadParameterEx cause = new AcsJBadParameterEx();
			cause.setParameter("targetCompUrl");
			cause.setParameterValue("null");
			throw new AcsJContainerServicesEx(cause);
		}
		ComponentInfo cInfo = null;
		
		try {
			// the call
			cInfo = m_acsManagerProxy.get_collocated_component(getEffectiveClientHandle(), spec.toComponentSpec(), false, targetCompUrl);
			
		} catch (AcsJmaciErrTypeEx ex) {				
			String msg = "Failed to retrieve component '" + spec.getComponentName() + "' created such that it runs collocated with '"+ targetCompUrl + "'.";
			m_logger.log(Level.FINE, msg, ex); // it's serious, but the caller is supposed to log this. Container only logs just in case.
		    throw new AcsJContainerServicesEx(ex);
		}
		catch (Throwable thr) {
			String msg = "Unexpectedly failed to retrieve component '" + spec.getComponentName() + "' created such that it runs collocated with '"+ targetCompUrl + "'.";
			m_logger.log(Level.FINE, msg, thr); 
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}

		// cInfo.reference == null should no longer happen since the maci exception changes for ACS 6.0
		// @todo check and remove this
		if (cInfo.reference == null) {
			String msg = "Failed to retrieve component '" + spec.getComponentName() + "' created such that it runs collocated with '"+ targetCompUrl + "'.";
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
			cInfo = m_acsManagerProxy.get_dynamic_component(getEffectiveClientHandle(), compSpec, markAsDefault);
			
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



	public org.omg.CORBA.Object getReferenceWithCustomClientSideTimeout(org.omg.CORBA.Object originalCorbaRef, double timeoutSeconds) 
		throws AcsJContainerServicesEx {
		return acsCorba.wrapForRoundtripTimeout(originalCorbaRef, timeoutSeconds);
	}

	
	
	/**
	 * @see alma.acs.container.ContainerServices#getCDB()
	 */
	@Override
	public DAL getCDB() throws AcsJContainerServicesEx {
		synchronized (lazyCreationSync) {
			if (cdb == null) {
				String errMsg = "Failed to get the reference to the CDB component/service.";
				try {
					org.omg.CORBA.Object dalObj = m_acsManagerProxy.get_service("CDB", true);
					cdb = DALHelper.narrow(dalObj);
				} catch (AcsJmaciErrTypeEx ex) {
					m_logger.log(Level.FINE, errMsg, ex);
					throw new AcsJContainerServicesEx(ex);
				} catch (Throwable thr) {
					String msg = "Unexpectedly failed to get the CDB reference!";
					m_logger.log(Level.FINE, msg, thr);
					AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
					ex.setContextInfo(msg);
					throw ex;
				}
			}
		}
		return cdb;
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
	 * Since ACS 9.1 this method is implemented by delegating to 
	 * {@link #releaseComponent(String, alma.acs.container.ContainerServices.ComponentReleaseCallback)}, 
	 * keeping the old synchronous behavior by blocking on a callback object, with a timeout of 60 seconds.
	 * Since ACS 10.0, errors are logged at level DEBUG.
	 *  
	 * @see alma.acs.container.ContainerServices#releaseComponent(java.lang.String)
	 */
	public void releaseComponent(String curl) {
		ComponentReleaseCallback callback = new ComponentReleaseCallbackWithLogging(m_logger, AcsLogLevel.DEBUG);
		releaseComponent(curl, callback);
		try {
			callback.awaitComponentRelease(60, TimeUnit.SECONDS);
		} catch (InterruptedException ex) {
			m_logger.log(AcsLogLevel.DEBUG, "Interrupted while waiting for release of component " + curl);
		}
	}


	/**
	 * Used by {@link ContainerServicesImpl#releaseComponent(String, alma.acs.container.ContainerServices.ComponentReleaseCallback)}
	 * to wrap the user-supplied <code>ComponentReleaseCallback</code> for usage over Corba and for cleaner exception dispatching.
	 */
	private class ComponentReleaseCallbackCorbaHandler extends ResponseReceiver<Integer> {
		private final ComponentReleaseCallback delegate;
		private final org.omg.CORBA.Object stub;
		/**
		 * @param delegate The user-provided callback pojo (unaware of corba)
		 * @param stub  The client stub that will be released only when the component has been deactivated.
		 */
		ComponentReleaseCallbackCorbaHandler(ComponentReleaseCallback delegate, org.omg.CORBA.Object stub ) {
			this.delegate = delegate;
			this.stub = stub;
		}
		@Override
		public void incomingException(AcsJException ex) {
			try {
				if (ex instanceof AcsJComponentDeactivationUncleanEx) {
					delegate.componentReleased((AcsJComponentDeactivationUncleanEx)ex);
				}
				else if (ex instanceof AcsJComponentDeactivationFailedEx) {
					delegate.errorComponentReleaseFailed((AcsJComponentDeactivationFailedEx)ex);
				}
				else {
					m_logger.log(Level.WARNING, "Received unexpected exception from manager#release_component_async, please report to ACS developers.", ex);
					delegate.errorCommunicationFailure(ex); // strictly speaking the wrong method, but better than nothing.
				}
			}
			catch (RuntimeException handlerEx) {
				m_logger.log(Level.FINE, "User-supplied handler threw an exception.", handlerEx);
			}
			finally {
				delegate.callOver();
				stub._release();
			}
		}
		@Override
		public void incomingResponse(Integer numberRemainingClients) {
			// we do not expose numberRemainingClients in the CS API
			try {
				delegate.componentReleased(null);
			}
			catch (RuntimeException handlerEx) {
				m_logger.log(Level.FINE, "User-supplied handler threw an exception.", handlerEx);
			}
			finally {
				delegate.callOver();
				stub._release();
			}
		}
	}


	@Override
	public void releaseComponent(String curl, ComponentReleaseCallback callback) {
		// we keep the "forceful" release option as a switch in the code. 
		// It was taken out for ACS 7.0, but may come back in the future. 
		final boolean forcibly = false;
		
		if (curl == null) {
			String msg = "Invalid curl 'null', nothing to release.";
			m_logger.log(( callback == null ? AcsLogLevel.INFO : AcsLogLevel.DEBUG ), msg);
			if (callback != null) {
				callback.errorNoPermission(msg);
				callback.callOver();
			}
			return;
		}
		
		org.omg.CORBA.Object stub = null;
		// This use of synchronized makes the code thread safe without locking across the remote call to manager#release_component etc
		synchronized (m_usedComponentsMap) {
			if (!m_usedComponentsMap.containsKey(curl)) {
				String msg = "ignoring request by client '" + m_clientName + (
							m_usedNonStickyComponentsMap.containsKey(curl) 
							? "' to release component '" + curl + "' because the reference is non-sticky and does not need to be released."
							: "' to release other component with unknown curl='" + curl + "'."
							);
				m_logger.log(( callback == null ? AcsLogLevel.INFO : AcsLogLevel.DEBUG ), msg);
				if (callback != null) {
					callback.errorNoPermission(msg);
					callback.callOver();
				}
				return;
			}
			
			// the CURL is in the map and gets removed now
			stub = m_usedComponentsMap.get(curl);
			m_usedComponentsMap.remove(curl);
		}
		
		m_logger.fine("about to release component " + curl + (forcibly ? " forcibly" : ""));
		try {
			if (forcibly) {
				m_acsManagerProxy.force_release_component(getEffectiveClientHandle(), curl);
			}
			else {
				CBlong myCBlong = null;
				if (callback != null) {
					// @TODO reuse ComponentReleaseCallbackCorbaHandler
					ComponentReleaseCallbackCorbaHandler callbackCorba = new ComponentReleaseCallbackCorbaHandler(callback, stub);
					myCBlong = RequesterUtil.giveCBLong(this, callbackCorba);
				}
				m_acsManagerProxy.release_component(getEffectiveClientHandle(), curl, myCBlong);
			}
			m_logger.info("client '" + m_clientName + "' has successfully released " +  " a component with curl=" + curl);

			if (callback == null) { // otherwise this step should be deferred until we receive the callback, to not abort running calls with COMM_FAILURE
				stub._release();
			}
		} 
		catch (AcsJNoPermissionEx ex) {
			AcsLogLevel level = ( callback == null ? AcsLogLevel.WARNING : AcsLogLevel.DEBUG );
			m_logger.log(level, "client '" + m_clientName + "' (handle " + getEffectiveClientHandle() + ") cannot release " + 
					" with the manager the component with curl=" + curl, ex);
			if (callback != null) {
				callback.errorNoPermission(ex.getReason());
			}
		}
		catch (Throwable thr) { // any org.omg.CORBA.SystemException, or whatever else can happen
			AcsLogLevel level = ( callback == null ? AcsLogLevel.WARNING : AcsLogLevel.DEBUG );
			m_logger.log(level, "client '" + m_clientName + "' (handle " + getEffectiveClientHandle() + ") failed to release " + 
					" with the manager the component with curl=" + curl, thr);
			if (callback != null) {
				callback.errorCommunicationFailure(thr);
			}
		}
	}


	/**
	 * @see alma.acs.container.ContainerServices#activateOffShoot(org.omg.PortableServer.Servant)
	 */
	@Override
	public <T extends Servant & OffShootOperations> OffShoot activateOffShoot(T servant)
		throws AcsJContainerServicesEx
	{
		return activateOffShoot(servant, null);
	}

	/**
	 * @see alma.acs.container.ContainerServices#activateOffShoot(org.omg.PortableServer.Servant)
	 */
	@Override
	public <T extends OffShootOperations> OffShoot activateOffShoot(T offshootImpl, Class<T> idlOpInterface)
		throws AcsJContainerServicesEx
	{

		Servant servant = null;
		boolean isTie = false;
		boolean haveToInject = false;

		// Checks
		checkOffShoot(offshootImpl);

		// If we receive an object that is not a servant it means that it requires XML automatic bindings.
		// We create the corresponding POATie object, the dynamic proxy binder,
		// and set the offshoot implementation as the final delegate
		if( !(offshootImpl instanceof Servant) ) {

			if( idlOpInterface == null )
				throw new AcsJContainerServicesEx(new NullPointerException("Received null idlOpInterface when asking to activate XML offshoot"));

			if( !idlOpInterface.isAssignableFrom(offshootImpl.getClass()) ) {
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
				ex.setContextInfo("Received OffShoot of type '" + offshootImpl.getClass().getName() +
				    "' does not inherits from '" + idlOpInterface.getName() +  "'");
				throw ex;
			}

			// Guess the name of the xyzPOATie class, build it, and delegate
			String poaTieClassName = null;
			try {

				m_logger.fine("Creating POATie servant for offshoot '" + offshootImpl.getClass().getName() + "'");
				// Get the POATie class and the expected xyzOperations interface
				String baseClassName = idlOpInterface.getName().substring(0, idlOpInterface.getName().length()-1);
				poaTieClassName =  baseClassName + "POATie";
				Class<?> poaTieClazz = Class.forName( poaTieClassName );
				Method implGetter = poaTieClazz.getMethod("_delegate", (Class[]) null);
				Class<?> operationsIF = implGetter.getReturnType();

				// Create the dynamic XML entities wrapper
				Object proxy = DynamicProxyFactory.getDynamicProxyFactory(m_logger)
				                 .createServerProxy(operationsIF, offshootImpl, idlOpInterface);

				// Create the POATie object, give it the proxy, and set it as our servant
				Constructor<?> c = poaTieClazz.getConstructor(new Class[]{operationsIF});
				servant = (Servant)c.newInstance(proxy);

				if( m_componentXmlTranslatorProxy != null )
					haveToInject = true;

			} catch (ClassNotFoundException e) {
				String msg = "Failed to create servant for offshoot " + offshootImpl.getClass().getName() + ": class '" + poaTieClassName + "' cannot be found";
				m_logger.log(AcsLogLevel.ERROR, msg, e);
				AcsJContainerServicesEx ex = new AcsJContainerServicesEx();
				ex.setContextInfo(msg);
				throw ex;
			} catch(Exception e) {
				throw new AcsJContainerServicesEx(e);
			}
			
		}
		else {
			m_logger.fine("Don't need to create servant for offshoot '" + offshootImpl.getClass().getName() + "'");
			servant = (Servant)offshootImpl;
		}


		// check if the servant is the Tie variant, which allows proxy-based call interception by the container
		String servantName = servant.getClass().getName();
		if (servantName.endsWith("POATie")) {
			try {
				// the _delegate getter method is mandated by the IDL-to-Java mapping spec
				Method implGetter = servant.getClass().getMethod("_delegate", (Class[]) null);
				isTie = true;
				Class<?> operationsIF = implGetter.getReturnType();
				java.lang.Object offshootTiedImpl = implGetter.invoke(servant, (java.lang.Object[]) null);
				// now we insert the interceptor between the tie skeleton and the impl.
				// Offshoots have no name, so we construct one from the component name and the offshoot interface name
				// 
				String qualOffshootName = getName() + "/" + operationsIF.getName().substring(0, operationsIF.getName().length() - "Operations".length());
				java.lang.Object interceptingOffshootImpl = ContainerSealant.createContainerSealant(
						operationsIF, offshootTiedImpl, qualOffshootName, true, m_logger, 
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
			m_activatedOffshootsMap.put(offshootImpl, servant);
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

		// finally, put the CORBA-object/implementation into the component's proxy invocation handler,
		// so when requesting an offshoot into the component, we return the corresponding CORBA object
		if( haveToInject ) {
			m_logger.fine("Injecting offshoot '" + offshootImpl.getClass().getName() + "' to '" + m_clientName + "' component XML binder");
			ComponentInvocationHandler handler = (ComponentInvocationHandler)Proxy.getInvocationHandler(m_componentXmlTranslatorProxy);
			handler.addOffshoot(offshootImpl, shoot);
		}

		m_logger.fine("successfully activated offshoot of type " + offshootImpl.getClass().getName());
		return shoot;
	}

	@Override
	public void deactivateOffShoot(Object offshootImpl)
	throws AcsJContainerServicesEx
	{
		checkOffShoot(offshootImpl);
		try {
			acsCorba.deactivateOffShoot(m_activatedOffshootsMap.get(offshootImpl), m_clientPOA);
			m_activatedOffshootsMap.remove(offshootImpl);
			m_logger.fine("successfully deactivated offshoot of type " + offshootImpl.getClass().getName());
		} catch (AcsJContainerEx e) {
			throw new AcsJContainerServicesEx(e);
		}
	}

	/**
	 * @param cbServant
	 * @throws ContainerException
	 */
	private void checkOffShoot(Object servant) throws AcsJContainerServicesEx {
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
		synchronized (lazyCreationSync) {
			if (advancedContainerServices == null) {
				advancedContainerServices = new AdvancedContainerServicesImpl(this, m_logger);
				// todo: once the legitimate cases of calling this method are settled, remove the log message.
				m_logger.info("component '" + getName() + "' requested AdvancedContainerServices");
			}
		}
		return advancedContainerServices;
	}

	/**
	 * {@inheritDoc}.
	 * <p>
	 * TODO: implement shortcutting of xml (de-)serialization for collocated component or offshoot:
	 * ask AcsContainer if it knows componentReference, and if it has transpXml-IF;
	 * if so, get component impl directly;
	 * check if respective component helper allows direct calls to transpXmlIF
	 * (by not implementing _getInterfaceTranslator, or some explicit flag);
	 * move intercepting layer (ContainerSealant) so that it's still in between the components.
	 *   
	 * @see alma.acs.container.ContainerServices#getTransparentXmlComponent(java.lang.Class, org.omg.CORBA.Object, java.lang.Class)
	 */
	public <T, F> T getTransparentXmlWrapper(Class<T> transparentXmlIF, F flatXmlObject, Class<F> flatXmlIF)
    	throws AcsJContainerServicesEx
    {
    	if (m_logger.isLoggable(Level.FINEST)) {
	        m_logger.finest("creating xml binding class aware wrapper around remote object " + 
	                "implementing " + flatXmlIF.getName() + "...");
    	}

        T wrapper = null;
        try
        {
            wrapper = DynamicProxyFactory.getDynamicProxyFactory(m_logger).createClientProxy(
                    transparentXmlIF,
                    flatXmlObject,
                    flatXmlIF);
        }
        catch (Throwable thr)
        {
        	String msg = "failed to create XML binding class wrapper for remote object implementing " + flatXmlIF.getName();
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
	 * Gets the handle to be used toward the manager, which is 
	 * <ul>
	 *   <li> The handle obtained from the manager at login for normal clients
	 *   <li> The component handle assigned  by the manager at component activation time, 
	 *        if this ContainerServices instance is used for a component
	 * </ul>
	 * We don't cache the handle from acsManagerProxy because it may change after a re-login,
	 * and then we get errors if the stale handle would be used.
	 * @return  The correct handle to be used to identify this client to the manager.
	 */
	private int getEffectiveClientHandle() {
		return (m_componentHandle > 0 ? m_componentHandle : m_acsManagerProxy.getManagerHandle());
	}


    /**
     * @see alma.acs.container.ContainerServices#getThreadFactory()
     */
	@Override
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



	/**
	 * Cleans up all the resources that need to be closed, like closing opened notification channels
	 *
	 * @since ACS 8.1.0
	 */
	public void cleanUp() {

		/* Cleanup through externally registered callbacks */
		for (CleanUpCallback cleanUpCallback : cleanUpCallbacks) {
			try {
				cleanUpCallback.containerServicesCleanUp();
			}
			catch (Throwable thr) {
				m_logger.log(Level.WARNING, "Failed to clean up registered client object", thr);
			}
		}

		/* Disconnect NC subscribers */
		for(String channel: m_subscribers.keySet()) {
			AcsEventSubscriber subscriber = m_subscribers.get(channel);
			try {
				subscriber.disconnect();
				String tmp[] = channel.split("/");
				m_logger.log(AcsLogLevel.NOTICE, "Automatically disconnected subscriber for NC '" + tmp[tmp.length - 1] + "'");
			} catch (IllegalStateException e) {
				// Silently ignore this exception, as the subscriber was already disconnected. Well done, developers! :)
			}
		}

		/* Disconnect NC publishers */
		for(String channel: m_publishers.keySet()) {
			AcsEventPublisher<?> subscriber = m_publishers.get(channel);
			try {
				subscriber.disconnect();
				String tmp[] = channel.split("/");
				m_logger.log(AcsLogLevel.NOTICE, "Automatically disconnected publisher for NC '" + tmp[tmp.length - 1] + "'");
			} catch (IllegalStateException e) {
				// Silently ignore this exception, as the subscriber was already disconnected. Well done, developers! :)
			}
		}

		/* Cleanup the alarm source */
		m_alarmSource.tearDown();
	}

	/**
	 * A hack, see {@link ContainerServicesImpl#registerCleanUpCallback(CleanUpCallback)}.
	 */
	public static interface CleanUpCallback {
		public void containerServicesCleanUp();
	}

	/**
	 * This is a hack: NC classes can register themselves to be notified,
	 * in order to release remote Corba resources (and prevent crashes of Notify Service...).
	 * Note that without this hack, the lifecycle of NC classes is only managed by the application code,
	 * which means that ACS could not enforce the clean up.
	 * <p>
	 * @TODO remove this once the NC classes are properly integrated into container services
	 * @param cb
	 * @since ACS 8.1.0
	 */
	public void registerCleanUpCallback(ContainerServicesImpl.CleanUpCallback cb) {
		cleanUpCallbacks.add(cb);
	}


	private NamingContext getNameService() throws AcsJContainerServicesEx {

		NamingContext nameService = null;

		try
		{
			org.omg.CORBA.Object nameServiceObj = m_acsManagerProxy.get_service("NameService", true);
			nameService = NamingContextHelper.narrow(nameServiceObj);
		} catch (AcsJmaciErrTypeEx ex) {
			m_logger.log(Level.FINE, "Failed to get the reference to the NameService service", ex);
			throw new AcsJContainerServicesEx(ex);
		} catch (Throwable thr) {
			String msg = "Unexpectedly failed to get the NameService reference!";
			m_logger.log(Level.FINE, msg, thr);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}

		return nameService;
	}

	/**
	 * @see alma.acs.container.ContainerServices#createNotificationChannelSubscriber(String)
	 */
	@Override
	public AcsEventSubscriber createNotificationChannelSubscriber(String channelName) throws AcsJContainerServicesEx {
		return createNotificationChannelSubscriber(channelName, null); //TODO (rtobar): Is this fine? I'm only 99% sure
	}
	
	/**
	 * @see alma.acs.container.ContainerServices#createNotificationChannelSubscriber(String, String)
	 */
	@Override
	public AcsEventSubscriber createNotificationChannelSubscriber(String channelName, String channelNotifyServiceDomainName) throws AcsJContainerServicesEx {

		AcsEventSubscriber subscriber = null;

		try {
			Object[] args = new Object[]{
					channelName,
					channelNotifyServiceDomainName,
					this,
					getNameService(),
					m_clientName
			};
			Class<?> clazz = Class.forName(CLASSNAME_NC_SUBSCRIBER);
			Constructor<?> constructor = clazz.getConstructor(String.class, String.class, ContainerServicesBase.class, NamingContext.class, String.class);
			subscriber = (AcsEventSubscriber)constructor.newInstance(args);
		} catch(ClassNotFoundException e) {
			// TODO: maybe we could prevent future NCSubscriber creation tries, since the class isn't and will not be loaded
			//       The same applies for the next "catch" block
			m_logger.log(AcsLogLevel.ERROR, "Cannot create NC subscriber because the 'NCSubscriber' class is not present in the classpath", e);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
			ex.setContextInfo("'" + CLASSNAME_NC_SUBSCRIBER + "' class not present in the classpath");
			throw ex;
		} catch(ClassCastException e) {
			m_logger.log(AcsLogLevel.ERROR, "Cannot create NC subscriber because loaded class is not of type 'AcsEventSubscriber", e);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
			ex.setContextInfo("'" + CLASSNAME_NC_SUBSCRIBER + "' class does not extend 'AcsEventSubscriber'");
			throw ex;
		} catch(Throwable e) {
			m_logger.log(AcsLogLevel.ERROR, "Unexpected error while creating new AcsEventSubscriber object", e);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
			throw ex;
		}

		m_subscribers.put( (channelNotifyServiceDomainName == null ? "" : channelNotifyServiceDomainName) + "/" + channelName, subscriber);
		return subscriber;
	}

	/**
	 * @see ContainerServices#createNotificationChannelPublisher(String)
	 */
	@Override
	public <T> AcsEventPublisher<T> createNotificationChannelPublisher(String channelName, Class<T> eventType) throws AcsJContainerServicesEx {
		return createNotificationChannelPublisher(channelName, null, eventType);
	}

	
	/**
	 * @see ContainerServices#createNotificationChannelPublisher(String, String)
	 */
	@Override
	public <T> AcsEventPublisher<T> createNotificationChannelPublisher(String channelName, String channelNotifyServiceDomainName, Class<T> eventType) throws AcsJContainerServicesEx {

		AcsEventPublisher<T> publisher = null;

		try {
			Object[] args = new Object[]{
					channelName,
					channelNotifyServiceDomainName,
					this,
					getNameService()
			};
//			// TODO: Can we do this without the direct cast? The usual "asSubclass" is not enough 
			// because we don't create a subclass of Class<T> but rather of Class<U<T>>.
			// Also the getGenericInterfaces / ParameterizedType trick does not work because because we don't have a 
			// concrete parameterized type.
			Class<AcsEventPublisher<T>> clazz = (Class<AcsEventPublisher<T>>) Class.forName(CLASSNAME_NC_PUBLISHER);
			Constructor<? extends AcsEventPublisher<T>> constructor = clazz.getConstructor(String.class, String.class, ContainerServicesBase.class, NamingContext.class);
			publisher = constructor.newInstance(args);
		} catch(ClassNotFoundException e) {
			// TODO: maybe we could prevent future NCPublisher creation tries, since the class isn't and will not be loaded
			//       The same applies for the next "catch" block
			m_logger.log(AcsLogLevel.ERROR, "Cannot create NC publisher because the 'NCPublisher' class is not present in the classpath", e);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
			ex.setContextInfo("'" + CLASSNAME_NC_PUBLISHER + "' class not present in the classpath");
			throw ex;
		} catch(ClassCastException e) {
			m_logger.log(AcsLogLevel.ERROR, "Cannot create NC publisher because loaded class '" + CLASSNAME_NC_PUBLISHER + "' is not of type 'AcsEventPublisher", e);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
			ex.setContextInfo("'" + CLASSNAME_NC_PUBLISHER + "' class does not extend 'AcsEventPublisher'");
			throw ex;
		} catch(Throwable e) {
			m_logger.log(AcsLogLevel.ERROR, "Unexpected error while creating new AcsEventPublisher object", e);
			AcsJContainerServicesEx ex = new AcsJContainerServicesEx(e);
			throw ex;
		}

		m_publishers.put( (channelNotifyServiceDomainName == null ? "" : channelNotifyServiceDomainName) + "/" + channelName, publisher);
		
		// crude and arbitrary measure against misbehaving clients. In one case we've seen more than 2000 suppliers created by accident. 
		if (m_publishers.size() > 200) {
			m_logger.warning("Component or client '" + m_clientName + "' has already created " + m_publishers.size() + " event publishers. Developers should check if this is really necessary.");
		}
		
		return publisher;
	}

	/**
	 * @deprecated remove along with raiseAlarm, clearAlarm
	 */
	private void submitAlarm(String faultFamily, String faultMember, int faultCode, boolean raise) throws AcsJContainerServicesEx {
		m_alarmSource.setAlarm(faultFamily, faultMember, faultCode, raise);
	}

	/**
	 * {@inheritDoc}
	 * @deprecated
	 */
	@Override
	public void raiseAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJContainerServicesEx {
		submitAlarm(faultFamily, faultMember, faultCode, true);
	}

	/**
	 * {@inheritDoc}
	 * @deprecated
	 */
	@Override
	public void clearAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJContainerServicesEx {
		submitAlarm(faultFamily, faultMember, faultCode, false);
	}

	@Override
	public AlarmSource getAlarmSource() throws AcsJContainerServicesEx {
		return m_alarmSource;
	}

}