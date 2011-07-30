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

import java.io.IOException;
import java.util.logging.Level;

import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import si.ijs.maci.ComponentInfo;

import alma.ACS.ComponentStates;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.classloading.AcsComponentClassLoader;
import alma.acs.component.ComponentLifecycle;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.AcsLogger;
import alma.maciErrType.ComponentDeactivationUncleanEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationFailedEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationUncleanEx;


/**
 * Adapter between the container on the one side and a component with all its child objects on the other.
 * For the component, <code>ComponentAdapter</code> is the container, 
 * since it calls the <code>ComponentLifecycle</code> methods. 
 * For the container, it represents the component with its meta data. 
 *   
 * @author hsommer Nov 6, 2002 2:48:13 PM
 */
public class ComponentAdapter 
{
	// fields from si.ijs.maci.ComponentInfo
	private String m_type;
	private String m_code;
	private org.omg.CORBA.Object m_reference; // the corba object
	private String m_compInstanceName;
	private int[] m_clients;
	private String m_containerName;
	private int m_compHandle;
	private int m_access;
	private String[] m_interfaces;

	// other fields
	
	private AcsLogger m_containerLogger;

	// the component itself and its XML translator proxy, if any
	private ComponentLifecycle m_component;
		
	// the tie skeleton that receives ORB calls
	private Servant m_servant;
	
	// the POA to be used for this component 
	private POA m_componentPOA;
	// the servant manager for this component POA
	private ComponentServantManager compServantManager;
	
	private final AcsCorba acsCorba;

	private final AcsManagerProxy m_managerProxy;

	private final ClassLoader m_componentClassLoader;

	private final ComponentStateManagerImpl m_componentStateManager;

	private final ContainerServicesImpl m_containerServices;

	private final CleaningDaemonThreadFactory m_threadFactory;
	

	/**
	 * Method ComponentAdapter.
	 * @param compName component instance name (curl)
	 * @param type  IDL type
	 * @param code  Java impl class of the component <b>helper</b> (subclass of {@link ComponentHelper}); 
	 * @param compHandle  component handle assigned by the manager
	 * @param containerName
	 * @param component  the instance of the component implementation class
	 * @param managerProxy  the shared manager proxy object
	 * @param componentClassLoader the classloader to be used as the currentThread-ClassLoader when component lifecycle methods are invoked.
	 * @param logger  logger to be used by this class (not by the component though)
	 * @param acsCorba 
	 * @throws AcsJContainerServicesEx 
	 */
	ComponentAdapter(String compName, String type, String code,
					int compHandle, String containerName,
					ComponentLifecycle component,
					AcsManagerProxy managerProxy, 
					ClassLoader componentClassLoader,
					AcsLogger logger,
					AcsCorba acsCorba)
		throws AcsJContainerEx
	{
		// store params
		m_compInstanceName = compName;
		m_type = type;
		m_code = code;
		m_compHandle = compHandle;
		m_containerName = containerName;
		m_component = component;
		m_componentClassLoader = componentClassLoader;
		m_containerLogger = logger;
		this.acsCorba = acsCorba;

		// init arrays to avoid nullpointer trouble
		m_interfaces = new String[0];
		m_clients = new int[0];
		

		m_componentPOA = acsCorba.createPOAForComponent(m_compInstanceName);
		
		m_componentStateManager = new ComponentStateManagerImpl(m_compInstanceName, m_containerLogger);
			
        m_threadFactory = new CleaningDaemonThreadFactory(compName, m_containerLogger);
        m_threadFactory.setNewThreadContextClassLoader(m_componentClassLoader);
        
        m_managerProxy = managerProxy;
        
        m_containerServices = 
			new ContainerServicesImpl(managerProxy, m_componentPOA, acsCorba, m_containerLogger, 
					m_compHandle, m_compInstanceName, m_componentStateManager, m_threadFactory);
	}

	void setComponentXmlTranslatorProxy(Object xmlTranslatorProxy) {
		m_containerServices.setComponentXmlTranslatorProxy(xmlTranslatorProxy);
	}

	ContainerServicesImpl getContainerServices() {
		return m_containerServices;
	}
	
	void activateComponent(Servant servant)
		throws AcsJContainerEx
	{
		if (m_containerLogger.isLoggable(Level.FINER)) {
			m_containerLogger.finer("entering ComponentAdapter#activateComponent for " + m_compInstanceName);
		}

		m_servant = servant;
		
		try {
			compServantManager = acsCorba.setServantManagerOnComponentPOA(m_componentPOA);
			m_reference = acsCorba.activateComponent(servant, m_compInstanceName, m_componentPOA);
		}
		catch (Throwable thr) {
			String msg = "failed to activate component " + m_compInstanceName + " of type " + m_component.getClass().getName();
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo(msg);
			throw ex;
		}	
		
		m_interfaces = _getInterfaces();
	}



	void initializeComponent() throws ComponentLifecycleException {
        ClassLoader contCL = Thread.currentThread().getContextClassLoader();
        m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_INITIALIZING);
        Throwable thr = null;
        Thread.currentThread().setContextClassLoader(m_componentClassLoader);
        try {
            m_component.initialize(m_containerServices);
        } catch (Throwable t) {
            thr = t;
        } finally {
            Thread.currentThread().setContextClassLoader(contCL);
        }
        
        if (thr != null) {
            if (thr instanceof ComponentLifecycleException) {
                m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_ERROR);
                throw (ComponentLifecycleException) thr;
            }
            m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_ERROR);
            throw new ComponentLifecycleException(thr);
        }
        m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_INITIALIZED);
    }

    
    void executeComponent() throws ComponentLifecycleException {
        ClassLoader contCL = Thread.currentThread().getContextClassLoader();
        Throwable thr = null;
        Thread.currentThread().setContextClassLoader(m_componentClassLoader);
        try {
            m_component.execute();
        } catch (Throwable t) {
            thr = t;
        } finally {
            Thread.currentThread().setContextClassLoader(contCL);
        }
        
        if (thr != null) {
            if (thr instanceof ComponentLifecycleException) {
                m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_ERROR);
                throw (ComponentLifecycleException) thr;
            }
            m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_ERROR);
            throw new ComponentLifecycleException(thr);
        }
        m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_OPERATIONAL);
    }


	private String[] _getInterfaces()
	{
		String[] interfaces = null;
		try 
		{
			interfaces = m_servant._all_interfaces(m_componentPOA, m_compInstanceName.getBytes());
			
			if (m_containerLogger.isLoggable(Level.FINE)) {			
				StringBuffer buff = new StringBuffer("");
				for (int i = 0; i < interfaces.length; i++)
				{
					buff.append(interfaces[i]);
				}
				m_containerLogger.fine("interfaces of component '" + m_compInstanceName + "': " + buff.toString());
			}
		}			
		catch (Exception ex) 
		{
			m_containerLogger.info("failed to retrieve interface information for component " + 
									m_compInstanceName);
		}
		if (interfaces == null)
		{
			interfaces = new String[1];
			interfaces[0] = "IDL:omg.org/CORBA/Object:1.0";
		}
		return interfaces;		
	}
	
	
	/**
	 * Deactivates a component.
	 * <ol>
	 *  <li>First the component's POA manager is put into inactive state, so that all incoming calls to this component are rejected. 
	 *      However, we wait for currently executing calls to finish, with a timeout as described below.
	 *  <ul>
	 *   <li>Rejection applies to requests already received and queued by the ORB (but that have not started executing), 
	 *       as well as to requests that clients will send in the future. 
	 *   <li>Note that entering into the inactive state may take forever if the component hangs in a functional call.
	 *   <li>Therefore we use a timeout to proceed in such cases where POA manager deactivation does not happen in time.
	 *       This bears the risk of undesirable behavior caused by calling the {@link ComponentLifecycle#cleanUp() cleanUp} 
	 *       method while other threads still perform functional calls on the component.
	 *  </ul>
	 *  <li>Second the component itself is deactivated:
	 *  <ul>
	 *   <li>The lifecycle method {@link ComponentLifecycle#cleanUp() cleanUp} is called, currently without enforcing a timeout.
	 *   <li>TODO: use a timeout, unless we decide that a client-side timeout for releaseComponent is good enough.
	 *  </ul>
	 *  <li>Third the component is disconnected from CORBA ("etherealized" from the POA).
	 *  <ul>
	 *   <li>Note that also etherealization may take forever if the component hangs in a call.
	 *   <li>Therefore we use a timeout to proceed with deactivation in such cases where etherealization does not happen in time.
	 *   <li>Currently a component that failed to etherealize in time can stay active as long as the container is alive.
	 *       TODO: check if using the "container sealant" we can identify and stop the active ORB threads.
	 *  </ul>
	 * </ol>   
	 * This method logs errors as FINER if they also cause an exception, and as WARNING if they cannot lead to an exception
	 * because other more important error conditions are present.
	 * 
	 * @throws ComponentDeactivationUncleanEx, ComponentDeactivationFailedEx 
	 */
	void deactivateComponent() throws AcsJComponentDeactivationUncleanEx, AcsJComponentDeactivationFailedEx {
		if (m_containerLogger.isLoggable(Level.FINER)) {
			m_containerLogger.finer("About to deactivate component " + m_compInstanceName + " with handle " + getHandle());
		}
		
		AcsJComponentDeactivationUncleanEx deactivationUncleanEx = null;
		AcsJComponentDeactivationFailedEx deactivationFailedEx = null;
		try
		{
			// (1) try to reject calls by sending poa manager to inactive state  
			// TODO: make the timeout configurable
			int deactivateTimeoutMillis = 10000;
			boolean isInactive = acsCorba.deactivateComponentPOAManager(m_componentPOA, m_compInstanceName, deactivateTimeoutMillis);
			if (isInactive && m_containerLogger.isLoggable(Level.FINER)) {
				m_containerLogger.finer("Now rejecting any calls to component '" + m_compInstanceName + "'. Will call cleanUp() next.");
			}
			else if (!isInactive) {
				String msg = "Component '" + m_compInstanceName + "' failed to reject calls within " + 
							deactivateTimeoutMillis + " ms, probably because of pending calls. Will call cleanUp() anyway.";
				m_containerLogger.warning(msg);
				deactivationUncleanEx = new AcsJComponentDeactivationUncleanEx();
				deactivationUncleanEx.setCURL(m_compInstanceName);
				deactivationUncleanEx.setReason(msg);
				// do not yet throw deactivationUncleanEx as we need to go through the other steps first
			}
			
			// (2) call the lifecycle method cleanUp and also clean container services and other support classes 
			ClassLoader contCL = Thread.currentThread().getContextClassLoader();
			Thread.currentThread().setContextClassLoader(m_componentClassLoader);
			try {
				// TODO: also use a timeout for cleanUp
				m_component.cleanUp();
			} catch (Throwable thr) {
				// AcsJComponentCleanUpEx is declared, but any other ex will be wrapped by AcsJComponentDeactivationUncleanEx as well
				m_containerLogger.log(Level.FINE, "Failure in cleanUp() method of component " + m_compInstanceName, thr);
				deactivationUncleanEx = new AcsJComponentDeactivationUncleanEx(thr); // this would override a previous ex from POA deactivation 
				deactivationUncleanEx.setCURL(m_compInstanceName);
				// do not yet throw deactivationUncleanEx as we need to nonetheless destroy the POA
			} finally {
				Thread.currentThread().setContextClassLoader(contCL);
				try {
					m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_DEFUNCT);
				} catch (ComponentLifecycleException ex) {
					if (deactivationUncleanEx == null) { // an ex from cleanUp would be more important
						deactivationUncleanEx = new AcsJComponentDeactivationUncleanEx(ex);
						deactivationUncleanEx.setCURL(m_compInstanceName);
					}
					else {
						m_containerLogger.log(Level.WARNING, "Failed to set component state DEFUNCT on " + m_compInstanceName, ex);
					}
				}
				m_containerServices.cleanUp();
				m_threadFactory.cleanUp();
			}

			// (3) destroy the component POA
			// since we already tried to discard requests using the poa manager before,
			// the additional timeout can be kept small. If calls are pending, we fail.
			int etherealizeTimeoutMillis = 1000;
			boolean isEtherealized = acsCorba.destroyComponentPOA(m_componentPOA, compServantManager, etherealizeTimeoutMillis);
			if (isEtherealized && m_containerLogger.isLoggable(Level.FINER)) {
				m_containerLogger.finer("Component '" + m_compInstanceName + "' is etherealized.");
			}
			else if (!isEtherealized){
				m_containerLogger.warning("Component '" + m_compInstanceName + "' failed to be etherealized in " + 
						etherealizeTimeoutMillis + " ms, probably because of pending calls.");
				deactivationFailedEx = new AcsJComponentDeactivationFailedEx();
				deactivationFailedEx.setCURL(m_compInstanceName);
				deactivationFailedEx.setReason("Component POA etherialization timed out after " + etherealizeTimeoutMillis + " ms.");
				deactivationFailedEx.setIsPermanentFailure(true); // @TODO: distinguish the cases better
				// do not yet throw deactivationFailedEx as we need to nonetheless close the classloader
			}
			
			// (4) "close" m_componentClassLoader (otherwise JVM native mem leak, see COMP-4929)
			if (m_componentClassLoader instanceof AcsComponentClassLoader) {
				try {
					((AcsComponentClassLoader)m_componentClassLoader).close();
				} catch (IOException ex) {
					m_containerLogger.log(Level.WARNING, "Failed to close component class loader", ex);
				}
			}
		}
		catch (RuntimeException ex) {
			if (deactivationFailedEx == null) { // exception from POA destruction has precedence
				deactivationFailedEx = new AcsJComponentDeactivationFailedEx(ex);
				deactivationFailedEx.setCURL(m_compInstanceName);
				deactivationFailedEx.setReason("Unexpected exception caught during component deactivation.");
			}
			else {
				m_containerLogger.log(Level.WARNING, "Unexpected exception caught during deactivation of component " + m_compInstanceName, ex);
			}
		}

		if (deactivationFailedEx != null) {
			if (m_containerLogger.isLoggable(Level.FINER)) {
				m_containerLogger.log(Level.FINER, "Deactivation of component " + m_compInstanceName + " failed. "
						+ "Will throw AcsJComponentDeactivationFailedEx", deactivationFailedEx);
			}
			throw deactivationFailedEx;
		}
		if (deactivationUncleanEx != null) {
			if (m_containerLogger.isLoggable(Level.FINER)) {
				m_containerLogger.log(Level.FINER, "Deactivation of component " + m_compInstanceName + " finished with problems. "
						+ "Will throw AcsJComponentDeactivationUncleanEx", deactivationUncleanEx);
			}
			throw deactivationUncleanEx;
		}
		
		if (m_containerLogger.isLoggable(Level.FINER)) {
			m_containerLogger.finer("Done deactivating component " + m_compInstanceName  + " with handle " + getHandle());
		}
	}

	
	/**
	 * Returns a <code>Runnable</code> that can abort the component in the following way.
	 * <ol>
	 * <li>Sets the component state to <code>ABORTING</code>
	 * <li>Calls {@link ComponentLifecycle#aboutToAbort()} 
	 * <li>Sets the component state to <code>DEFUNCT</code>
     * <li>Kills all surviving user threads created by the component using {@link ContainerServices#getThreadFactory() getThreadFactory}
	 * <li>if <code>killComponentPOA==true</code>, destroys the POA for this component;
	 * </ol>
	 * This method returns immediately, so the caller can then run the returned Runnable in its own thread. 
	 */
	Runnable getComponentAbortionist(final boolean killComponentPOA) {
        
        Runnable abortionist = new Runnable() {
            public void run() {
                try {
                    // todo: check how time consuming this POA deactivation is,
                    // and if it should be postponed till after aboutToAbort()
                    // ... seems we don't need this at all!
                    // m_componentPOA.deactivate_object(m_compInstanceName.getBytes());

                    m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_ABORTING);

                    ClassLoader contCL = Thread.currentThread().getContextClassLoader();
                    Thread.currentThread().setContextClassLoader(m_componentClassLoader);
                    try {
                        m_component.aboutToAbort();
                    } finally {
                        Thread.currentThread().setContextClassLoader(contCL);
                        m_componentStateManager.setStateByContainer(ComponentStates.COMPSTATE_DEFUNCT);
                        m_containerServices.cleanUp();
                        m_threadFactory.cleanUp();
                        if (killComponentPOA) {
                            m_componentPOA.destroy(false, false);
                        }
                    }
                } catch (Throwable t) {
                    m_containerLogger.log(Level.INFO, "failed to abort component " + getName(), t);
                    t.printStackTrace();
                }
            }
        };

        return abortionist;
    }

	

	ComponentInfo getComponentInfo()
	{
		ComponentInfo ci = new ComponentInfo(
			m_type, m_code, m_reference, m_compInstanceName, m_clients, m_managerProxy.getManagerHandle(), m_containerName, m_compHandle, m_access, m_interfaces);
		return ci;

	}
	
	
	/**
	 * todo: check with rest of ACS which fields really make a component unique in the system.
	 * Seems kind of undefined.
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj)
	{
		if (obj == null || !(obj instanceof ComponentAdapter))
		{
			return false;
		}
		ComponentAdapter other = (ComponentAdapter) obj;
		return (
			m_compInstanceName.equals(other.getName()) &&
			m_compHandle == other.getHandle() &&
			m_type == other.getType()
		);				
	}


	/**
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode()
	{
		return m_compInstanceName.hashCode();
	}

	/**
	 * Returns the reference.
	 * @return org.omg.CORBA.Object
	 */
	public org.omg.CORBA.Object getReference()
	{
		return m_reference;
	}


	/**
	 * Returns the handle.
	 * @return int
	 */
	public int getHandle()
	{
		return m_compHandle;
	}

// CDB hack
//   /**
//	* @param handle
//	*/
//   public void setHandle(int handle)
//   {
//	   m_compHandle = handle;
//   }
// end CDB hack
	
	/**
	 * Returns the name.
	 * @return String
	 */
	public String getName()
	{
		return m_compInstanceName;
	}

	/**
	 * Returns the type.
	 * @return String
	 */
	public String getType()
	{
		return m_type;
	}

	/**
	 * To be called by the container to change the component state.
	 * In some cases, the state will be changed by this ComponentAdapter though.
	 * 
	 * @return the <code>ComponentStateManager</code> that gives acces to the state.
	 */
	ComponentStateManagerImpl getComponentStateManager()
	{
		return m_componentStateManager;
	}

// @TODO	
//	/**
//	 * @return true if the component managed by this adapter declares itself to be stateless.
//	 */
//	boolean isStatelessComponent() {
//		return ( m_component instanceof StatelessComponentLifecycle ); 
//	}

	
    protected void finalize() throws Throwable {
    	if (m_containerLogger.isLoggable(Level.FINEST)) {
    		m_containerLogger.finest("finalize() called by JVM for impl classes of component " + getName() + '(' + getHandle() + ')');
    	}
        super.finalize();
    }


	/**
	 * With this optional call, automatic invocation logging for certain component methods can be disabled.
	 * (Data will just be forwarded to containerServices)  
	 * @param excludedMethods 
	 * @see ComponentHelper#getComponentMethodsExcludedFromInvocationLogging()
	 */
	void setMethodsExcludedFromInvocationLogging(String[] excludedMethods) {
		m_containerServices.setMethodsExcludedFromInvocationLogging(excludedMethods);
	}

}
