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
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.IntHolder;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;

import si.ijs.maci.ClientOperations;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.Container;
import si.ijs.maci.ContainerHelper;
import si.ijs.maci.ContainerPOA;
import si.ijs.maci.ManagerOperations;

import alma.ACS.ComponentStates;
import alma.acs.component.ComponentLifecycle;
import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.container.classloader.AcsComponentClassLoader;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig; 
import alma.acs.logging.config.LogConfigException;
import alma.acs.util.StopWatch;

// \todo Commented out until the Alarm System is better integrated.
//import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;

/**
 * The main container class that interfaces with the maci manager.
 * By extending <code>ContainerPOA</code>, an instance of this class is a corba object
 * that implements the maci container interface.
 * <p>
 * Only one instance of this class can be created per JVM.
 * <p>
 * @author hsommer
 * created 24-Sep-2002 14:24:48
 */
public class AcsContainer extends ContainerPOA
{
    /**
     * It's a singleton, but not with a static getAcsContainer() method to restrict access;
     * <code>s_instance</code> is needed to enforce single instantiation.
     */
    private static AcsContainer s_instance;

    private final String m_containerName;

    private final AcsManagerProxy m_managerProxy;

    private final AcsCorba m_acsCorba;

    private final boolean isEmbedded;

    private final Logger m_logger;

    private final ComponentMap m_activeComponentMap;

    /**
     * Cache for method {@link #getCDB()}. Don't use this field directly.
     */
    private DAL cdb;

    /**
     * Use {@link #isShuttingDown()}, {@link #setShuttingDown(boolean)}
     * instead of directly accessing this member (thread-safety)
     */
    private boolean m_shuttingDown = false;

    /** see comments in {@link #authenticate(String)} */
    private boolean useRecoveryMode = true;

    /** actions for shutdown() */
    public static final int CONTAINER_RELOAD = 0;
    public static final int CONTAINER_REBOOT = 1;
    public static final int CONTAINER_EXIT = 2;


    /////////////////////////////////////////////////////////////
    // creation, initialization of container
    /////////////////////////////////////////////////////////////

    /**
     * @param containerName
     * @param acsCorba
     * @param managerProxy
     * @param isEmbedded  true if this container runs within an application. Affects shutdown behavior.
     * @throws ContainerException  if anything goes wrong, or if another instance of this class
     *                              has already been created.
     */
    AcsContainer(String containerName, AcsCorba acsCorba, AcsManagerProxy managerProxy, boolean isEmbedded)
        throws ContainerException
    {
        if (s_instance == null)
        {
            m_containerName = containerName;
            m_managerProxy = managerProxy;

            this.isEmbedded = isEmbedded;

            ClientLogManager clm = ClientLogManager.getAcsLogManager();
            m_logger = clm.getLoggerForContainer(containerName);

            m_activeComponentMap = new ComponentMap(m_logger);
            m_acsCorba = acsCorba;

            registerWithCorba();
            
            LogConfig logConfig = clm.getLogConfig(); // todo: turn into member field when we get new logging config notification method in the container interface 
//            logConfig.setInternalLogger(m_logger);
            logConfig.setCDBContainerPath("MACI/Containers/" + containerName);
            logConfig.setCDB(getCDB());
            try {
                logConfig.initialize();
            } catch (LogConfigException ex) {
                // if the CDB can't be read, we still want to run the container, so we only log the problems
                m_logger.log(Level.FINE, "Failed to configure logging (default values will be used). Reason: " + ex.getMessage());
            }

            s_instance = this;

// \todo Commented out until the Alarm System is better integrated.
//            try {
//            	ACSAlarmSystemInterfaceFactory.init(m_managerProxy.getManager());
//            } catch (Exception e) {
//            	throw new ContainerException("Error initializing the alarm system factory");
//            }

        }
        else
        {
            throw new ContainerException("illegal attempt to create more than one instance of " +
                                            AcsContainer.class.getName() + " inside one JVM.");
        }
    }

    
    /**
     * Gets a reference to the CDB.
     * Reuses the previously obtained reference.
     * Implemented as on-demand remote call, so always use this method 
     * instead of directly accessing the field {@link #cdb}. 
     * <p>
     * TODO: reuse this CDB reference in ContainerServicesImpl for method getCDB()
     * @return the CDB reference, or <code>null</code> if it could not be obtained.
     */
    DAL getCDB() {
        if (cdb != null) {
            return cdb;
        }
        
        IntHolder status = new IntHolder();
        try {
            // manager's get_service contains get_component, so even if the CDB becomes a real component, we can leave this 
            org.omg.CORBA.Object dalObj = m_managerProxy.get_service("CDB", true, status);
            cdb = DALHelper.narrow(dalObj);
        }
        catch (Exception e) {
            m_logger.log(Level.WARNING, "Failed to access the CDB.", e);
        }
        if (status.value != ManagerOperations.COMPONENT_ACTIVATED) {
            m_logger.log(Level.WARNING, "Failed to access the CDB. Status value was " + status.value);
        }
        
        return cdb;
    }


    void setRecoveryMode(boolean recoveryStart) {
        useRecoveryMode = recoveryStart;
    }


    /**
     * To be called only once from the ctor.
     * @throws ContainerException
     */
    private void registerWithCorba() throws ContainerException
    {
        // activate the Container as a CORBA object.
        org.omg.CORBA.Object obj = m_acsCorba.activateContainer(this, m_containerName);

        if (obj == null)
        {
            throw new ContainerException("failed to register this AcsContainer with the ORB.");
        }

        Container container = ContainerHelper.narrow(obj);
        if (container == null)
        {
            throw new ContainerException("failed to narrow the AcsContainer to a Container.");
        }
        else
        {
            m_logger.finer("AcsContainer successfully registered with the ORB as a Container");
        }
    }


    /**
     * Will attempt to log into the manager.
     * If the manager reference is not available, will enter a loop and keep trying.
     * If login fails on an available manager, will throw a ContainerException.
     *
     * @throws ContainerException
     */
    void loginToManager() throws ContainerException
    {
        Container thisContainer = _this(m_acsCorba.getORB());
        m_managerProxy.loginToManager(thisContainer, true);
    }



    /////////////////////////////////////////////////////////////
    // Implementation of ContainerOperations#activate_component
    /////////////////////////////////////////////////////////////

    /**
     * Activates a component so that it's ready to receive functional calls
     * after returning from this method. Called by the ACS Manager.
     * <p>
     * From MACI IDL:
     * <i>
     * Activate a component whose type (class) and name (instance) are given.
     * In the process of activation, component's code-base is loaded into memory if it is not there already.
     * The code-base resides in an executable file (usually a dynamic-link library or a shared library -- DLL).
     * On platforms that do not automatically load dependent executables (e.g., VxWorks),
     * the container identifies the dependancies by querying the executable and loads them automatically.
     * Once the code is loaded, it is asked to construct a servant of a given type.
     * The servant is then initialized with the Configuration Database (CDB) and Persistance Database (PDB) data.
     * The servant is attached to the component, and a reference to it is returned.
     * </i>
     * <p>
     * @param componentHandle  handle of the component that is being activated. This handle is used
     *              by the component when it will present itself to the Manager.
     *              The component is expected to remember this handle for its entire life-time.
     * @param compName  name of the component to instantiate (instance name, comes from CDB)
     * @param exe   component helper implementation class; must be a subclass of
     *               {@link alma.acs.container.ComponentHelper}.
     * @param type  the type of the component to instantiate (Corba IR id).
     * @return   Returns the reference to the object that has just been activated.
     *               If the component could not the activated, a nil reference is returned.
     *
     * @see si.ijs.maci.ContainerOperations#activate_component(int, String, String, String)
     */
    public ComponentInfo activate_component(int componentHandle, String compName, String exe, String type)
    {
        ComponentInfo componentInfo = null;

        StopWatch activationWatch = new StopWatch(m_logger);

        // to make component activations stick out in the log list
        m_logger.fine("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<");
        m_logger.info("activate_component: handle=" + componentHandle + " name=" + compName +
                        " helperClass=" + exe + " type=" + type);

        ComponentAdapter compAdapter = null;
        try
        {
            synchronized (m_activeComponentMap) {
                ComponentAdapter existingCompAdapter = getExistingComponent(componentHandle, compName, type);
                if (existingCompAdapter != null) {
                    return existingCompAdapter.getComponentInfo();
                }
                else if (!m_activeComponentMap.reserveComponent(componentHandle)) {
                        throw new ContainerException("Component with handle '" + componentHandle +
                                "' is already being activated by this container. Manager should have prevented double activation.");
                }
            }

            ClassLoader compCL = null;
            // the property 'acs.components.classpath.jardirs' is currently set by the script acsStartContainer
            // to a list of all relevant 'lib/ACScomponents/' directories
            String compJarDirs = System.getProperty(AcsComponentClassLoader.PROPERTY_JARDIRS);
            if (compJarDirs != null) {
                compCL = new AcsComponentClassLoader(Thread.currentThread().getContextClassLoader(), m_logger, compName);
            }
            else {
                // fallback: load component impl classes in the global class loader
                compCL = Thread.currentThread().getContextClassLoader();
            }

            // Create component helper using component classloader.
            // Note that the base class alma.acs.container.ComponentHelper will still be loaded by the container CL,
            // although the current subclassing design is a bit dirtier than it could be in the sense that a mean
            // component could deploy modified container classes (e.g. in method getInterfaceTranslator).
            // Nothing big to worry about though...
            ComponentHelper compHelper = createComponentHelper(compName, exe, compCL);

            // Creates component implementation and connects it with the Corba-generated POATie object.
            // Objects for container interception ("tight container") and for automatic xml binding class
            // de-/serialization are chained up and inserted here. End-to-end they have to translate between the
            // operations interface derived from corba IDL and the component's declared internalInterface.
            //

            // guarantees that it implements ComponentLifecycle
            Object compImpl = compHelper.getComponentImpl();

//m_logger.finest(compName + " component impl created, with classloader " + compImpl.getClass().getClassLoader().getClass().getName());

            Class operationsIFClass = compHelper.getOperationsInterface();
            Constructor poaTieCtor =
                compHelper.getPOATieClass().getConstructor(new Class[]{operationsIFClass});

            Object operationsIFImpl = null;
            // todo: use different condition (e.g. a new boolean from compHelper) so that a component
            // can implement both the operations IF and the inner IF, e.g. to do manual parameter
            // translations for some methods only...
            if (operationsIFClass.isInstance(compImpl))
            {
                m_logger.finer("component " + compName + " implements operations interface directly; no dynamic translator proxy used.");
                operationsIFImpl = compImpl;
            }
            else
            {
                m_logger.finer("creating dynamic proxy to map corba interface calls to component " + compName + ".");
                operationsIFImpl = compHelper.getInterfaceTranslator();
            }

            // make it a tight container (one that intercepts functional method calls)
            String[] methodsExcludedFromInvocationLogging = compHelper.getComponentMethodsExcludedFromInvocationLogging();
            Object poaDelegate = ContainerSealant.createContainerSealant(
                operationsIFClass, operationsIFImpl, compName, false, m_logger, compCL, methodsExcludedFromInvocationLogging);

            // construct the POATie skeleton with operationsIFImpl as the delegate object
            Servant servant = null;
            try {
                servant = (Servant) poaTieCtor.newInstance(new Object[]{poaDelegate});
            }
            catch (Exception ex)
            {
                String msg = "failed to instantiate the servant object for component " +
                                compName + " of type " + compImpl.getClass().getName();
                throw new ContainerException(msg, ex);
            }

            //
            // administrate the new component
            //

            compAdapter = new ComponentAdapter(compName, type, exe, componentHandle,
                    m_managerProxy.getManagerHandle(), m_containerName, (ComponentLifecycle) compImpl,
                    m_managerProxy, compCL, m_logger, m_acsCorba);

            // for future offshoots created by this component we must pass on the no-auto-logging info
            compAdapter.setMethodsExcludedFromInvocationLogging(methodsExcludedFromInvocationLogging);

            compAdapter.activateComponent(servant);

            // even though the component is now an activated Corba object already,
            // it won't be called yet since the maciManager will only pass around
            // access information after we've returned from this activate_component method.
            // Therefore it's not too late to call initialize and execute, which are
            // guaranteed to be called before incoming functional calls must be expected.
            // At the moment we have to call these two methods one after the other;
            // if the Manager supports new calling semantics, we could separate the two
            // as described in ComponentLifecycle
            m_logger.fine("about to initialize component " + compName);
            compAdapter.initializeComponent();
            compAdapter.executeComponent();

            // we've deferred storing the component in the map until after it's been initialized successfully
            m_activeComponentMap.put(componentHandle, compAdapter);

            long activTime = activationWatch.getLapTimeMillis();
            m_logger.info("component " + compName + " activated and initialized in " + activTime + " ms.");

            componentInfo = compAdapter.getComponentInfo();
        }
        catch (Throwable thr)
        {
            m_logger.log(Level.SEVERE, "Failed to activate component " + compName + ", problem was: ", thr);
            if (compAdapter != null) {
                try {
                    compAdapter.deactivateComponent();
                } catch (ContainerException ex) {
                    m_logger.log(Level.FINE, ex.getMessage(), ex);
                }
            }
            m_activeComponentMap.remove(componentHandle);
            // invalid info (replacement for null) -- TODO: change idl to throw corba exception
            componentInfo = new ComponentInfo("<invalid>", "<invalid>", null, "<invalid>", new int[0], 0, "<invalid>", 0, 0, new String[0]);
        }
        finally
        {
            // to make (possibly nested) component activations stick out in the log list
            m_logger.fine(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
        }

        return componentInfo;
    }


    /**
     * Checks if there is an existing component that matches the spec.
     * This can happen if the Manager went down and back up.
     * Strategy:
     * <ul>
     * <li>Same handles:
     *      <ul>
     *      <li>reuses component if name and type match;
     *      <li>deactivates existing component if name or type don't match;
     *          then returns <code>null</code>.
     *      <li>
     *      </ul>
     * <li>Different handles:
     *      <ul>
     *      <li>reuses component if name and type match, with the existing handle
     *          (the manager will have to update its handle table)
     *      <li>just returns null if name or type don't match;
     *          this is the standard case of activating a genuinly new component.
     *      </ul>
     * </ul>
     * @param componentHandle
     * @param name
     * @param type
     * @return the adapter for an existing component according to the above rules, or <code>null</code> if none exists.
     */
    private ComponentAdapter getExistingComponent(int componentHandle, String name, String type)
//      throws ContainerException
    {
        // try same handle
        ComponentAdapter existingCompAdapter = m_activeComponentMap.get(componentHandle);
        if (existingCompAdapter != null)
        {
            String msg = "component with handle " + componentHandle + " ('" + name + "') already activated";
            m_logger.warning(msg);

            // compare name and type to distinguish real identity and handle fraud
            if (existingCompAdapter.getName().equals(name) &&
                existingCompAdapter.getType().equals(type) )
            {
                String msgReuse = "will reuse existing component " + name;
                m_logger.warning(msgReuse);
            }
            else
            {
                String msgMismatch = "component with handle " + componentHandle +
                            ": name and/or type don't match between existing and requested component: " +
                            "existing(" + existingCompAdapter.getName() + ", " + existingCompAdapter.getType() + ") vs. " +
                            "requested(" + name + ", " + type + "). " +
                            "Will deactivate existing component and load the requested.";
                m_logger.warning(msgMismatch);

                deactivate_components(new int[]{componentHandle});
                existingCompAdapter = null;
            }
        }
        else // different handles
        {
            // check for other comp with same name (but different handle).
            // we've had this illegal case happen before,
            // resulting in an exception when attempting to create
            // the new POA for the new component with the same name.
            // Even though this should never happen, we take account of reality (maciManager-bug?)
            // and reuse a component with the same name if we find one.
            existingCompAdapter = m_activeComponentMap.getComponentByNameAndType(name, type);
            if (existingCompAdapter != null)
            {
                int orgHandle = existingCompAdapter.getHandle();

//              // if the original handle was temporarily generated by the container
//              // (as part of the CDB hack deal), then we take the good handle from the manager
//              if (ContainerServicesImplCDBHack.isHackedHandle(orgHandle))
//              {
//                  m_logger.info("component has been activated locally already... will change its handle to '" +
//                  componentHandle + "' as requested by the manager.");
//                  existingCompAdapter.setHandle(componentHandle);
//                  m_activeComponentMap.put(componentHandle, existingCompAdapter);
//                  m_activeComponentMap.remove(orgHandle);
//              }
//              else
//              {
                    String msg = "container refuses to activate component '" + name + "' (" + type + ") " +
                                "since another component with the same name and type " +
                                "has already been activated using a different handle (" + orgHandle + "). " +
                                "Will keep the original handle.";
                    m_logger.warning(msg);
                    // hopefully Manager will accept the original handle returned...
//              }
            }
            else
            {
                // the most common case: all is different, we just leave existingCompAdapter == null
            }
        }

        return existingCompAdapter;
    }


    private ComponentHelper createComponentHelper(String compName, String exe, ClassLoader compCL)
        throws ContainerException
    {
        m_logger.finer("creating component helper instance of type '" + exe + "' using classloader " + compCL.getClass().getName());

        Class compHelperClass = null;
        try  {
            compHelperClass = Class.forName(exe, true, compCL);
        } catch (ClassNotFoundException ex) {
            throw new ContainerException("component helper class '" + exe + "' not found.", ex);
        }

        if (!ComponentHelper.class.isAssignableFrom(compHelperClass)) {
            throw new ContainerException("component helper class '" + exe + "' does not inherit from required base class "
                                            + ComponentHelper.class.getName());
        }

        Constructor helperCtor = null;
        ComponentHelper compHelper = null;
        try {
            helperCtor = compHelperClass.getConstructor(new Class[]{Logger.class});
        } catch (NoSuchMethodException e) {
            String msg = "component helper class '" + exe + "' has no constructor " +
            " that takes a java.util.Logger";
            m_logger.fine(msg);
            throw new ContainerException(msg, e);
        }

        try {
            compHelper = (ComponentHelper) helperCtor.newInstance(new Object[]{m_logger});
        } catch (Exception e) {
            throw new ContainerException("component helper class '" + exe + "' could not be instantiated", e);
        }
        compHelper.setComponentInstanceName(compName);

        return compHelper;
    }


    /////////////////////////////////////////////////////////////
    // Implementation of ContainerOperations#set_component_shutdown_order
    /////////////////////////////////////////////////////////////

    /**
     * Called by the manager to update the container's knowledge about optimum shutdown order
     * of its components.
     * This information will only be needed for a locally initiated container shutdown,
     * since the manager will call {@link #deactivate_components(int[])} in a regular shutdown.
     */
    public void set_component_shutdown_order(int[] handleSeq) {
        String handleSeqString = null;
        if (handleSeq == null) {
            handleSeqString = "null";
        }
        else {
            handleSeqString = "";
            for (int i = 0; i < handleSeq.length; i++) {
                handleSeqString += handleSeq[i] + " ";
            }
            m_activeComponentMap.sort(handleSeq);
        }
        m_logger.fine("set_component_shutdown_order called. Handles: " + handleSeqString);

    }


    /////////////////////////////////////////////////////////////
    // Implementation of ContainerOperations#deactivate_component
    /////////////////////////////////////////////////////////////

    /**
     * Deactivates all components whose handles are given.
     * <p>
     * From maci.idl:
     * <i>Deactivation is the inverse process of activation: component is detached from the POA,
     * and thus made unavailable through CORBA, and its resources are freed.
     * If its code-base is no longer used, it is unloaded from memory.</i>
     * <p>
     *
     * @param handles  a sequence of handles identifying components that are to be released.
     *                  If null, then all active components will be deactivated!
     *
     * @see si.ijs.maci.ContainerOperations#deactivate_components(int[])
     * @see ComponentAdapter#deactivateComponent()
     *
     */
    public void deactivate_components(int[] handles)
    {
        logManagerRequest("received call to deactivate_components", handles);

        // get the component adapters which are all != null, but might be in the wrong state
        ComponentAdapter[] compAdapters = null;
        if (handles == null || handles.length == 0) {
            compAdapters = m_activeComponentMap.getAllComponentAdapters();
        }
        else {
            compAdapters = m_activeComponentMap.getComponentAdapters(handles);
        }

        // synchronization issue: mark the components for deactivation first
        // resulting adapters are all in an appropriate state;
        // another thread coming in with overlapping handles will not get the same components back
        ComponentAdapter[] validAdapters = markAndFilterForDeactivation(compAdapters);

        try
        {
            for (int i = 0; i < validAdapters.length; i++)
            {
                ComponentAdapter compAdapter = validAdapters[i];
                int compHandle = compAdapter.getHandle();

                if (m_logger.isLoggable(Level.FINER)) {
                	m_logger.finer("will deactivate component '" + compAdapter.getName() + "' with handle " + compHandle);
                }

                // todo: check if it's still necessary to delete from the map,
                // now that we have the DEFUNCT state concept
                m_activeComponentMap.remove(compHandle);

                try {
                    // todo: perhaps use thread pool and deactivate a bunch in parallel
                    compAdapter.deactivateComponent();
                    if (m_logger.isLoggable(Level.FINER)) {
                    	m_logger.finer("deactivated component '" + compAdapter.getName() + "' with handle " + compHandle);
                    }
                }
                catch (Exception ex) {
                    m_logger.log(Level.INFO, "failed to properly deactivate component " + compAdapter.getName(), ex);
                }
            }
        }
        catch (Throwable thr)
        {
            m_logger.log(Level.SEVERE, "failed to deactivate at least one component!", thr);
            // TODO: change idl to throw corba exception
        }
    }


    /**
     * Filters out those components from <code>compAdapters</code>
     * which can be deactivated (not DESTROYING | ABORTING | DEFUNCT).
     *
     * Must be synchronized to avoid deactivating the same component more than once.
     * @param compAdapters
     * @return  adapters of components that are ready to be deactivated
     */
    private synchronized ComponentAdapter[] markAndFilterForDeactivation(ComponentAdapter[] compAdapters)
    {
        ArrayList<ComponentAdapter> adaptersForDestruction = new ArrayList<ComponentAdapter>();
        for (int i = 0; i < compAdapters.length; i++)
        {
            try
            {
                ComponentAdapter compAdapter = compAdapters[i];
                ComponentStates state = compAdapter.getComponentStateManager().getCurrentState();
                if (state == ComponentStates.COMPSTATE_DESTROYING ||
                    state == ComponentStates.COMPSTATE_ABORTING ||
                    state == ComponentStates.COMPSTATE_DEFUNCT)
                {
                    m_logger.fine("coming too late to deactivate component " + compAdapter.getName());
                }
                else
                {
                    compAdapter.getComponentStateManager().setStateByContainer(ComponentStates.COMPSTATE_DESTROYING);
                    adaptersForDestruction.add(compAdapter);
                }
            }
            catch (Throwable thr)
            {
                m_logger.log(Level.WARNING, "something went wrong while marking components for destruction. Will continue.", thr);
            }
        }
        return ( adaptersForDestruction.toArray(new ComponentAdapter[adaptersForDestruction.size()]) );
    }


    /////////////////////////////////////////////////////////////
    // Implementation of ContainerOperations#restart_component
    /////////////////////////////////////////////////////////////

    /**
     * Not yet implemented. Left for ACS 3.1 (or later...), see comments at
     * http://almasw.hq.eso.org/almasw/bin/view/ACS/NewMaciIdl, e.g.
     * GianlucaChiozzi - 21 Oct 2003
     * <i>We do not have really clear ideas yet about this.
     * I think that the most accepted ipothesis is that restart of a component means destroy/create
     * while restart of a container means shutdown/restart.
     * For ACS 3.0 I would forget about this issue and leave it for ACS 3.1.</i>
     * <p>
     * from maci idl:
     * <i>Restarts a component. Returns a new reference of the restarted component.</i>
     *
     * @param compHandle   Handle of the component to be restarted.
     * @see si.ijs.maci.ContainerOperations#restart_component(int)
     */
    public org.omg.CORBA.Object restart_component(int compHandle)
    {
        m_logger.warning("method restart_component not yet implemented.");
        return null;
    }


    /////////////////////////////////////////////////////////////
    // Implementation of ContainerOperations#shutdown
    /////////////////////////////////////////////////////////////

    /**
     * Action to take after shutting down (ignored for the time being).
     * Bits 8 thru 15 of this parameter denote the action, which can be one of:
     * <UL>
     * <LI>0 -- reload the container</LI>
     * <LI>1 -- reboot the computer</LI>
     * <LI>2 -- exit the container</LI>
     * </UL>
     *
     * The bits 0 thru 7 (values 0 to 255) are the return value that the Container
     * will pass to the operating system.
     * TODO: get rid of this silly bit-multiplexing
     *
     * @see si.ijs.maci.ContainerOperations#shutdown(int)
     */
    public void shutdown(int encryptedAction)
    {
        shutdown(encryptedAction, true);
    }


    /**
     * Shuts down the container.
     * Can be called either remotely through the CORBA {@link #shutdown(int)} method,
     * or from elsewhere in the container JVM.
     * <p>
     * Depending on the <code>gracefully</code> parameter,
     * either {@link #deactivate_components(int[])} or {@link #abortAllComponents(long)}
     * is called.
     * <p>
     * This method can be called from different threads
     * <ul>
     * <li>one of the ORB threads (shutdown initiated by the manager)
     * <li>shutdown thread (Ctrl-C VM hook)
     * </ul>
     *
     * @param encryptedAction  ignored for the time being (always EXIT)
     * @param gracefully    if true, this method only returns after <code>cleanUp</code>
     *                      has been called on all components.
     *                      if false, it returns faster, running the components' abort methods
     *                      in separate threads for at most 3 seconds.
     * @see #shutdown(int)
     * @see ShutdownHook
     */
    void shutdown(int encryptedAction, boolean gracefully)
    {
        int action = (encryptedAction >> 8) & 0xFF;
        m_logger.info("received call to 'shutdown', action=" + action +
                        " (encryptedAction=" + encryptedAction +
                        "), gracefully=" + gracefully + ".");

        if (isShuttingDown())
        {
            m_logger.fine("call to shutdown() while shutting down will be ignored...");
            return;
        }
        setShuttingDown(true);
        m_managerProxy.shutdownNotify();

        // shut down all active components
        if (gracefully)
        {
            deactivate_components(null);
        }
        else
        {
            abortAllComponents(3000);
        }

        m_managerProxy.logoutFromManager();

        if (!isEmbedded) {
            ClientLogManager.getAcsLogManager().shutdown(gracefully);
            // in regular shutdown situations, we wait for the ORB to be destroyed.
            // Otherwise starting another container soon after will fail with RootPOA init problems.
            // in any case, the following call will return immediately, so that the container#shutdown() method can return.
            // Once the ORB is down, the main thread will unblock, and bring the container to an end.
            m_acsCorba.shutdownORB(gracefully);
        }

        // to allow creation of a new container in the same VM
        s_instance = null;
    }


    /**
     * Aborts all components that are not already aborting
     * or already defunct.
     * <p>
     * This method runs the components' <code>aboutToAbort</code> methods
     * concurrently in separate daemon threads.
     * It returns when all components have been aborted, or when the abortion
     * is timed out after <code>maxWaitTimeMillis</code>.
     * <p>
     * @param maxWaitTimeMillis   maximum time in milliseconds spent in <code>aboutToAbort</code> of all components.
     * @see ComponentLifecycle#aboutToAbort()
     * @see ComponentAdapter#getComponentAbortionist(boolean)
     */
    private synchronized void abortAllComponents(long maxWaitTimeMillis)
    {
        // just to verify the threading situation
        m_logger.info("will shut down all active components, thread=" +
                        Thread.currentThread().getName());

        boolean didAbort = false;

        ComponentAdapter[] compAdapters = m_activeComponentMap.getAllComponentAdapters();

        // Current ad-hoc policy:
        // * half of the components (corePoolSize) will be aborted in freshly created threads.
        // * the other half will go in a queue, which is polled by the threads once they are done with their initial tasks.
        // Todo: think about a better policy to limit the max thread number.
        // For this we need experience with typical time demands for abort methods, vs. resource issues from excessive thread creation.
        int corePoolSize = compAdapters.length / 2 + 1;
        int maxThreadNumber = compAdapters.length + 1; // will not be needed with the current queue capacity (but important for "sick" cases like compAdapters.length == 1)
        int queueCapacity = compAdapters.length / 2 + 1;

        // From ThreadPoolExecutor API:
        // A bounded queue (for example, an ArrayBlockingQueue) helps prevent resource exhaustion when used with finite maximumPoolSizes,
        // but can be more difficult to tune and control. Queue sizes and maximum pool sizes may be traded off for each other:
        // Using large queues and small pools minimizes CPU usage, OS resources, and context-switching overhead, but can lead to
        // artificially low throughput.
        // If tasks frequently block (for example if they are I/O bound), a system may be able to schedule time for more threads than you otherwise allow.
        // Use of small queues generally requires larger pool sizes, which keeps CPUs busier but may encounter unacceptable scheduling overhead,
        // which also decreases throughput.
        BlockingQueue<Runnable> execQueue = new ArrayBlockingQueue<Runnable>(queueCapacity);

        ExecutorService executor = new ThreadPoolExecutor(corePoolSize, maxThreadNumber,
                60L, TimeUnit.SECONDS,
                execQueue,
                new DaemonThreadFactory() );

        for (int i = 0; i < compAdapters.length; i++) {
            ComponentAdapter compAdapter = compAdapters[i];
            ComponentStates state = compAdapter.getComponentStateManager().getCurrentState();

            if (state != ComponentStates.COMPSTATE_ABORTING &&
                state != ComponentStates.COMPSTATE_DEFUNCT )
            {
                didAbort = true;

                // don't waste time to kill the component POA separately
                // (will be killed along with its parent POA "ComponentPOA" at shutdown)
                Runnable abortionist = compAdapter.getComponentAbortionist(false);
                try {
                    // run in a separate thread
                    executor.execute(abortionist);
                }
                catch (RejectedExecutionException e) {
                    System.err.println("failed to abort component '" + compAdapter.getName() + "' because of exception " + e.getMessage());
                }
            }
        }
        executor.shutdown();
        boolean doneInTime = false;
        try {
            // block until all threads are finished or timeout occurs
            doneInTime = executor.awaitTermination(maxWaitTimeMillis, TimeUnit.MILLISECONDS);
        }
        catch (InterruptedException ie) {
        	// must go on
        }

        if (didAbort) {
            String msg = "done with aborting components ";
            if (doneInTime) {
                msg += "in time.";
            }
            else {
                msg += "(timed out).";
            }
            m_logger.info(msg);
//            System.out.println(msg);
        }
        else {
            String msg = "no components needed to be aborted.";
            m_logger.info(msg);
//            System.out.println(msg);
        }
    }


    /////////////////////////////////////////////////////////////
    // Implementation of ContainerOperations#get_component_info
    /////////////////////////////////////////////////////////////

    /**
     * Returns information about a subset of components that are currently hosted by the Container.
     * Note: If the list of handles is empty, information about
     * all components hosted by the container is returned!
     *
     * @param handles
     * @return Information about the selected components.
     * @see si.ijs.maci.ContainerOperations#get_component_info(int[])
     **/
    public ComponentInfo[] get_component_info(int[] handles)
    {
        logManagerRequest("received call to get_component_info", handles);

        // get those ComponentInfos
        List<ComponentInfo> componentInfolist = new ArrayList<ComponentInfo>();

        ComponentAdapter[] adapters = null;
        if (handles != null && handles.length > 0) {
            adapters = m_activeComponentMap.getComponentAdapters(handles);
        }
        else {
            adapters = m_activeComponentMap.getAllComponentAdapters();
        }

        for (int i = 0; i < adapters.length; i++) {
            ComponentAdapter adapter = adapters[i];
            componentInfolist.add(adapter.getComponentInfo());
        }
        ComponentInfo[] ret = componentInfolist.toArray(new ComponentInfo[0]);

        return ret;
    }



    /////////////////////////////////////////////////////////////
    // Implementation of ClientOperations methods
    /////////////////////////////////////////////////////////////

    /**
     * @see si.ijs.maci.ClientOperations#name()
     */
    public String name()
    {
        m_logger.fine("call to name() answered with '" + m_containerName + "'.");
        return m_containerName;
    }


    /**
     * Disconnect notification.
     * The disconnect method is called by the Manager to notify the client
     * that it will be unavailable and that the client should log off.
     * @see si.ijs.maci.ClientOperations#disconnect()
     */
    public void disconnect()
    {
        m_logger.warning("Manager requests logout...");
        m_managerProxy.logoutFromManager();
        try
        {
            // to give the manager 10 sec time to go down or whatever
            Thread.sleep(10000);
        }
        catch (InterruptedException e1)
        {// ignore
        }

//      m_recoveryStart = true; // check discussion at http://almasw.hq.eso.org/almasw/bin/preview/ACS/NewMaciIdl

        try
        {
            // will loop until manager ref becomes available
            loginToManager();
        }
        catch (Exception e)
        {
            m_logger.log(Level.WARNING, "Failed to re-login to the manager. Will shut down.", e);
            shutdown(AcsContainer.CONTAINER_EXIT << 8, true);
        }
    }


    /**
     * Authentication method.
     * Below some passages from maci.idl:
     * <p>
     * Method authenticate is the challenge issued to the client after it tries to login.
     * The login will be successful if the client's authenticate() produces the expected result.
     * Only in this case will the Manager's login method return a valid handle,
     * which the client will later use as the id parameter with all calls to the Manager.
     * <p>
     *  The first character of the answer identifies the type of the client, and can be one of:
     *    <UL>
     *      <LI><TT>C</TT> A regular client (implements just the Client interface).</LI>
     *      <LI><TT>A</TT> A container (implements the Container interface).</LI>
     *      <LI><TT>AR</TT> A container with recovery capability (implements the Container interface). </LI>
     *      <LI><TT>S</TT> Supervisor (implements the Administrator interface).</LI>
     *    </UL>
     * <p>
     * Container may support recovery. If the container terminates unexpectedly,
     * and then recovers (after a reboot, for example), it logs in to the Manager and notifies
     * it that it supports recovery by responding with "AR" to the Client::authenticate method.
     * The Manager then uses the activate_component method on the container to bring all the components
     * the container used to have back to life.
     *
     * @return Answer to the question.
     * @see si.ijs.maci.ClientOperations#authenticate(String)
     */
    public String authenticate(String question)
    {
        String code = ( useRecoveryMode ? "AR" : "A" );
        String answer = code + ' ' + m_containerName;
        m_logger.fine("call to authenticate() answered with '" + answer + "'.");
        return answer;
    }


    /**
     * @see si.ijs.maci.ClientOperations#message(short, String)
     */
    public void message(short type, String message)
    {
        if (type == ClientOperations.MSG_ERROR)
        {
            m_logger.warning("Error message from the manager: " + message);
        }
        else if (type == ClientOperations.MSG_INFORMATION)
        {
            m_logger.info("Info message from the manager: " + message);
        }
        else
        {
            m_logger.info("Message of unknown type from the manager: " + message);
        }
    }


    /**
     * Replies with <code>true</code> so that Manager sees that this container is alive.
     * Prints a message to System.out, so that it's visible on the local console that the container is doing ok.
     * Does not log anything, because a failure to reply would show up remotely anyway.
     * @see si.ijs.maci.ClientOperations#ping()
     */
    public boolean ping()
    {
        Runtime rt = Runtime.getRuntime();
        long totalMemKB = rt.totalMemory()/1024;
        long usedMemKB = totalMemKB - rt.freeMemory()/1024;
        String memStatus = "Memory usage " + usedMemKB + " of " + totalMemKB + " kB ";
        long maxMem = rt.maxMemory();
        if (maxMem < Long.MAX_VALUE) {
            long maxMemKB = maxMem/1024;
            memStatus += "(= " + (usedMemKB*1000/maxMemKB)/10.0 + "% of JVM growth limit " + maxMemKB + " kB) ";
        }
        System.out.println("ping received, container alive. " + memStatus);
        return true;
    }


    /**
     * Notify client about the change (availability) of the components currently in use by this client.
     * For administrative clients, notification is issued for the change of availability
     * of any component in the domain.
     * @see si.ijs.maci.ClientOperations#components_available(ComponentInfo[])
     */
    public void components_available(ComponentInfo[] components)
    {
        if (components == null) {
            return;
        }
        StringBuffer buff = new StringBuffer("components_available: ");
        for (int i = 0; i < components.length; i++)
        {
            buff.append(components[i].name).append(" ");
        }
        m_logger.fine(buff.toString());

        // todo: forward the call to the affected components
        // allow components to register callback handler for this, see ongoing discussions
    }

    /**
     * @see si.ijs.maci.ClientOperations#components_unavailable(String[])
     */
    public void components_unavailable(String[] component_names)
    {
        if (component_names == null) {
            return;
        }
        StringBuffer buff = new StringBuffer("components_unavailable: ");
        for (int i = 0; i < component_names.length; i++)
        {
            buff.append(component_names[i]).append(" ");
        }
        m_logger.fine(buff.toString());

        // todo: forward the call to the affected components
        // allow components to register callback handler for this, see ongoing discussions
    }


    /////////////////////////////////////////////////////////////
    // other (util stuff)
    /////////////////////////////////////////////////////////////

    /**
     * Logs a request from the manager which involves an array of component handles.
     * (which is quite different from a LogManager request...)
     * <p>
     * Checks if the handles are valid.
     * If no component can be found for a given handle, the string <code>(unknown) </code>
     * is appended to that handle.
     * <p>
     * Used simply to avoid code duplication.
     *
     * @param msgBegin  begin of the log message
     * @param handles  handles to components that this container should know of
     */
    private void logManagerRequest(String msgBegin, int[] handles)
    {
        if (handles != null) {
            StringBuffer buff = new StringBuffer(msgBegin + "; handles = ");
            for (int i = 0; i < handles.length; i++)
            {
                buff.append(handles[i]);

                ComponentAdapter compAdapter = m_activeComponentMap.get(handles[i]);
                if (compAdapter == null)
                {
                    buff.append("(unknown) ");
                }
                else
                {
                    buff.append(' ');
                }
            }
            m_logger.fine(buff.toString());
        }
        else {
            m_logger.fine(msgBegin);
        }
    }


    /**
     * Getter method required for thread-safe use of m_shuttingDown
     * @return
     */
    private synchronized boolean isShuttingDown()
    {
        return m_shuttingDown;
    }

    /**
     * Setter method required for thread-safe use of m_shuttingDown
     * @param shuttingDown
     */
    private synchronized void setShuttingDown(boolean shuttingDown)
    {
        m_shuttingDown = shuttingDown;
    }





//  /**
//   * This method is copied over from the Abeans implementation and kept around for future copy/paste.
//   * Don't call it.
//   */
//  private void refreshState() //throws InitializationException
//  {
    //
    //      if (java.beans.Beans.isDesignTime())
    //      {
    //          System.out.println("ACS connector> can NOT be initialized during design time!");
    //          return;
    //      }
    //
    //      getConnectorInformation().setDefaultProperties(props);
    //      ORBdebug =
    //          props.getProperty(ACSConnectorInformation.ACS_ORB_DEBUG).equals("true")
    //              ? true
    //              : false;
    //      if (getIdentifier().isDebug())
    //          new DebugEntry(this, "Connector properties are: " + props).dispatch();
    //      m_managerLoc = props.getProperty(ACSConnectorInformation.ACS_MANAGER_CORBALOC);
    //
    //      String managerLoc1 =
    //          System.getProperty(ACSConnectorInformation.ACS_MANAGER_CORBALOC);
    //      if (managerLoc1 != null)
    //      {
    //          m_managerLoc = managerLoc1;
    //          new DebugEntry(
    //              this,
    //              "Using Manager corbaloc provided directly to the JVM through -D option: '"
    //                  + managerLoc1
    //                  + "'.")
    //              .dispatch();
    //      }
    //      else
    //      {
    //          /*      new DebugEntry(this, "Manager corbaloc is null!").dispatchError();
    //                  throw new InitializationException("Manager corbaloc is null!");
    //          */
    //          ManagerDialog diag = new ManagerDialog();
    //          if (m_managerLoc != null)
    //              diag.setManagerFieldText(m_managerLoc);
    //          diag.show();
    //          m_managerLoc = diag.getManagerFieldText();
    //      }
    //      if (m_managerLoc == null || "".equals(m_managerLoc))
    //          throw new InitializationException("'ManagerCORBALoc' property is not set. Aborting...");
    //
    //      // ORB stanza
    //      java.util.Properties props1 = java.lang.System.getProperties();
    //      props1.put("org.omg.CORBA.ORBClass", "com.ooc.CORBA.ORB");
    //      props1.put("org.omg.CORBA.ORBSingletonClass", "com.ooc.CORBA.ORBSingleton");
    //      if (ORBdebug)
    //          props1.put("ooc.orb.trace.connections", "2");
    //
    //      java.lang.System.setProperties(props1);
    //      orb = org.omg.CORBA.ORB.init(new String[0], props1);
    //
    //      new DebugEntry(this, "ORB initialized.").dispatch();
    //
    //      // POA stanza -- use RootPOA
    //      POA rootPOA = null;
    //      try
    //      {
    //          rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
    //          new DebugEntry(this, "RootPOA initialized.").dispatch();
    //      }
    //      catch (org.omg.CORBA.ORBPackage.InvalidName in)
    //      {
    //          throw new InitializationException("Cannot resolve RootPOA: " + in);
    //      }
    //      POAManager manager = rootPOA.the_POAManager();
    //      try
    //      {
    //          manager.activate();
    //          orbThread = new Thread(this);
    //          orbThread.start();
    //      }
    //      catch (Exception e)
    //      {
    //          throw new InitializationException("POAManager activation failed.", e);
    //      }
    //      /*  org.omg.CORBA.Object mgrfObj = orb.resolve_initial_reference("POAManagerFactory");
    //          com.ooc.OBPortableServer.POAManagerFactory mgrFactory = com.ooc.OBPortableServer.POAMamangerFactoryHelper.narrow(mgrfObj);
    //          POAManager myManager = mgrFactory.create_poa_manager("AbeansPOAManager");
    //          Policy[] policies = new Policy[0];
    //          POA myPOA = rootPOA.create_POA("AbeansPOA", myManager, policies);
    //      */
    //
    //      connectToManager();
    //      //  refreshDeviceLists();
    //
    //      //  initialize logging
    //      org.omg.CORBA.IntHolder holder = new org.omg.CORBA.IntHolder();
    //      try
    //      {
    //          log = LogHelper.narrow(getManager().get_COB(handle, "Log", true, holder));
    //          logStatus = holder.value;
    //      }
    //      catch (Exception e)
    //      {
    //          new DebugEntry(
    //              this,
    //              "Failed to connect to log. Remote logging will be disabled: " + e)
    //              .dispatchError();
    //          logStatus = LOG_FAILED;
    //      }
    //
    //      String domainFile = props.getProperty(ACSConnectorInformation.ACS_DOMAINS_FILE);
    //      if (domainFile == null || "".equals(domainFile))
    //      {
    //          if (getIdentifier().isDebug())
    //              new DebugEntry(
    //                  this,
    //                  "CORBA domains file does not exist. Using hard-coded defaults.")
    //                  .dispatch();
    //          useDefaultDomains();
    //      }
    //      else
    //      {
    //          StreamTokenizer tok = null;
    //          try
    //          {
    //              tok = new StreamTokenizer(new BufferedReader(new FileReader(domainFile)));
    //          }
    //          catch (Exception e)
    //          {
    //              new DebugEntry(
    //                  this,
    //                  "CORBA domains file is configured but does not exist. Using hard-coded defaults.")
    //                  .dispatch();
    //              useDefaultDomains();
    //          }
    //          HashMap domainsmap = new HashMap();
    //          ArrayList ordering = new ArrayList();
    //          ArrayList currentList = null;
    //          if (tok != null)
    //          {
    //              tok.eolIsSignificant(true);
    //              tok.wordChars('*', '*');
    //              tok.wordChars('_', '_');
    //              tok.wordChars('?', '?');
    //              int count = 0;
    //              try
    //              {
    //                  while (tok.nextToken() != tok.TT_EOF)
    //                  {
    //                      if (tok.ttype == tok.TT_WORD)
    //                      {
    //                          if (count == 0)
    //                          {
    //                              if (domainsmap.get(tok.sval) != null)
    //                                  throw new IllegalArgumentException("Two lines that begin with the same domain name cannot exist.");
    //                              else
    //                              {
    //                                  currentList = new ArrayList();
    //                                  domainsmap.put(tok.sval, currentList);
    //                                  ordering.add(tok.sval);
    //                              }
    //                          }
    //                          else
    //                              currentList.add(tok.sval);
    //                          count++;
    //                      }
    //                      else if (tok.ttype == tok.TT_EOL)
    //                      {
    //                          count = 0;
    //                      }
    //                  }
    //                  domains = new String[ordering.size()];
    //                  for (int i = 0; i < domains.length; i++)
    //                      domains[i] = (String) ordering.get(i);
    //                  //      domainsmap.keySet().toArray(domains);
    //                  domainTags = new String[domains.length][];
    //                  for (int i = 0; i < domains.length; i++)
    //                  {
    //                      ArrayList list = (ArrayList) domainsmap.get(domains[i]);
    //                      domainTags[i] = new String[list.size()];
    //                      list.toArray(domainTags[i]);
    //                  }
    //                  StringBuffer buf = new StringBuffer(500);
    //                  for (int i = 0; i < domains.length; i++)
    //                  {
    //                      domains[i] = domains[i].replace('_', ' ');
    //                      buf.append(domains[i]);
    //                      buf.append(' ');
    //                      //      System.out.println(domains[i]);
    //                  }
    //                  new DebugEntry(this, "User defined domains are: " + buf).dispatch();
    //              }
    //              catch (Exception e)
    //              {
    //                  new DebugEntry(
    //                      this,
    //                      "Error while parsing the domains file: "
    //                          + e
    //                          + " Hard-coded defaults will be used.")
    //                      .dispatch();
    //                  useDefaultDomains();
    //              }
    //          }
    //      }
//  }



}




