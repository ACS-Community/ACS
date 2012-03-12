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
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.PortableServer.Servant;

import si.ijs.maci.AuthenticationData;
import si.ijs.maci.CBComponentInfo;
import si.ijs.maci.ClientOperations;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.Container;
import si.ijs.maci.ContainerHelper;
import si.ijs.maci.ContainerOperations;
import si.ijs.maci.ContainerPOA;
import si.ijs.maci.ImplLangType;
import si.ijs.maci.LoggingConfigurablePackage.LogLevels;
import alma.ACS.ACSComponentOperations;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.ComponentStates;
import alma.ACSErrTypeCommon.IllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnknownEx;
import alma.AcsContainerLog.LOG_CompAct_Corba_OK;
import alma.AcsContainerLog.LOG_CompAct_Init_OK;
import alma.AcsContainerLog.LOG_CompAct_Instance_OK;
import alma.AcsContainerLog.LOG_CompAct_Loading_OK;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.classloading.AcsComponentClassLoader;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.corba.AcsCorba;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigException;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.StopWatch;
import alma.acs.util.UTCUtility;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.maci.loggingconfig.UnnamedLogger;
import alma.maciErrType.CannotActivateComponentEx;
import alma.maciErrType.CannotRestartComponentEx;
import alma.maciErrType.ComponentDeactivationFailedEx;
import alma.maciErrType.ComponentDeactivationUncleanEx;
import alma.maciErrType.LoggerDoesNotExistEx;
import alma.maciErrType.wrappers.AcsJCannotActivateComponentEx;
import alma.maciErrType.wrappers.AcsJCannotRestartComponentEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationFailedEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationUncleanEx;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALHelper;

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
    private static volatile AcsContainer s_instance;

    /**
     * Start time, used during login to the manager.
     */
    private final long startTimeUTClong;
    
    private final String m_containerName;

    /** 
     * An ID assigned by the manager at first login. 
     * This ID survives logout/login to the manager.
     * For container shutdown/crashes, a new ID is assigned the following login. 
     */
    private long executionId = -1; 
    
    private final AcsManagerProxy m_managerProxy;

    private final AcsCorba m_acsCorba;

    private final boolean isEmbedded;

    private final AcsLogger m_logger;

    private final ThreadFactory containerThreadFactory;
    
	final ThreadPoolExecutor threadPoolExecutor;

	private final ComponentMap m_activeComponentMap;

    private ContainerServicesImpl m_alarmContainerServices;

	/**
	 * Gate that blocks some (currently: only activate_component and LoggingConfigurable methods) 
	 * incoming calls while the container is initializing itself and therefore not fully ready.
	 * In particular, this will be used to hold component activation requests for autostart components,
	 * which the manager sends right after the container has logged in to the manager.
	 */
	private final CountDownLatch containerStartOrbThreadGate;

    /**
     * Singleton logging config object shared with ClientLogManager.
     */
    private LogConfig logConfig;
    
    /**
     * Cache for method {@link #getCDB()}. Don't use this field directly.
     */
    private DAL cdb;

	private final AtomicBoolean shuttingDown = new AtomicBoolean(false);

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
	 * Constructor which creates a container that is registered as a CORBA object, but not yet logged in to the manager
	 * (for that, call {@link #initialize()}.
	 * 
	 * @param containerName
	 * @param acsCorba
	 * @param managerProxy
	 * @param isEmbedded
	 *            true if this container runs within an application. Affects shutdown behavior.
	 * @throws AcsJContainerEx
	 *             if anything goes wrong, or if another instance of this class has already been created.
	 */
	AcsContainer(String containerName, AcsCorba acsCorba, AcsManagerProxy managerProxy, boolean isEmbedded)
			throws AcsJContainerEx {
		if (s_instance == null) {
			startTimeUTClong = UTCUtility.utcJavaToOmg(System.currentTimeMillis());
			m_containerName = containerName;
			containerStartOrbThreadGate = new CountDownLatch(1);
			m_managerProxy = managerProxy;
			this.isEmbedded = isEmbedded;
			m_logger = ClientLogManager.getAcsLogManager().getLoggerForContainer(containerName);
			containerThreadFactory = new CleaningDaemonThreadFactory(m_containerName, m_logger, "Container");
			
			// @TODO: Use "jacorb.poa.thread_pool_max" for max number of activation threads, 
			// but without direct coupling to jacorb-specific variable.
			threadPoolExecutor = new ThreadPoolExecutor(10, 10, 3, TimeUnit.MINUTES, 
					new LinkedBlockingQueue<Runnable>(), containerThreadFactory, 
					new ThreadPoolExecutor.AbortPolicy());
			threadPoolExecutor.allowCoreThreadTimeOut(true);
			
			m_activeComponentMap = new ComponentMap(m_logger);
			m_acsCorba = acsCorba;

			// @TODO: Check with C++ and Py container how they use ContainerStatusORBInitBeginMsg.
			// Here we interpret is as registering the container object with the ORB, 
			// but it could mean "initialization of the ORB itself", and then these logs should move to 
			// AcsContainerRunner.run(String[]), around initCorba and runCorba
			System.out.println(ContainerOperations.ContainerStatusORBInitBeginMsg); 
			registerWithCorba();
			System.out.println(ContainerOperations.ContainerStatusORBInitEndMsg);

			s_instance = this;
		} else {
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo("illegal attempt to create more than one instance of " + AcsContainer.class.getName()
					+ " inside one JVM.");
			throw ex;
		}
	}

	/**
	 * Container initialization such as logging in to the manager, configuring logging, initializing the alarm system.
	 * This is taken out of the ctor just to keep is lean and be able to instantiate a minimum container for testing.
	 * 
	 * @throws AcsJContainerServicesEx
	 */
	void initialize() throws AcsJContainerEx {

		System.out.println(ContainerOperations.ContainerStatusMgrInitBeginMsg);
		loginToManager();
		System.out.println(ContainerOperations.ContainerStatusMgrInitEndMsg);
		
		// init logging 
		logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
		// logConfig.setInternalLogger(m_logger);
		logConfig.setCDBLoggingConfigPath("MACI/Containers/" + m_containerName);
		logConfig.setCDB(getCDB());
		try {
			logConfig.initialize(false);
		} catch (LogConfigException ex) {
			// if the CDB can't be read, we still want to run the container, thus we only log the problem here
			m_logger.log(Level.FINE, "Failed to configure logging (default values will be used).", ex);
		}

		double timeoutSeconds;
		try {
			// should give the configured timeout, or the schema default if the container config exists but no timeout is configured there.
			timeoutSeconds = getCDB().get_DAO_Servant("MACI/Containers/"+m_containerName).get_double("Timeout");
		} catch (Exception ex) {
			// container config does not exist at all. Use schema-defined default value. 
			timeoutSeconds = new alma.maci.containerconfig.Container().getTimeout();
		}
		try {
			m_acsCorba.setORBLevelRoundtripTimeout(timeoutSeconds);
		} catch (Exception ex) {
			m_logger.log(Level.WARNING, "No CDB timeout applied to the container.", ex);
		}
		
		// init the alarm system
		//
		// TODO: clean up the construction of CS which is ad-hoc implemented right before ACS 7.0
		// in order to allow CERN alarm libs to get their static field for ContainerServices set.
		// Currently the alarm system acts under the container name.
		// Setting of static fields in the AS classes should be removed altogether.


		// TODO: maybe we should pass a alarms@container logger instead of the normal one
		m_alarmContainerServices = new ContainerServicesImpl(m_managerProxy, m_acsCorba.createPOAForComponent("alarmSystem"),
        		m_acsCorba, m_logger, 0, m_containerName, null, containerThreadFactory) {
			private AcsLogger alarmLogger;
        	public AcsLogger getLogger() {
                if (alarmLogger == null) {
                	// @TODO perhaps get a container logger "alarms@containername"
                	alarmLogger = ClientLogManager.getAcsLogManager().getLoggerForContainer(getName());
                }
                return alarmLogger;
        	}
		};

		try {
			ACSAlarmSystemInterfaceFactory.init(m_alarmContainerServices);
		} catch (Throwable thr) {
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo("Error initializing the alarm system factory");
			throw ex;
		}

		// enable (throttle) alarms from the logging subsystem
		ClientLogManager.LogAlarmHandler logAlarmHandler = new ClientLogManager.LogAlarmHandler() {
			@Override
			public void raiseAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJCouldntPerformActionEx {
				try {
					m_alarmContainerServices.getAlarmSource().raiseAlarm(faultFamily, faultMember, faultCode);
				} catch (AcsJContainerServicesEx e) {
					throw new AcsJCouldntPerformActionEx(e);
				}
			}
			@Override
			public void clearAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJCouldntPerformActionEx {
				try {
					m_alarmContainerServices.getAlarmSource().clearAlarm(faultFamily, faultMember, faultCode);
				} catch (AcsJContainerServicesEx e) {
					e.printStackTrace();
				}
			}
		};
		ClientLogManager.getAcsLogManager().enableLoggingAlarms(logAlarmHandler);

		
		// init the BACI framework
		try {
			Class<?> clazz = Class.forName("alma.ACS.jbaci.BACIFramework");
			Object baciFramework = clazz.getField("INSTANCE").get(null);
			clazz.getMethod("initialize", ThreadFactory.class).invoke(baciFramework, containerThreadFactory);
		} catch(Exception e) {
			AcsJContainerEx ex = new AcsJContainerEx(e);
			ex.setContextInfo("Error initializing the BACI framework");

			// TODO: This is temporary, just to avoid test crashes. The exception should be actually thrown
			m_logger.log(AcsLogLevel.WARNING, "Error initializing the BACI framework, container will run withouth the BACI Framework initialized", ex);
			//throw ex;
		}

		// unleash any waiting ORB threads that were held until container init has finished
		containerStartOrbThreadGate.countDown();
	}
    
    /**
	 * Gets a reference to the CDB. Reuses the previously obtained reference.
	 * Implemented as on-demand remote call, so always use this method instead
	 * of directly accessing the field {@link #cdb}.
	 * <p>
	 * @TODO: reuse this CDB reference in ContainerServicesImpl for method getCDB()
	 * <p>
	 * @TODO: Perhaps register for change notification at {@link DAL#add_change_listener(com.cosylab.CDB.DALChangeListener)}.
	 * (Currently Alma does not use live CDB updates, but the feature is there...)
	 * 
	 * @return the CDB reference, or <code>null</code> if it could not be obtained.
	 */
    DAL getCDB() {
        if (cdb != null) {
            return cdb;
        }
        
        try {
            // manager's get_service contains get_component, so even if the CDB becomes a real component, we can leave this 
            org.omg.CORBA.Object dalObj = m_managerProxy.get_service("CDB", true);
            cdb = DALHelper.narrow(dalObj);
            if (cdb == null) {
                m_logger.log(Level.WARNING, "Failed to access the CDB.");
            }
        }
        catch (Exception e) {
            m_logger.log(Level.WARNING, "Failed to access the CDB.", e);
        }
        
        return cdb;
    }


    void setRecoveryMode(boolean recoveryStart) {
        useRecoveryMode = recoveryStart;
    }


    /**
	 * To be called only once from the ctor.
	 * 
	 * @throws AcsJContainerEx
	 */
	private void registerWithCorba() throws AcsJContainerEx {
		// activate the Container as a CORBA object.
		org.omg.CORBA.Object obj = m_acsCorba.activateContainer(this, m_containerName);

		if (obj == null) {
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo("failed to register this AcsContainer with the ORB.");
			throw ex;
		}

		Container container;
		try {
			container = ContainerHelper.narrow(obj);
			if (container == null) {
				throw new NullPointerException("Container CORBA-narrow returned a null.");
			}
		} catch (Throwable thr) {
			AcsJContainerEx ex = new AcsJContainerEx();
			ex.setContextInfo("failed to narrow the AcsContainer to CORBA type Container.");
			throw ex;
		}

		m_logger.finer("AcsContainer successfully registered with the ORB as a Container");
	}


    
    
    /**
     * Will attempt to log into the manager.
     * If the manager reference is not available, will enter a loop and keep trying.
     * If login fails on an available manager, will throw a AcsJContainerServicesEx.
     *
     * @throws AcsJContainerServicesEx
     */
    protected void loginToManager() throws AcsJContainerEx {
        m_managerProxy.loginToManager(m_acsCorba.getContainerCorbaRef(this), true);
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
     * the container identifies the dependencies by querying the executable and loads them automatically.
     * Once the code is loaded, it is asked to construct a servant of a given type.
     * The servant is then initialized with the Configuration Database (CDB) and Persistance Database (PDB) data.
     * The servant is attached to the component, and a reference to it is returned.
     * </i>
     * <p>
     * @param componentHandle  handle of the component that is being activated. This handle is used
     *              by the component when it will present itself to the Manager.
     *              The component is expected to remember this handle for its entire life-time.
     * @param execution_id              
     * @param compName  name of the component to instantiate (instance name, comes from CDB)
     * @param exe   component helper implementation class; must be a subclass of
     *               {@link alma.acs.container.ComponentHelper}.
     * @param type  the type of the component to instantiate (Corba IR id).
     * @return   Returns the reference to the object that has just been activated.
     *               If the component could not the activated, a nil reference is returned.
     *
     * @see si.ijs.maci.ContainerOperations#activate_component(int, String, String, String)
     */
	public ComponentInfo activate_component(int componentHandle, long execution_id, String compName, String exe, String type)
		throws CannotActivateComponentEx
	{
		// reject the call if container is shutting down
		if (shuttingDown.get()) {
			String msg = "activate_component() rejected because of container shutdown.";
			m_logger.fine(msg);
			AcsJCannotActivateComponentEx ex = new AcsJCannotActivateComponentEx();
			ex.setCURL(compName);
			ex.setDetailedReason(msg);
			throw ex.toCannotActivateComponentEx();
		}

		ComponentInfo componentInfo = null;
		
		StopWatch activationWatch = new StopWatch(m_logger);

		// to make component activations stick out in the log list
		m_logger.finer("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<");
		m_logger.fine("activate_component: handle=" + componentHandle + " name=" + compName + " helperClass=" + exe
				+ " type=" + type);

		// if the container is still starting up, then hold the request until the container is ready
		boolean contInitWaitSuccess = false; 
		try {
			contInitWaitSuccess = containerStartOrbThreadGate.await(30, TimeUnit.SECONDS);
		} catch (InterruptedException ex1) {
			// just leave contInitWaitSuccess == false
		}
		if (!contInitWaitSuccess) {
			String msg = "Activation of component " + compName + " timed out after 30 s waiting for the container to finish its initialization.";
			m_logger.warning(msg);
			AcsJCannotActivateComponentEx ex = new AcsJCannotActivateComponentEx();
			ex.setCURL(compName);
			ex.setDetailedReason(msg);
			throw ex.toCannotActivateComponentEx();
		}

		ComponentAdapter compAdapter = null;
        try
        {
            synchronized (m_activeComponentMap) {
                ComponentAdapter existingCompAdapter = getExistingComponent(componentHandle, compName, type);
                if (existingCompAdapter != null) {
                    return existingCompAdapter.getComponentInfo();
                }
                else if (!m_activeComponentMap.reserveComponent(componentHandle)) {
                	AcsJContainerEx ex = new AcsJContainerEx();
                	ex.setContextInfo("Component with handle '" + componentHandle +
                                "' is already being activated by this container. Manager should have prevented double activation.");
                	throw ex;
                }
            }

            ClassLoader compCL = null;
            // the property 'acs.components.classpath.jardirs' is set by the script acsStartContainer
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
            StopWatch compStopWatch = new StopWatch();
            ComponentLifecycle compImpl = compHelper.getComponentImpl();
            LOG_CompAct_Instance_OK.log(m_logger, compName, compStopWatch.getLapTimeMillis());
//m_logger.finest(compName + " component impl created, with classloader " + compImpl.getClass().getClassLoader().getClass().getName());

            Class<? extends ACSComponentOperations> operationsIFClass = compHelper.getOperationsInterface();
            Constructor<? extends Servant> poaTieCtor =
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

                if( !Proxy.isProxyClass(operationsIFImpl.getClass()) && !(operationsIFImpl instanceof ExternalInterfaceTranslator) )
                	m_logger.log(AcsLogLevel.NOTICE,"interface translator proxy for component " + compName + " isn't " +
                			"the default one, and doesn't expose the default as one either. This may cause problem when invoking " +
                			"xml-aware offshoot getters");
            }

            // make it a tight container (one that intercepts functional method calls)
            String[] methodsExcludedFromInvocationLogging = compHelper.getComponentMethodsExcludedFromInvocationLogging();
            Object poaDelegate = ContainerSealant.createContainerSealant(
                operationsIFClass, operationsIFImpl, compName, false, m_logger, compCL, methodsExcludedFromInvocationLogging);

            // construct the POATie skeleton with operationsIFImpl as the delegate object
            Servant servant = null;
            try {
                servant = poaTieCtor.newInstance(new Object[]{poaDelegate});
            }
            catch (Throwable thr) {
            	AcsJContainerEx ex = new AcsJContainerEx(thr);
            	ex.setContextInfo("failed to instantiate the servant object for component " +
                                compName + " of type " + compImpl.getClass().getName());
                throw ex;
            }

            //
            // administrate the new component
            //

            compAdapter = new ComponentAdapter(compName, type, exe, componentHandle,
                    m_containerName, compImpl, m_managerProxy, compCL, m_logger, m_acsCorba);

            // to support automatic offshoot translation for xml-binded offshoots, we need to pass the dynamic adaptor
            if( !operationsIFClass.isInstance(compImpl) ) {
            	// if an external interface translator was given by the user, get the default interface translator
            	if( operationsIFImpl instanceof ExternalInterfaceTranslator )
            		operationsIFImpl = ((ExternalInterfaceTranslator)operationsIFImpl).getDefaultInterfaceTranslator();
            	compAdapter.setComponentXmlTranslatorProxy(operationsIFImpl);
            }

            // for future offshoots created by this component we must pass on the no-auto-logging info
            compAdapter.setMethodsExcludedFromInvocationLogging(methodsExcludedFromInvocationLogging);
            
            compStopWatch.reset();
            compAdapter.activateComponent(servant);
            LOG_CompAct_Corba_OK.log(m_logger, compName, compStopWatch.getLapTimeMillis());
            
            // now it's time to turn off ORB logging if the new component is requesting this
            if (compHelper.requiresOrbCentralLogSuppression()) {
            	ClientLogManager.getAcsLogManager().suppressCorbaRemoteLogging();
            }
            
            // even though the component is now an activated Corba object already,
            // it won't be called yet since the maciManager will only pass around
            // access information after we've returned from this activate_component method.
            // Therefore it's not too late to call initialize and execute, which are
            // guaranteed to be called before incoming functional calls must be expected.
            // At the moment we have to call these two methods one after the other;
            // if the Manager supports new calling semantics, we could separate the two
            // as described in ComponentLifecycle
            m_logger.fine("about to initialize component " + compName);
            compStopWatch.reset();
            compAdapter.initializeComponent();
            compAdapter.executeComponent();
            LOG_CompAct_Init_OK.log(m_logger, compName, compStopWatch.getLapTimeMillis());

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
                } catch (Exception ex) {
                    m_logger.log(Level.FINE, ex.getMessage(), ex);
                }
            }
            m_activeComponentMap.remove(componentHandle);
            
            AcsJCannotActivateComponentEx ex = new AcsJCannotActivateComponentEx(thr);
            throw ex.toCannotActivateComponentEx();
        }
        finally
        {
            // to make (possibly nested) component activations stick out in the log list
            m_logger.finer(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
        }

        return componentInfo;
    }

    /* (non-Javadoc)
	 * @see si.ijs.maci.ContainerOperations#activate_component_async(int, long, java.lang.String, java.lang.String, java.lang.String, si.ijs.maci.CBComponentInfo, alma.ACS.CBDescIn)
	 */
	public void activate_component_async(final int h, final long execution_id, final String name,
			final String exe, final String type, final CBComponentInfo callback, final CBDescIn desc)
	{
		m_logger.finer("activate_component_async request received for '" + name + 
				"', enqueueing (taskCount: " + threadPoolExecutor.getTaskCount() + ", active threads: " + 
				threadPoolExecutor.getActiveCount() + ", maxPoolSize: " + threadPoolExecutor.getMaximumPoolSize() + ").");
		
		threadPoolExecutor.execute(new Runnable() {
			
			@Override
			public void run() {
				m_logger.finer("activate_component_async request for '" + name + "' is being processed now.");
				CBDescOut descOut = new CBDescOut(0, desc.id_tag);

				try
				{
					ComponentInfo componentInfo = activate_component(h, execution_id, name, exe, type);
					callback.done(componentInfo, new alma.ACSErrTypeOK.wrappers.ACSErrOKAcsJCompletion().toCorbaCompletion(), descOut);
				}
				catch (CannotActivateComponentEx ae)
				{
					AcsJCannotActivateComponentEx aae = AcsJCannotActivateComponentEx.fromCannotActivateComponentEx(ae);
					ComponentInfo dummyComponentInfo = 
						new ComponentInfo(
								type,
								exe,
								null,
								name,
								new int[0],
								0,
								m_containerName,
								h,
								0,
								new String[0]
								);
					callback.done(dummyComponentInfo, aae.toAcsJCompletion().toCorbaCompletion(), descOut);
				}
				catch (Throwable th)
				{
					AcsJException ae = new AcsJUnknownEx(th);
					ComponentInfo dummyComponentInfo = 
						new ComponentInfo(
								type,
								exe,
								null,
								name,
								new int[0],
								0,
								m_containerName,
								h,
								0,
								new String[0]
								);
					callback.done(dummyComponentInfo, ae.toAcsJCompletion().toCorbaCompletion(), descOut);
				}
			}
		});
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

                try {
					deactivate_component(componentHandle);
				} catch (Exception ex) {
					m_logger.log(Level.FINE, "Failed to deactivate existing component with handle " + componentHandle);
				}
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

                String msg = "container refuses to activate component '" + name + "' (" + type + ") " +
                            "since another component with the same name and type " +
                            "has already been activated using a different handle (" + orgHandle + "). " +
                            "Will keep the original handle.";
                m_logger.warning(msg);
            }
            else
            {
                // the most common case: all is different, we just leave existingCompAdapter == null
            }
        }

        return existingCompAdapter;
    }


    private ComponentHelper createComponentHelper(String compName, String exe, ClassLoader compCL)
        throws AcsJContainerEx
    {
        m_logger.finer("creating component helper instance of type '" + exe + "' using classloader " + compCL.getClass().getName());
        
        StopWatch sw = new StopWatch();
        Class<? extends ComponentHelper> compHelperClass = null;
        try  {
            compHelperClass = (Class.forName(exe, true, compCL).asSubclass(ComponentHelper.class));
        } catch (ClassNotFoundException ex) {
        	AcsJContainerEx ex2 = new AcsJContainerEx(ex);
        	ex2.setContextInfo("component helper class '" + exe + "' not found.");
        	throw ex2;
        } catch (ClassCastException ex) {
        	AcsJContainerEx ex2 = new AcsJContainerEx();
        	ex2.setContextInfo("component helper class '" + exe + "' does not inherit from required base class "
                                            + ComponentHelper.class.getName());
        	throw ex2;
        }
        // We really only measure the time to load the component helper class, 
        // but since we expect the comp impl class to be in the same jar file, our class loader should 
        // then learn about it and be very fast loading it later.
        LOG_CompAct_Loading_OK.log(m_logger, compName, sw.getLapTimeMillis());
        
        Constructor<? extends ComponentHelper> helperCtor = null;
        ComponentHelper compHelper = null;
        try {
            helperCtor = compHelperClass.getConstructor(new Class[]{Logger.class});
        } catch (NoSuchMethodException ex) {
            String msg = "component helper class '" + exe + "' has no constructor " + " that takes a java.util.Logger";
            m_logger.fine(msg);
        	AcsJContainerEx ex2 = new AcsJContainerEx(ex);
        	ex2.setContextInfo(msg);
        	throw ex2;
        }

        try {
            compHelper = helperCtor.newInstance(new Object[]{m_logger});
        } catch (Throwable thr) {
        	AcsJContainerEx ex = new AcsJContainerEx(thr);
        	ex.setContextInfo("component helper class '" + exe + "' could not be instantiated");
        	throw ex;
        }
        // here we don't log LOG_CompAct_Instance_OK because instantiating the component itself is expected to take longer
        // than instantiating the comp helper here. 
        // To be more accurate, we'd have to add up those times, which would be rather ugly in the current code.
        
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
     * since the manager will call {@link #deactivate_component(int)} in a regular shutdown.
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
	 * From maci.idl: <i>Deactivation is the inverse process of activation: component is detached from the POA, and thus
	 * made unavailable through CORBA, and its resources are freed. If its code-base is no longer used, it is unloaded
	 * from memory.</i>
	 * 
	 * @param handles
	 *            a handle identifying the component that should be released. If <code>handle == 0</code>, then all
	 *            active components will be deactivated!
	 * 
	 * @see si.ijs.maci.ContainerOperations#deactivate_component(int)
	 * @see ComponentAdapter#deactivateComponent()
	 * 
	 */
	@Override
	public void deactivate_component(int handle) throws ComponentDeactivationUncleanEx, ComponentDeactivationFailedEx {
		m_logger.fine("received call to deactivate_component, handle=" + handle);

		// get the component adapter which will be != null, but might be in the wrong state
		ComponentAdapter[] compAdapters = m_activeComponentMap.getComponentAdapters(new int[] { handle });

		// (synchronizedly) mark the component for deactivation first.
		// If an adapter is returned, it will be in the appropriate state;
		// another thread coming in with the same handle will not get this adapter back.
		ComponentAdapter[] validAdapters = markAndFilterForDeactivation(compAdapters);

		if (validAdapters.length > 1) {
			// @todo error
		}
		else if (validAdapters.length == 0) {
			// @todo log the no-op
		}
		else {
			ComponentAdapter compAdapter = validAdapters[0];
			try {
				deactivateComponentInternal(compAdapter);
			} catch (AcsJComponentDeactivationUncleanEx ex) {
				throw ex.toComponentDeactivationUncleanEx();
			} catch (AcsJComponentDeactivationFailedEx ex) {
				throw ex.toComponentDeactivationFailedEx();
			}
		}
	}
	
	/**
	 * Called internally during shutdown. 
	 * A deactivation exception will be logged at INFO level but will not prevent deactvation attempts of the other components.
	 */
	private void deactivateAllComponents() {

		// get the component adapters which are all != null, but might be in the wrong state
		ComponentAdapter[] compAdapters = m_activeComponentMap.getAllComponentAdapters();

		// (synchronizedly) mark the component(s) for deactivation first.
		// resulting adapters are all in an appropriate state;
		// another thread coming in with overlapping handles will not get the same adapters back
		ComponentAdapter[] validAdapters = markAndFilterForDeactivation(compAdapters);

		// todo: perhaps use thread pool and deactivate a bunch in parallel
		for (ComponentAdapter compAdapter : validAdapters) {
			try {
				deactivateComponentInternal(compAdapter);
			} catch (Exception ex) {
				m_logger.log(Level.INFO, "failed to properly deactivate component " + compAdapter.getName(), ex);
			}
		}
	}

	/**
	 * The common part for deactivating either one or many components.
	 * This method will directly deactivate the given component adatper, without caring about its state 
	 * or synchronization issues.
	 * <p>
	 * Note that the container "forgets" about this component, even if deactivation fails with a 
	 * <code>AcsJComponentDeactivationFailedEx</code>:
	 * <ul>
	 *   <li> If the poa deactivation failed with a timeout, it could be that it fully deactivates a bit later,
	 *        which would make the reference useless.
	 *   <li> Even if such a component stays around, it may be so messed up that it is too risky to use it further. 
	 * </ul>
	 * The consequence is that an attempt to activate another instance of the same component may fail in the future,
	 * without a specific message from the container that a previous instance had failed. The information can only
	 * be connected from the logs in this case.
	 * The exception parameter AcsJComponentDeactivationFailedEx#IsPermanentFailure therefore has no meaning for the manager. 
* @TODO complete this comment: A client might find it useful though, and set to false in case of timeout, because then the client can hope that this comp will disappear later.
	 * 
	 * @throws AcsJComponentDeactivationFailedEx 
	 * @throws AcsJComponentDeactivationUncleanEx 
	 * @see #deactivate_component(int)
	 * @see #deactivateAllComponents()
	 */
	private void deactivateComponentInternal(ComponentAdapter compAdapter) throws AcsJComponentDeactivationUncleanEx, AcsJComponentDeactivationFailedEx {
		try {
			
			compAdapter.deactivateComponent();

		} finally {
			m_activeComponentMap.remove(compAdapter.getHandle());
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
    	throws CannotRestartComponentEx
    {
    	String curl = null;
    	try {
			ComponentAdapter compAdapter = m_activeComponentMap.get(compHandle);
			if (compAdapter != null) {
				curl = compAdapter.getName(); 
			}
			
			m_logger.warning("method restart_component not yet implemented.");
			
			AcsJCannotRestartComponentEx ex = new AcsJCannotRestartComponentEx();
			ex.setCURL(curl);
			throw ex;
		} catch (AcsJCannotRestartComponentEx ex) {
			throw ex.toCannotRestartComponentEx();
		}
		catch (Throwable thr) {
			AcsJCannotRestartComponentEx ex = new AcsJCannotRestartComponentEx(thr);
			ex.setCURL(curl);
			throw ex.toCannotRestartComponentEx();
		}
    }


    /////////////////////////////////////////////////////////////
    // Implementation of ContainerOperations#shutdown
    /////////////////////////////////////////////////////////////

	/**
	 * Action to take after shutting down (ignored for the time being). Bits 8 thru 15 of this parameter denote the
	 * action, which can be one of:
	 * <UL>
	 * <LI>0 -- reload the container</LI>
	 * <LI>1 -- reboot the computer</LI>
	 * <LI>2 -- exit the container</LI>
	 * </UL>
	 * 
	 * The bits 0 thru 7 (values 0 to 255) are the return value that the Container should pass to the operating system.
	 * TODO: get rid of this silly bit-multiplexing
	 * 
	 * @see si.ijs.maci.ContainerOperations#shutdown(int)
	 */
	public void shutdown(int encryptedAction) {
		shutdown(encryptedAction, true, true);
	}


	/**
	 * Shuts down the container.
	 * <p>
	 * Depending on the <code>gracefully</code> parameter, either {@link #deactivate_components(int[])} or
	 * {@link #abortAllComponents(long)} is called.
	 * <p>
	 * This method may be called from the following threads:
	 * <ul>
	 * <li>one of the ORB invocation threads (shutdown initiated by the manager)
	 * <li>shutdown thread (Ctrl-C VM hook)
	 * </ul>
	 * @param encryptedAction
	 *            ignored for the time being (always EXIT)
	 * @param gracefully
	 *            if true, this method only returns after <code>cleanUp</code> has been called on all components. 
	 *            if false, it returns faster, running the components' abort methods in separate threads for at most 3 seconds.
	 * @see #shutdown(int)
	 * @see ShutdownHook
	 */
	void shutdown(int encryptedAction, boolean gracefully, boolean isOrbThread) {
		int action = (encryptedAction >> 8) & 0xFF;
		m_logger.info("received call to 'shutdown', action=" + action + " (encryptedAction=" + encryptedAction
				+ "), gracefully=" + gracefully + ".");

		if (shuttingDown.getAndSet(true)) {
			m_logger.fine("call to shutdown() while shutting down will be ignored...");
			return;
		}
		
		m_managerProxy.shutdownNotify();
		
		threadPoolExecutor.shutdown();

		// shut down all active components
		if (gracefully) {
			try {
				deactivateAllComponents();
			} catch (Exception ex) {
				m_logger.log(Level.WARNING, "Failed to deactivate components for graceful shutdown");
			}
		} else {
			abortAllComponents(3000);
		}

		// shutdown alarm subsystem
		ACSAlarmSystemInterfaceFactory.done();

		// shutdown BACI framework
		try {
			Class<?> clazz = Class.forName("alma.ACS.jbaci.BACIFramework");
			Object baciFramework = clazz.getField("INSTANCE").get(null);
			clazz.getMethod("shutdown").invoke(baciFramework);
		} catch(Exception e) {
			m_logger.log(Level.WARNING, "Failed to graceful shutdown the BACI framework");
		}

		m_managerProxy.logoutFromManager();

		// shutdown logging subsystem
		if (!isEmbedded) {
			ClientLogManager.getAcsLogManager().shutdown(gracefully);
			// here the flag "wait_for_completion" is taken from "gracefully"
			// Note that it is not considered anyway if isOrbThread == true
			m_acsCorba.shutdownORB(gracefully, isOrbThread);
		}

		if (m_alarmContainerServices != null) {
			m_alarmContainerServices.cleanUp();
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
                containerThreadFactory );

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



    // ///////////////////////////////////////////////////////////
	// Implementation of ClientOperations methods
	// ///////////////////////////////////////////////////////////

	/**
	 * @see si.ijs.maci.ClientOperations#name()
	 */
	public String name() {
		m_logger.fine("call to name() answered with '" + m_containerName + "'.");
		return m_containerName;
	}

    
	
    /**
	 * Disconnect notification. The disconnect method is called by the Manager to notify the client that it will be
	 * unavailable and that the client should log off.
	 * <p>
	 * Since ACS 7.0.2, this method returns quickly and uses a different thread to log back in with the manager. This is
	 * cleaner than taking a thread from the ORB's pool for a possibly long time, although the manager does not care 
	 * because this method is defined as oneway in IDL.
	 * 
	 * @see si.ijs.maci.ClientOperations#disconnect()
	 */
	public void disconnect() {
		m_logger.warning("Manager requests logout...");
		m_managerProxy.logoutFromManager();
		try {
			// to give the manager 10 sec time to go down or whatever
			Thread.sleep(10000);
		} catch (InterruptedException e1) {
			// ignore
		}

		// m_recoveryStart = true; // check discussion at http://almasw.hq.eso.org/almasw/bin/preview/ACS/NewMaciIdl

		Runnable reloginRunnable = new Runnable() {
			public void run() {
				try {
					// will loop until manager ref becomes available
					loginToManager();
				} catch (Exception e) {
					m_logger.log(Level.WARNING, "Failed to re-login to the manager. Will shut down.", e);
					// it could be that the connection failed exactly because we are shutting down, 
					// and we want to avoid the warning from another shutdown call...
					if (!shuttingDown.get()) {
						shutdown(AcsContainer.CONTAINER_EXIT << 8, true, false);
					}
				}
			}
		};
		containerThreadFactory.newThread(reloginRunnable).start();
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
	public AuthenticationData authenticate(long execution_id, String question) {
		// the old string-based answer is kept around for the time being
        String code = ( useRecoveryMode ? "AR" : "A" );
        String answer = code + ' ' + m_containerName;
        
        // keep old executionId if it exists
        if (executionId < 0) {
        	executionId = execution_id;
        }
        
        AuthenticationData ret = new AuthenticationData(
        		answer, 
        		ClientType.CONTAINER_TYPE, 
        		ImplLangType.JAVA, 
        		useRecoveryMode, 
        		startTimeUTClong, 
        		executionId);
        return ret;
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
     * @see si.ijs.maci.ClientOperations#taggedmessage(short, short, String)
     */
    public void taggedmessage(short type, short messageID, String message)
    {
	if (messageID == ClientOperations.MSGID_AUTOLOAD_START)
	{
	    System.out.println(ContainerOperations.ContainerStatusCompAutoloadBeginMsg);
	}
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
	if (messageID == ClientOperations.MSGID_AUTOLOAD_END)
	{
	    System.out.println(ContainerOperations.ContainerStatusCompAutoloadEndMsg);
	    System.out.println(ContainerOperations.ContainerStatusReadyMsg);
	}
    }


	/**
	 * Replies with <code>true</code> so that Manager sees that this container is alive.
	 * <p>
	 * Prints a message to System.out, so that it's visible on the local console that the container is doing ok. Does
	 * not log anything, because a failure to reply would show up remotely anyway.
	 * <p>
	 * No message is printed if the container log level is above INFO, see http://jira.alma.cl/browse/COMP-1736.
	 * 
	 * @see si.ijs.maci.ClientOperations#ping()
	 */
	public boolean ping() {
		// we cannot use m_logger.isLoggable because only the stdout log level should be considered,
		// and it would be even worse a hack to go from the logger to its stdout handler to ask for the level there.
		AcsLogLevelDefinition stdoutLevel;
		try {
			stdoutLevel = AcsLogLevelDefinition.fromXsdLogLevel(logConfig.getNamedLoggerConfig(m_containerName).getMinLogLevelLocal());
		} catch (AcsJIllegalArgumentEx ex) {
			stdoutLevel = AcsLogLevelDefinition.INFO;
			ex.printStackTrace();
		}
		if (stdoutLevel.compareTo(AcsLogLevelDefinition.INFO) <= 0) {
			Runtime rt = Runtime.getRuntime();
			long totalMemKB = rt.totalMemory() / 1024;
			long usedMemKB = totalMemKB - rt.freeMemory() / 1024;
			String memStatus = "Memory usage " + usedMemKB + " of " + totalMemKB + " kB ";
			long maxMem = rt.maxMemory();
			if (maxMem < Long.MAX_VALUE) {
				long maxMemKB = maxMem / 1024;
				memStatus += "(= " + (usedMemKB * 1000 / maxMemKB) / 10.0 + "% of JVM growth limit " + maxMemKB
						+ " kB) ";
			}
			String timestamp = IsoDateFormat.formatCurrentDate();
			System.out.println(timestamp + " [" + m_containerName + "] ping received, container alive. " + memStatus);
		}
		// ThreadMXBean threadMX = ManagementFactory.getThreadMXBean();
		// threadMX.

		return true;
	}


    /**
     * Notify client about the change (availability) of the components currently in use by this client.
     * 
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

        // have containerservices instances notify their clients        
        List<ComponentDescriptor> compDescs = ComponentDescriptor.fromComponentInfoArray(components);
        for (ComponentAdapter compAd : m_activeComponentMap.getAllComponentAdapters()) {
      	  compAd.getContainerServices().fireComponentsAvailable(compDescs);
        }
    }

    /**
     * Notify client that some of the components currently in use by client
     * have become unavailable.
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

        // have containerservices instances notify their clients
        List<String> compNames = Arrays.asList(component_names); 
        for (ComponentAdapter compAd : m_activeComponentMap.getAllComponentAdapters()) {
      	  compAd.getContainerServices().fireComponentsUnavailable(compNames);
        }
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



    // ///////////////////////////////////////////////////////////
	// LoggingConfigurable interface
	// ///////////////////////////////////////////////////////////

	/**
	 * Gets the log levels of the default logging configuration. These levels
	 * are used by all loggers that have not been configured individually.
	 */
	public LogLevels get_default_logLevels() {
		tryToWaitForContainerStart();
		LogLevels logLevels = new LogLevels();
		logLevels.useDefault = false;
		logLevels.minLogLevel = (short) logConfig.getDefaultMinLogLevel().value;
		logLevels.minLogLevelLocal = (short) logConfig.getDefaultMinLogLevelLocal().value;
		return logLevels;
	}

	/**
	 * Sets the log levels of the default logging configuration. These levels
	 * are used by all loggers that have not been configured individually.
	 */
	public void set_default_logLevels(LogLevels levels) throws IllegalArgumentEx {
		tryToWaitForContainerStart();
		try {
			logConfig.setDefaultMinLogLevel(AcsLogLevelDefinition.fromInteger(levels.minLogLevel));
			logConfig.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.fromInteger(levels.minLogLevelLocal));
		} catch (AcsJIllegalArgumentEx ex) {
			throw ex.toIllegalArgumentEx();
		}
	}

	/**
	 * Gets the names of all loggers, to allow configuring their levels
	 * individually. The names are those that appear in the log records in the
	 * field "SourceObject". This includes the container logger, ORB logger,
	 * component loggers, and (only C++) GlobalLogger.
	 * <p>
	 * The returned logger names are randomly ordered.
	 */
	public String[] get_logger_names() {
		tryToWaitForContainerStart();
		Set<String> loggerNames = logConfig.getLoggerNames();
		return loggerNames.toArray(new String[loggerNames.size()]);
	}

	/**
	 * Gets log levels for a particular named logger. If the returned field
	 * LogLevels.useDefault is true, then the logger uses the default levels,
	 * see get_default_logLevels(); otherwise the returned local and remote
	 * levels apply.
	 * <p>
	 * For possible convenience, the default levels are returned in addition to 
	 * setting {@link LogLevels#useDefault} to <code>true</code>.
	 */
	public LogLevels get_logLevels(String logger_name) throws LoggerDoesNotExistEx {
		tryToWaitForContainerStart();
		UnnamedLogger xsdLevels = logConfig.getNamedLoggerConfig(logger_name);
		boolean useDefault = !logConfig.hasCustomConfig(logger_name); 
		LogLevels ret = AcsLogLevelDefinition.createIdlLogLevelsFromXsd(useDefault, xsdLevels);
		return ret;
	}

	/**
	 * Sets log levels for a particular named logger. If levels.useDefault is
	 * true, then the logger will be reset to using default levels; otherwise it
	 * will use the supplied local and remote levels.
	 */
	public void set_logLevels(String logger_name, LogLevels levels) throws LoggerDoesNotExistEx, IllegalArgumentEx {
		tryToWaitForContainerStart();
		if (levels.useDefault) {
			logConfig.clearNamedLoggerConfig(logger_name);
		}
		else {
			try {
				UnnamedLogger config = AcsLogLevelDefinition.createXsdLogLevelsFromIdl(levels);
				logConfig.setNamedLoggerConfig(logger_name, config);
			} catch (AcsJIllegalArgumentEx ex) {
				throw ex.toIllegalArgumentEx();
			}
		}
	}

	/**
	 * Commands the container or manager to read in again the logging
	 * configuration from the CDB and to reconfigure the loggers accordingly.
	 * This allows for persistent changes in the logging configuration to become
	 * effective, and also for changes of more advanced parameters.
	 * <p>
	 * Note that unlike for the logging initialization in {@link #initialize()},
	 * now we give precedence to the CDB values over any previous settings.
	 */
	public void refresh_logging_config() {
		tryToWaitForContainerStart();
		try {
			logConfig.initialize(true);
		} catch (LogConfigException ex) {
			// if the CDB can't be read, we still want to run the container, thus we only log the problem here
			m_logger.log(Level.FINE, "Failed to configure logging (default values will be used).", ex);
		}
	}


		
	/** ************************ END LoggingConfigurable ************************ */

	
	/**
	 * Waits for the container to finish startup, see {@link #containerStartOrbThreadGate}.
	 * Does not expose InterruptedException, so that in case of such an exception 
	 * this method will return without the container having finished its startup. 
	 */
	protected void tryToWaitForContainerStart() {
		try {
			containerStartOrbThreadGate.await(30, TimeUnit.SECONDS);
		} catch (InterruptedException ex1) {
			// ignore
		}
	}
	
}
