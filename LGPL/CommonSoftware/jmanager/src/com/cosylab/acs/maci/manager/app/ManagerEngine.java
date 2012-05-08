/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.manager.app;

import java.io.File;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.Context;

import org.jacorb.orb.acs.AcsORBProfiler;
import org.jacorb.orb.acs.AcsProfilingORB;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NamingContextHelper;
import org.omg.PortableServer.IdAssignmentPolicyValue;
import org.omg.PortableServer.LifespanPolicyValue;
import org.omg.PortableServer.POA;
import org.prevayler.implementation.SnapshotPrevayler;

import com.cosylab.acs.maci.CoreException;
import com.cosylab.acs.maci.HandleConstants;
import com.cosylab.acs.maci.manager.ManagerImpl;
import com.cosylab.acs.maci.manager.ManagerShutdown;
import com.cosylab.acs.maci.plug.CORBAReferenceSerializator;
import com.cosylab.acs.maci.plug.CORBATransport;
import com.cosylab.acs.maci.plug.DefaultCORBAService;
import com.cosylab.acs.maci.plug.ManagerProxyImpl;
import com.cosylab.acs.maci.plug.NamingServiceRemoteDirectory;
import com.cosylab.cdb.client.CDBAccess;
import com.cosylab.util.FileHelper;

import si.ijs.maci.ManagerHelper;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigException;

/**
 * Engine of the Manager GUI application.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ManagerEngine 
{
	
	/**
	 * Manager domain (empty until manager federation is implemented).
	 */
	private final static String MANAGER_DOMAIN = "";

	/**
	 * CORBA Manager Servant ID.
	 */
	private final static byte[] MANAGER_ID = { 'M', 'a', 'n', 'a', 'g', 'e', 'r' };

	/**
	 * Directory name under ACS.temp where recovery fieles are stored
	 */
	private final static String RECOVERY_DIR_NAME = "Manager_Recovery";

	/**
	 * Implementation of the Manager.
	 */
	private ManagerImpl manager = null;

	/**
	 * Manager CORBA Proxy implementation.
	 */
	private ManagerProxyImpl managerProxy = null;

	/**
	 * Manager POA.
	 */
	private POA managerPOA = null;

	/**
	 * Manager reference.
	 */
	private si.ijs.maci.Manager managerReference = null;

	/**
	 * Implementation of the shutdown method.
	 */
	private ManagerShutdown shutdownImplementation = null;
	
	/**
	 * Recovery files directory.
	 */
	private String recoveryLocation = null;
	
	/**
	 * Default logger is global logger.
	 * This default logger should never be used, since {@link #initializeManager()} will create and assign a proper ACS logger.
	 */
	private AcsLogger logger = AcsLogger.fromJdkLogger(Logger.global, "ur-logger");

	/**
	 * CORBA service.
	 */
	private DefaultCORBAService corbaService = null;

	public static final String DISABLE_PREVAYLER = "acs.disablePrevayler";
	private static final boolean isPrevaylerDisabled = Boolean.getBoolean(DISABLE_PREVAYLER);

	/**
	 * Constructor for ManagerEngine.
	 * 
	 * @param	shutdownImplementation	implementation of the shutdown method.
	 */
	public ManagerEngine(ManagerShutdown shutdownImplementation)
	{
		super();
		this.shutdownImplementation = shutdownImplementation;
	}

	/**
	 * Destroy.
	 */
	public void destroy()
	{
		try
		{
			destroyManager();
		}
		catch (Throwable th)
		{
			logger.log(Level.SEVERE,  "Failed to deactivate Manager.", th);
		}
	}

	/**
	 * Initialize manager.
	 */
	public void initialize()
	{
		try
		{
			initializeManager();
			initializeShutdownHook();
		}
		catch (Throwable ex)
		{
			logger.log(Level.SEVERE, "FAILED TO INITIALIZE MANAGER.", ex);
			if (!System.getProperty("ACS.noExit", "false").equalsIgnoreCase("true"))
				System.exit(1);
		}

		logger.info("All initializations done.");
	}

	/**
	 * Initialize and activate Manager.
	 */
	private void initializeManager() throws Throwable
	{
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("Manager", true);
		
		logger.info("Initializing Manager.");
		
		//
		// CORBA
		//

		// obtain CORBA Service
		corbaService = new DefaultCORBAService(logger);

		// get ORB
		final ORB orb = corbaService.getORB();
		if (orb == null)
		{
			CoreException ce = new CoreException("CORBA Service can not provide ORB.");
			throw ce;
		}
		
		// get RootPOA
		POA rootPOA = corbaService.getRootPOA();
		if (rootPOA == null)
		{
			CoreException ce = new CoreException("CORBA Service can not provide RootPOA.");
			throw ce;
		}

		//
		// Remote Directory
		//
		
		NamingServiceRemoteDirectory remoteDirectory = new NamingServiceRemoteDirectory(orb, logger);
		
		Context context = null;
		if (remoteDirectory != null)
			context = remoteDirectory.getContext();
		
		//
		// Initialize CORBA
		//

		// set USER_ID, PERSISTENT policies
		org.omg.CORBA.Policy [] policies = new org.omg.CORBA.Policy[2];
		
		/*
		// set USER_ID, PERSISTENT,BIDIRECTIONAL policies
		org.omg.CORBA.Policy [] policies = new org.omg.CORBA.Policy[3];
		*/
		
		policies[0] = rootPOA.create_id_assignment_policy(IdAssignmentPolicyValue.USER_ID);
		policies[1] = rootPOA.create_lifespan_policy(LifespanPolicyValue.PERSISTENT);
		
		/*
		// create BIDIRECTIONAL policy
		Any bidirValue = orb.create_any();
		BidirectionalPolicyValueHelper.insert(bidirValue, BOTH.value);
		policies[2] = orb.create_policy(BIDIRECTIONAL_POLICY_TYPE.value, bidirValue);
        */
        
		// create ManagerPOA
		managerPOA = rootPOA.create_POA("ManagerPOA", rootPOA.the_POAManager(), policies);
		
		// destroy policies
		for (int i = 0; i < policies.length; i++) 
		    policies[i].destroy();			
		

		// initialize Manager implementation
		CORBAReferenceSerializator.setOrb(orb); // allow object reference serialization 
		manager = new ManagerImpl();
		manager.setDomain(MANAGER_DOMAIN);

		recoveryLocation = FileHelper.getTempFileName(null, RECOVERY_DIR_NAME);
		String readRecovery = System.getProperties().getProperty("Manager.recovery", "true");
		
		if( readRecovery.equalsIgnoreCase("false") ) {
			// if we are not interested in recovery files just delete them
			File recoveryDir = new File(recoveryLocation);
			//recoveryDir.delete();
			File[] files = recoveryDir.listFiles();
			for (int i = 0; files != null && i < files.length; i++)
				files[i].delete();
			// Now check if there are log files left. Maybe user do not have enough permision
			// or we are didn't set proper permission before Manager killed.
			// That can lead to unwanted or illegal state so we will refuse to continue
			files = recoveryDir.listFiles();
			for (int i = 0; files != null && i < files.length; i++) {
				if( files[i].getName().endsWith(".commandLog") )
					throw new Exception("Some recovery files are left in recovery location probably because of permission\nUnable to start without recovery state!");
			}
		}
		else
		{
			// remove old recovery files
			RecoveryFilesRemover.removeRecoveryFiles(new File(recoveryLocation));
		}
		
	    SnapshotPrevayler prevayler = null;
		if (isPrevaylerDisabled)
		{
			System.out.println( "Prevayler disabled!");
		}
		else
		{
			prevayler = new SnapshotPrevayler(manager, recoveryLocation);
		
			if( readRecovery.equalsIgnoreCase("false") ) {
				// just to invalidate prevaylers message
				System.out.println( "Skipping saved manager state!");
			}
	
			manager = (ManagerImpl)prevayler.system();
		}
		
	    CDBAccess cdbAccess = new CDBAccess(orb, logger);
		
	    LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
		logConfig.setCDBLoggingConfigPath("MACI/Managers/Manager");
		logConfig.setCDB(cdbAccess.connectAndGetDAL());
		try {
			logConfig.initialize(false);
		} catch (LogConfigException ex) {
			// if the CDB can't be read, we still want to run the manager, so
			// we only log the problems
			logger.log(Level.FINE, "Failed to configure logging (default values will be used). Reason: " + ex.getMessage());
		}
	    
		// initialize manager "mock" container services
		ManagerContainerServices managerContainerServices = new ManagerContainerServices(orb, managerPOA, cdbAccess.getDAL(), logger);
		
		manager.initialize(prevayler, cdbAccess, context, logger, managerContainerServices);
		manager.setShutdownImplementation(shutdownImplementation);

		// setup ORB profiling
		try {
			if (orb instanceof AcsProfilingORB) {
				AcsORBProfiler profiler = new ManagerOrbProfiler(manager, logger);
				((AcsProfilingORB)orb).registerAcsORBProfiler(profiler);
				logger.finer("Orb profiling set up, using class " + ManagerOrbProfiler.class.getName());
			}
		} catch (Throwable th) {
			logger.log(Level.WARNING, "Failed to setup ORB profiling.", th);
		}

		if (prevayler != null)
		{
			FileHelper.setFileAttributes( "g+w", recoveryLocation );
			// create new task for snapshoot creation,
			final long MINUTE_IN_MS = 60*1000;
			new RecoverySnapshotTask(prevayler, 1*MINUTE_IN_MS, recoveryLocation);
		}
		
		// initialize Manager CORBA Proxy (create servant)
		managerProxy = new ManagerProxyImpl(manager, logger);

		//activate object
		managerPOA.activate_object_with_id( MANAGER_ID, managerProxy );
		
		// get object reference from the servant
		org.omg.CORBA.Object obj = managerPOA.servant_to_reference(managerProxy);
		managerReference = ManagerHelper.narrow(obj);
		
		// get IOR
		String ior = orb.object_to_string(managerReference);

		// notify user
		logger.info("Manager activated with " + ior);

		// register special service components to the Manager
		manager.setManagerComponentReference(managerReference);

		// set transport
		manager.setTransport(new CORBATransport(orb, ior));
		
		// register NameService
		if (remoteDirectory != null)
		{
			String reference = remoteDirectory.getReference();
			if (reference != null)
			{
				// convert iiop to corbaloc
				if (reference.startsWith("iiop://"))
				{
					reference = reference.replaceFirst("iiop://", "corbaloc::");
					if (reference.charAt(reference.length()-1) != '/')	
						reference += "/NameService";
					else
						reference += "NameService";
				}
			}
			
			try
			{
				obj = NamingContextHelper.narrow(orb.string_to_object(reference));
			}
			catch (Exception ex)
			{
				// noop
			}
			
			manager.setRemoteDirectoryComponentReference(obj);
		}

		// intitialize federation here - after remote directory is set (if it is)
		// (this is not a nice solution)
		Hashtable federationDirectoryProperties = new Hashtable();
		// set CosNamingFactory 
		federationDirectoryProperties.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.cosnaming.CNCtxFactory");
		// set orb
		federationDirectoryProperties.put("java.naming.corba.orb", orb);
		manager.initializeFederation(federationDirectoryProperties);

		// initialize remote logging
		new Thread(new Runnable() {
			public void run()
			{
				ClientLogManager.getAcsLogManager().initRemoteLogging(orb, managerReference, manager.getHandle(), true);
			}
		}, "Remote logging initializer").start();
		
		manager.initializationDone();
	}

	/**
	 * Destroy the Manager.
	 */
	private void destroyManager() throws Exception
	{
		
		logger.info("Destroying Manager.");
		
		// firsty destroy Manager implementation (if necessary)		
		if (!manager.isShuttingDown())
		{
			try
			{
				manager.shutdown(HandleConstants.MANAGER_MASK, 0);
			}
			catch (Throwable ex)
			{
				CoreException ce = new CoreException("Failed to destroy manager.", ex);
				logger.log(Level.WARNING, "Failed to destroy manager.", ce);
			}
		}

		// deactivate Manager
		if (managerPOA != null && managerReference != null)
		{
			managerPOA.deactivate_object(MANAGER_ID);
			managerPOA = null;
		}

		// destroy CORBA service
		if (corbaService != null)
			corbaService.destroy();
		
		// add rights to group in order to be able to start with '-n'
		if (recoveryLocation != null)
			FileHelper.setFileAttributes( "g+w", recoveryLocation );

	}

	/**
	 * Initialize shutdown hook (CTRL-C signal).
	 */
	private void initializeShutdownHook()
	{
		/**
		 * Manager shitdown hook thread implementation.
		 */
		class ManagerShutdownHookThread extends Thread
		{
			public ManagerShutdownHookThread()
			{
				super("ManagerShutdownHook");
			}
			
			public void run()
			{
				// call shutdown
				if (shutdownImplementation != null && !shutdownImplementation.isShutdownInProgress())
				{
					// fire destroy application
					shutdownImplementation.shutdown(true);
				}
			}
		}
		
		// register shutdown hook
		Runtime.getRuntime().addShutdownHook(new ManagerShutdownHookThread());

	}

	/**
	 * Returns number of pending requests.
	 * @return	number of pending requests.
	 */
	public int getNumberOfPendingRequests()
	{
		if (managerProxy != null)
			return managerProxy.getNumberOfPendingRequests();
		else
			return -1;
	}
	
	/**
	 * Get logger.
	 * @return logger.
	 */
	public Logger getLogger()
	{
		return logger;
	}
}
