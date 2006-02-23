/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.manager.app;

import java.io.File;
import java.util.Hashtable;

import javax.naming.Context;

import org.omg.CORBA.ORB;
import org.omg.CosNaming.NamingContextHelper;
import org.omg.PortableServer.IdAssignmentPolicyValue;
import org.omg.PortableServer.LifespanPolicyValue;
import org.omg.PortableServer.POA;
import org.prevayler.implementation.SnapshotPrevayler;
import si.ijs.maci.ManagerHelper;

import EDU.oswego.cs.dl.util.concurrent.Sync;
import abeans.core.CoreException;
import abeans.core.Root;
import abeans.core.defaults.JavaMessageLog;
import abeans.core.defaults.MessageLog;
import abeans.core.defaults.MessageLogEntry;
import abeans.framework.ApplicationContext;
import abeans.pluggable.RemoteDirectory;
import abeans.pluggable.acs.CORBAService;
import abeans.pluggable.acs.NamingServiceRemoteDirectory;
import abeans.pluggable.acs.logging.LoggingLevel;

import com.cosylab.abeans.AbeansEngine;
import com.cosylab.acs.cdb.CDBAccess;
import com.cosylab.acs.maci.HandleConstants;
import com.cosylab.acs.maci.manager.ManagerImpl;
import com.cosylab.acs.maci.manager.ManagerShutdown;
import com.cosylab.acs.maci.plug.CORBAReferenceSerializator;
import com.cosylab.acs.maci.plug.ManagerProxyImpl;
import com.cosylab.util.FileHelper;

/**
 * Engine of the Manager GUI application.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ManagerEngine extends AbeansEngine
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
	 * <code>ApplicationContext</code> of <code>ManagerEngine</code>.
	 */
	private ApplicationContext applicationContext = null;

	/**
	 * Implementation of the shutdown method.
	 */
	private ManagerShutdown shutdownImplementation = null;
	
	/**
	 * Recovery files directory.
	 */
	private String recoveryLocation = null;
	
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
	 * @see com.cosylab.abeans.AbeansEngine#getName()
	 */
	public String getName()
	{
		return "Manager";
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userFrameworkInitialize()
	 */
	public void userFrameworkInitialize()
	{
		applicationContext = getApplicationContext();
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userDestroy()
	 */
	public void userDestroy()
	{
		try
		{
			destroyManager();
		}
		catch (Exception ex)
		{
			new MessageLogEntry(this, "userDestroy", "Failed to deactivate Manager.", ex, LoggingLevel.WARNING).dispatch();
		}
	}

	/**
	 * @see com.cosylab.abeans.AbeansEngine#userAllInitializationsDone()
	 */
	public void userAllInitializationsDone()
	{
		try
		{
			initializeManager();
			initializeShutdownHook();
		}
		catch (Exception ex)
		{
			new MessageLogEntry(this, "userFrameworkInitialize", "FAILED TO INITIALIZE MANAGER.", ex, LoggingLevel.CRITICAL).dispatch();
			ex.printStackTrace();
			if (!System.getProperty("ACS.noExit", "false").equalsIgnoreCase("true"))
				System.exit(1);
		}

		new MessageLogEntry(this, "userAllInitializationsDone", "All initializations done.", LoggingLevel.INFO).dispatch();
	}

	/**
	 * @see abeans.framework.ApplicationEngine#getModelName()
	 */
	public String getModelName()
	{
		// we use Channel model for CDB
		//return "Channel";
		return null;
	}

	/**
	 * Initialize and activate Manager.
	 */
	private void initializeManager() throws Exception
	{
		
		new MessageLogEntry(this, "initializeManager", "Initializing Manager.", LoggingLevel.INFO).dispatch();
		
		// TODO remove when fixed memory leak workaround in AbeansLogger !!!
		MessageLog messageLog =	(MessageLog)Root.getComponentManager().getComponent(MessageLog.class);
		if (messageLog instanceof JavaMessageLog)
			((JavaMessageLog)messageLog).getSystemLogger().clearBufferInto(null);
		
		//
		// CORBA
		//

		// obtain CORBA Service
		CORBAService corbaService =	(CORBAService)Root.getComponentManager().getComponent(CORBAService.class);
		if (corbaService == null)
		{
			CoreException ce = new CoreException(this, "Abeans CORBA Service is not installed.");
			ce.caughtIn(this, "initializeManager");
			throw ce;
		}

		// get ORB
		ORB orb = corbaService.getORB();
		if (orb == null)
		{
			CoreException ce = new CoreException(this,	"Abeans CORBA Service can not provide ORB.");
			ce.caughtIn(this, "initializeManager");
			throw ce;
		}
		
		// get RootPOA
		POA rootPOA = corbaService.getRootPOA();
		if (rootPOA == null)
		{
			CoreException ce = new CoreException(this,	"Abeans CORBA Service can not provide RootPOA.");
			ce.caughtIn(this, "initializeManager");
			throw ce;
		}

		//
		// Remote Directory
		//
		
		RemoteDirectory remoteDirectory = (RemoteDirectory)Root.getComponentManager().getComponent(RemoteDirectory.class);
		if (remoteDirectory == null) 
		{
			//CoreException ce = new CoreException(this, "Abeans Remote Directory is not installed.");
			//ce.caughtIn(this, "initializeManager");
			//new MessageLogEntry(this, "initializeManager", "Failed to connect to the Naming Service - disabling Naming Service Mapping.", ce, LoggingLevel.WARNING).dispatch();

			new MessageLogEntry(this, "initializeManager", "Failed to connect to the Naming Service - disabling Naming Service Mapping.", LoggingLevel.WARNING).dispatch();
		}

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
		
	    SnapshotPrevayler prevayler = new SnapshotPrevayler(manager, recoveryLocation);
		
		if( readRecovery.equalsIgnoreCase("false") ) {
			// just to invalidate preavaylers message
			System.out.println( "Skipping saved manager state!");
		}

	    manager = (ManagerImpl)prevayler.system();

		manager.initialize(prevayler, applicationContext, new CDBAccess(orb), context);
		manager.setShutdownImplementation(shutdownImplementation);
		
		FileHelper.setFileAttributes( "g+w", recoveryLocation );
		// create new task for snapshoot creation 
		//new RecoverySnapshotTask(prevayler, 1*Sync.ONE_HOUR, recoveryLocation);
		new RecoverySnapshotTask(prevayler, 1*Sync.ONE_MINUTE, recoveryLocation);
		
		// initialize Manager CORBA Proxy (create servant)
		managerProxy = new ManagerProxyImpl(manager);

		//activate object
		managerPOA.activate_object_with_id( MANAGER_ID, managerProxy );
		
		// get object reference from the servant
		org.omg.CORBA.Object obj = managerPOA.servant_to_reference(managerProxy);
		managerReference = ManagerHelper.narrow(obj);
		
		// get IOR
		String ior = orb.object_to_string(managerReference);

		// notify user
		new MessageLogEntry(this, "initializeManager", "Manager activated with " + ior, LoggingLevel.INFO).dispatch();

		// register special service components to the Manager
		manager.setManagerComponentReference(managerReference);

		// register NameService
		if (remoteDirectory != null)
		{
			String reference = ((NamingServiceRemoteDirectory)remoteDirectory).getReference();
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

		// intitialize fedration here - after remote directory is set (if it is)
		// (this is not a nice solution)
		Hashtable federationDirectoryProperties = new Hashtable();
		// set CosNamingFactory 
		federationDirectoryProperties.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.cosnaming.CNCtxFactory");
		// set orb
		federationDirectoryProperties.put("java.naming.corba.orb", orb);
		manager.initializeFederation(federationDirectoryProperties);
	}

	/**
	 * Destroy the Manager.
	 */
	private void destroyManager() throws Exception
	{
		
		new MessageLogEntry(this, "destroyManager", "Destroying Manager.", LoggingLevel.INFO).dispatch();
		
		// firsty destroy Manager implementation (if necessary)		
		if (!manager.isShuttingDown())
		{
			try
			{
				manager.shutdown(HandleConstants.MANAGER_MASK, 0);
			}
			catch (Exception ex)
			{
				CoreException ce = new CoreException(this, "Failed to destroy manager.", ex);
				ce.caughtIn(this, "destroyManager");
	
				new MessageLogEntry(this, "destroyManager", "Failed to destroy manager.", ce, LoggingLevel.WARNING).dispatch();
			}
		}

		// deactivate Manager
		if (managerPOA != null && managerReference != null)
		{
			managerPOA.deactivate_object(MANAGER_ID);
			managerPOA = null;
		}
		
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
}
