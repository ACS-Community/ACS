/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.plug;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

/**
 * Class that provides default ACS CORBA service implementation.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class DefaultCORBAService implements Runnable
{
	/**
	 * Object Request Broker (ORB) object.
	 */
	private ORB orb = null;

	/**
	 * Additional sync check (what if component is destroyed before thread is started).
	 */
	private volatile boolean destroyState = false;

	/**
	 * Root Portable Object Adapter (POA) object.
	 */
	private POA rootPOA = null;
	
	/**
	 * Logger.
	 */
	private Logger logger = null;

	/**
	 * Constructor for DefaultCORBAService.
	 */
	public DefaultCORBAService(Logger logger)
	{
		this.logger = logger;
		internalInitialize();
	}

	/**
	 * Initializes the CORBA.
	 */
	private void internalInitialize() 
	{

		// ORB stanza
		Properties orbprops = System.getProperties();
		
		Properties serviceConfig = new Properties();

		serviceConfig.put("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
		serviceConfig.put("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");

		serviceConfig.put("jacorb.implname", "ORB");

		serviceConfig.put("jacorb.orb.objectKeyMap.Manager", "ORB/ManagerPOA/Manager");

		serviceConfig.put("jacorb.verbosity", "1");

		serviceConfig.put("org.omg.PortableInterceptor.ORBInitializerClass.bidir_init",
				"org.jacorb.orb.connection.BiDirConnectionInitializer");

		// add additional properties
		if (serviceConfig != null)
			orbprops.putAll(serviceConfig);
		
		orb = ORB.init(new String[0], orbprops);
			
		// POA stanza, use rootPOA
		try
		{
			// resolve RootPOA
			rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
				
			// activate POA
			POAManager manager = rootPOA.the_POAManager();
			manager.activate();
		
		} catch (Throwable th) {
			logger.log(Level.SEVERE, "Failed to initialize CORBA.", th);
		}
		
		// spawn ORB thread (to process incoming requests)
		new Thread(this, "DefaultCORBAService").start();
	}

	/**
	 * Overloads the destroy to first perform a ORB shutdown.
	 */
	public void destroy()
	{
		destroyState = true;
		
		// destory ORB
		if (orb != null)
		{
			try
			{
				// possible solution with orb.work_pending
				// but JacORB has no implementation of it
				
				// do not wait for completion
				orb.shutdown(false);
				
				// and finally destroy
				orb.destroy();
				orb = null;
			}
			catch (Throwable th) {
				// @TODO revisit org.omg.CORBA.COMM_FAILURE in ORB.shutdown() after JacORB upgrade
				//logger.log(Level.FINER, "Harmless exception caught while destroying ORB.", th);
			}
		}

	}
	
	/**
	 * Returns Object Request Broker (ORB) object.
	 * 
	 * @return		Object Request Broker (ORB) object
	 */
	public ORB getORB()
	{
		return orb;
	}

	/**
	 * Returns root Portable Object Adapter (POA) object.
	 * 
	 * @return		root Portable Object Adapter (POA) object
	 */
	public POA getRootPOA()
	{
		return rootPOA;
	}

	/**
	 * Main thread to handle CORBA requests.
	 * 
	 * @see java.lang.Runnable#run()
	 */
	public void run()
	{
		ORB localORBRef = orb;
		if (!destroyState && localORBRef != null)
			localORBRef.run();
	}

}
