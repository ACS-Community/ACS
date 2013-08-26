/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.test;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import alma.acs.util.ACSPorts;

import si.ijs.maci.AuthenticationData;
import si.ijs.maci.Client;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientPOA;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ComponentSpec;
import si.ijs.maci.ImplLangType;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;

import alma.maciErrType.InvalidComponentSpecEx;
import alma.maciErrType.NoDefaultComponentEx;

/**
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class ManagerDynComponentTest implements Runnable
{

	/**
	 * MACI Client implementation.
	 */
	private class ClientImpl extends ClientPOA
	{

		/**
		 * @see com.cosylab.acs.maci.Client#authenticate(long, String)
		 */
		public AuthenticationData authenticate(long executionId, String question)
		{
			return new AuthenticationData("", ClientType.CLIENT_TYPE, ImplLangType.JAVA, false, System.currentTimeMillis(), executionId);
		}

		/**
		 * @see si.ijs.maci.ClientOperations#components_available(si.ijs.maci.ComponentInfo[])
		 */
		public void components_available(ComponentInfo[] arg0) {
		}

		/**
		 * @see si.ijs.maci.ClientOperations#components_unavailable(java.lang.String[])
		 */
		public void components_unavailable(String[] arg0) {
		}

		/**
		 * @see si.ijs.maci.ClientOperations#disconnect()
		 */
		public void disconnect() {
			System.out.println("Manager requires disconnection.");
		}

		/**
		 * @see si.ijs.maci.ClientOperations#message(short, java.lang.String)
		 */
		public void message(short type, String message) {
			System.out.println("Message from manager: " + message);
		}

		/**
		 * @see si.ijs.maci.ClientOperations#taggedmessage(short, short, java.lang.String)
		 */
		public void taggedmessage(short type, short tag, String message) {
		        System.out.print("Tagged message from manager: Tag(");
		        System.out.print(tag);
			System.out.println(") Message: " + message);
		}

		/**
		 * @see si.ijs.maci.ClientOperations#name()
		 */
		public String name() {
			return ManagerDynComponentTest.this.getClass().getName();
		}

		/**
		 * @see si.ijs.maci.ClientOperations#ping()
		 */
		public boolean ping() {
			return true;
		}

	}


	/**
	 * CORBA ORB.
	 */
	private ORB orb;

	/**
	 * CORBA ORB.run() event-handler thread.
	 */
	private Thread orbThread;

	/**
	 * Initializes CORBA.
	 */
	private void initializeCORBA()
	{
		System.out.println("Initializing CORBA...");

		// ORB stanza
		java.util.Properties orbprops = java.lang.System.getProperties();

		// to make code completely independed, properties have to be set using JVM -D mechanism

		// ORBacus
		//orbprops.put("org.omg.CORBA.ORBClass", "com.ooc.CORBA.ORB");
		//orbprops.put("org.omg.CORBA.ORBSingletonClass", "com.ooc.CORBA.ORBSingleton");

		// JacORB
		//orbprops.put("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
		//orbprops.put("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");

		// Java JDK (none)

		orb = org.omg.CORBA.ORB.init(new String[0], orbprops);

		// POA stanza -- use RootPOA
		POA rootPOA = null;
		try
		{
			rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		} catch (org.omg.CORBA.ORBPackage.InvalidName in) {
			throw new IllegalStateException("Cannot resolve RootPOA: " + in);
		}
		
		POAManager manager = rootPOA.the_POAManager();
		try
		{
			// activate POA
			manager.activate();

			// start CORBA event-handler thread 
			orbThread = new Thread(this);
			orbThread.start();
		} catch (Exception e)
		{
			throw new IllegalStateException("POAManager activation failed: " + e);
		}

		System.out.println("CORBA initialized.");
	}

	/**
	 * Initializes CORBA.
	 */
	private void finalizeCORBA()
	{
		System.out.println("Finalizing CORBA...");

		if (orb != null)
			orb.shutdown(true);

		System.out.println("CORBA finalized.");
	}

	/**
	 * @see java.lang.Runnable#run()
	 */
	public void run()
	{
		if (orb != null)
			orb.run();
	}
	
	/**
	 * Resolves manager reference.
	 * @return manager reference, <code>null</code> on failure.
	 */
	private Manager resolveManager()
	{
		if (orb == null)
			return null;
			
		String managerReference = System.getProperty("ACS.manager");
		if (managerReference == null)
			managerReference = "corbaloc:" + ACSPorts.getIP() + ":" + ACSPorts.getManagerPort() + "/Manager";

		System.out.println("Resolving manager reference '" + managerReference + "'.");

		try
		{
			org.omg.CORBA.Object obj = orb.string_to_object(managerReference);
			if (obj == null)
				throw new NullPointerException("'null' reference returned.");
				
			Manager manager = ManagerHelper.narrow(obj);
			if (manager == null)
				throw new NullPointerException("'null' narrowed reference returned.");
			
			System.out.println("Manager reference successfully resolved.");
			
			return manager;
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			System.out.println("Failed to resolve manager reference.");
			return null;
		}

	}
	
	/**
	 * Login to the manager.
	 * @param	manager	manager reference.
	 * @return	client info, <code>null</code> on failure.
	 */
	private ClientInfo login(Manager manager)
	{
		if (orb == null || manager == null)
			return null;
			
		System.out.println("Logging to the manager...");

		try
		{
			ClientImpl clientImpl = new ClientImpl();
			Client client = clientImpl._this(orb);

			ClientInfo info = manager.login(client);
			if (info == null)
				throw new Exception("Failed to login to the manager since returned ClientInfo is 'null'.");

			System.out.println("Logged in to the manager.");
				
			return info;
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			System.out.println("Failed to login to the manager.");
			return null;
		}

	}

	/**
	 * Logout from the manager.
	 * @param	manager	manager reference.
	 */
	private void logout(Manager manager, ClientInfo clientInfo)
	{
		if (orb == null || manager == null)
			return;
			
		System.out.println("Logging out from the manager...");

		try
		{
			manager.logout(clientInfo.h);

			System.out.println("Logged out from the manager.");
				
		}
		catch (Exception ex)
		{
			ex.printStackTrace();
			System.out.println("Failed to logout from the manager.");
		}

	}

	/**
	 * Main test routine.
	 */
	public void test()
	{
		initializeCORBA();

		Manager manager = resolveManager();
		if (manager != null)
		{
			ClientInfo clientInfo = login(manager);
			if (clientInfo != null)
			{
				System.out.println("All initialization done.");
				
				try
				{
				
					/* ------------------------------------------------- */
					
					manager.restart_component(clientInfo.h, "invalid");
			
					manager.get_component(clientInfo.h, "MOUNT1", true);
					
					manager.restart_component(clientInfo.h, "MOUNT1");
					
					/* ------------------------------------------------- */
	
					try
					{
						manager.get_default_component(clientInfo.h, "invalid");
						System.err.println("NoDefaultComponent exception expected.");
					}
					catch (NoDefaultComponentEx nde)
					{
						// this is OK
					}

					/* ------------------------------------------------- */
	
					try
					{
						manager.get_dynamic_component(clientInfo.h, new ComponentSpec("*", "invalidType", "*", "*"), true);
						System.err.println("InvalidComponentSpec exception expected.");
					}
					catch (InvalidComponentSpecEx ics)
					{
						// this is OK
					}

					/* ------------------------------------------------- */

					ComponentInfo componentInfo = manager.get_dynamic_component(clientInfo.h, new ComponentSpec("FULL_DYNAMIC", "IDL:alma/PS/PowerSupply:1.0", "acsexmplPS", "Container"), true);
					System.out.println(componentInfo.h);
					
				}
				catch (Exception ex)
				{
					ex.printStackTrace();
				}
				
				logout(manager, clientInfo);
			}
		}
		
		finalizeCORBA();
	}


	/**
	 * Main entry point.
	 * @param args
	 */
	public static void main(String[] args)
	{
		new ManagerDynComponentTest().test();
	}

}
