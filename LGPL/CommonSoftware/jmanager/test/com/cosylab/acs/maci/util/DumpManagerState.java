package com.cosylab.acs.maci.util;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import com.cosylab.acs.maci.HandleHelper;

import si.ijs.maci.AdministratorPOA;
import si.ijs.maci.AuthenticationData;
import si.ijs.maci.Client;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.ImplLangType;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;


/**
 * @author msekoranja
 */
public class DumpManagerState implements Runnable
{

	/**
	 * MACI Client implementation.
	 */
	private class ClientImpl extends AdministratorPOA
	{

		/**
		 * @see si.ijs.maci.ClientOperations#authenticate(long, java.lang.String)
		 */
		public AuthenticationData authenticate(long executionId, String question) {
			return new AuthenticationData("S", ClientType.ADMINISTRATOR_TYPE, ImplLangType.JAVA, false, System.currentTimeMillis(), executionId);
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
		 * @see si.ijs.maci.ClientOperations#taggedmessage(short, java.lang.String)
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
			return DumpManagerState.this.getClass().getName();
		}

		/**
		 * @see si.ijs.maci.ClientOperations#ping()
		 */
		public boolean ping() {
			return true;
		}

		/* (non-Javadoc)
		 * @see si.ijs.maci.AdministratorOperations#client_logged_in(si.ijs.maci.ClientInfo, long, long)
		 */
		public void client_logged_in(ClientInfo info, long timestamp, long execution_id) {
			// TODO Auto-generated method stub
			
		}

		/* (non-Javadoc)
		 * @see si.ijs.maci.AdministratorOperations#client_logged_out(int, long)
		 */
		public void client_logged_out(int h, long timestamp) {
			// TODO Auto-generated method stub
			
		}

		/* (non-Javadoc)
		 * @see si.ijs.maci.AdministratorOperations#component_activated(si.ijs.maci.ComponentInfo, long, long)
		 */
		public void component_activated(ComponentInfo info, long timestamp, long execution_id) {
			// TODO Auto-generated method stub
			
		}

		/* (non-Javadoc)
		 * @see si.ijs.maci.AdministratorOperations#component_deactivated(int, long)
		 */
		public void component_deactivated(int h, long timestamp) {
			// TODO Auto-generated method stub
			
		}

		/* (non-Javadoc)
		 * @see si.ijs.maci.AdministratorOperations#components_released(int[], int[], long)
		 */
		public void components_released(int[] clients, int[] components, long timestamp) {
			// TODO Auto-generated method stub
			
		}

		/* (non-Javadoc)
		 * @see si.ijs.maci.AdministratorOperations#components_requested(int[], int[], long)
		 */
		public void components_requested(int[] clients, int[] components, long timestamp) {
			// TODO Auto-generated method stub
			
		}

		/* (non-Javadoc)
		 * @see si.ijs.maci.AdministratorOperations#container_logged_in(si.ijs.maci.ContainerInfo, long, long)
		 */
		public void container_logged_in(ContainerInfo info, long timestamp, long execution_id) {
			// TODO Auto-generated method stub
			
		}

		/* (non-Javadoc)
		 * @see si.ijs.maci.AdministratorOperations#container_logged_out(int, long)
		 */
		public void container_logged_out(int h, long timestamp) {
			// TODO Auto-generated method stub
			
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
			managerReference = "corbaloc::localhost:3000/Manager";

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

				//
				// list all active components
				//
				
				try
				{
					ComponentInfo[] componentInfo = manager.get_component_info(clientInfo.h, new int[0], "*", "*", true);
					if (componentInfo == null)
						throw new Exception("null client info returned.");
						
					System.out.println(componentInfo.length + " component(s) returned:");
					for (int i = 0; i < componentInfo.length; i++)
					{
						System.out.println("\tName            : " + componentInfo[i].name);
						System.out.println("\tHandle          : " + componentInfo[i].h + ", " + HandleHelper.toString(componentInfo[i].h));
						System.out.println("\tType            : " + componentInfo[i].type);
						System.out.println("\tCode            : " + componentInfo[i].code);
						System.out.println("\tContainer name  : " + componentInfo[i].container_name);
						System.out.println("\tContainer handle: " + HandleHelper.toString(componentInfo[i].container));
						System.out.println("\tClients         : count = " + componentInfo[i].clients.length);
						for (int j = 0; j < componentInfo[i].clients.length; j++)
							System.out.println("\t             \t" + componentInfo[i].clients[j]);
						System.out.println("\t-------------------------------");
					}
				}
				catch (Throwable th)
				{
					th.printStackTrace();
				}
				
				System.out.println();
				System.out.println();
				System.out.println();
				
				//
				// list all active containers
				//
				
				try
				{
					ContainerInfo[] containersInfo = manager.get_container_info(clientInfo.h, new int[0], "*");
					if (containersInfo == null)
						throw new Exception("null container info returned.");
						
					System.out.println(containersInfo.length + " container(s) returned:");
					for (int i = 0; i < containersInfo.length; i++)
					{
						System.out.println("\tName            : " + containersInfo[i].name);
						System.out.println("\tHandle          : " + containersInfo[i].h + ", " + HandleHelper.toString(containersInfo[i].h));
						System.out.println("\tComponents      : count = " + containersInfo[i].components.length);
						for (int j = 0; j < containersInfo[i].components.length; j++)
							System.out.println("\t             \t" + containersInfo[i].components[j]);
						System.out.println("\t-------------------------------");
					}
				}
				catch (Throwable th)
				{
					th.printStackTrace();
				}

				System.out.println();
				System.out.println();
				System.out.println();

				//
				// list all active clients
				//
				
				try
				{
					ClientInfo[] clientsInfo = manager.get_client_info(clientInfo.h, new int[0], "*");
					if (clientsInfo == null)
						throw new Exception("null component info returned.");
						
					System.out.println(clientsInfo.length + " clients(s) returned:");
					for (int i = 0; i < clientsInfo.length; i++)
					{
						System.out.println("\tName            : " + clientsInfo[i].name);
						System.out.println("\tHandle          : " + clientsInfo[i].h + ", " + HandleHelper.toString(clientsInfo[i].h));
						System.out.println("\tComponents      : count = " + clientsInfo[i].components.length);
						for (int j = 0; j < clientsInfo[i].components.length; j++)
							System.out.println("\t             \t" + clientsInfo[i].components[j]);
						System.out.println("\t-------------------------------");
					}
				}
				catch (Throwable th)
				{
					th.printStackTrace();
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
		new DumpManagerState().test();
	}


}
