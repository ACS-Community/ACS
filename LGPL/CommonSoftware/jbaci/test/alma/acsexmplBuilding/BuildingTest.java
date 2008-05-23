/*
 * @@COPYRIGHT@@
 */


package alma.acsexmplBuilding;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import si.ijs.maci.AuthenticationData;
import si.ijs.maci.Client;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientPOA;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ImplLangType;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBstringPOA;
import alma.ACS.Monitor;
import alma.ACS.ROstring;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.acs.util.ACSPorts;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;

/**
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class BuildingTest implements Runnable
{

	/**
	 * MACI Client implementation.
	 */
	private class ClientImpl extends ClientPOA
	{

		public AuthenticationData authenticate(long execution_id, String question)
		{
			AuthenticationData ret = new AuthenticationData(
					 "C", 
					 ClientType.CLIENT_TYPE,
					 ImplLangType.JAVA,
					 false, 
					 UTCUtility.utcJavaToOmg(System.currentTimeMillis()),
					 execution_id);
			return ret;
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

		public void taggedmessage(short arg0, short arg1, String arg2) {
			// TODO Auto-generated method stub
		}

		/**
		 * @see si.ijs.maci.ClientOperations#name()
		 */
		public String name() {
			return BuildingTest.this.getClass().getName();
		}

		/**
		 * @see si.ijs.maci.ClientOperations#ping()
		 */
		public boolean ping() {
			return true;
		}


	}

	/**
	 * String callback implementation.
	 */
	private class CBstringImpl extends CBstringPOA
	{
		/**
		 * ISO 8601 date formatter.
		 */
		private SimpleDateFormat timeFormatter = new IsoDateFormat();

		/**
		 * @see alma.ACS.CBstringOperations#done(java.lang.String, alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public void done(String value, Completion completion, CBDescOut desc) {
			System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (done) Value: " + value);
		}

		/**
		 * @see alma.ACS.CBstringOperations#working(java.lang.String, alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public void working(String value, Completion completion, CBDescOut desc) {
			System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (working) Value: " + value);
		}

		/**
		 * @see alma.ACS.CallbackOperations#negotiate(long, alma.ACS.CBDescOut)
		 */
		public boolean negotiate(long timeout, CBDescOut completion) {
			return false;
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
			managerReference = "corbaloc::" + ACSPorts.getIP() + ":3000/Manager";

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
				// get component
				//
				final String COMPONENT_NAME = "BUILDING1";
				org.omg.CORBA.Object obj;
				try {
					obj = manager.get_component(clientInfo.h, COMPONENT_NAME, true);
					Building building = BuildingHelper.narrow(obj);
					ROstring version = building.version();
					
					// syncvalue retrival
					System.out.println("Version: " + version.get_sync(new CompletionHolder()));
					
					// create a monitor
					Monitor monitor = version.create_monitor(new CBstringImpl()._this(orb), new CBDescIn());
					
					// sleep for a while
					try { Thread.sleep(10000); } catch (InterruptedException ie) {};
					
					monitor.destroy();
				} catch (Exception e) { // CannotGetComponentEx, ComponentConfigurationNotFoundEx
					System.err.println("Failed to obtain component: " + COMPONENT_NAME);
				}
				
				// release now
				try {
				   manager.release_component(clientInfo.h, COMPONENT_NAME);
				}  catch (Exception ex) {
				   ex.printStackTrace();
				   System.out.println("Failed to release component.");
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
		new BuildingTest().test();
	}


}
