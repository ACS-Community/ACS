/*
 * @@COPYRIGHT@@
 */


package test.jbaci;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.omg.CORBA.IntHolder;
import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBstringSeqPOA;
import alma.ACS.MonitorstringSeq;
import alma.ACS.ROstringSeq;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;

import si.ijs.maci.Client;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientPOA;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;
import alma.acs.util.ACSPorts;
import alma.acs.util.UTCUtility;
import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;

/**
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class SimpleMasterComponentTest implements Runnable
{

	/**
	 * MACI Client implementation.
	 */
	private class ClientImpl extends ClientPOA
	{

		/**
		 * @see si.ijs.maci.ClientOperations#authenticate(java.lang.String)
		 */
		public String authenticate(String arg0) {
			return "C";
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
		 * @see si.ijs.maci.ClientOperations#name()
		 */
		public String name() {
			return SimpleMasterComponentTest.this.getClass().getName();
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
	private class CBstringSeqImpl extends CBstringSeqPOA
	{
		/**
		 * ISO 8601 date formatter.
		 */
		private SimpleDateFormat timeFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS");

		/**
		 * @see alma.ACS.CBstringSeqOperations#done(java.lang.String[], alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public void done(String[] value, Completion completion, CBDescOut desc) {
			System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (done) Value: " + getStringArray(value));
		}

		/**
		 * @see alma.ACS.CBstringSeqOperations#working(java.lang.String[], alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public void working(String[] value, Completion completion, CBDescOut desc) {
			System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (working) Value: " + getStringArray(value));
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
			managerReference = "corbaloc::"+ ACSPorts.getIP() +  ":3000/Manager";

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
	 * Helper method to prcreate string array.
	 * @param	array	array of strings.
	 */
	private static String getStringArray(String[] array)
	{
		if (array == null)
			return "null";

		StringBuffer buff = new StringBuffer(100);			
		buff.append('[');
		
		for (int i = 0; i < array.length-1; i++)
		{
			buff.append(array[i]);
			buff.append(", ");
		}
			
		if (array.length > 0)
			buff.append(array[array.length-1]);
			
		buff.append(']');
		
		return buff.toString();
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
				final String COMPONENT_NAME = "SIMPLEMASTERCOMPONENT1";
				org.omg.CORBA.Object obj;
				try {
					obj = manager.get_component(clientInfo.h, COMPONENT_NAME, true);
					SimpleMasterComponent simpleMasterComponent = SimpleMasterComponentHelper.narrow(obj);
					ROstringSeq currentStateHierarchy = simpleMasterComponent.currentStateHierarchy();
					
					// syncvalue retrival
					System.out.println("Current state hierarchy: " +
						getStringArray(currentStateHierarchy.get_sync(new CompletionHolder())));

					System.out.println("Monitoring - 1s interval...");
					
					// create a monitor
					MonitorstringSeq monitor = currentStateHierarchy.create_monitor(new CBstringSeqImpl()._this(orb), new CBDescIn());
					
					// sleep for a while
					try { Thread.sleep(10000); } catch (InterruptedException ie) {};
					
					monitor.set_timer_trigger(0);
					monitor.set_value_trigger(new String[0], true);
					
					System.out.println("On-change monitoring...");

					// sleep for a while
					try { Thread.sleep(10000); } catch (InterruptedException ie) {};
					
					monitor.destroy();					
				} catch (Exception e) { // CannotGetComponentEx, ComponentConfigurationNotFoundEx
					System.err.println("Failed to obtain component: " + COMPONENT_NAME);
				}

				// release now
				manager.release_components(clientInfo.h, new String[] { COMPONENT_NAME });

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
		new SimpleMasterComponentTest().test();
	}


}
