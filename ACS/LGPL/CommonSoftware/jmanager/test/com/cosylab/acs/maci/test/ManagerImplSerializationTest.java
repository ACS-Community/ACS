package com.cosylab.acs.maci.test;

import java.io.File;
import java.util.Properties;

import com.cosylab.acs.maci.manager.HandleDataStore;
import com.cosylab.acs.maci.manager.ManagerImpl;
import com.cosylab.acs.maci.plug.ClientProxyImpl;
import com.cosylab.util.FileHelper;

import junit.framework.TestCase;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;
import org.prevayler.PrevalentSystem;
import org.prevayler.implementation.SnapshotPrevayler;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;

import alma.acs.util.ACSPorts;

/**
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class ManagerImplSerializationTest extends TestCase {

	class MyManager extends com.cosylab.acs.maci.manager.app.Manager {

		public void internalDestroy() {
			super.internalDestroy();
			notify();
		}

	}
	
	public final String clientName = "testClient";
	public final String managerReference = "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getManagerPort() + "/Manager";

	protected ORB orb;
	protected Manager manager; // CORBA object as it is seen by others
	protected String recoveryLocation;
	protected MyManager myManager; // my runner for ManagerEngine

	/**
	 * Constructor for ManagerImplSerializationTest.
	 * @param arg0
	 */
	public ManagerImplSerializationTest(String arg0) {
		super(arg0);
	}

	/**
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		
		Properties systemProps = System.getProperties(); 
		
		//systemProps.setProperty("ACS.tmp", ".");
		//String namingReference = "iiop://" + InetAddress.getLocalHost().getHostName() + ":xxxx";
		//System.out.println("***///*** Using naming reference " + namingReference);
		//systemProps.setProperty("NamingServiceRemoteDirectory.reference", namingReference);
		//String dalReference ="corbaloc::" + InetAddress.getLocalHost().getHostName() + ":"+ACSPorts.getCDBPort()+"/CDB";
		//systemProps.setProperty("DAL.defaultReference", dalReference);

		// set port
		String managerPort = String.valueOf(ACSPorts.getManagerPort());
		systemProps.setProperty("OAPort", managerPort);

		// empty logs and snapshoots
		recoveryLocation = FileHelper.getTempFileName(null, "Manager_Recovery");
		File file = new File(recoveryLocation);
		File[] files = file.listFiles();
		for (int i = 0; files != null && i < files.length; i++)
			files[i].delete();


		// get the manager up
		myManager = new MyManager();

		// get its reference 
		try {
			manager = ManagerHelper.narrow(orb.string_to_object(managerReference));
			if( manager == null )
				fail();
		} catch (RuntimeException e) {
			e.printStackTrace();
			fail();
		}
	}

	/**
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		super.tearDown();

		synchronized (myManager) {
			manager.shutdown(0x05000000, 0);
			myManager.wait();
		}
		orb = null;
	}

	public PrevalentSystem deserializeManager(PrevalentSystem system) {
		SnapshotPrevayler prevayler = null;
		try {
			prevayler = new SnapshotPrevayler(system, recoveryLocation);
		}
		catch (Exception e) {
			e.printStackTrace();
			fail();
		}
		return prevayler.system();
	}

	public void testClientLogin() {
		// login a client
		ClientProxyImpl client = new ClientProxyImpl(clientName, myManager.getManagerEngine().getLogger());
		client.login(orb, manager);

		// get object as it is stored in recovery store
		ManagerImpl newManager = (ManagerImpl) deserializeManager(new ManagerImpl());

		// since we used clean recovery store we must have our client only there    
		HandleDataStore clients = newManager.getClients();
		assertEquals(clients.first(), 1);
		com.cosylab.acs.maci.ClientInfo clientInfo = (com.cosylab.acs.maci.ClientInfo) clients.get(clients.first());
		assertEquals(clientInfo.getName(), clientName);

		client.logout();

		// read state again
		newManager = (ManagerImpl) deserializeManager(new ManagerImpl());
		clients = newManager.getClients();
		assertEquals(clients.first(), 0);
	}

	public void testClientDie() {
		// should be something else then deafault because
		// the default is in use by the Manager
		Properties table = new Properties();
		table.put("OAPort", "12121");
		// new ORB instance
		ORB ourOrb = ORB.init(new String[0], table);
		try
		{
			POA rootPOA = POAHelper.narrow(ourOrb.resolve_initial_references("RootPOA"));
			
			// activate POA
			POAManager manager = rootPOA.the_POAManager();
			manager.activate();

		} catch (Exception e)
		{
			e.printStackTrace();
		}
		
		ClientProxyImpl client = new ClientProxyImpl(clientName, myManager.getManagerEngine().getLogger());
		client.login(ourOrb, manager);
		// just destroy its ORB
		ourOrb.shutdown(true);

		// get object as it is stored in recovery store
		ManagerImpl newManager = (ManagerImpl) deserializeManager(new ManagerImpl());
		HandleDataStore clients = newManager.getClients();
		// the client now is still in stored data
		assertEquals(clients.first(), 1);
		newManager.initialize(null,null,null,null,null);
		// now wait for timer task to remove client
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
			e.printStackTrace();
			fail();
		}

		// since the client died the menager automaticaly log it out    
		clients = newManager.getClients();
		assertEquals(clients.first(), 0); // not any more
	}

	public void testContainerDie() throws Throwable {
		// should be something else then deafault because
		// the default is in use by the Manager
		Properties table = new Properties();
		table.put("OAPort", "12121");
		// new ORB instance
		ORB ourOrb = ORB.init(new String[0], table);
		try
		{
			POA rootPOA = POAHelper.narrow(ourOrb.resolve_initial_references("RootPOA"));
			
			// activate POA
			POAManager manager = rootPOA.the_POAManager();
			manager.activate();

		} catch (Exception e)
		{
			e.printStackTrace();
		}
		
		ContainerProxyImpl activator = new ContainerProxyImpl(clientName);
		si.ijs.maci.ClientInfo clientInfo = manager.login(activator._this(ourOrb));
		if (clientInfo == null || clientInfo.h == 0)
			fail("Unable to login to manager");
		
		// just destroy its ORB
		ourOrb.shutdown(true);

		// get object as it is stored in recovery store
		ManagerImpl newManager = (ManagerImpl) deserializeManager(new ManagerImpl());
		HandleDataStore activators = newManager.getContainers();
		// the client now is still in stored data
		assertEquals(activators.first(), 1);
		newManager.initialize(null,null,null,null,null);
		// now wait for timer task to remove client
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
			e.printStackTrace();
			fail();
		}

		// since the client died the menager automaticaly log it out    
		activators = newManager.getClients();
		assertEquals(activators.first(), 0); // not any more
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(ManagerImplSerializationTest.class);
		System.exit(0);
	}
}
