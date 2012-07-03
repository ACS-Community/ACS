/*
 * @@COPYRIGHT@@
 */
package com.cosylab.acs.maci.test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.prevayler.implementation.SnapshotPrevayler;

import com.cosylab.acs.maci.ClientInfo;
import com.cosylab.acs.maci.ComponentInfo;
import com.cosylab.acs.maci.ComponentSpec;
import com.cosylab.acs.maci.HandleConstants;
import com.cosylab.acs.maci.manager.ManagerImpl;
import com.cosylab.acs.maci.plug.DefaultCORBAService;
import com.cosylab.cdb.client.CDBAccess;
import com.cosylab.util.FileHelper;

/**
 * ManagerImpl Prevayler tests.
 * Make sure CDB is running.
 * 
 * @author Matej Sekoranja
 * @version @@VERSION@@
 */
public class ManagerPrevaylerTest extends TestCase
{

	/**
	 * Directory name under ACS.temp where recovery fieles are stored
	 */
	private final static String RECOVERY_DIR_NAME = "Manager_Recovery";
	
	static final boolean isPrevaylerDisabled = false;

	ManagerImpl manager;
	
	final Logger logger = Logger.global;

    final int STARTUP_COBS_SLEEP_TIME_MS = 6000;
    final int SLEEP_TIME_MS = 3000;
	
	TestTransport transport;
	
	/**
	 * Constructor for ManagerImplTest
	 * 
	 * @param arg0
	 */
	public ManagerPrevaylerTest(String arg0)
	{
		super(arg0);
	}
	
	private DefaultCORBAService corbaServce;
	private SnapshotPrevayler prevayler = null;

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{
		logger.setLevel(Level.OFF);
		
		manager = new ManagerImpl();
		if (isPrevaylerDisabled)
		{
			prevayler = null;
			System.out.println( "Prevayler disabled!");
		}
		else
		{
			String recoveryLocation = FileHelper.getTempFileName(null, RECOVERY_DIR_NAME);

			System.out.println( "Removing old recovery files.");
			// remove old recovery files
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

			prevayler = new SnapshotPrevayler(manager, recoveryLocation);
		
			manager = (ManagerImpl)prevayler.system();
		}
			
		corbaServce = new DefaultCORBAService(logger);
		manager.initialize(prevayler, new CDBAccess(corbaServce.getORB(), logger), null, logger, null);
		//manager.setDomain(domain);
		
		// enable prevayler (disabled by default)
		manager.getStatePersitenceFlag().set(true);
	
		transport = new TestTransport();
		manager.setTransport(transport);
	}

	/**
	 * @see junit.framework.TestCase#tearDown()
	 */
	protected void tearDown() throws Exception
	{
		if (manager != null)
		{
			manager.shutdown(HandleConstants.MANAGER_MASK, 0);
			manager = null;
		}
		
		if (corbaServce != null)
		{
			corbaServce.destroy();
			corbaServce = null;
		}
	}

	static final int COMPONENTS = 100;
	static final int CLIENTS = 100;
	static final int COMPONENT_ACTIVATION_TIME_MS = 2000;
	
	static final int COUNT_REPORT_EVERY = 100;
	static final int CLIENT_START_DELAY_MS = 100;

	// some debug output
	private AtomicInteger count = new AtomicInteger(0);

	/**
	 * Test getDynamicComponents.
	 */
	public void testGetDynamicComponents() throws Throwable
	{
			TestContainer dynContainer = new TestDynamicContainer("DynContainer");
			dynContainer.setActivationTime(COMPONENT_ACTIVATION_TIME_MS); 
			ClientInfo dynContainerInfo = manager.login(dynContainer);
			assertNotNull(dynContainerInfo);
			
			// wait for container to startup
			try { Thread.sleep(STARTUP_COBS_SLEEP_TIME_MS); } catch (InterruptedException ie) {}

			final ArrayList<Thread> threads = new ArrayList<Thread>(CLIENTS);
			final ArrayList<Integer> clientHandles = new ArrayList<Integer>(CLIENTS);
			
			long startTime = System.currentTimeMillis();
			
			for (int i = 0; i < CLIENTS; i++)
			{
				final String clientName = "client" + i;
				Thread thread = new Thread(new Runnable() {
					
					@Override
					public void run() {
						try
						{
							int clientHandle = dynamicComponentClientInstance(clientName);
							clientHandles.add(clientHandle);
						} catch (Throwable th) {
							th.printStackTrace();
						}
					}
				}, clientName);
				threads.add(thread);
			}
			
			// start all
			for (Thread thread : threads)
				thread.start();
			
			// wait for all to complete
			for (Thread thread : threads)
				thread.join();
			
			
			long endTime = System.currentTimeMillis() - CLIENT_START_DELAY_MS;
			System.out.println((CLIENTS*COMPONENTS) + " components activated in " + (endTime-startTime)/1000 + " seconds");
			
			// bypass prevayler to get serialization size
			ByteArrayOutputStream buffer = new ByteArrayOutputStream();
			ObjectOutputStream oos = new ObjectOutputStream(buffer);
			synchronized (prevayler) {
				oos.writeObject(manager);
			}
			System.out.println("Manager object serialization size: " + buffer.size() + " bytes");
			
			manager.logout(dynContainerInfo.getHandle());

			// logout all the client
			for (int h : clientHandles)
				if (h != 0) manager.logout(h);
			
			buffer.reset();
			oos.reset();
			synchronized (prevayler) {
				oos.writeObject(manager);
			}
			System.out.println("Manager object serialization size after full deactivation: " + buffer.size() + " bytes");
	}

	private int dynamicComponentClientInstance(final String clientInstance) throws Throwable
	{
		// simple trick to align all the threads
		Thread.sleep(CLIENT_START_DELAY_MS);
		
		TestClient client = new TestClient(clientInstance);
		ClientInfo info = manager.login(client);

		// all dynamic case
		try
		{
			for (int i = 0; i < COMPONENTS; i++)
			{
				ComponentInfo componentInfo = manager.getDynamicComponent(info.getHandle(),
						new ComponentSpec(clientInstance + "-dynComponent-" + i, "java.lang.Object",
								"java.lang.Object", "DynContainer"), true);
				assertTrue(componentInfo != null);
				
				int nc = count.incrementAndGet() ; 
				if (nc % COUNT_REPORT_EVERY == 0)
					System.out.println(nc + " components activated until now...");
			}
		}
		catch (Throwable ex)
		{
			ex.printStackTrace();
			fail();
		}

		return info.getHandle();
	}
	
	public static TestSuite suite() {
		return new TestSuite(ManagerPrevaylerTest.class);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(ManagerPrevaylerTest.class);
		System.exit(0);
	}

}

