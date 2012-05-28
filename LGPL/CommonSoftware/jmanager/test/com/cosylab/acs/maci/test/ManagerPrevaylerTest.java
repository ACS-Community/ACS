/*
 * @@COPYRIGHT@@
 */
package com.cosylab.acs.maci.test;

import java.util.ArrayList;
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
	
	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{
		logger.setLevel(Level.OFF);
		
		manager = new ManagerImpl();
	    SnapshotPrevayler prevayler = null;
		if (isPrevaylerDisabled)
		{
			System.out.println( "Prevayler disabled!");
		}
		else
		{
			String recoveryLocation = FileHelper.getTempFileName(null, RECOVERY_DIR_NAME);
			prevayler = new SnapshotPrevayler(manager, recoveryLocation);
		
			manager = (ManagerImpl)prevayler.system();
		}
			
		corbaServce = new DefaultCORBAService(logger);
		manager.initialize(prevayler, new CDBAccess(corbaServce.getORB(), logger), null, logger, null);
		//manager.setDomain(domain);
	
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
	static final int CLIENTS = 10;

	/**
	 * Test getDynamicComponents.
	 */
	public void testGetDynamicComponents() throws Throwable
	{
			TestContainer dynContainer = new TestDynamicContainer("DynContainer");
			ClientInfo dynContainerInfo = manager.login(dynContainer);
			assertNotNull(dynContainerInfo);
			
			// wait for container to startup
			try { Thread.sleep(STARTUP_COBS_SLEEP_TIME_MS); } catch (InterruptedException ie) {}

			ArrayList<Thread> threads = new ArrayList<Thread>(CLIENTS);
			for (int i = 0; i < CLIENTS; i++)
			{
				final String clientName = "client" + i;
				Thread thread = new Thread(new Runnable() {
					
					@Override
					public void run() {
						try
						{
							dynamicComponentClientInstance(clientName);
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
	}

	private void dynamicComponentClientInstance(final String clientInstance) throws Throwable
	{
		// simple trick to align all the threads
		Thread.sleep(1000);
		
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
			}
		}
		catch (Throwable ex)
		{
			ex.printStackTrace();
			fail();
		}
	}
	
	public static TestSuite suite() {
		return new TestSuite(ManagerPrevaylerTest.class);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(ManagerPrevaylerTest.class);
		System.exit(0);
	}

}

