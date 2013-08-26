package alma.acs.container.corba;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import junit.framework.TestCase;

import org.omg.PortableServer.POA;
import org.omg.PortableServer.Servant;

import alma.acs.container.ComponentServantManager;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentHelper;
import alma.jconttest.DummyComponentPOATie;
import alma.jconttest.DummyComponentImpl.DummyComponentImpl;

/**
 * Tests {@link alma.acs.container.corba.AcsCorba} and related usage of ORB and POAs.
 * Runs without any ACS background support.
 * 
 * @author hsommer
 */
public class AcsCorbaTest extends TestCase {

	private AcsLogger m_logger;
	private AcsCorba acsCorba;
	
	// simple otherthread-to-mainthread exception passing
	private volatile Throwable exceptionInThread;

	
	protected void setUp() throws Exception {
		exceptionInThread = null;
		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("AcsCorbaTest#" + getName(), false);
		ClientLogManager.getAcsLogManager().suppressRemoteLogging();
		m_logger.info("-----------------------------------------------");
		m_logger.info("AcsCorbaTest#setUp()");

//		OrbConfigurator.setDebug(true);  // will give detailed ORB/POA logs to stdout
		acsCorba = new AcsCorba(m_logger);
		acsCorba.initCorba(new String[0], OrbConfigurator.ORB_DEFAULT_PORT);
	}

	protected void tearDown() throws Exception {
		if (acsCorba != null) {
			acsCorba.shutdownORB(true, false);
		}
	}

	/**
	 * Activates / deactivates a component 1000 times.
	 * The component is run outside a container, but with the same ORB/POA usage as inside the container.
	 * <p>
	 * For each iteration, 
	 * <ol>
	 *   <li>component POA is created
	 *   <li>component implementation is created as the servant for our POA
	 *   <li>servant activator {@link ComponentServantManager} is created and registered with POA
	 *   <li>component is activated through component POA
	 *   <li>synchronous call to component method {@link DummyComponentImpl#dummyComponentsCanDoCloseToNothing()} over CORBA.
	 *   <li><b>synchronous</b> call to component method {@link DummyComponentImpl#callThatTakesSomeTime(int)} <b>with zero waiting time</b> over CORBA.
	 *   <li>call to {@link org.jacorb.poa.POA#destroy(boolean, boolean)} with <code>etherialize_objects==true</code>
	 *       and <code>wait_for_completion==false</code>.
	 *   <li>wait for component etherialization using {@link ComponentServantManager#waitForEtherealize(int)}. 
	 * </ol>
	 * <p>
	 * This method should be seen as a suggestion for the Java container to implement component deactivation.
	 * @throws Exception
	 * @see #_testComponentPOALifecycle(boolean, int)
	 */
	public void testComponentPOALifecycleSync() throws Exception { 
		_testComponentPOALifecycle(false, 1000);
	}
	
	/**
	 * This test method is currently hidden from JUnit because it fails with JacORB 1.4. and 2.2.4.
	 * Problem: RequestController#waitForCompletion returns immediately when currently processing calls are local.
	 * This could be related to JacORB bug 132, see http://www.jacorb.org/cgi-bin/bugzilla/show_bug.cgi?id=132. <br>
	 * TODO: try again when the Java ORB gets updated or replaced in ACS.
	 * <p>
	 * Activates / deactivates a component 10 times, each time calling a method that still runs while 
	 * its POA is getting deactivated. This is to check the container's ability to wait for currently 
	 * processing requests to terminate, before <code>cleanUp</code> is called on the component.
	 * <p>
	 * The component is run outside a container, but with the same ORB/POA usage as inside the container.
	 * @throws Exception
	 */
	public void __dontrun__testComponentPOALifecycleAsync() throws Exception { 
		_testComponentPOALifecycle(true, 10);
	}

	
	/**
	 * This test method can also be used to experiment with 
	 * @param destroyWhileBusy
	 * @param iterations
	 * @throws Exception
	 */
	private void _testComponentPOALifecycle(boolean destroyWhileBusy, int iterations) throws Exception {
		final String compName = "virtualTestComp";
		
		for (int i=0; i < iterations; i++) {			
			
			m_logger.info("Will create and destroy component instance #" + i);
			final POA compPOA = acsCorba.createPOAForComponent(compName);
			assertNotNull(compPOA);		
			
			// create a test component servant using that POA
			final SyncDummyComponentImpl impl = new SyncDummyComponentImpl();
			Servant servant = new DummyComponentPOATie(impl);
			
			final ComponentServantManager servantManager = acsCorba.setServantManagerOnComponentPOA(compPOA);

			// activate the component 
			org.omg.CORBA.Object objRef = acsCorba.activateComponent(servant, compName, compPOA);			
			
			// make a simple CORBA call to the component, and then destroy the POA
			final DummyComponent testCompRef = DummyComponentHelper.narrow(objRef);
			testCompRef.dummyComponentsCanDoCloseToNothing();			
			
			if (destroyWhileBusy) {
				final CountDownLatch sync = new CountDownLatch(1);
				impl.setMethodCallSync(sync);
				Runnable compMethodCallRunnable = new Runnable() {
					public void run() {
						try {
							testCompRef.callThatTakesSomeTime(1000);
						} catch (Exception ex) {
							exceptionInThread = ex; 
						}
					}
				};
				m_logger.info("Will destroy component POA while active request is still running.");
				(new Thread(compMethodCallRunnable)).start();
				boolean properSync = sync.await(10000, TimeUnit.MILLISECONDS);
				assertTrue(properSync);
			}
			else {
				testCompRef.callThatTakesSomeTime(0);
			}
			// timeout should be larger than pending call (callThatTakesSomeTime)
			boolean isInactive = acsCorba.deactivateComponentPOAManager(compPOA, compName, 2000);
			assertTrue(isInactive);
			// active calls are supposedly over already, so the timeout can be smaller than execution time for "callThatTakesSomeTime"
			boolean isEtherealized = acsCorba.destroyComponentPOA(compPOA, servantManager, 500);
			assertTrue("Timeout here probably means that 'deactivateComponentPOAManager' did not properly wait for active requests to finish.", isEtherealized); 

			if (exceptionInThread != null) {
				fail("asynchronous component call (#callThatTakesSomeTime) failed: " + exceptionInThread.toString());
			}					
		}
		m_logger.info("Done with testComponentPOALifecycle()");
	}
	
	
	public void testPOAConfig() throws Exception {
		
		final String compName = "virtualTestComp";
		
		POA compPOA = acsCorba.createPOAForComponent(compName);
		org.jacorb.poa.POA jacCompPOA = null;
		if (compPOA instanceof org.jacorb.poa.POA) {
			jacCompPOA = (org.jacorb.poa.POA) compPOA;
		}
		else {
			fail("this test is only meant for JacORB. Instead the POA impl is of type " + compPOA.getClass().getName());
		}
		
//		Policy threadPolicy = jacCompPOA.getPolicy(THREAD_POLICY_ID.value);
//		assertNotNull(threadPolicy); // currently null, which defaults to ORB_CTRL_MODEL (see POA#sSingleThreadModel())
		
		assertTrue(jacCompPOA.isUseServantManager());
		
		String theName = jacCompPOA.the_name();
		assertEquals("unexpected poa name ", "ComponentPOA_" + compName, theName);
		
		String qualName = jacCompPOA._getQualifiedName();
		assertEquals("unexpected qualified poa name ", "ComponentPOA/ComponentPOA_" + compName, qualName);

		String poaId = new String(jacCompPOA.getPOAId());
		assertEquals("unexpected poaId ", "StandardImplName/ComponentPOA/ComponentPOA_" + compName, poaId);
		
		// create a thread-aware test component servant using that POA
		DummyComponentImpl impl = new DummyComponentImpl() {
			public void dummyComponentsCanDoCloseToNothing() {
				Thread orbThread = Thread.currentThread();
				System.out.println("component called in thread " + orbThread.getName());
			}			
		};
		Servant servant = new DummyComponentPOATie(impl);
		
		// activate the component 
		org.omg.CORBA.Object objRef = acsCorba.activateComponent(servant, compName, compPOA);			
		
		// make CORBA calls to the component , and destroy the POA
		DummyComponent testCompRef = DummyComponentHelper.narrow(objRef);
		testCompRef.dummyComponentsCanDoCloseToNothing();

		// analyze thread structure		
		ThreadGroup rootThreadGroup = Thread.currentThread().getThreadGroup();
		while (rootThreadGroup.getParent() != null) {
			rootThreadGroup = rootThreadGroup.getParent();
		}
		ThreadGroup[] allThreadGroups = new ThreadGroup[rootThreadGroup.activeCount()*2]; // hopefully large enough 
		int numThreadGroups = rootThreadGroup.enumerate(allThreadGroups, true);
		for (int i = 0; i < numThreadGroups; i++) {
			System.out.println("thread group " + allThreadGroups[i].getName() + ":");
			Thread[] allThreadsInGroup = new Thread[allThreadGroups[i].activeCount()*2]; // hopefully large enough 
			int numThreads = allThreadGroups[i].enumerate(allThreadsInGroup, false);
			for (int j = 0; j < numThreads; j++) {
				System.out.println("\t" + allThreadsInGroup[j].getName());
			}
		}
		
		compPOA.destroy(false, true);
	}
}

/**
 * Special version of this component for local use. 
 * The CountDownLatch set in <code>setMethodCallSync</code> can be used to synchronize between the client and
 * method invocations on the servant. This allows the client to block until the servant method is actually being executed. 
 * @author hsommer
 */
class SyncDummyComponentImpl extends DummyComponentImpl {
	private volatile CountDownLatch sync;
	void setMethodCallSync(CountDownLatch sync) {
		this.sync = sync;
	}
	public void callThatTakesSomeTime(int timeInMillisec) {
		callSyncNotify();
		super.callThatTakesSomeTime(timeInMillisec);
	}
	public void dummyComponentsCanDoCloseToNothing() {
		callSyncNotify();
		super.dummyComponentsCanDoCloseToNothing();
	}
	private void callSyncNotify() {
		if (sync != null) {
			sync.countDown();
		}
	}
}
