package alma.acs.container.corba;

import java.util.logging.Level;

import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.client.ComponentClientTestCase;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentHelper;

/**
 * Tests {@link alma.acs.container.corba.AcsCorba} and related usage of ORB and POAs.
 * Requires ACS runtime with a remote container.
 * 
 * @author hsommer
 * @see alma.acs.container.corba.AcsCorbaTest 
 */
public class AcsCorbaTestWithContainer extends ComponentClientTestCase {

	private static final String DUMMYCOMP_TYPENAME = "IDL:alma/jconttest/DummyComponent:1.0";
	private DummyComponent dummyComponent;

	// simple otherthread-to-mainthread exception passing
	private volatile Throwable exceptionInThread;

	public AcsCorbaTestWithContainer() throws Exception {
		super("AcsCorbaTestWithContainer");
	}

	
	protected void setUp() throws Exception {
		Thread.sleep(2000); // to make sure that logging client is up and captures all logs
		super.setUp();
	}

	protected void tearDown() throws Exception {
		m_logger.info("done, tearDown");
		super.tearDown();
	}

	
	public void testParallelCalls() throws Exception {
		org.omg.CORBA.Object compObj = getContainerServices().getDynamicComponent(new ComponentQueryDescriptor(null, DUMMYCOMP_TYPENAME), false);
		assertNotNull(compObj);
		dummyComponent = DummyComponentHelper.narrow(compObj);
		String compName = dummyComponent.name();
		assertNotNull(compName);
	
		exceptionInThread = null;
		
		// run a call to 'callThatTakesSomeTime' from a client thread
		Runnable compMethodCallRunnable = new Runnable() {
			public void run() {
				try {
					dummyComponent.callThatTakesSomeTime(2000);
				} catch (Exception ex) {
					m_logger.log(Level.SEVERE, "Async client call 'dummyComponent#callThatTakesSomeTime' failed with exception.", ex);
					exceptionInThread = ex;
				}
			}
		};
		(new Thread(compMethodCallRunnable)).start();
		
		// now run another call from the main thread
		Thread.sleep(500); // just to make sure the first call is out
		compMethodCallRunnable.run();

		// some other parallel call that uses ContainerServices
		getContainerServices().getDynamicComponent(new ComponentQueryDescriptor(null, DUMMYCOMP_TYPENAME), false);
		
		assertNull("got an exception in the first of two calls", exceptionInThread);
	}
	
	
	/**
	 * Activates / deactivates a component 10 times, each time calling a method that still runs while 
	 * its POA is getting deactivated. The purpose is to check the container's ability to wait for currently 
	 * processing requests to terminate, before <code>cleanUp</code> is called on the component.
	 * <p>
	 * TODO: extend this scenario so that another collocated component issues the long lasting call.
	 * This may not work due to a JacORB bug, which we should find out about. 
	 * Confer the currently disabled test {@link alma.acs.container.corba.AcsCorbaTest#__testComponentPOALifecycleAsync()}.
	 * 
	 * @throws Exception
	 */
	public void testComponentPOALifecycleAsync() throws Exception { 
		_testComponentPOALifecycle(true, 10);
	}

	
	/**
	 * @param destroyWhileBusy
	 * @param iterations
	 * @throws Exception
	 */
	private void _testComponentPOALifecycle(boolean destroyWhileBusy, int iterations) throws Exception {
		// times in milliseconds
		final int remoteCallDurationMin = 2000;
		final int callOverheadMax = 500;
				
		for (int i=0; i < iterations; i++) {
			
			m_logger.info("Will create, use and destroy component instance #" + i);

			org.omg.CORBA.Object compObj = getContainerServices().getDynamicComponent(
					new ComponentQueryDescriptor(null, DUMMYCOMP_TYPENAME), false);
			assertNotNull(compObj);
			dummyComponent = DummyComponentHelper.narrow(compObj);
			String compName = dummyComponent.name();
			assertNotNull(compName);
		
			exceptionInThread = null;
			
			// make CORBA calls to the component, and have it destroyed
			dummyComponent.dummyComponentsCanDoCloseToNothing();			
			
			if (destroyWhileBusy) {
				Runnable compMethodCallRunnable = new Runnable() {
					public void run() {
						try {
							dummyComponent.callThatTakesSomeTime(remoteCallDurationMin);
						} catch (Exception ex) {
							m_logger.log(Level.SEVERE, "Async client call 'dummyComponent#callThatTakesSomeTime' failed with exception.", ex);
							exceptionInThread = ex;
						}
					}
				};
				m_logger.info("Will release component while active request is still running.");
				(new Thread(compMethodCallRunnable)).start();
				// todo use proper synchronization to wait until dymmyComponent has received the call to 'callThatTakesSomeTime'
				// this could be done using ACS callbacks.
				Thread.sleep(callOverheadMax);
			}
			else {
				dummyComponent.callThatTakesSomeTime(0);
			}
			// we expect the releaseComponent call to take some time, because the container must wait for the 
			// currently active call to finish before the component can be unloaded.
			long timeBeforeRelease = System.currentTimeMillis(); 
			getContainerServices().releaseComponent(compName);
			int timeReleaseCompCall = (int) (System.currentTimeMillis() - timeBeforeRelease); 
			assertTrue(timeReleaseCompCall > remoteCallDurationMin-callOverheadMax);
			
			if (exceptionInThread != null) {
				fail("asynchronous component call number " + i + " (callThatTakesSomeTime) failed: " + exceptionInThread.toString());
			}					
		}
	}
}
