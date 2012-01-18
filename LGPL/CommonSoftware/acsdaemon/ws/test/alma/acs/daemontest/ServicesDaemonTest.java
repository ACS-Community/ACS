package alma.acs.daemontest;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;

import junit.framework.TestCase;

import org.omg.CORBA.ORB;
import org.omg.CORBA.TRANSIENT;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;

import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.container.corba.AcsCorba;
import alma.acs.exceptions.AcsJCompletion;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.ACSPorts;
import alma.acs.util.AcsLocations;
import alma.acs.util.StopWatch;
import alma.acsdaemon.DaemonCallback;
import alma.acsdaemon.DaemonCallbackHelper;
import alma.acsdaemon.DaemonSequenceCallback;
import alma.acsdaemon.DaemonSequenceCallbackHelper;
import alma.acsdaemon.ServiceDefinitionBuilder;
import alma.acsdaemon.ServicesDaemon;
import alma.acsdaemon.ServicesDaemonHelper;
import alma.acsdaemon.systemNotificationServiceAlarms;
import alma.acsdaemon.systemNotificationServiceDefault;
import alma.acsdaemon.systemNotificationServiceLogging;
import alma.acsdaemonErrType.ServiceAlreadyRunningEx;
import alma.acsdaemonErrType.wrappers.AcsJServiceAlreadyRunningEx;


/**
 * **** Work in progress, not in TAT yet! ****
 * <p>
 * Corba-related code copied from ComponentClientTestCase,
 * which we don't use here because it requires a running ACS with manager
 * which we don't need for the daemon test.
 * 
 * @author hsommer
 */
public class ServicesDaemonTest extends TestCase 
{
	private static final String namePrefix = "ServicesDaemonTest";
	private AcsCorba acsCorba;
	protected AcsLogger logger;
	
	private ServicesDaemon daemon;
	private String host;
	private short instanceNumber = (short) 4; //ACSPorts.getBasePort();

	public ServicesDaemonTest() throws Exception {
		super(namePrefix);
	}

	/////////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////// Infrastructural methods //////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////

	protected void runTest() throws Throwable {
		try {
			super.runTest();
		}
		catch (Throwable thr) {
			if (logger != null) {
				logger.log(Level.WARNING, "JUnit test error in " + getFullName(), thr);
			}
			throw thr;
		}
	}
	
	protected String getFullName() {
		String fullName = namePrefix + "#" + getName();
		return fullName;
	}

	
	protected void setUp() throws Exception {
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getFullName(), false);
		ClientLogManager.getAcsLogManager().suppressRemoteLogging();
		logger.info("\n------------ " + getName() + " --------------");

		try {
			acsCorba = new AcsCorba(logger);
			acsCorba.initCorbaForClient(false);
		} catch (Exception ex) {
			logger.log(Level.SEVERE, "failed to initialize the ORB", ex);
			if (acsCorba != null) {
				try {
					acsCorba.shutdownORB(true, false);
					acsCorba.doneCorba();
				} catch (Exception ex2) {
					// to JUnit we want to forward the original exception, 
					// not any other exception from cleaning up the orb, 
					// which we would not have done without the first exception.
					ex2.printStackTrace();
				}
			}
			throw ex;
		}

		host = ACSPorts.getIP(); //or "alma78.hq.eso.org" for Heiko on eclipse
		
		daemon = getServicesDaemon(host);
	}

	protected void tearDown() throws Exception {
		if (acsCorba != null) {
			acsCorba.shutdownORB(true, false);
			acsCorba.doneCorba();
		}
		// just in case... should give the OS time to reclaim ORB ports and so on
		Thread.sleep(100);
	}

	private ServicesDaemon getServicesDaemon(String host) {
		String daemonLoc = AcsLocations.convertToServicesDaemonLocation(host);
		assertNotNull("corbaloc for service daemon must not be null", daemonLoc);
		logger.fine("Using services daemon corbaloc " + daemonLoc);
		
		ORB orb = acsCorba.getORB();
		assertNotNull("ORB provided by inherited acsCorba must not be null", daemonLoc);
		
		org.omg.CORBA.Object obj = orb.string_to_object(daemonLoc);
		ServicesDaemon ret = null; 
		try {
			ret = ServicesDaemonHelper.narrow(obj);
		} catch (TRANSIENT ex) {
			fail("Failed to get reference to services daemon '" + daemonLoc + "' because of TRANSIENT ex");
		}
		assertNotNull("Corba ref to services daemon must not be null", ret);
		return ret;
	}

	private NamingContext getNamingService() {
		String cloc = "corbaloc::" + host + ":" + ACSPorts.globalInstance(instanceNumber).giveNamingServicePort() + "/NameService";
		logger.info("Attempting to get naming service with " + cloc);
		org.omg.CORBA.Object objRef = acsCorba.getORB().string_to_object(cloc);
		return NamingContextHelper.narrow(objRef);
	}

	private void assertNamingService(boolean running) {
		try {
			NamingContext naming = getNamingService();
			if (running) {
				logger.info("Got naming service as expected.");
			}
			else {
				fail("Expected TRANSIENT exception because naming service should not be running.");
			}
		} catch (Exception ex) {
			if (!running && ex instanceof TRANSIENT) {
				logger.info("Got expected org.omg.CORBA.TRANSIENT because naming service is not yet running.");
			}
			else {
				fail("Unexpected exception" + ex.toString());
			}
		}
	}
	
	private void assertCDB() {
		String cloc = "corbaloc::" + host + ":" + ACSPorts.globalInstance(instanceNumber).giveCDBPort() + "/CDB";
		logger.info("Attempting to get CDB with " + cloc);
		org.omg.CORBA.Object objRef = acsCorba.getORB().string_to_object(cloc);
		assertTrue(objRef._is_a("IDL:cosylab.com/CDB/DAL:1.0"));
		logger.info("Resolved CDB");
	}
	
	private void assertManager() {
		String cloc = "corbaloc::" + host + ":" + ACSPorts.globalInstance(instanceNumber).giveManagerPort() + "/Manager";
		logger.info("Attempting to get the manager with " + cloc);
		org.omg.CORBA.Object objRef = acsCorba.getORB().string_to_object(cloc);
		assertTrue(objRef._is_a("IDL:ijs.si/maci/Manager:1.0"));
		logger.info("Resolved the manager");
	}
	
	/**
	 * Creates a {@link DaemonCallbackImpl} object and registers it with the ORB.
	 */
	private DaemonCallback activateDaemonCallback(DaemonCallbackImpl daemonCallbackImpl) throws AcsJContainerEx, AcsJUnexpectedExceptionEx {
		DaemonCallback daemonCallback = DaemonCallbackHelper.narrow(acsCorba.activateOffShoot(daemonCallbackImpl, acsCorba.getRootPOA()));
		return daemonCallback;
	}

	/**
	 * Creates a {@link DaemonCallbackImpl} object and registers it with the ORB.
	 */
	private DaemonSequenceCallback activateDaemonSequenceCallback(DaemonSequenceCallbackImpl callbackImpl) throws AcsJContainerEx, AcsJUnexpectedExceptionEx {
		DaemonSequenceCallback daemonSequenceCallback = DaemonSequenceCallbackHelper.narrow(acsCorba.activateOffShoot(callbackImpl, acsCorba.getRootPOA()));
		return daemonSequenceCallback;
	}

	/////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////// Test methods //////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Starts and stops the naming service via the daemon,
	 * checking the callbacks.
	 */
	public void testNamingSrvStartStopCheckCallback() throws Exception {
		assertNamingService(false);

		DaemonCallbackImpl daemonCallbackImpl = new DaemonCallbackImpl(logger);
		DaemonCallback dcb = activateDaemonCallback(daemonCallbackImpl);
		
		// start naming service
		daemonCallbackImpl.prepareWaitForDone("naming");
		StopWatch sw = new StopWatch(logger);
		daemon.start_naming_service(dcb, instanceNumber);
		assertTrue("Failed to start naming service in 10 s", daemonCallbackImpl.waitForDone(10, TimeUnit.SECONDS));
		sw.logLapTime("call start_naming_service");
		assertFalse(AcsJCompletion.fromCorbaCompletion(daemonCallbackImpl.getLastDoneCompletion()).isError());
		
		assertNamingService(true);
		
		// stop naming service
		daemonCallbackImpl.prepareWaitForDone("naming");
		sw.reset();
		daemon.stop_naming_service(dcb, instanceNumber);
		assertTrue("Failed to stop naming service in 5 s", daemonCallbackImpl.waitForDone(5, TimeUnit.SECONDS));
		sw.logLapTime("call stop_naming_service");
	}


	/**
	 * Test starting service twice. It must return ServiceAlreadyRunning exception (immediately or via callback). 
	 */
	public void testStartServiceTwice() throws Exception {
		DaemonCallbackImpl daemonCallbackImpl = new DaemonCallbackImpl(logger);
		DaemonCallback dcb = activateDaemonCallback(daemonCallbackImpl);
		
		// start naming service and wait till it's up
		daemonCallbackImpl.prepareWaitForDone("naming");
		daemon.start_naming_service(dcb, instanceNumber);
		assertTrue("Failed to start naming service in 10 s", 
			daemonCallbackImpl.waitForDone(10, TimeUnit.SECONDS));
		assertNamingService(true);
		
		try {
			// now start it again
			daemonCallbackImpl.prepareWaitForDone("naming");
			daemon.start_naming_service(dcb, instanceNumber);
			assertTrue("Failed to get reply about starting of second naming service within 10 s", 
				daemonCallbackImpl.waitForDone(10, TimeUnit.SECONDS));
			// error via callback?
			if (daemonCallbackImpl.getLastDoneCompletion().type != alma.ACSErr.acsdaemonErrType.value || daemonCallbackImpl.getLastDoneCompletion().code != alma.acsdaemonErrType.ServiceAlreadyRunning.value)
				fail("Expected ServiceAlreadyRunningEx when starting naming service twice.");
		} 
		catch (ServiceAlreadyRunningEx ex) {
			// good
			logger.info("Got expected ServiceAlreadyRunningEx from attempting to start naming service twice.");
			assertEquals("Naming", AcsJServiceAlreadyRunningEx.fromServiceAlreadyRunningEx(ex).getService());
		}
		finally {
			daemonCallbackImpl.prepareWaitForDone("naming");
			daemon.stop_naming_service(dcb, instanceNumber);
			assertTrue("Failed to stop naming service in 5 s", 
				daemonCallbackImpl.waitForDone(5, TimeUnit.SECONDS));
		}
	}

	public void testStopServiceBeforeStarted() throws Exception {
		// TODO
	}
	
	
	/**
	 * Simple test that only uses the start_xxx and Stop_xxx methods of the daemon,
	 * which is one step up from the old acsStart method, but does not use the convenience
	 * of the service description. All services are started on the same host. Later they are stopped.
	 */
	public void testStartAcsServiceIndividually() throws Throwable {
		
		DaemonCallbackImpl daemonCallbackImpl_1 = new DaemonCallbackImpl(logger);
		DaemonCallbackImpl daemonCallbackImpl_2 = new DaemonCallbackImpl(logger);
		DaemonCallbackImpl daemonCallbackImpl_3 = new DaemonCallbackImpl(logger);
		DaemonCallback dcb_1 = activateDaemonCallback(daemonCallbackImpl_1);
		DaemonCallback dcb_2 = activateDaemonCallback(daemonCallbackImpl_2);
		DaemonCallback dcb_3 = activateDaemonCallback(daemonCallbackImpl_3);
		
		List<Throwable> thrs = new ArrayList<Throwable>();
		try {
			// start naming service and wait till it's up
			daemonCallbackImpl_1.prepareWaitForDone("naming");
			daemon.start_naming_service(dcb_1, instanceNumber);
			assertTrue("Failed to start naming service in 10 s", 
					daemonCallbackImpl_1.waitForDone(10, TimeUnit.SECONDS));
			logger.info("Got naming service");
	
			// start interface repository but don't wait for it yet ( start other services in parallel)
			daemonCallbackImpl_2.prepareWaitForDone("IFR");
			daemon.start_interface_repository(true, true, dcb_2, instanceNumber);
	
			// start CDB and wait till it's up
			daemonCallbackImpl_1.prepareWaitForDone("CDB");
			daemon.start_xml_cdb(dcb_1, instanceNumber, false, ""); // TODO try explicit path ($ACS_CDB replacement)
			assertTrue("Failed to start CDB in 15 s", 
					daemonCallbackImpl_1.waitForDone(15, TimeUnit.SECONDS));
			assertCDB();
			
			// start manager and wait till it's up
			daemonCallbackImpl_1.prepareWaitForDone("manager");
			daemon.start_manager("", dcb_1, instanceNumber, false);
			assertTrue("Failed to start the ACS manager in 10 s", 
					daemonCallbackImpl_1.waitForDone(10, TimeUnit.SECONDS));
			assertManager();
			
			// now wait for the IR if necessary
			assertTrue("Failed to start interface repository 30 s after all other services have started", 
					daemonCallbackImpl_2.waitForDone(30, TimeUnit.SECONDS));
			logger.info("Got the IFR");
	
			// start 3 of the 4 known notify services in parallel.
			// We want to call the start_notification_service method in parallel, which yields an even more parallel service start
			// than with calling this asynchronous method 3 times in a sequence.
			// @TODO Currently this test fails due to what seems a deadlock in the daemon. With just one NC factory it works, but with 3 we get a timeout.
			daemonCallbackImpl_1.prepareWaitForDone("NC factory default");
			daemonCallbackImpl_2.prepareWaitForDone("NC factory logging");
			daemonCallbackImpl_3.prepareWaitForDone("NC factory alarms");
			class NotifySrvStarter implements Callable<Void> {
				private final String srvName;
				private final DaemonCallback cb;
				NotifySrvStarter(String srvName, DaemonCallback cb) {
					this.srvName = srvName;
					this.cb = cb;
				}
				public Void call() throws Exception {
					daemon.start_notification_service(srvName, cb, instanceNumber);
					return null;
				}
			}
			ExecutorService pool = Executors.newFixedThreadPool(3, new DaemonThreadFactory());
			Future<Void> defaultNotifSrvFuture = pool.submit(new NotifySrvStarter(systemNotificationServiceDefault.value, dcb_1)); 

			Future<Void> loggingNotifSrvFuture = pool.submit(new NotifySrvStarter(systemNotificationServiceLogging.value, dcb_2));

			Future<Void> alarmNotifSrvFuture = pool.submit(new NotifySrvStarter(systemNotificationServiceAlarms.value, dcb_3));
			try {
				defaultNotifSrvFuture.get(20, TimeUnit.SECONDS);
				loggingNotifSrvFuture.get(20, TimeUnit.SECONDS);
				alarmNotifSrvFuture.get(20, TimeUnit.SECONDS);
			} 
			catch (ExecutionException ex) {
				// throw the ex that came from the call() method
				throw ex.getCause();
			}
			catch (TimeoutException ex2) {
				fail("Failed to return from 'start_notification_service' within 20 seconds. ");
			}
			// now wait for the notification services to be actually started
			daemonCallbackImpl_1.waitForDone(10, TimeUnit.SECONDS);
			daemonCallbackImpl_2.waitForDone(10, TimeUnit.SECONDS);
			daemonCallbackImpl_3.waitForDone(10, TimeUnit.SECONDS);
		}
		catch (Throwable thr) {
			thrs.add(thr);
		}
		finally {
			// stop all services. Continue after errors to make sure the services are really stopped.
			
			// Stop the NC factories, calling the stop_notification_service methods one after the other
			// and then waiting for the asynch calls to finish.
			try {
				daemonCallbackImpl_1.prepareWaitForDone("stop NC factory default");
				daemon.stop_notification_service(systemNotificationServiceDefault.value, dcb_1, instanceNumber);
			}
			catch (Throwable thr) {
				thrs.add(thr);
			}
			try {
				daemonCallbackImpl_2.prepareWaitForDone("stop NC factory logging");
				daemon.stop_notification_service(systemNotificationServiceLogging.value, dcb_2, instanceNumber);
			}
			catch (Throwable thr) {
				thrs.add(thr);
			}
			try {
				daemonCallbackImpl_3.prepareWaitForDone("stop NC factory alarms");
				daemon.stop_notification_service(systemNotificationServiceAlarms.value, dcb_3, instanceNumber);
			}
			catch (Throwable thr) {
				thrs.add(thr);
			}
			try {
				assertTrue("Failed to stop the default NC factory in 10 s", 
						daemonCallbackImpl_1.waitForDone(10, TimeUnit.SECONDS));
				assertTrue("Failed to stop the logging NC factory in 10 s", 
						daemonCallbackImpl_2.waitForDone(10, TimeUnit.SECONDS));
				assertTrue("Failed to stop the logging NC factory in 10 s", 
						daemonCallbackImpl_3.waitForDone(10, TimeUnit.SECONDS));
			}
			catch (Throwable thr) {
				thrs.add(thr);
			}

			// stop the IFR
			try {
				daemonCallbackImpl_1.prepareWaitForDone("stop IFR");
				daemon.stop_interface_repository(dcb_1, instanceNumber);
				assertTrue("Failed to stop the interface repository in 10 s", 
						daemonCallbackImpl_1.waitForDone(10, TimeUnit.SECONDS));
			}
			catch (Throwable thr) {
				thrs.add(thr);
			}

			// stop the manager
			try {
				daemonCallbackImpl_1.prepareWaitForDone("stop manager");
				daemon.stop_manager("", dcb_1, instanceNumber);
				assertTrue("Failed to stop the manager in 10 s", 
						daemonCallbackImpl_1.waitForDone(10, TimeUnit.SECONDS));
			}
			catch (Throwable thr) {
				thrs.add(thr);
			}
		}
		if (thrs.size() > 0) {
			logger.info("Failure: there were " + thrs.size() + " errors.");
			throw thrs.get(0);
		}
	}


	public void testServicesBuilder() throws Exception {
		ServiceDefinitionBuilder sdb = daemon.create_service_definition_builder(instanceNumber);
		assertNotNull(sdb);
		assertEquals("ACS instance number must match the one provided to the factory", instanceNumber, sdb.acs_instance_number());

		// now we add services in an order that is not a legal startup order, to check re-ordering
		final String cdbPath = System.getProperty("user.dir");
		sdb.add_notification_service(systemNotificationServiceDefault.value, host);
		sdb.add_manager(host, "", true);
		sdb.add_notification_service(systemNotificationServiceLogging.value, host);
		sdb.add_logging_service(host, ""); // "" means that default name "Log" should be used
		sdb.add_naming_service(host);
		sdb.add_xml_cdb(host, true, cdbPath);
		sdb.add_interface_repository(host, true, false);
		
		String xmlSrvDef = sdb.get_services_definition();
		sdb.close();
		System.out.println(xmlSrvDef);

		String ls = "\n"; //System.getProperty("line.separator");
		String xmlExpected = 
			"<acs_services_definition instance=\"" + instanceNumber + "\">" + ls +
				"<notification_service name=\"" + systemNotificationServiceDefault.value + "\" host=\"" + host + "\" />" + ls +
				"<manager host=\"" + host + "\" recovery=\"true\" />" + ls +
				"<notification_service name=\"" + systemNotificationServiceLogging.value + "\" host=\"" + host + "\" />" + ls +
				"<logging_service host=\"" + host + "\" />" + ls + 
				"<naming_service host=\"" + host + "\" />" + ls + 
				"<cdb host=\"" + host + "\" recovery=\"true\" cdb_xml_dir=\"" + cdbPath + "\" />" + ls +
				"<interface_repository host=\"" + host + "\" load=\"true\" wait_load=\"false\" />" + ls + 
			"</acs_services_definition>" +ls;
		assertEquals(xmlExpected, xmlSrvDef);
		
		// Run these services
		DaemonSequenceCallbackImpl daemonSequenceCallbackImpl = new DaemonSequenceCallbackImpl(logger);
		DaemonSequenceCallback daemonSequenceCallback = activateDaemonSequenceCallback(daemonSequenceCallbackImpl);
		daemonSequenceCallbackImpl.prepareWaitForDone();
		
		StopWatch sw = new StopWatch(logger);
		daemon.start_services(xmlSrvDef, false, daemonSequenceCallback);
		assertTrue("The services did not start in 2 minutes",daemonSequenceCallbackImpl.waitForDone(2, TimeUnit.MINUTES));
		sw.logLapTime("call start_services");
		
		// Stop the services
		daemonSequenceCallbackImpl.prepareWaitForDone();
		sw = new StopWatch(logger);
		daemon.stop_services(xmlSrvDef,daemonSequenceCallback);
		assertTrue("The services did not stop in 2 minutes",daemonSequenceCallbackImpl.waitForDone(2, TimeUnit.MINUTES));
		sw.logLapTime("call stop_services");
	}

}
