package alma.acs.daemontest;

import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.omg.CORBA.ORB;
import org.omg.CORBA.TRANSIENT;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;

import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.acs.container.corba.AcsCorba;
import alma.acs.exceptions.AcsJCompletion;
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
	protected Logger logger;
	
	private ServicesDaemon daemon;
	private String host;
	private short instanceNumber = (short) ACSPorts.getBasePort();

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
			logger.log(Level.SEVERE, "failed to initialize the ORB, or connect to the ACS Manager, " + 
					"or to set up the container services.", ex);
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

		host = "localhost"; //or "alma78.hq.eso.org" for Heiko on eclipse
		
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
	private DaemonSequenceCallback createDaemonSequenceCallback() throws AcsJContainerEx, AcsJUnexpectedExceptionEx {
		DaemonSequenceCallbackImpl daemonSequenceCallbackImpl = new DaemonSequenceCallbackImpl(logger);
		DaemonSequenceCallback daemonSequenceCallback = DaemonSequenceCallbackHelper.narrow(acsCorba.activateOffShoot(daemonSequenceCallbackImpl, acsCorba.getRootPOA()));
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
		daemonCallbackImpl.prepareWaitForDone();
		StopWatch sw = new StopWatch(logger);
		daemon.start_naming_service(dcb, instanceNumber);
		assertTrue("Failed to start naming service in 10 s", daemonCallbackImpl.waitForDone(10, TimeUnit.SECONDS));
		sw.logLapTime("call start_naming_service");
		assertFalse(AcsJCompletion.fromCorbaCompletion(daemonCallbackImpl.getLastDoneCompletion()).isError());
		
		assertNamingService(true);
		
		// stop naming service
		daemonCallbackImpl.prepareWaitForDone();
		sw.reset();
		daemon.stop_naming_service(dcb, instanceNumber);
		assertTrue("Failed to stop naming service in 5 s", daemonCallbackImpl.waitForDone(5, TimeUnit.SECONDS));
		sw.logLapTime("call stop_naming_service");
	}


	/**
	 * @TODO: currently fails
	 */
	public void testStartServiceTwice() throws Exception {
		DaemonCallbackImpl daemonCallbackImpl = new DaemonCallbackImpl(logger);
		DaemonCallback dcb = activateDaemonCallback(daemonCallbackImpl);
		
		// start naming service and wait till it's up
		daemonCallbackImpl.prepareWaitForDone();
		daemon.start_naming_service(dcb, instanceNumber);
		assertTrue("Failed to start naming service in 10 s", 
			daemonCallbackImpl.waitForDone(10, TimeUnit.SECONDS));
		assertNamingService(true);
		
		try {
			// now start it again
			daemonCallbackImpl.prepareWaitForDone();
			daemon.start_naming_service(dcb, instanceNumber);
			assertTrue("Failed to get reply about starting of second naming service within 10 s", 
				daemonCallbackImpl.waitForDone(10, TimeUnit.SECONDS));
			// we don't expect a done() callback though
			fail("Expected ServiceAlreadyRunningEx when starting naming service twice.");
		} 
		catch (ServiceAlreadyRunningEx ex) {
			// good
			logger.info("Got expected ServiceAlreadyRunningEx from attempting to start naming service twice.");
			assertEquals("Naming", AcsJServiceAlreadyRunningEx.fromServiceAlreadyRunningEx(ex).getService());
		}
		finally {
			daemonCallbackImpl.prepareWaitForDone();
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
	public void testStartAcsServiceIndividually() throws Exception {
		
		DaemonCallbackImpl daemonCallbackImpl_1 = new DaemonCallbackImpl(logger);
		DaemonCallbackImpl daemonCallbackImpl_2 = new DaemonCallbackImpl(logger);
		DaemonCallback dcb_1 = activateDaemonCallback(daemonCallbackImpl_1);
		DaemonCallback dcb_2 = activateDaemonCallback(daemonCallbackImpl_2);
		
		// start naming service and wait till it's up
		daemonCallbackImpl_1.prepareWaitForDone();
		daemon.start_naming_service(dcb_1, instanceNumber);
		assertTrue("Failed to start naming service in 10 s", 
				daemonCallbackImpl_1.waitForDone(10, TimeUnit.SECONDS));
		logger.info("Got naming service");

		// start interface repository but don't wait for it yet ( start other services in parallel)
		daemonCallbackImpl_2.prepareWaitForDone();
		daemon.start_interface_repository(true, true, dcb_2, instanceNumber);

		// start CDB and wait till it's up
		daemonCallbackImpl_1.prepareWaitForDone();
		daemon.start_xml_cdb(dcb_1, instanceNumber, false, ""); // TODO try explicit path ($ACS_CDB replacement)
		assertTrue("Failed to start CDB in 10 s", 
				daemonCallbackImpl_1.waitForDone(10, TimeUnit.SECONDS));
		assertCDB();
		
		// start manager and wait till it's up
		daemonCallbackImpl_1.prepareWaitForDone();
		daemon.start_manager("", dcb_1, instanceNumber, false);
		assertTrue("Failed to start the ACS manager in 10 s", 
				daemonCallbackImpl_1.waitForDone(10, TimeUnit.SECONDS));
		assertManager();
		
		// now wait for the IR
		assertTrue("Failed to start interface repository 30 s after all other services have started", 
				daemonCallbackImpl_2.waitForDone(30, TimeUnit.SECONDS));
		logger.info("Got the IFR");

		// @TODO stop these services
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
		DaemonSequenceCallback daemonSequenceCallback = createDaemonSequenceCallback();
		// @TODO : start and stop this
//		daemon.start_services(xmlSrvDef, false, daemonSequenceCallback);
	}

}
