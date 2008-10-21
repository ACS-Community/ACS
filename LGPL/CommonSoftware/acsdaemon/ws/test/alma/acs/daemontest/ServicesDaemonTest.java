package alma.acs.daemontest;

import java.util.logging.Level;
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.omg.CORBA.Context;
import org.omg.CORBA.ContextList;
import org.omg.CORBA.DomainManager;
import org.omg.CORBA.ExceptionList;
import org.omg.CORBA.NVList;
import org.omg.CORBA.NamedValue;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.Policy;
import org.omg.CORBA.Request;
import org.omg.CORBA.SetOverrideType;

import sun.security.action.GetLongAction;

import alma.ACSErr.Completion;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.ACSPorts;
import alma.acs.util.AcsLocations;
import alma.acs.util.StopWatch;
import alma.acsdaemon.DaemonCallback;
import alma.acsdaemon.DaemonCallbackHelper;
import alma.acsdaemon.ServiceDefinitionBuilder;
import alma.acsdaemon.ServicesDaemon_ACS80;
import alma.acsdaemon.ServicesDaemon_ACS80Helper;
import alma.acsdaemon.systemNotificationServiceDefault;
import alma.acsdaemon.systemNotificationServiceLogging;


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
	
	private ServicesDaemon_ACS80 daemon;
	private String host;
	
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

	private ServicesDaemon_ACS80 getServicesDaemon(String host) {
		String daemonLoc = AcsLocations.convertToServicesDaemonLocation(host);
		assertNotNull("corbaloc for service daemon must not be null", daemonLoc);
		logger.fine("Using services daemon corbaloc " + daemonLoc);
		
		ORB orb = acsCorba.getORB();
		assertNotNull("ORB provided by inherited acsCorba must not be null", daemonLoc);
		
		org.omg.CORBA.Object obj = orb.string_to_object(daemonLoc);
		ServicesDaemon_ACS80 ret = ServicesDaemon_ACS80Helper.narrow(obj);
		assertNotNull("Corba ref to services daemon must not be null", ret);
		return ret;
	}

	/**
	 * Creates a {@link DaemonCallbackImpl} object and registers it with the ORB.
	 */
	private DaemonCallback createDaemonCallback() throws AcsJContainerEx, AcsJUnexpectedExceptionEx {
		DaemonCallbackImpl daemonCallbackImpl = new DaemonCallbackImpl(logger);
		DaemonCallback daemonCallback = DaemonCallbackHelper.narrow(acsCorba.activateOffShoot(daemonCallbackImpl, acsCorba.getRootPOA()));
		return daemonCallback;
	}


	/////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////// Test methods //////////////////////////////////////
	/////////////////////////////////////////////////////////////////////////////////////////

	public void testServicesBuilder() throws Exception {
		short instanceNumber = (short) ACSPorts.getBasePort();
		ServiceDefinitionBuilder sdb = daemon.create_service_definition_builder(instanceNumber);
		assertNotNull(sdb);
		assertEquals("ACS instance number must match the one provided to the factory", instanceNumber, sdb.acs_instance_number());

		// now we add services in an order that is not a legal startup order, to check re-ordering
		sdb.add_notification_service(systemNotificationServiceDefault.value, host);
		sdb.add_manager(host, "", true);
		sdb.add_notification_service(systemNotificationServiceLogging.value, host);
		sdb.add_logging_service(host, ""); // "" means that default name "Log" should be used
		sdb.add_naming_service(host);
		sdb.add_xml_cdb(host, true, System.getProperty("user.dir"));
		sdb.add_interface_repository(host, true, false);
		
		String xmlSrvDef = sdb.get_services_definition();
		assertNotNull("XML services description must not be null");
		System.out.println(xmlSrvDef);
		
		sdb.close();
	}
	
	public void testStartAcsServiceIndividually() throws Exception {
		short instanceNumber = (short) ACSPorts.getBasePort();
		DaemonCallback daemonCallback = createDaemonCallback();
		
		StopWatch sw = new StopWatch(logger);
		daemon.start_naming_service(daemonCallback, instanceNumber);
		sw.logLapTime("call start_naming_service");
	}


}
