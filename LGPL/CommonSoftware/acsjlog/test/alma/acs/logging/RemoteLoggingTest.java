package alma.acs.logging;

import java.util.logging.Logger;

import junit.framework.TestCase;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.DsLogAdmin.LogOperations;

import si.ijs.maci.Manager;
import si.ijs.maci._ManagerStub;

import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.logging.mocks.LogEmptyImpl;



/**
 * Tests remote logging, using mock objects, and therefore does not need a running ACS
 * which would not even exist in the strict module build order.
 */
public class RemoteLoggingTest extends TestCase
{	
    protected void setUp() throws Exception {
        System.out.println("\n------------ " + getName() + " --------------");
    }
	
	
	/**
	 * Tests the simple case where logging occurs after the call to 
	 * {@link ClientLogManager#initRemoteLogging(ORB, Manager, int, boolean)}. 
	 */
	public void testSimpleRemoteLogging() throws Exception {
		ClientLogManagerStandalone clm = new ClientLogManagerStandalone();
		
		// sync call (waits till remote logging is initialized)
		initRemoteLogging(clm);

		Logger logger = clm.getLoggerForApplication("testNormalRemoteLogging", true);
		logger.info("A healthy info log");
		logger.severe("And a severe log that actually is not severe");
		
		clm.shutdown(true);
	}
	
	
	public void testConcurrentRemoteInitAndStop() throws InterruptedException {
		
		DaemonThreadFactory dtf = new DaemonThreadFactory(getName());

		// we try this out for different delays
		int[] delays = {0, 20, 100, 500};
		for (int networkDelay : delays) {
			for (int shutdownDelay : delays) {
				System.out.println("*** Network delay = " + networkDelay + " and shutdown delay = " + shutdownDelay + " ***");
				
				final ClientLogManagerStandalone clm = new ClientLogManagerStandalone();
				LogConfig logConfig = clm.getLogConfig();
				logConfig.setDefaultMinLogLevel(AcsLogLevelDefinition.TRACE);
				logConfig.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.TRACE);
				Logger logger = clm.getLoggerForApplication(getName(), true);
				clm.setDelayMillis(networkDelay);
				// log something before we init the remote logging:
				logger.info("A healthy info log");
				logger.fine("now that was fine.");
				
				// call initRemoteLogging from a separate thread
				Thread initRemoteLoggingThread = dtf.newThread(new Runnable() {
					public void run() {
						initRemoteLogging(clm);
					}				
				});
				initRemoteLoggingThread.start();
				
				// todo: log some stuff
				
				Thread.sleep(shutdownDelay);
				clm.shutdown(true);
				initRemoteLoggingThread.join();
			}
		}
		
	}
	
	
	
	private boolean initRemoteLogging(ClientLogManagerStandalone clm) {
		ORB orb = ORB.init(); // unconfigured ORB will do, just needed to produce Any objects for sending remote logs.
		Manager managerDummy = new _ManagerStub(); // will only be used for != null check.
		
		return clm.initRemoteLogging(orb, managerDummy, 1, true);
	}	
	
	/**
	 * Modified {@link ClientLogManager} which skips the manager call and uses a mock Log service.
	 * Remote communication delays are simulated, see {@link #setDelayMillis(long)}. 
	 */
	private static class ClientLogManagerStandalone extends ClientLogManager {
		
		private LogOperations logServiceMock = new LogEmptyImpl() {
			public void write_records(Any[] records) {
				for (Any any : records) {
					// just print to stdout, to be verified by TAT
					System.out.println("Remote log: " + any.extract_string());
				}
				delay();
			}
		};
	
		private long delayMillis = 100;
				
		protected LogOperations getLogService(Manager manager, int managerHandle) {
			delay();
			return logServiceMock;
		}
		
		void setDelayMillis(long delayMillis) {
			this.delayMillis = delayMillis;
		}
		
		void delay() {
			try {
				Thread.sleep(delayMillis);
			} catch (InterruptedException ex) {
				ex.printStackTrace(System.out);
			}
		}
	}
		
}
