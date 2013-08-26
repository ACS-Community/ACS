package alma.acs.logging;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import junit.framework.TestCase;

import org.omg.CORBA.Any;
import org.omg.CORBA.ORB;
import org.omg.DsLogAdmin.LogOperations;

import si.ijs.maci.Manager;
import si.ijs.maci._ManagerStub;

import alma.Logging.AcsLogServiceOperations;
import alma.Logging.XmlLogRecord;
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
		ClientLogManagerStandalone clm = new ClientLogManagerStandalone(null);
		
		// sync call (waits till remote logging is initialized)
		initRemoteLogging(clm);

		Logger logger = clm.getLoggerForApplication("testNormalRemoteLogging", true);
		logger.info("A healthy info log");
		logger.severe("And a severe log that actually is not severe");
		
		clm.shutdown(true);
	}
	
	
	/**
	 * Tests the situation in which the ClientLogManager is shut down before it has finished the 
	 * asynchronous <code>initRemoteLogging</code> call.
	 * This can happen in a <code>ComponentClient</code>-based application that performs a short task.
	 * <p>
	 * It is important to make the stdout printing part of this test,
	 * e.g. by using TAT without output suppression tricks,
	 * because among other things we expect the string
	 * <code>Will abort ClientLogManager#initRemoteLogging because remote logging seems no longer needed.</code>
	 * when initRemoteLogging is interrupted by a shutdown.
	 * <p>
	 * The main test thread that logs some messages and the thread that calls initRemoteLogging 
	 * compete for the internal lock {@link ClientLogManager#logQueueLock}, whose functioning is being tested here.
	 * <p>
	 * <b>Unfortunately this cannot be totally deterministic, so we must accept occasional failures
	 * of the kind that the output contains "Remote log: <Info .../>" instead of "Will abort ..." strings, or vice versa.</b>
	 */
	public void testConcurrentRemoteInitAndStop() throws InterruptedException {
		
		DaemonThreadFactory dtf = new DaemonThreadFactory(getName());

		// we try this out for different simulated network delays and shutdown delays
		int[] networkDelays = {0, 2, 16, 21, 100}; // @TODO chose values that give deterministic results on most test machines.
		int[] shutdownDelays = {1, 20, 106}; // @TODO chose values that give deterministic results on most test machines.

		for (int networkDelay : networkDelays) {
			// shutdown delay is the time in milliseconds between the last log and the call to clientlogmanager#shutdown,
			// with the additional arrangement that initRemoteLogging was running when the last log was produced.
			for (int shutdownDelay : shutdownDelays) {
				System.out.println("\n*** Network delay = " + networkDelay + " and shutdown delay = " + shutdownDelay + " ***");
				
				CountDownLatch syncOnPrepareRemoteLogging = new CountDownLatch(1);
				final ClientLogManagerStandalone clm = new ClientLogManagerStandalone(syncOnPrepareRemoteLogging);
				LogConfig logConfig = clm.getLogConfig();
				logConfig.setDefaultMinLogLevel(AcsLogLevelDefinition.TRACE);
				logConfig.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.TRACE);
				Logger logger = clm.getLoggerForApplication(getName(), true);

				// log something before we init the remote logging:
				logger.info("A healthy info log before initRemoteLogging ("+networkDelay+"/"+shutdownDelay+")");
				Thread.sleep(2); // to keep these two logs in order, which makes manual ref file comparison easier. 
				logger.fine("now that was a fine log before initRemoteLogging ("+networkDelay+"/"+shutdownDelay+")");
				
				clm.setDelayMillis(networkDelay); // simulated network delay for initRemoteLogging-getLogService and write_records
				// call initRemoteLogging from a separate thread
				Thread initRemoteLoggingThread = dtf.newThread(new Runnable() {
					public void run() {
						initRemoteLogging(clm);
					}
				});
				initRemoteLoggingThread.start();
				// wait until this thread is actually running, which we check via notification from the ClientLogManager#prepareRemoteLogging method
				assertTrue("initRemoteLogging should have called prepareRemoteLogging by now...", syncOnPrepareRemoteLogging.await(10, TimeUnit.SECONDS)); // timeout should never apply, just used to stop the test if it gets messed up. 
				
				logger.info("Another info log after initRemoteLogging ("+networkDelay+"/"+shutdownDelay+")");
				
				// depending on the values of networkDelay and shutdownDelay, we may be calling shutdown while 
				// our ClientLogManager is still delivering the log messages.
				Thread.sleep(shutdownDelay);
				clm.shutdown(true);
				
				// wait for the thread that called initRemoteLogging
				initRemoteLoggingThread.join(10000);
				
				// wait a bit more for the mock log dispatcher to print out its xml log record
				Thread.sleep(1000);
			}
		}
	}

	
	
	///////////////////////////////////////////////////////////
	//////////////////    Helper methods    ///////////////////
	///////////////////////////////////////////////////////////
	
	/**
	 * Calls {@linkplain ClientLogManagerStandalone#initRemoteLogging(ORB, Manager, int, boolean)}
	 * with appropriate dummy parameters.
	 * @param clm The instance to call initRemoteLogging on.
	 * @return true if simulated remote logging was initialized successfully
	 */
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

		private final CountDownLatch syncOnPrepareRemoteLogging;
		private volatile long delayMillis = 100;

		/**
		 * Mock impl of the Log service. All methods are total no-ops, 
		 * except for {@link LogOperations#write_records(Any[]) which prints the xml log records 
		 * contained in the given Any objects to stdout, and simulates network delay by sleeping 
		 * via a call to {@link #delay()}.
		 */
		private final AcsLogServiceOperations logServiceMock = new LogEmptyImpl() 
		{
			public void write_records(Any[] records) {
				for (Any any : records) {
					// just print to stdout, to be verified by TAT
					System.out.println("Remote log: " + any.extract_string());
				}
				delay();
			}
			
			@Override
			public void writeRecords(XmlLogRecord[] xmlLogRecords) {
				for (XmlLogRecord record : xmlLogRecords) {
					// just print to stdout, to be verified by TAT
					System.out.println("Remote log: " + record.xml);
				}
				delay();
			}
		};

		/**
		 * Constructor.
		 * @param syncOnPrepareRemoteLogging
		 *          Optional synchronization aid (may be <code>null</code>). 
		 *          Method {@linkplain #prepareRemoteLogging()} will call <code>countDown()</code>
		 *          to allow a test to wait until {@linkplain #initRemoteLogging(ORB, Manager, int, boolean)} is actually running
		 *          in cases where it gets started from a separate thread.
		 *          Note that the parent constructor will not call <code>countDown()</code> even though it calls <code>prepareRemoteLogging</code>,
		 *          because we only set the CountDownLatch after calling the parent ctor. Thus passing a <code>CountDownLatch(1)</code> will work.
		 */
		ClientLogManagerStandalone(CountDownLatch syncOnPrepareRemoteLogging) {
			super();
			this.syncOnPrepareRemoteLogging = syncOnPrepareRemoteLogging;
		}
		
		/** 
		 * This is called by {@linkplain ClientLogManager#initRemoteLogging(ORB, Manager, int, boolean)}
		 * and simulates the access to the Log service by sleeping via {@linkplain #delay()}. 
		 * @see alma.acs.logging.ClientLogManager#getLogService(si.ijs.maci.Manager, int)
		 */
		protected AcsLogServiceOperations getLogService(Manager manager, int managerHandle) {
			delay();
			return logServiceMock;
		}
		
		/**
		 * Sets the delay in milliseconds which subsequent calls to {@link #delay()} will sleep for.
		 * Default is 100 ms if this method does not get called.
		 */
		void setDelayMillis(long delayMillis) {
			this.delayMillis = delayMillis;
		}
		
		/**
		 * Sleeps for the time given in {@linkplain #setDelayMillis(long)}.
		 */
		void delay() {
			try {
				Thread.sleep(delayMillis);
			} catch (InterruptedException ex) {
				ex.printStackTrace(System.out);
			}
		}
		
		/**
		 * Overloaded only to allow clients to sync with execution of {@link #initRemoteLogging(ORB, Manager, int, boolean)}.
		 * @see #ClientLogManagerStandalone(CountDownLatch)
		 */
		protected void prepareRemoteLogging() {
			if (syncOnPrepareRemoteLogging != null) {
				syncOnPrepareRemoteLogging.countDown();
			}
			super.prepareRemoteLogging();
		}
	}
		
}
