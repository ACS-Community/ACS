package alma.acs.cdb.tests;

import java.util.concurrent.Callable;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import org.omg.CORBA.ORB;

import alma.acs.concurrent.ThreadBurstExecutorService;
import alma.acs.util.ACSPorts;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.JDALHelper;

import junit.framework.TestCase;

/**
 * This class stresses the DAL, by sending it different calls combinations, sequentially or concurrently.
 * It prints some nice numbers on the screen, but it could also give some simple asserts as it is a unit test
 * @author rtobar
 */
public class PerformanceTest extends TestCase {

	private String strIOR = "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getCDBPort() + "/CDB";
	private ORB orb;
	private DAL dal;

	private static final int CALLS_PER_SECOND_TRIES = 500;

	private static final int SEQUENTIAL_CALLS_ITERATIONS = 50;
	private static final int CONCURRENT_CALLS_ITERATIONS = 50;
	private static final int NUMBER_OF_SEQUENTIAL_CALLS = 100;
	private static final int NUMBER_OF_CONCURRENT_CALLS = 100;

	private static final int [] concurrentCallsIterations = {1, 2, 4, 8, 16};

	// Existing nodes in the CDB used for querying
	private static final String MANAGER_DAO     = "MACI/Managers/Manager";
	private static final String CONTAINER_DAO   = "MACI/Containers/Container";
	private static final String TESTPS_DAO      = "alma/TEST_PS_1";
	private static final String FILTERWHEEL_DAO = "alma/FILTERWHEEL1";
	private static final String COMPONENTS_DAO  = "MACI/Components";
	private static final String ABM001_DAO      = "MACI/Components/CONTROL/AmbManager/ABM001";
	private static final String ABM002_DAO      = "MACI/Components/CONTROL/AmbManager/ABM002";
	private static final String ARTM_DAO        = "MACI/Components/CONTROL/AmbManager/ARTM";
	private static final String DMC_DAO         = "MACI/Components/CONTROL/AmbManager/DMC";
	private static final String ANTENNA_DAO     = "MACI/Components/CONTROL/Antenna";
	private static final String GPS_DAO         = "MACI/Components/CONTROL/GPS";
	private static final String OPERATOR_DAO    = "MACI/Components/CONTROL/Operator";
	private static final String SNDLONODE40_DAO = "MACI/Components/CONTROL/SecondLO/Node0x40/Generic";
	private static final String SNDLONODE41_DAO = "MACI/Components/CONTROL/SecondLO/Node0x41/Generic";
	private static final String SNDLONODE42_DAO = "MACI/Components/CONTROL/SecondLO/Node0x42/Generic";
	private static final String SNDLONODE43_DAO = "MACI/Components/CONTROL/SecondLO/Node0x43/Generic";

	// Non-existing nodes in the CDB used for querying
	private static final String NONEXISTING01 = "MACI/Components/CONTROL/MyOwnAntenna01";
	private static final String NONEXISTING02 = "MACI/Components/CONTROL/MyOwnAntenna02";
	private static final String NONEXISTING03 = "MACI/Components/CONTROL/MyOwnAntenna03";
	private static final String NONEXISTING04 = "MACI/Components/CONTROL/MyOwnAntenna04";
	private static final String NONEXISTING05 = "MACI/Components/CONTROL/MyOwnAntenna05";
	private static final String NONEXISTING06 = "MACI/Components/CONTROL/MyOwnAntenna06";
	private static final String NONEXISTING07 = "MACI/Components/CONTROL/MyOwnAntenna07";
	private static final String NONEXISTING08 = "MACI/Components/CONTROL/MyOwnAntenna08";
	private static final String NONEXISTING09 = "Lala/Lalo09";
	private static final String NONEXISTING10 = "Lala/Lalo10";
	private static final String NONEXISTING11 = "Lala/Lalo11";
	private static final String NONEXISTING12 = "Lala/Lalo12";
	private static final String NONEXISTING13 = "Lala/Lalo13";
	private static final String NONEXISTING14 = "Lala/Lalo14";
	private static final String NONEXISTING15 = "Lala/Lalo15";
	private static final String NONEXISTING16 = "Lala/Lalo16";

	// Semi-existing nodes in the CDB used for querying
	private static final String SEMIEXISTING01 = "MACI/Containers/EmptyContainerDefinition01";
	private static final String SEMIEXISTING02 = "MACI/Containers/EmptyContainerDefinition02";
	private static final String SEMIEXISTING03 = "MACI/Containers/EmptyContainerDefinition03";
	private static final String SEMIEXISTING04 = "MACI/Containers/EmptyContainerDefinition04";
	private static final String SEMIEXISTING05 = "MACI/Containers/EmptyContainerDefinition05";
	private static final String SEMIEXISTING06 = "MACI/Containers/EmptyContainerDefinition06";
	private static final String SEMIEXISTING07 = "MACI/Containers/EmptyContainerDefinition07";
	private static final String SEMIEXISTING08 = "MACI/Containers/EmptyContainerDefinition08";
	private static final String SEMIEXISTING09 = "MACI/Containers/EmptyContainerDefinition09";
	private static final String SEMIEXISTING10 = "MACI/Containers/EmptyContainerDefinition10";
	private static final String SEMIEXISTING11 = "MACI/Containers/EmptyContainerDefinition11";
	private static final String SEMIEXISTING12 = "MACI/Containers/EmptyContainerDefinition12";
	private static final String SEMIEXISTING13 = "MACI/Containers/EmptyContainerDefinition13";
	private static final String SEMIEXISTING14 = "MACI/Containers/EmptyContainerDefinition14";
	private static final String SEMIEXISTING15 = "MACI/Containers/EmptyContainerDefinition15";
	private static final String SEMIEXISTING16 = "MACI/Containers/EmptyContainerDefinition16";

	protected void setUp() throws Exception {
		super.setUp();

		String args[] = {};
		orb = ORB.init(args, null);
		dal = JDALHelper.narrow(orb.string_to_object(strIOR));
	}

	private void runConcurrentCalls(String []daos) throws Exception {
		long start;
		long end;
		double average;
		ThreadBurstExecutorService service;
		ThreadFactory threadFactory = new ThreadFactory(){
			public Thread newThread(Runnable r) {
				return new Thread(r);
			}
		};

		for (int j = 0; j != concurrentCallsIterations.length; j++) {

			System.out.println(" Asking for " + concurrentCallsIterations[j] + " nodes on each call");
			average = 0;
			for (int iteration = 0; iteration != CONCURRENT_CALLS_ITERATIONS; iteration++) {

				service = new ThreadBurstExecutorService(threadFactory);

				for (int i = 0; i != NUMBER_OF_CONCURRENT_CALLS; i++)
					service
							.submit(new DALClientCallable(daos,
									concurrentCallsIterations[j]), 30,
									TimeUnit.SECONDS);

				start = System.currentTimeMillis();
				service.executeAllAndWait(30, TimeUnit.SECONDS);
				end = System.currentTimeMillis();
				average += (end - start);
			}

			System.out.println("  Concurrent calls: " + NUMBER_OF_CONCURRENT_CALLS
					+ ". Different nodes: " + daos.length
					+ ". Tries: " + CONCURRENT_CALLS_ITERATIONS
					+ ". Average time: " + average
					/ CONCURRENT_CALLS_ITERATIONS + " [ms]");
		}
		System.out.println("");
	}

	private void runSequentialCalls(String []daos) {

		long start;
		long end;
		double average = 0;

		for (int iterations = 0; iterations != SEQUENTIAL_CALLS_ITERATIONS; iterations++) {
			start = System.currentTimeMillis();
			for (int i = 0; i != NUMBER_OF_SEQUENTIAL_CALLS; i++) {
				int index = (int)(Math.random()*(daos.length-1));
				try {
					dal.get_DAO(daos[index]);
				} catch (Exception e) {
					// Do nothing
				}
			}
			end = System.currentTimeMillis();
			average += (end-start);
		}

		System.out.println("Sequential calls: " + NUMBER_OF_SEQUENTIAL_CALLS + ". Different nodes: " + daos.length + ". Tries: " + SEQUENTIAL_CALLS_ITERATIONS + ". Average time: " + average/SEQUENTIAL_CALLS_ITERATIONS + " [ms]");		
	}

	public void testConcurrentCalls() throws Exception {

		System.out.println("Stressing the get_DAO() method with concurrent threads");
		System.out.println("======================================================");
		runConcurrentCalls( new String[] {MANAGER_DAO} );
		runConcurrentCalls( new String[] {MANAGER_DAO, CONTAINER_DAO, TESTPS_DAO, FILTERWHEEL_DAO} );
		runConcurrentCalls(
				new String[] {MANAGER_DAO, CONTAINER_DAO, TESTPS_DAO, FILTERWHEEL_DAO,
				              COMPONENTS_DAO, ABM001_DAO, ABM002_DAO, ARTM_DAO,
				              DMC_DAO, ANTENNA_DAO, GPS_DAO, OPERATOR_DAO,
				              SNDLONODE40_DAO, SNDLONODE41_DAO, SNDLONODE42_DAO, SNDLONODE43_DAO} );
	}

	public void testSequentialCalls() throws Exception {

		System.out.println("Stressing the get_DAO() method with sequential threads");
		System.out.println("======================================================");
		runSequentialCalls( new String[] {MANAGER_DAO} );
		runSequentialCalls( new String[] {MANAGER_DAO, CONTAINER_DAO, TESTPS_DAO, FILTERWHEEL_DAO} );
		runSequentialCalls(
				new String[] {MANAGER_DAO, CONTAINER_DAO, TESTPS_DAO, FILTERWHEEL_DAO,
				              COMPONENTS_DAO, ABM001_DAO, ABM002_DAO, ARTM_DAO,
				              DMC_DAO, ANTENNA_DAO, GPS_DAO, OPERATOR_DAO,
				              SNDLONODE40_DAO, SNDLONODE41_DAO, SNDLONODE42_DAO, SNDLONODE43_DAO} );
	}

	public void testCallsPerSecond() throws Exception {

		System.out.println("Measuring average call/s");
		System.out.println("========================");
		double average = 0;
		for(int i=0; i!= CALLS_PER_SECOND_TRIES; i++) {
			CallerTask ct = new CallerTask(MANAGER_DAO);
			Thread t = new Thread(ct);
			t.start();
			Thread.sleep(1000);
			ct.end();
			average += ct.numberOfCalls();
		}
		System.out.println("Average sequential calls/s: " + (average/CALLS_PER_SECOND_TRIES));
	}

	public void testNonExistingConcurrentCalls() throws Exception {

		System.out.println("Stressing the get_DAO() MISSING NODES (concurrent threads)");
		System.out.println("==========================================================");
		runConcurrentCalls( new String[] {NONEXISTING01} );
		runConcurrentCalls( new String[] {NONEXISTING01, NONEXISTING02, NONEXISTING03, NONEXISTING04} );
		runConcurrentCalls(
				new String[] {NONEXISTING01, NONEXISTING02, NONEXISTING03, NONEXISTING04,
				              NONEXISTING05, NONEXISTING06, NONEXISTING07, NONEXISTING08,
				              NONEXISTING09, NONEXISTING10, NONEXISTING11, NONEXISTING12,
				              NONEXISTING13, NONEXISTING14, NONEXISTING15, NONEXISTING16} );
	}

	public void testNonExistingSequentialCalls() throws Exception {

		System.out.println("Stressing the get_DAO() MISSING NODES (sequential threads)");
		System.out.println("==========================================================");
		runSequentialCalls( new String[] {NONEXISTING01} );
		runSequentialCalls( new String[] {NONEXISTING01, NONEXISTING02, NONEXISTING03, NONEXISTING04} );
		runSequentialCalls(
				new String[] {NONEXISTING01, NONEXISTING02, NONEXISTING03, NONEXISTING04,
			                  NONEXISTING05, NONEXISTING06, NONEXISTING07, NONEXISTING08,
			                  NONEXISTING09, NONEXISTING10, NONEXISTING11, NONEXISTING12,
			                  NONEXISTING13, NONEXISTING14, NONEXISTING15, NONEXISTING16} );
	}

	public void testNonExistingCallsPerSecond() throws Exception {

		System.out.println("Measuring average call/s MISSING NODES");
		System.out.println("======================================");
		double average = 0;
		for(int i=0; i!= CALLS_PER_SECOND_TRIES; i++) {
			CallerTask ct = new CallerTask(NONEXISTING01);
			Thread t = new Thread(ct);
			t.start();
			Thread.sleep(1000);
			ct.end();
			average += ct.numberOfCalls();
		}
		System.out.println("Average sequential calls/s: " + (average/CALLS_PER_SECOND_TRIES));
	}

	public void testSemiExistingConcurrentCalls() throws Exception {

		System.out.println("Stressing the get_DAO() MISSING NODES (concurrent threads)");
		System.out.println("==========================================================");
		runConcurrentCalls( new String[] {SEMIEXISTING01} );
		runConcurrentCalls( new String[] {SEMIEXISTING01, SEMIEXISTING02, SEMIEXISTING03, SEMIEXISTING04} );
		runConcurrentCalls(
				new String[] {SEMIEXISTING01, SEMIEXISTING02, SEMIEXISTING03, SEMIEXISTING04,
				              SEMIEXISTING05, SEMIEXISTING06, SEMIEXISTING07, SEMIEXISTING08,
				              SEMIEXISTING09, SEMIEXISTING10, SEMIEXISTING11, SEMIEXISTING12,
				              SEMIEXISTING13, SEMIEXISTING14, SEMIEXISTING15, SEMIEXISTING16} );
	}

	public void testSemiExistingSequentialCalls() throws Exception {

		System.out.println("Stressing the get_DAO() MISSING NODES (sequential threads)");
		System.out.println("==========================================================");
		runSequentialCalls( new String[] {SEMIEXISTING01} );
		runSequentialCalls( new String[] {SEMIEXISTING01, SEMIEXISTING02, SEMIEXISTING03, SEMIEXISTING04} );
		runSequentialCalls(
				new String[] {SEMIEXISTING01, SEMIEXISTING02, SEMIEXISTING03, SEMIEXISTING04,
			                  SEMIEXISTING05, SEMIEXISTING06, SEMIEXISTING07, SEMIEXISTING08,
			                  SEMIEXISTING09, SEMIEXISTING10, SEMIEXISTING11, SEMIEXISTING12,
			                  SEMIEXISTING13, SEMIEXISTING14, SEMIEXISTING15, SEMIEXISTING16} );
	}

	public void testSemiExistingCallsPerSecond() throws Exception {

		System.out.println("Measuring average call/s MISSING NODES");
		System.out.println("======================================");
		double average = 0;
		for(int i=0; i!= CALLS_PER_SECOND_TRIES; i++) {
			CallerTask ct = new CallerTask(SEMIEXISTING01);
			Thread t = new Thread(ct);
			t.start();
			Thread.sleep(1000);
			ct.end();
			average += ct.numberOfCalls();
		}
		System.out.println("Average sequential calls/s: " + (average/CALLS_PER_SECOND_TRIES));
	}

	protected void tearDown() throws Exception {
		super.tearDown();
		dal._release();
		orb.destroy();
	}

	private class DALClientCallable implements Callable<Void> {

		private int _iterations;
		private String[] _daos;

		public DALClientCallable(String []daos, int iterations) {
			_daos = daos;
			_iterations = iterations;
		}

		public Void call() throws Exception {
			for(int i=0; i!= _iterations; i++) {
				int index = (int)(Math.random()*(_daos.length-1));
				dal.get_DAO(_daos[index]);
			}
			return null;
		}
	}

	private class CallerTask implements Runnable {

		private boolean keepRunning = true;
		private long _calls = 0;
		private String _dao;

		public CallerTask(String dao) {
			_dao = dao;
		}
		
		public void run() {
				while (keepRunning) {
					try {
						_calls++;
						dal.get_DAO(_dao);
					} catch (Exception e) {
						// Nothing special
					}
				}
		}

		public void end() {
			keepRunning = false;
		}

		public long numberOfCalls() {
			return _calls;
		}
	}
}
