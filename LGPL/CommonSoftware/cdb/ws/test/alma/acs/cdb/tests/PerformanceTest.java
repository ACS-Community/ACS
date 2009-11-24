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

public class PerformanceTest extends TestCase {

	private String strIOR = "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getCDBPort() + "/CDB";
	private ORB orb;
	private DAL dal;

	private static final int CALLS_PER_SECOND_TRIES = 50;

	private static final int SEQUENTIAL_CALLS_ITERATIONS = 50;
	private static final int CONCURRENT_CALLS_ITERATIONS = 50;
	private static final int NUMBER_OF_SEQUENTIAL_CALLS = 100;
	private static final int NUMBER_OF_CONCURRENT_CALLS = 100;

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

	protected void setUp() throws Exception {
		super.setUp();

		String args[] = {};
		orb = ORB.init(args, null);
		dal = JDALHelper.narrow(orb.string_to_object(strIOR));
	}

	private void runConcurrentCalls(String []daos) throws Exception {
		long start;
		long end;
		double average = 0;
		ThreadBurstExecutorService service;
		ThreadFactory threadFactory = new ThreadFactory(){
			public Thread newThread(Runnable r) {
				return new Thread(r);
			}
		};

		for (int iteration = 0; iteration != CONCURRENT_CALLS_ITERATIONS; iteration++) {

			service = new ThreadBurstExecutorService(threadFactory);

			for (int i = 0; i != NUMBER_OF_CONCURRENT_CALLS; i++)
				service.submit(new DALClientCallable(daos), 30, TimeUnit.SECONDS);

			start = System.currentTimeMillis();
			service.executeAllAndWait(30, TimeUnit.SECONDS);
			end = System.currentTimeMillis();
			average += (end - start);

		}

		System.out.println("Concurrent calls: " + NUMBER_OF_CONCURRENT_CALLS + ". Different DAOs: " + daos.length + ". Tries: " + CONCURRENT_CALLS_ITERATIONS + ". Average time: " + average/CONCURRENT_CALLS_ITERATIONS + " [ms]");
	}

	private void runSequentialCalls(String []daos) throws Exception {

		long start;
		long end;
		double average = 0;

		for (int iterations = 0; iterations != SEQUENTIAL_CALLS_ITERATIONS; iterations++) {
			start = System.currentTimeMillis();
			for (int i = 0; i != NUMBER_OF_SEQUENTIAL_CALLS; i++) {
				int index = (int)(Math.random()*(daos.length-1));
				dal.get_DAO(daos[index]);
			}
			end = System.currentTimeMillis();
			average += (end-start);
		}

		System.out.println("Sequential calls: " + NUMBER_OF_SEQUENTIAL_CALLS + ". Different DAOs: " + daos.length + ". Tries: " + SEQUENTIAL_CALLS_ITERATIONS + ". Average time: " + average/SEQUENTIAL_CALLS_ITERATIONS + " [ms]");		
	}

	public void testConcurrentCalls() throws Exception {

		runConcurrentCalls( new String[] {MANAGER_DAO} );
		runConcurrentCalls( new String[] {MANAGER_DAO, CONTAINER_DAO, TESTPS_DAO, FILTERWHEEL_DAO} );
		runConcurrentCalls(
				new String[] {MANAGER_DAO, CONTAINER_DAO, TESTPS_DAO, FILTERWHEEL_DAO,
				              COMPONENTS_DAO, ABM001_DAO, ABM002_DAO, ARTM_DAO,
				              DMC_DAO, ANTENNA_DAO, GPS_DAO, OPERATOR_DAO,
				              SNDLONODE40_DAO, SNDLONODE41_DAO, SNDLONODE42_DAO, SNDLONODE43_DAO} );

	}

	public void testSequentialCalls() throws Exception {

		runSequentialCalls( new String[] {MANAGER_DAO} );
		runSequentialCalls( new String[] {MANAGER_DAO, CONTAINER_DAO, TESTPS_DAO, FILTERWHEEL_DAO} );
		runSequentialCalls(
				new String[] {MANAGER_DAO, CONTAINER_DAO, TESTPS_DAO, FILTERWHEEL_DAO,
				              COMPONENTS_DAO, ABM001_DAO, ABM002_DAO, ARTM_DAO,
				              DMC_DAO, ANTENNA_DAO, GPS_DAO, OPERATOR_DAO,
				              SNDLONODE40_DAO, SNDLONODE41_DAO, SNDLONODE42_DAO, SNDLONODE43_DAO} );

	}

	public void testCallsPerSecond() throws Exception {

		double average = 0;
		for(int i=0; i!= CALLS_PER_SECOND_TRIES; i++) {
			CallerTask ct = new CallerTask();
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

		private String[] _daos;

		public DALClientCallable(String []daos) {
			_daos = daos;
		}

		public Void call() throws Exception {
			int index = (int)(Math.random()*(_daos.length-1));
			dal.get_DAO(_daos[index]);
			return null;
		}
	}

	private class CallerTask implements Runnable {

		private boolean keepRunning = true;
		private long _calls = 0;

		public void run() {
			try {
				while (keepRunning) {
					dal.get_DAO(MANAGER_DAO);
					_calls++;
				}
			} catch (Exception e) {
				// Nothing, should't happen
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
