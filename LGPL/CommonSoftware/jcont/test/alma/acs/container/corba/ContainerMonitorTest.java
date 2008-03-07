/*
 * ALMA - Atacama Large Millimiter Array
 * (c) Universidad Tecnica Federico Santa Maria, 2008
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.container.corba;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.monitoring.RemoteThreadsClient;
import alma.acs.monitoring.RemoteThreadsMBean;
import alma.acs.monitoring.RemoteThreadsUtil;
import alma.jconttest.DummyComponent;
import alma.jconttest.DummyComponentHelper;

/**
 * JUnit test class for testing the {@link alma.acs.monitoring.RemoteThreadsMBean}
 * work over an ACS Container
 * @author rtobar
 * Created on Feb 25, 2008, 7:35:10 PM
 */
public class ContainerMonitorTest extends ComponentClientTestCase {

	private DummyComponent comp = null;
	
	private RemoteThreadsClient rtc   = null;
	private RemoteThreadsMBean  mbean = null;
	
	private int max_threads = 0;
	private int min_threads = 0;
	
	private final String DUMMYCOMP_TYPENAME = "IDL:alma/jconttest/DummyComponent:1.0";
	private final String MAX_PROP = "jacorb.poa.thread_pool_max";
	private final String MIN_PROP = "jacorb.poa.thread_pool_min";
	private int MAX_ITERATIONS = 100;
	
	private volatile int[][] acsThreads    = null;
	private volatile int[][] jacorbThreads = null;
	
	private volatile int acs    = 0;
	private volatile int jacorb = 0;
	
	public ContainerMonitorTest(String name) throws Exception {
		super(name);
	}

	// Init all the JMX remote MBean stuff, the JacORB information,
	// and the connection to the DummyComponent
	protected void setUp() throws Exception {

		super.setUp();

		int containerPID = 0;

		try {

			// The only way to get the java processes list is to use the
			// jps command (it's like a ps, but for only java processes).
			// The command output is parsed to get the name of the runnable
			// class. Then, we search for the alma.acs.container.AcsContainerRunner
			// class, and then for the name of the ontainer. Finally, we get the
			// associated PID, which we store.
			//
			// The sun.tools.jps.Jps class in the tools.jar jarfile is used
			// by the jps process. Anyways, this class only has a main
			// method, so no information can be retrieved by means of
			// public methods. So, if the main method is invoked in a static
			// way, the output should be parsed as well as it's being done
			// now (this is far more complex, since the stdout should be
			// redirected to another PrinterStream, and then read from it).
			// Better we execute the jps command and read directly from its
			// associated InputStream.

			String s;
			Process p = Runtime.getRuntime().exec("jps -lm");
			
			BufferedReader stdInput = new BufferedReader(new 
					InputStreamReader(p.getInputStream()));
			
			// read the output from the command
			while ((s = stdInput.readLine()) != null) {
				String[] ss = s.split("[\t ]+");
				if (ss.length > 2) {
					if (ss[1].equals("alma.acs.container.AcsContainerRunner")) 
						for (int i=2; i < ss.length; i++) {
							if (ss[i].equals("silentContainer")) {
								containerPID = Integer.valueOf(ss[0]);
								break;
							}
						}
				}
			}
			stdInput.close();

			if (containerPID == 0) fail("silentContainer's PID not found.");
			
			m_logger.info("Got silentContainer's PID: " + containerPID);
		}
		catch (IOException e) {
			fail(e.getMessage());
		}
		
		rtc = new RemoteThreadsClient(containerPID);
		
		assertNotNull(rtc);
		assertTrue(rtc.connect());
		mbean = rtc.getMBean();
		
		if( acsCorba.getORB() instanceof org.jacorb.orb.ORB ) {
			max_threads = ((org.jacorb.orb.ORB)acsCorba.getORB()).getConfiguration().getAttributeAsInteger(MAX_PROP);
			min_threads = ((org.jacorb.orb.ORB)acsCorba.getORB()).getConfiguration().getAttributeAsInteger(MIN_PROP);
		}
		
		m_logger.info("Max/Min for JacORB's thread pool: " + max_threads + "/" + min_threads);
		assertTrue(max_threads > min_threads);
		
		// 10 more threads to stress the container
		max_threads += 10;
		
		org.omg.CORBA.Object compObj = getContainerServices().getDefaultComponent(DUMMYCOMP_TYPENAME);
		assertNotNull(compObj);
		comp = DummyComponentHelper.narrow(compObj);
		String compName = comp.name();
		assertNotNull(compName);
		
		acsThreads    = new int[2][max_threads*MAX_ITERATIONS];
		jacorbThreads = new int[2][max_threads*MAX_ITERATIONS];
	}

	public void testStress() throws Exception {
		
		Thread []myThreads = new Thread[max_threads];

		// Print some info that should be sed'ed and grep'ed by
		// tat .sed and .grep files
		m_logger.info("Total number of JacORB threads: " + mbean.getJacORBThreadsCount());
		m_logger.info("Total number of ACS threads: " + mbean.getAcsContainerThreadsCount());
		
		String threadInfo = RemoteThreadsUtil.printThreadsInfo(RemoteThreadsUtil.toThreadsInfo(
				mbean.getThreadsInfo("org.jacorb.poa.RequestProcessor",Thread.State.WAITING)), true
		);
		
		String[] threadInfoSeparated = threadInfo.split("\n");
		
		for(int i=0; i!= threadInfoSeparated.length; i++)
			m_logger.info(threadInfoSeparated[i]);
		
		// The present threads should be more that the min
		assertTrue(mbean.getThreadsCount("org.jacorb.poa.RequestProcessor",null) >= min_threads);
		
		// The present threads should be less than max
		assertTrue(mbean.getThreadsCount("org.jacorb.poa.RequestProcessor",null) <= max_threads);
		
		// Test the thread counting on idle mode
		for(int i = 0; i!= max_threads*MAX_ITERATIONS; i++) {
			acsThreads[0][acs++] = mbean.getAcsContainerThreadsCount();
			jacorbThreads[0][jacorb++] = mbean.getThreadsCount("org.jacorb.poa.RequestProcessor",null);
		}

		acs = 0;
		jacorb = 0;
		
		// We create "max_threads" threads and use them to communicate with
		// the DummyComponent to just send information
		for (int i = 0; i < max_threads; i++) {
			myThreads[i] = new Thread(new Runnable() {
				
				// On each of them we collect data...
				public void run() {
					for(int i=0; i!=MAX_ITERATIONS ; i++) {
						comp.callThatTakesSomeTime(20);
						acsThreads[1][acs++] = mbean.getAcsContainerThreadsCount();
						jacorbThreads[1][jacorb++] = mbean.getThreadsCount("org.jacorb.poa.RequestProcessor",null);
					}
				}
			});
		}

		// init the threads
		for (int i = 0; i < max_threads; i++) {
			myThreads[i].start();
		}
		
		// and join them
		for (int i = 0; i < max_threads; i++) {
			myThreads[i].join();
		}
	}
	
	protected void tearDown() throws Exception {
		super.tearDown();
		double acsAverageIdle = 0;
		double acsAverageStress = 0;
		double jacorbAverageIdle = 0;
		double jacorbAverageStress = 0;
		
		//	Calculate the average of all the calls while idle
		for (int i = 0; i < max_threads*MAX_ITERATIONS; i++) {
			acsAverageIdle += acsThreads[0][i];
			jacorbAverageIdle += jacorbThreads[0][i];
		}

		// Calculate the average of all the calls under stress
		for (int i = 0; i < max_threads*MAX_ITERATIONS; i++) {
			acsAverageStress += acsThreads[1][i];
			jacorbAverageStress += jacorbThreads[1][i];
		}

		m_logger.info("ACS Container threads average (idle/stress): "
				+ (acsAverageIdle/max_threads/MAX_ITERATIONS) + "/" + (acsAverageStress/max_threads/MAX_ITERATIONS) );
		m_logger.info("JacORB threads average        (idle/stress): "
				+ (jacorbAverageIdle/max_threads/MAX_ITERATIONS)  + "/" + (jacorbAverageStress/max_threads/MAX_ITERATIONS));
	}

}
