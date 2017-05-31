package jbaci.test;

import junit.framework.TestCase;
import junit.framework.TestSuite;

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
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;
import alma.ACS.Alarmdouble;
import alma.ACS.AlarmdoublePOA;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.ROdouble;
import alma.ACS.Subscription;
import alma.ACSErr.CompletionHolder;
import alma.acs.util.ACSPorts;
import alma.jbaci.AlarmTestServer;
import alma.jbaci.AlarmTestServerHelper;

/**
 * @author <a href="mailto:takashi.nakamotoATnao.ac.jp">Takashi Nakamoto</a>
 * @version $id$
 */
public class PropertyAlarmTest extends TestCase {

	/**
	 * Object Request Broker (ORB) object.
	 */
	private ORB orb;

	/**
	 * Root Portable Object Adapter (POA) object.
	 */
	private POA rootPOA;

	/**
	 * Property to be tested.
	 */
	private Manager manager;
	
	/**
	 * Name of AlarmTestServer component to be tested.
	 */
	private static final String COMPONENT_NAME = "ALARM_TEST_SRV";
	
	/**
	 * AlarmTestServer instance.
	 */
	private AlarmTestServer testServer;

	/**
	 * Initialize CORBA.
	 */
	private void initCORBA() throws Exception {
		orb = ORB.init(new String[0], System.getProperties());

		// resolve RootPOA
		rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));

		// activate POA
		POAManager manager = rootPOA.the_POAManager();
		manager.activate();

		// spawn ORB thread
		new Thread(new Runnable() {
			public void run() {
				orb.run();
			}
		}, "CORBA").start();
	}

	/**
	 * Overloads the destroy to first perform a ORB shutdown.
	 */
	public void destroyCORBA() {
		// destory ORB
		if (orb != null) {
			// do not wait for completion
			orb.shutdown(false);

			// and finally destroy
			orb.destroy();
		}
	}

	/**
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		initCORBA();

		manager = ManagerHelper.narrow(orb.string_to_object("corbaloc::"
				+ ACSPorts.getIP() + ":" + ACSPorts.getManagerPort()
				+ "/Manager"));
		org.omg.CORBA.Object obj = manager.get_component(0x05000000,
				COMPONENT_NAME, true);

		testServer = AlarmTestServerHelper.narrow(obj);
	}

	/**
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		manager.release_component(0x05000000, COMPONENT_NAME);
		destroyCORBA();
	}

	/*
	public void testDouble() throws Throwable {
		CompletionHolder c = new CompletionHolder();
		double d = 0.0;
		for (int i = 0; i < 30; i++) {
			d = testServer.doubleProp().get_sync(c);
			System.out.println("d = " + d + " (timestamp = " + c.value.timeStamp +")");
			Thread.sleep(100);
		}
		
		// TODO check if the sequence of the obtained values is in sine form.
		assertEquals(d, -1.0);
	}
	*/

	public void testAlarmRODouble() throws Throwable {
		ROdouble prop = testServer.doubleProp();
		
		double alarm_high_on  = prop.alarm_high_on();
		double alarm_high_off = prop.alarm_high_off();
		double alarm_low_on   = prop.alarm_low_on();
		double alarm_low_off  = prop.alarm_low_off();
		AlarmCounter counter = new AlarmCounter();
		
		System.out.println("Configuration of testAlarmRODouble:");
		System.out.println("alarm_high_on: " + alarm_high_on);
		System.out.println("alarm_high_off: " + alarm_high_off);
		System.out.println("alarm_low_on: " + alarm_low_on);
		System.out.println("alarm_low_off: " + alarm_low_off);
				
		AlarmdoublePOA cbPOA = new AlarmdoublePOA() {
			public void alarm_raised(double value, alma.ACSErr.Completion c, alma.ACS.CBDescOut desc) {
				assertTrue(value <= alarm_low_on || alarm_high_on <= value);
				counter.incrementRaised();
				System.out.println("alarm_raised: " + value);
			}
			public void alarm_cleared(double value, alma.ACSErr.Completion c, alma.ACS.CBDescOut desc) {
				assertTrue(alarm_low_off <= value && value <= alarm_high_off);
				counter.incrementCleared();
				System.out.println("alarm_cleared: " + value);
			}
			@Override
			public boolean negotiate(long time_to_transmit, CBDescOut desc) {
				// TODO Auto-generated method stub
				return false;
			}
		};
		
		// Timeout is set to 1 second.
		CBDescIn desc = new CBDescIn(10000000, 10000000, 0);
		Subscription sub = testServer.doubleProp().new_subscription_Alarm(cbPOA._this(orb), desc);
		
		// Wait 0.05 second.
		Thread.sleep(50);
		
		// Check if either alarm_raised() or alarm_cleared() is called only once.
		// By the BACI specification, the BACI property has to send an alarm event 
		// immediately after the client has subscribed to the alarm.
		//
		// As of 17 March, 2017, it was found that C++ BACI implementation cannot pass this
		// test.
		assertEquals(1, counter.getClearedCount() + counter.getRaisedCount());
		
		sub.resume();
		
		CompletionHolder c = new CompletionHolder();
		double d = 0.0;
		for (int i = 0; i < 30; i++) {
			d = testServer.doubleProp().get_sync(c);
			System.out.println("d = " + d + " (timestamp = " + c.value.timeStamp +")");
			Thread.sleep(100);
		}
		sub.suspend();
		sub.destroy();
		
		System.out.println("raised_count: " + counter.getRaisedCount());
		System.out.println("cleared_count: " + counter.getClearedCount());

		// Check if alarm is cleared and raised, at least, two times.
		// If it isn't, it is considered that this test case does not
		// test the alarm functionality, and it fails. This is probably
		// because the configuration in CDB is wrong, so if this test
		// case fails, fix AlarmTestServerImpl.xml in CDB.
		assertTrue(counter.getClearedCount() >= 2);
		assertTrue(counter.getRaisedCount()  >= 2);
	}

	public static TestSuite suite() {
		return new TestSuite(PropertyAlarmTest.class);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(PropertyAlarmTest.class);
		System.exit(0);
	}
}
