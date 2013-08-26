/*
 * @@COPYRIGHT@@
 */

package alma.PS;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;

import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBdoublePOA;
import alma.ACS.CBvoidPOA;
import alma.ACS.Monitordouble;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROdouble;
import alma.ACS.RWdouble;
import alma.ACS.TimeSeqHolder;
import alma.ACS.doubleSeqHolder;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.acs.util.ACSPorts;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;

/**
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class PropertyDoubleTest extends TestCase {

	/**
	 * Callback response class. 
	 */
	class CBResponse
	{
		public static final int INVALID_TYPE = 0;
		public static final int DONE_TYPE = 1;
		public static final int WORKING_TYPE = 2;
		
		public CBResponse(Completion completion, CBDescOut desc, int type)
		{
			this(completion, desc, type, Double.MIN_VALUE);
		}

		public CBResponse(Completion completion, CBDescOut desc, int type, double value)
		{
			this.completion = completion;
			this.desc = desc;
			this.type = type;
			this.value = value;
		}
		
		public Completion completion = null;
		public CBDescOut desc = null;
		public int type = INVALID_TYPE;
		public double value = Double.MIN_VALUE;	
	}
	
	/**
	 * Implementation of <code>CBvoid</code>. 
	 */
	class CBvoidImpl extends CBvoidPOA
	{
		/**
		 * Sync. response queue.
		 */
		protected Vector responseQueue = new Vector(); 
		
		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void done(Completion completion, CBDescOut desc) {
			responseQueue.add(new CBResponse(completion, desc, CBResponse.DONE_TYPE));
			this.notify();
		}

		/**
		 * @see alma.ACS.CBvoidOperations#working(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void working(Completion completion, CBDescOut desc) {
			responseQueue.add(new CBResponse(completion, desc, CBResponse.WORKING_TYPE));
			//this.notify();
		}

		/**
		 * @see alma.ACS.CallbackOperations#negotiate(long, alma.ACS.CBDescOut)
		 */
		public boolean negotiate(long arg0, CBDescOut arg1) {
			return false;
		}

		/**
		 * Get reponse queue.
		 * @return
		 */
		public Vector getResponseQueue() {
			return responseQueue;
		}

	}

	/**
	 * Implementation of <code>CBvoid</code>. 
	 */
	class CBdoubleImpl extends CBdoublePOA
	{
		/**
		 * Sync. response queue.
		 */
		protected Vector responseQueue = new Vector(); 
		
		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void done(double value, Completion completion, CBDescOut desc) {
//	TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (done) Value: " + value);
			responseQueue.add(new CBResponse(completion, desc, CBResponse.DONE_TYPE, value));
			this.notify();
		}

/**
 * ISO 8601 date formatter.
 */
//TODO tmp
private SimpleDateFormat timeFormatter = new IsoDateFormat();


		/**
		 * @see alma.ACS.CBvoidOperations#working(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void working(double value, Completion completion, CBDescOut desc) {
// TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " Value: " + value);
			responseQueue.add(new CBResponse(completion, desc, CBResponse.WORKING_TYPE, value));
			//this.notify();
		}

		/**
		 * @see alma.ACS.CallbackOperations#negotiate(long, alma.ACS.CBDescOut)
		 */
		public boolean negotiate(long arg0, CBDescOut arg1) {
			return false;
		}

		/**
		 * Get reponse queue.
		 * @return
		 */
		public Vector getResponseQueue() {
			return responseQueue;
		}

	}

	/**
	 * Object Request Broker (ORB) object.
	 */
	private ORB orb = null;

	/**
	 * Root Portable Object Adapter (POA) object.
	 */
	private POA rootPOA = null;

	/**
	 * property to be tested.
	 */
	private ROdouble ROproperty = null;
	private RWdouble RWproperty = null;

	/**
	 * Property to be tested.
	 */
	private Manager manager = null;

	/**
	 * PowerSupply component to be tested.
	 */
	private static final String COMPONENT_NAME = "PBEND_B_01"; 

	/**
	 * Initialize CORBA.
	 */
	private void initCORBA() throws Exception
	{
		orb = ORB.init(new String[0], System.getProperties());
				
		// POA stanza, use rootPOA
		try
		{
			// resolve RootPOA
			rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			
			// activate POA
			POAManager manager = rootPOA.the_POAManager();
			manager.activate();

			// spawn ORB thread	
			new Thread(new Runnable() {
				public void run()
				{
					orb.run();
				}
			}, "CORBA").start();

		} catch (Exception e)
		{
			throw e;
		}

	}

	/**
	 * Overloads the destroy to first perform a ORB shutdown.
	 */
	public void destroyCORBA()
	{
		// destory ORB
		if (orb != null)
		{
			// do not wait for completion
			orb.shutdown(false);
			
			// and finally destroy
			orb.destroy();
		}
			
	}

	/*
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		initCORBA();
		
		// TODO tmp
		manager = ManagerHelper.narrow(orb.string_to_object("corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getManagerPort()+ "/Manager"));
		org.omg.CORBA.Object obj = manager.get_component(0x05000000, COMPONENT_NAME, true);
			
		PowerSupply ps = PowerSupplyHelper.narrow(obj);
		ROproperty = ps.readback();
		RWproperty = ps.current();
	}

	/*
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		// TODO tmp
		manager.release_component(0x05000000, COMPONENT_NAME);
		destroyCORBA();
	}

	public void testCharacteristics() throws Throwable
	{
		/*
		// RWdouble
		assertEquals(0.0, property.min_value(), 0.0);
		assertEquals(1000.0, property.max_value(), 0.0);
		*/

		assertEquals("readback", ROproperty.name());
		assertEquals(COMPONENT_NAME, ROproperty.characteristic_component_name());

		assertEquals("Readback", ROproperty.description());
		assertEquals("%9.4f", ROproperty.format());
		assertEquals("A", ROproperty.units());
		assertEquals(65535, ROproperty.resolution());

		assertEquals(10000, ROproperty.min_timer_trigger());
		assertEquals(0.0, ROproperty.default_value(), 0.0);
		
		assertEquals(0.01526, ROproperty.min_delta_trigger(), 0.0);
		assertEquals(0.0, ROproperty.graph_min(), 0.0);
		assertEquals(1000.0, ROproperty.graph_max(), 0.0);
		assertEquals(0.01526, ROproperty.min_step(), 0.0);

		assertEquals(10.0, ROproperty.alarm_low_on(), 0.0);
		assertEquals(20.0, ROproperty.alarm_low_off(), 0.0);
		assertEquals(990.0, ROproperty.alarm_high_on(), 0.0);
		assertEquals(980.0, ROproperty.alarm_high_off(), 0.0);
	
	}

	public void testNewSubscriptionAlarm() {
		try
		{
			CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
			ROproperty.new_subscription_Alarm(null, descIn);
			fail("NO_IMPLEMENT exception expected");
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
	}

	public void testGetSync() {
		CompletionHolder completionHolder = new CompletionHolder();
		// TODO check value
		/*double value =*/ ROproperty.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);
		// less than 5s
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);
	}

	public void testGetAsync() {
		
		CBdoubleImpl cb = new CBdoubleImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			ROproperty.get_async(cb._this(orb), descIn);
			try
			{
				// wait for 5s
				cb.wait(5000);
			}
			catch (InterruptedException ie) {}
		}
			
		// only 1 response is expected
		assertEquals(1, cb.getResponseQueue().size());
		CBResponse response = (CBResponse)cb.getResponseQueue().firstElement();
		
		// check reponse type
		assertEquals(CBResponse.DONE_TYPE, response.type);
		
		// check value
		// TODO check value
		//assertEquals(property.default_value(), response.value, 0.0);
		
		// check descriptor
		assertEquals(descIn.id_tag, response.desc.id_tag);
		
		// check completion
		assertEquals(0, response.completion.code);
		assertEquals(0, response.completion.type);
		assertEquals(0, response.completion.previousError.length);
		// less than 5s
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-response.completion.timeStamp)<50000000);
		
	}
	public void testSetAsync() {
		
		CBvoidImpl cb = new CBvoidImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			RWproperty.set_async(500.0,cb._this(orb), descIn);
			try
			{
				// wait for 5s
				cb.wait(5000);
			}
			catch (InterruptedException ie) {}
		}
			
		// only 1 response is expected
		//assertEquals(1, cb.getResponseQueue().size());
		//CBResponse response = (CBResponse)cb.getResponseQueue().firstElement();
		
		// check reponse type
		//assertEquals(CBResponse.DONE_TYPE, response.type);
		
		// check value
		// TODO check value
		
		CompletionHolder completionHolder = new CompletionHolder();
		double value = RWproperty.get_sync(completionHolder);
		assertEquals(500.0, value, 0.0);
		
	}

	public void testGetHistory() {
		
		// wait until history fills
		try
		{
			// 7 sec
			Thread.sleep(7000);
		}
		catch (InterruptedException ie) {}

		doubleSeqHolder dsh = new doubleSeqHolder();
		TimeSeqHolder tsh = new TimeSeqHolder();
		int len = ROproperty.get_history(5, dsh, tsh);
		assertEquals(5, len);
		assertEquals(dsh.value.length, tsh.value.length);
		
		// TODO tmp
		for (int i = 0; i < dsh.value.length; i++)
			System.out.println("[" + i + "] (" + 
								new java.util.Date(UTCUtility.utcOmgToJava(tsh.value[i])) + 
							   ") "+dsh.value[i]);
			
	}

	public void testCreateMonitor() {
		// TODO do test with null callback ;)
		// TODO implement...
		CBdoubleImpl cb = new CBdoubleImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitordouble monitor = ROproperty.create_monitor(cb._this(orb), descIn);
		try
		{
			// 10.5 sec
			Thread.sleep(10500);
		}
		catch (InterruptedException ie) {}

		// TODO test 10 calls, sync monitors

		synchronized(cb)
		{
			try
			{
				monitor.destroy();

				// wait for 3s
				cb.wait(3000);
			}
			catch (InterruptedException ie) {}
		}
		
		// TODO test if done was called
	}

	public void testOnChangeMonitor() {
		// TODO implement...
		CBdoubleImpl cb = new CBdoubleImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitordouble monitor = ROproperty.create_monitor(cb._this(orb), descIn);
		// disable on time trigger
		monitor.set_timer_trigger(0);

		try
		{
			// sleep for 5 sec
			Thread.sleep(3000);
		}
		catch (InterruptedException ie) {}

		// TODO monitors should not come


		// every change test
		monitor.set_value_trigger(0, true);
		
		// TODO change value here... 
		// ups RO monitor ;)
		// !!! TMP - tested with backdoor via alarm_high_on()...
		ROproperty.alarm_high_on();
		
		try
		{
			Thread.sleep(3000);
		}
		catch (InterruptedException ie) {}






		// disable test
		monitor.set_value_trigger(0, false);

		ROproperty.alarm_high_on();
		
		try
		{
			Thread.sleep(3000);
		}
		catch (InterruptedException ie) {}

		monitor.set_value_trigger(0, true);

		ROproperty.alarm_high_on();

		try
		{
			Thread.sleep(3000);
		}
		catch (InterruptedException ie) {}
		
System.out.println("------");

		// disable test
		monitor.suspend();

		ROproperty.alarm_high_on();

		try
		{
			Thread.sleep(3000);
		}
		catch (InterruptedException ie) {}

		/// ... this should revive it
		monitor.resume();


System.out.println("------");

ROproperty.alarm_high_on();

		try
		{
			Thread.sleep(3000);
		}
		catch (InterruptedException ie) {}

		synchronized(cb)
		{
			try
			{
				monitor.destroy();

				// wait for 3s
				cb.wait(3000);
			}
			catch (InterruptedException ie) {}
		}
		
		// TODO test if done was called
	}

	public void testCreatePostponedMonitor() {
		// TODO tmp
		if (true) return;
		
		try
		{
			CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
			ROproperty.create_postponed_monitor(0, null, descIn);
			fail("NO_IMPLEMENT exception expected");
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
	}

	public void testGetCharacteristicByName() throws NoSuchCharacteristic {
		try
		{
			ROproperty.get_characteristic_by_name("format");
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
	}

	public void testFindCharacteristic() {
		try
		{
			ROproperty.find_characteristic("format");
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
	}

	public void testGetAllCharacteristics() {
		try
		{
			ROproperty.get_all_characteristics();
			//fail("NO_IMPLEMENT exception expected");
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
	}

	/*
	public testAll()
	{
		public void testCharacteristics();
		public void testNewSubscriptionAlarm();
		public void testGetSync();
		public void testGetAsync();
		public void testGetHistory();
		public void testCreateMonitor();
		public void testCreatePostponedMonitor();
		public void testGetCharacteristicByName();
		public void testFindCharacteristic();
		public void testGetAllCharacteristics();
	}
	*/

	public static TestSuite suite() {
		return new TestSuite(PropertyDoubleTest.class);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(PropertyDoubleTest.class);
		System.exit(0);
	}

}
