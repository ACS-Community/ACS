/*
 * @@COPYRIGHT@@
 */

package alma.ACS.jbaci.enumProp.test;

import java.util.Arrays;
import java.util.Vector;

import jbaciEnumPropTest.ROStates;
import jbaciEnumPropTest.RWStates;
import jbaciEnumPropTest.States;
import jbaciEnumPropTest.StatesSeqHolder;
import jbaciEnumPropTest.jbaciEnumTestComponent;
import jbaciEnumPropTest.jbaciEnumTestComponentHelper;
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
import alma.ACS.CBpatternPOA;
import alma.ACS.CBvoidPOA;
import alma.ACS.Monitorpattern;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.TimeSeqHolder;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.acs.util.ACSPorts;
import alma.acs.util.UTCUtility;

/**
 * @author <a href="mailto:matej.sekoranjaATcosylab.com">Matej Sekoranja</a>
 * @version $id$
 */
public class EnumPropertyTest extends TestCase {

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
			this(completion, desc, type, null);
		}

		public CBResponse(Completion completion, CBDescOut desc, int type, States value)
		{
			this.completion = completion;
			this.desc = desc;
			this.type = type;
			this.value = value;
		}
		
		public Completion completion = null;
		public CBDescOut desc = null;
		public int type = INVALID_TYPE;
		public States value;	
	}
	
	/**
	 * Implementation of <code>CBvoid</code>. 
	 */
	class CBvoidImpl extends CBvoidPOA
	{
		/**
		 * Sync. response queue.
		 */
		protected Vector<CBResponse> responseQueue = new Vector<CBResponse>(); 
		
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
	class CBStatesImpl extends CBpatternPOA
	{
		/**
		 * Sync. response queue.
		 */
		protected Vector<CBResponse> responseQueue = new Vector<CBResponse>(); 
		
		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void done(long value, Completion completion, CBDescOut desc) {
			responseQueue.add(new CBResponse(completion, desc, CBResponse.DONE_TYPE, States.from_int((int)value)));
			this.notify();
		}

		/**
		 * @see alma.ACS.CBvoidOperations#working(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void working(long value, Completion completion, CBDescOut desc) {
			responseQueue.add(new CBResponse(completion, desc, CBResponse.WORKING_TYPE, States.from_int((int)value)));
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
	private ROStates ROproperty = null;
	private RWStates RWproperty = null;

	/**
	 * Property to be tested.
	 */
	private Manager manager = null;

	/**
	 * PowerSupply component to be tested.
	 */
	private static final String COMPONENT_NAME = "JBACI_ENUM_PROP"; 

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

		jbaciEnumTestComponent c = jbaciEnumTestComponentHelper.narrow(obj);
		ROproperty = c.currentState();
		RWproperty = c.currentStateRW();
	}

	/*
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		// TODO tmp
		manager.release_component(0x05000000, COMPONENT_NAME);
		destroyCORBA();
	}

	public void testROCharacteristics() throws Throwable
	{

		assertEquals("currentState", ROproperty.name());
		assertEquals(COMPONENT_NAME, ROproperty.characteristic_component_name());

		assertEquals("State", ROproperty.description());
		assertEquals("%d", ROproperty.format());
		assertEquals("w/o", ROproperty.units());
		assertEquals(7, ROproperty.resolution());

		assertEquals(10000, ROproperty.min_timer_trigger());
		assertEquals(States.from_int(0), ROproperty.default_value());
		
		Arrays.toString(ROproperty.allStates());
		Arrays.toString(ROproperty.statesDescription());
		Arrays.toString(ROproperty.condition());
		Arrays.toString(ROproperty.alarm_off());
		Arrays.toString(ROproperty.alarm_on());
	}

	public void testRWCharacteristics() throws Throwable
	{

		assertEquals("currentStateRW", RWproperty.name());
		assertEquals(COMPONENT_NAME, RWproperty.characteristic_component_name());

		assertEquals("State", RWproperty.description());
		assertEquals("%d", RWproperty.format());
		assertEquals("w/o", RWproperty.units());
		assertEquals(7, RWproperty.resolution());

		assertEquals(10000, RWproperty.min_timer_trigger());
		assertEquals(States.from_int(0), RWproperty.default_value());
		
		Arrays.toString(RWproperty.allStates());
		Arrays.toString(RWproperty.statesDescription());
	}

	/*
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
*/

	public void testGetSync() {
		CompletionHolder completionHolder = new CompletionHolder();
		// TODO check value
		assertEquals(ROproperty.default_value(), ROproperty.get_sync(completionHolder));

		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);
		// less than 5s
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);
	}

	public void testGetAsync() {
		
		CBStatesImpl cb = new CBStatesImpl();
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
		assertEquals(ROproperty.default_value(), response.value);
		
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
			RWproperty.set_async(States.DIAGNOSE,cb._this(orb), descIn);
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
		CompletionHolder completionHolder = new CompletionHolder();
		States value = RWproperty.get_sync(completionHolder);
		assertEquals(States.DIAGNOSE, value);
		
	}

	public void testSetSync() {
		
		RWproperty.set_sync(States.ENABLED);
		
		// check value
		CompletionHolder completionHolder = new CompletionHolder();
		States value = RWproperty.get_sync(completionHolder);
		assertEquals(States.ENABLED, value);
	}

	public void testSetNonBlockingSync() {
		
		RWproperty.set_nonblocking(States.SHUTDOWN);
		
		// check value
		CompletionHolder completionHolder = new CompletionHolder();
		States value = RWproperty.get_sync(completionHolder);
		assertEquals(States.SHUTDOWN, value);
	}

	public void testGetHistory() {
		
		// wait until history fills
		try
		{
			// 7 sec
			Thread.sleep(7000);
		}
		catch (InterruptedException ie) {}

		StatesSeqHolder dsh = new StatesSeqHolder();
		TimeSeqHolder tsh = new TimeSeqHolder();
		int len = ROproperty.get_history(5, dsh, tsh);
		assertEquals(5, len);
		assertEquals(dsh.value.length, tsh.value.length);
		
		// TODO tmp
		for (int i = 0; i < dsh.value.length; i++)
			System.out.println("[" + i + "]" + dsh.value[i]);
			
	}

	public void testCreateMonitor() {
		// TODO do test with null callback ;)
		// TODO implement...
		CBStatesImpl cb = new CBStatesImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitorpattern monitor = ROproperty.create_monitor(cb._this(orb), descIn);
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
	
	public static TestSuite suite() {
		return new TestSuite(EnumPropertyTest.class);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(EnumPropertyTest.class);
		System.exit(0);
	}

}
