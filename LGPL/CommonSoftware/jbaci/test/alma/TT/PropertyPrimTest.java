/*
 * @@COPYRIGHT@@
 */

package alma.TT;

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
import alma.ACS.CBfloatPOA;
import alma.ACS.CBlongPOA;
import alma.ACS.CBvoidPOA;
import alma.ACS.Monitordouble;
import alma.ACS.Monitorfloat;
import alma.ACS.Monitorlong;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROdouble;
import alma.ACS.ROfloat;
import alma.ACS.ROlong;
import alma.ACS.RWdouble;
import alma.ACS.RWfloat;
import alma.ACS.RWlong;
import alma.ACS.TimeSeqHolder;
import alma.ACS.doubleSeqHolder;
import alma.ACS.floatSeqHolder;
import alma.ACS.longSeqHolder;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.acs.util.ACSPorts;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;

/**
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @version $id$
 */
public class PropertyPrimTest extends TestCase {

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
	//maps to java Int!
	class CBResponseLong
	{
		public static final int INVALID_TYPE = 0;
		public static final int DONE_TYPE = 1;
		public static final int WORKING_TYPE = 2;
		
		public CBResponseLong(Completion completion, CBDescOut desc, int type)
		{
			this(completion, desc, type, Integer.MIN_VALUE);
		}

		public CBResponseLong(Completion completion, CBDescOut desc, int type, int value)
		{
			this.completion = completion;
			this.desc = desc;
			this.type = type;
			this.value = value;
		}
		
		public Completion completion = null;
		public CBDescOut desc = null;
		public int type = INVALID_TYPE;
		public int value = Integer.MIN_VALUE;	
	}
	class CBResponseFloat
	{
		public static final int INVALID_TYPE = 0;
		public static final int DONE_TYPE = 1;
		public static final int WORKING_TYPE = 2;
		
		public CBResponseFloat(Completion completion, CBDescOut desc, int type)
		{
			this(completion, desc, type, Float.MIN_VALUE);
		}

		public CBResponseFloat(Completion completion, CBDescOut desc, int type, float value)
		{
			this.completion = completion;
			this.desc = desc;
			this.type = type;
			this.value = value;
		}
		
		public Completion completion = null;
		public CBDescOut desc = null;
		public int type = INVALID_TYPE;
		public float value = Float.MIN_VALUE;	
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
	 * Implementation of <code>CBvoid</code>. 
	 */
	class CBfloatImpl extends CBfloatPOA
	{
		/**
		 * Sync. response queue.
		 */
		protected Vector responseQueue = new Vector(); 
		
		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void done(float value, Completion completion, CBDescOut desc) {
//	TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (done) Value: " + value);
			responseQueue.add(new CBResponseFloat(completion, desc, CBResponse.DONE_TYPE, value));
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
		public synchronized void working(float value, Completion completion, CBDescOut desc) {
// TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " Value: " + value);
			responseQueue.add(new CBResponseFloat(completion, desc, CBResponse.WORKING_TYPE, value));
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
	//long maps to java int!
	class CBlongImpl extends CBlongPOA
	{
		/**
		 * Sync. response queue.
		 */
		protected Vector responseQueue = new Vector(); 
		
		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void done(int value, Completion completion, CBDescOut desc) {
//	TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (done) Value: " + value);
			responseQueue.add(new CBResponseLong(completion, desc, CBResponse.DONE_TYPE, value));
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
		public synchronized void working(int value, Completion completion, CBDescOut desc) {
// TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " Value: " + value);
			responseQueue.add(new CBResponseLong(completion, desc, CBResponse.WORKING_TYPE, value));
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
	private ROdouble ROpropertyDouble = null;
	private RWdouble RWpropertyDouble = null;
	private ROlong ROpropertyLong = null;
	private RWlong RWpropertyLong = null;
	private ROfloat ROpropertyFloat = null;
	private RWfloat RWpropertyFloat = null;



	/**
	 * Property to be tested.
	 */
	private Manager manager = null;

	/**
	 * PowerSupply component to be tested.
	 */
	private static final String COMPONENT_NAME = "PRIMTESTING"; 

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
			
		PrimComponent pc = PrimComponentHelper.narrow(obj);
		ROpropertyDouble = pc.doubleRO();
		RWpropertyDouble = pc.doubleRW();
		ROpropertyLong = pc.longRO();
		RWpropertyLong = pc.longRW();
		ROpropertyFloat = pc.floatRO();
		RWpropertyFloat = pc.floatRW();
		
		
	}

	/*
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		// TODO tmp
		manager.release_component(0x05000000, COMPONENT_NAME);
		destroyCORBA();
	}

	/*
	 *  testCharacteristics 
	 *  
	 */
	
	//Long property
	public void testCharacteristicsLongRO() throws Throwable
	{
		//RO
		assertEquals("longRO", ROpropertyLong.name());
		assertEquals(COMPONENT_NAME, ROpropertyLong.characteristic_component_name());

		assertEquals("long RO", ROpropertyLong.description());
		assertEquals("%9.3f", ROpropertyLong.format());
		assertEquals("A", ROpropertyLong.units());
		assertEquals(65535, ROpropertyLong.resolution());

		assertEquals(10000, ROpropertyLong.min_timer_trigger());
		assertEquals(0, ROpropertyLong.default_value(), 0);
		
		assertEquals(1526, ROpropertyLong.min_delta_trigger(), 0);
		assertEquals(100, ROpropertyLong.graph_min(), 0);
		assertEquals(2000, ROpropertyLong.graph_max(), 0);
		assertEquals(19, ROpropertyLong.min_step(), 0);

		assertEquals(20, ROpropertyLong.alarm_low_on(), 0);
		assertEquals(30, ROpropertyLong.alarm_low_off(), 0);
		assertEquals(890, ROpropertyLong.alarm_high_on(), 0);
		assertEquals(880, ROpropertyLong.alarm_high_off(), 0);
			
	}
	
	
	
	public void testCharacteristicsLongRW() throws Throwable
	{
		
		// RW
		assertEquals(0, RWpropertyLong.min_value(), 0);
		assertEquals(3000, RWpropertyLong.max_value(), 0);

		assertEquals("longRW", RWpropertyLong.name());
		assertEquals(COMPONENT_NAME, RWpropertyLong.characteristic_component_name());

		assertEquals("long RW", RWpropertyLong.description());
		assertEquals("%9.4f", RWpropertyLong.format());
		assertEquals("A", RWpropertyLong.units());
		assertEquals(65535, RWpropertyLong.resolution());

		assertEquals(50000, RWpropertyLong.min_timer_trigger());
		assertEquals(5, RWpropertyLong.default_value(), 0);
		
		assertEquals(4, RWpropertyLong.min_delta_trigger(), 0);
		assertEquals(10, RWpropertyLong.graph_min(), 0);
		assertEquals(1000, RWpropertyLong.graph_max(), 0);
		assertEquals(2, RWpropertyLong.min_step(), 0);


			
	}

	//Double Property
	public void testCharacteristicsDoubleRO() throws Throwable
	{
		// RO

		assertEquals("doubleRO", ROpropertyDouble.name());
		assertEquals(COMPONENT_NAME, ROpropertyDouble.characteristic_component_name());

		assertEquals("double RO", ROpropertyDouble.description());
		assertEquals("%9.4f", ROpropertyDouble.format());
		assertEquals("A", ROpropertyDouble.units());
		assertEquals(65535, ROpropertyDouble.resolution());

		assertEquals(10000, ROpropertyDouble.min_timer_trigger());
		assertEquals(0.0, ROpropertyDouble.default_value(), 0.0);
		
		assertEquals(0.01526, ROpropertyDouble.min_delta_trigger(), 0.0);
		assertEquals(0.0, ROpropertyDouble.graph_min(), 0.0);
		assertEquals(1000.0, ROpropertyDouble.graph_max(), 0.0);
		assertEquals(0.01526, ROpropertyDouble.min_step(), 0.0);

		assertEquals(10.0, ROpropertyDouble.alarm_low_on(), 0.0);
		assertEquals(20.0, ROpropertyDouble.alarm_low_off(), 0.0);
		assertEquals(990.0, ROpropertyDouble.alarm_high_on(), 0.0);
		assertEquals(980.0, ROpropertyDouble.alarm_high_off(), 0.0);
	
	}
	
	public void testCharacteristicsDoubleRW() throws Throwable
	{
		// RW
		assertEquals(0.0, RWpropertyDouble.min_value(), 0.0);
		assertEquals(1000.0, RWpropertyDouble.max_value(), 0.0);

		assertEquals("doubleRW", RWpropertyDouble.name());
		assertEquals(COMPONENT_NAME, RWpropertyDouble.characteristic_component_name());

		assertEquals("double RW", RWpropertyDouble.description());
		assertEquals("%9.4f", RWpropertyDouble.format());
		assertEquals("A", RWpropertyDouble.units());
		assertEquals(65535, RWpropertyDouble.resolution());

		assertEquals(10000, RWpropertyDouble.min_timer_trigger());
		assertEquals(0.0, RWpropertyDouble.default_value(), 0.0);
		
		assertEquals(0.01526, RWpropertyDouble.min_delta_trigger(), 0.0);
		assertEquals(0.0, RWpropertyDouble.graph_min(), 0.0);
		assertEquals(1000.0, RWpropertyDouble.graph_max(), 0.0);
		assertEquals(0.01526, RWpropertyDouble.min_step(), 0.0);

	
	}

	//Float Property
	public void testCharacteristicsFloatRO() throws Throwable
	{
		//RO
		assertEquals("floatRO", ROpropertyFloat.name());
		assertEquals(COMPONENT_NAME, ROpropertyFloat.characteristic_component_name());

		assertEquals("float RO", ROpropertyFloat.description());
		assertEquals("%9.4f", ROpropertyFloat.format());
		assertEquals("A", ROpropertyFloat.units());
		assertEquals(65535, ROpropertyFloat.resolution());

		assertEquals(10000, ROpropertyFloat.min_timer_trigger());
		assertEquals(0.0, ROpropertyFloat.default_value(), 0.00001);
		
		assertEquals(0.01526, ROpropertyFloat.min_delta_trigger(), 0.00001);
		assertEquals(0.0, ROpropertyFloat.graph_min(), 0.00001);
		assertEquals(1000.0, ROpropertyFloat.graph_max(), 0.00001);
		assertEquals(0.01526, ROpropertyFloat.min_step(), 0.00001);

		assertEquals(10.0, ROpropertyFloat.alarm_low_on(), 0.00001);
		assertEquals(20.0, ROpropertyFloat.alarm_low_off(), 0.00001);
		assertEquals(990.0, ROpropertyFloat.alarm_high_on(), 0.00001);
		assertEquals(980.0, ROpropertyFloat.alarm_high_off(), 0.00001);
	
	}
	
	public void testCharacteristicsFloatRW() throws Throwable
	{
		// RW
		assertEquals(0.0, RWpropertyFloat.min_value(), 0.00001);
		assertEquals(1000.0, RWpropertyFloat.max_value(), 0.00001);

		assertEquals("floatRW", RWpropertyFloat.name());
		assertEquals(COMPONENT_NAME, RWpropertyFloat.characteristic_component_name());

		assertEquals("float RW", RWpropertyFloat.description());
		assertEquals("%9.4f", RWpropertyFloat.format());
		assertEquals("A", RWpropertyFloat.units());
		assertEquals(65535, RWpropertyFloat.resolution());

		assertEquals(10000, RWpropertyFloat.min_timer_trigger());
		assertEquals(0.0, RWpropertyFloat.default_value(), 0.00001);
		

		assertEquals(0.01526, RWpropertyFloat.min_delta_trigger(), 0.00001);
		assertEquals(0.0, RWpropertyFloat.graph_min(), 0.00001);
		assertEquals(1000.0, RWpropertyFloat.graph_max(), 0.00001);
		assertEquals(0.01526, RWpropertyFloat.min_step(), 0.00001);

	
	}
	
	
	public void testNewSubscriptionAlarm() {
		try
		{
			
			CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
			ROpropertyDouble.new_subscription_Alarm(null, descIn);
			fail("NO_IMPLEMENT exception expected");
			
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
	}

	public void testGetSyncAll() {
		//ROlong
		CompletionHolder completionHolder = new CompletionHolder();
		// TODO check value
		ROpropertyLong.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);
		
		//RWlong
		completionHolder = new CompletionHolder();
		// TODO check value
		RWpropertyLong.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);
		
		//RODouble
		completionHolder = new CompletionHolder();
		// TODO check value
		ROpropertyDouble.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);	
		
		//RWDouble
		completionHolder = new CompletionHolder();
		// TODO check value
		RWpropertyDouble.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);	
		
		//ROFloat
		completionHolder = new CompletionHolder();
		// TODO check value
		ROpropertyFloat.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);	
		
		//RWFloat
		completionHolder = new CompletionHolder();
		// TODO check value
		RWpropertyFloat.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);	
		
		
	}

	public void testGetAsyncDoubleRO() {
		
		
		CBdoubleImpl cb = new CBdoubleImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			ROpropertyDouble.get_async(cb._this(orb), descIn);
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
	
	public void testGetAsyncDoubleRW() {
		
		
		CBdoubleImpl cb = new CBdoubleImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			RWpropertyDouble.get_async(cb._this(orb), descIn);
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
	
	public void testGetAsyncLongRO() {
		
		
		CBlongImpl cb = new CBlongImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			ROpropertyLong.get_async(cb._this(orb), descIn);
			try
			{
				// wait for 5s
				cb.wait(5000);
			}
			catch (InterruptedException ie) {}
		}
			
		// only 1 response is expected
		assertEquals(1, cb.getResponseQueue().size());
		CBResponseLong response = (CBResponseLong)cb.getResponseQueue().firstElement();
		
		// check reponse type
		assertEquals(CBResponseLong.DONE_TYPE, response.type);
		
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
	
public void testGetAsyncLongRW() {
		
		
		CBlongImpl cb = new CBlongImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			RWpropertyLong.get_async(cb._this(orb), descIn);
			try
			{
				// wait for 5s
				cb.wait(5000);
			}
			catch (InterruptedException ie) {}
		}
			
		// only 1 response is expected
		assertEquals(1, cb.getResponseQueue().size());
		CBResponseLong response = (CBResponseLong)cb.getResponseQueue().firstElement();
		
		// check reponse type
		assertEquals(CBResponseLong.DONE_TYPE, response.type);
		
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
	
public void testGetAsyncFloatRO() {
	
	
	CBfloatImpl cb = new CBfloatImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
	synchronized(cb)
	{
		ROpropertyFloat.get_async(cb._this(orb), descIn);
		try
		{
			// wait for 5s
			cb.wait(5000);
		}
		catch (InterruptedException ie) {}
	}
		
	// only 1 response is expected
	assertEquals(1, cb.getResponseQueue().size());
	CBResponseFloat response = (CBResponseFloat)cb.getResponseQueue().firstElement();
	
	// check reponse type
	assertEquals(CBResponseFloat.DONE_TYPE, response.type);
	
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

public void testGetAsyncFloatRW() {
	
	
	CBfloatImpl cb = new CBfloatImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
	synchronized(cb)
	{
		RWpropertyFloat.get_async(cb._this(orb), descIn);
		try
		{
			// wait for 5s
			cb.wait(5000);
		}
		catch (InterruptedException ie) {}
	}
		
	// only 1 response is expected
	assertEquals(1, cb.getResponseQueue().size());
	CBResponseFloat response = (CBResponseFloat)cb.getResponseQueue().firstElement();
	
	// check reponse type
	assertEquals(CBResponseFloat.DONE_TYPE, response.type);
	
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
	
public void testSetAsyncLongRW() {
		
		
		CBvoidImpl cb = new CBvoidImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			RWpropertyLong.set_async(500,cb._this(orb), descIn);
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
		int value = RWpropertyLong.get_sync(completionHolder);
		assertEquals(500, value, 0);
		
		
	}

public void testSetAsyncDoubleRW() {
	
	
	CBvoidImpl cb = new CBvoidImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
	synchronized(cb)
	{
		RWpropertyDouble.set_async(500.0,cb._this(orb), descIn);
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
	double value = RWpropertyDouble.get_sync(completionHolder);
	assertEquals(500.0, value, 0);
	
	
}

public void testSetAsyncFloatRW() {
	
	
	CBvoidImpl cb = new CBvoidImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
	synchronized(cb)
	{
		RWpropertyFloat.set_async( (float)500.0,cb._this(orb), descIn);
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
	double value = RWpropertyFloat.get_sync(completionHolder);
	assertEquals(500.0, value, 0.001);
	
	
}



	public void testGetHistoryDouble() {
		
		// wait until history fills
		try
		{
			// 7 sec
			Thread.sleep(7000);
		}
		catch (InterruptedException ie) {}

		doubleSeqHolder dsh = new doubleSeqHolder();
		TimeSeqHolder tsh = new TimeSeqHolder();
		int len = ROpropertyDouble.get_history(5, dsh, tsh);
		assertEquals(5, len);
		assertEquals(dsh.value.length, tsh.value.length);
		
		// TODO tmp
		for (int i = 0; i < dsh.value.length; i++)
			System.out.println("[" + i + "] (" + 
								new java.util.Date(UTCUtility.utcOmgToJava(tsh.value[i])) + 
							   ") "+dsh.value[i]);
							   
			
	}
	
	public void testGetHistoryLong() {
		
		// wait until history fills
		try
		{
			// 7 sec
			Thread.sleep(7000);
		}
		catch (InterruptedException ie) {}

		longSeqHolder lsh = new longSeqHolder();
		TimeSeqHolder tsh = new TimeSeqHolder();
		int len = ROpropertyLong.get_history(5, lsh, tsh);
		assertEquals(5, len);
		assertEquals(lsh.value.length, tsh.value.length);
		
		// TODO tmp
		for (int i = 0; i < lsh.value.length; i++)
			System.out.println("[" + i + "] (" + 
								new java.util.Date(UTCUtility.utcOmgToJava(tsh.value[i])) + 
							   ") "+lsh.value[i]);
							   
			
	}
	
public void testGetHistoryFloat() {
		
		// wait until history fills
		try
		{
			// 7 sec
			Thread.sleep(7000);
		}
		catch (InterruptedException ie) {}

		floatSeqHolder fsh = new floatSeqHolder();
		TimeSeqHolder tsh = new TimeSeqHolder();
		int len = ROpropertyFloat.get_history(5, fsh, tsh);
		assertEquals(5, len);
		assertEquals(fsh.value.length, tsh.value.length);
		
		// TODO tmp
		for (int i = 0; i < fsh.value.length; i++)
			System.out.println("[" + i + "] (" + 
								new java.util.Date(UTCUtility.utcOmgToJava(tsh.value[i])) + 
							   ") "+fsh.value[i]);
							   
			
	}

	public void testCreateMonitorLong() {
		
		// TODO do test with null callback ;)
		// TODO implement...
		CBlongImpl cb = new CBlongImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitorlong monitor = ROpropertyLong.create_monitor(cb._this(orb), descIn);
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
	
public void testCreateMonitorDouble() {
		
		// TODO do test with null callback ;)
		// TODO implement...
		CBdoubleImpl cb = new CBdoubleImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitordouble monitor = ROpropertyDouble.create_monitor(cb._this(orb), descIn);
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

public void testCreateMonitorFloat() {
	
	// TODO do test with null callback ;)
	// TODO implement...
	CBfloatImpl cb = new CBfloatImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

	Monitorfloat monitor = ROpropertyFloat.create_monitor(cb._this(orb), descIn);
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


	public void testOnChangeMonitorLong() {
		
		// TODO implement...
		CBlongImpl cb = new CBlongImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitorlong monitor = ROpropertyLong
				.create_monitor(cb._this(orb), descIn);
		// disable on time trigger
		monitor.set_timer_trigger(0);

		try {
			// sleep for 5 sec
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// TODO monitors should not come

		// every change test
		monitor.set_value_trigger(0, true);

		// TODO change value here...
		// ups RO monitor ;)
		// !!! TMP - tested with backdoor via alarm_high_on()...
		ROpropertyLong.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// disable test
		monitor.set_value_trigger(0, false);

		ROpropertyLong.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		monitor.set_value_trigger(0, true);

		ROpropertyLong.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		System.out.println("------");

		// disable test
		monitor.suspend();

		ROpropertyLong.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// / ... this should revive it
		monitor.resume();

		System.out.println("------");

		ROpropertyLong.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		synchronized (cb) {
			try {
				monitor.destroy();

				// wait for 3s
				cb.wait(3000);
			} catch (InterruptedException ie) {
			}
		}

		// TODO test if done was called
		 
		 
	}
	
public void testOnChangeMonitorDouble() {
		
		// TODO implement...
		CBdoubleImpl cb = new CBdoubleImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitordouble monitor = ROpropertyDouble
				.create_monitor(cb._this(orb), descIn);
		// disable on time trigger
		monitor.set_timer_trigger(0);

		try {
			// sleep for 5 sec
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// TODO monitors should not come

		// every change test
		monitor.set_value_trigger(0, true);

		// TODO change value here...
		// ups RO monitor ;)
		// !!! TMP - tested with backdoor via alarm_high_on()...
		ROpropertyDouble.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// disable test
		monitor.set_value_trigger(0, false);

		ROpropertyDouble.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		monitor.set_value_trigger(0, true);

		ROpropertyDouble.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		System.out.println("------");

		// disable test
		monitor.suspend();

		ROpropertyDouble.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// / ... this should revive it
		monitor.resume();

		System.out.println("------");

		ROpropertyDouble.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		synchronized (cb) {
			try {
				monitor.destroy();

				// wait for 3s
				cb.wait(3000);
			} catch (InterruptedException ie) {
			}
		}

		// TODO test if done was called
		 
		 
	}

public void testOnChangeMonitorFloat() {
	
	// TODO implement...
	CBfloatImpl cb = new CBfloatImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

	Monitorfloat monitor = ROpropertyFloat
			.create_monitor(cb._this(orb), descIn);
	// disable on time trigger
	monitor.set_timer_trigger(0);

	try {
		// sleep for 5 sec
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	// TODO monitors should not come

	// every change test
	monitor.set_value_trigger(0, true);

	// TODO change value here...
	// ups RO monitor ;)
	// !!! TMP - tested with backdoor via alarm_high_on()...
	ROpropertyFloat.alarm_high_on();

	try {
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	// disable test
	monitor.set_value_trigger(0, false);

	ROpropertyFloat.alarm_high_on();

	try {
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	monitor.set_value_trigger(0, true);

	ROpropertyFloat.alarm_high_on();

	try {
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	System.out.println("------");

	// disable test
	monitor.suspend();

	ROpropertyFloat.alarm_high_on();

	try {
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	// / ... this should revive it
	monitor.resume();

	System.out.println("------");

	ROpropertyFloat.alarm_high_on();

	try {
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	synchronized (cb) {
		try {
			monitor.destroy();

			// wait for 3s
			cb.wait(3000);
		} catch (InterruptedException ie) {
		}
	}

	// TODO test if done was called
	 
	 
}

	public void testCreatePostponedMonitor() {
		
		// TODO tmp
		if (true) return;
		
		try
		{
			CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
			ROpropertyLong.create_postponed_monitor(0, null, descIn);
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
			ROpropertyLong.get_characteristic_by_name("format");
			RWpropertyLong.get_characteristic_by_name("format");
			ROpropertyDouble.get_characteristic_by_name("format");
			RWpropertyDouble.get_characteristic_by_name("format");
			ROpropertyFloat.get_characteristic_by_name("format");
			RWpropertyFloat.get_characteristic_by_name("format");
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
		
	}

	public void testFindCharacteristic() {
		
		try
		{
			ROpropertyLong.find_characteristic("format");
			RWpropertyLong.find_characteristic("format");
			ROpropertyDouble.find_characteristic("format");
			RWpropertyDouble.find_characteristic("format");
			ROpropertyFloat.find_characteristic("format");
			RWpropertyFloat.find_characteristic("format");
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
		
	}

	public void testGetAllCharacteristics() {
		
		try
		{
			ROpropertyLong.get_all_characteristics();
			RWpropertyLong.get_all_characteristics();
			ROpropertyDouble.get_all_characteristics();
			RWpropertyDouble.get_all_characteristics();
			ROpropertyFloat.get_all_characteristics();
			RWpropertyFloat.get_all_characteristics();
			
			
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
		return new TestSuite(PropertyPrimTest.class);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(PropertyPrimTest.class);
		System.exit(0);
	}

}
