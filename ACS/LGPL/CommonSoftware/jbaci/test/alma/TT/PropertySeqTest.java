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
import alma.ACS.CBdoubleSeqPOA;
import alma.ACS.CBfloatPOA;
import alma.ACS.CBfloatSeqPOA;
import alma.ACS.CBlongPOA;
import alma.ACS.CBlongSeqPOA;
import alma.ACS.CBvoidPOA;
import alma.ACS.Monitordouble;
import alma.ACS.Monitorfloat;
import alma.ACS.Monitorlong;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.ROdouble;
import alma.ACS.ROdoubleSeq;
import alma.ACS.ROfloat;
import alma.ACS.ROfloatSeq;
import alma.ACS.ROlong;
import alma.ACS.ROlongSeq;
import alma.ACS.RWdouble;
import alma.ACS.RWdoubleSeq;
import alma.ACS.RWfloat;
import alma.ACS.RWfloatSeq;
import alma.ACS.RWlong;
import alma.ACS.RWlongSeq;
import alma.ACS.TimeSeqHolder;
import alma.ACS.doubleSeqHolder;
import alma.ACS.doubleSeqSeqHolder;
import alma.ACS.floatSeqHolder;
import alma.ACS.floatSeqSeqHolder;
import alma.ACS.longSeqHolder;
import alma.ACS.longSeqSeqHolder;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.acs.util.ACSPorts;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;

/**
 * @author <a href="mailto:cmenayATcsrg.inf.utfsm.cl">Camilo Menay</a>
 * @version $id$
 */
public class PropertySeqTest extends TestCase {

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
	
	
	//doubleSeq
	class CBResponseDoubleSeq
	{
		public static final int INVALID_TYPE = 0;
		public static final int DONE_TYPE = 1;
		public static final int WORKING_TYPE = 2;
		
		public CBResponseDoubleSeq(Completion completion, CBDescOut desc, int type)
		{
			this(completion, desc, type, new double[]{0.0} );
		}

		public CBResponseDoubleSeq(Completion completion, CBDescOut desc, int type, double value[])
		{
			this.completion = completion;
			this.desc = desc;
			this.type = type;
			this.value = value;
		}
		
		public Completion completion = null;
		public CBDescOut desc = null;
		public int type = INVALID_TYPE;
		public double[] value = {0.0};	
	}
	
	
	class CBResponseLongSeq
	{
		public static final int INVALID_TYPE = 0;
		public static final int DONE_TYPE = 1;
		public static final int WORKING_TYPE = 2;
		
		public CBResponseLongSeq(Completion completion, CBDescOut desc, int type)
		{
			this(completion, desc, type, new int[]{0});
		}

		public CBResponseLongSeq(Completion completion, CBDescOut desc, int type, int[] value)
		{
			this.completion = completion;
			this.desc = desc;
			this.type = type;
			this.value = value;
		}
		
		public Completion completion = null;
		public CBDescOut desc = null;
		public int type = INVALID_TYPE;
		public int[] value = {0};	
	}
	class CBResponseFloatSeq
	{
		public static final int INVALID_TYPE = 0;
		public static final int DONE_TYPE = 1;
		public static final int WORKING_TYPE = 2;
		
		public CBResponseFloatSeq(Completion completion, CBDescOut desc, int type)
		{
			this(completion, desc, type, new float[]{ (float)0.0});
		}

		public CBResponseFloatSeq(Completion completion, CBDescOut desc, int type, float[] value)
		{
			this.completion = completion;
			this.desc = desc;
			this.type = type;
			this.value = value;
		}
		
		public Completion completion = null;
		public CBDescOut desc = null;
		public int type = INVALID_TYPE;
		public float[] value = {0.0f};
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
	class CBdoubleSeqImpl extends CBdoubleSeqPOA
	{
		/**
		 * Sync. response queue.
		 */
		protected Vector responseQueue = new Vector(); 
		
		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void done(double[] value, Completion completion, CBDescOut desc) {
//	TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (done) Value: " + value);
			responseQueue.add(new CBResponseDoubleSeq(completion, desc, CBResponse.DONE_TYPE, value));
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
		public synchronized void working(double[] value, Completion completion, CBDescOut desc) {
// TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " Value: " + value);
			responseQueue.add(new CBResponseDoubleSeq(completion, desc, CBResponse.WORKING_TYPE, value));
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
	class CBfloatSeqImpl extends CBfloatSeqPOA
	{
		/**
		 * Sync. response queue.
		 */
		protected Vector responseQueue = new Vector(); 
		
		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void done(float[] value, Completion completion, CBDescOut desc) {
//	TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (done) Value: " + value);
			responseQueue.add(new CBResponseFloatSeq(completion, desc, CBResponse.DONE_TYPE, value));
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
		public synchronized void working(float[] value, Completion completion, CBDescOut desc) {
// TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " Value: " + value);
			responseQueue.add(new CBResponseFloatSeq(completion, desc, CBResponse.WORKING_TYPE, value));
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
	class CBlongSeqImpl extends CBlongSeqPOA
	{
		/**
		 * Sync. response queue.
		 */
		protected Vector responseQueue = new Vector(); 
		
		/**
		 * @see alma.ACS.CBvoidOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)
		 */
		public synchronized void done(int[] value, Completion completion, CBDescOut desc) {
//	TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " (done) Value: " + value);
			responseQueue.add(new CBResponseLongSeq(completion, desc, CBResponse.DONE_TYPE, value));
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
		public synchronized void working(int[] value, Completion completion, CBDescOut desc) {
// TODO tmp
System.out.println(timeFormatter.format(new Date(UTCUtility.utcOmgToJava(completion.timeStamp))) + " Value: " + value);
			responseQueue.add(new CBResponseLongSeq(completion, desc, CBResponse.WORKING_TYPE, value));
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
	private ROdoubleSeq ROpropertyDoubleSeq = null;
	private RWdoubleSeq RWpropertyDoubleSeq = null;
	private ROlongSeq ROpropertyLongSeq = null;
	private RWlongSeq RWpropertyLongSeq = null;
	private ROfloatSeq ROpropertyFloatSeq = null;
	private RWfloatSeq RWpropertyFloatSeq = null;



	/**
	 * Property to be tested.
	 */
	private Manager manager = null;

	/**
	 * PowerSupply component to be tested.
	 */
	private static final String COMPONENT_NAME = "SEQTESTING"; 

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
			
		SeqComponent pc = SeqComponentHelper.narrow(obj);
		ROpropertyDoubleSeq = pc.doubleSeqRO();
		RWpropertyDoubleSeq = pc.doubleSeqRW();
		ROpropertyLongSeq = pc.longSeqRO();
		RWpropertyLongSeq = pc.longSeqRW();
		ROpropertyFloatSeq = pc.floatSeqRO();
		RWpropertyFloatSeq = pc.floatSeqRW();
		
		
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
		assertEquals("longRO", ROpropertyLongSeq.name());
		assertEquals(COMPONENT_NAME, ROpropertyLongSeq.characteristic_component_name());

		assertEquals("long RO", ROpropertyLongSeq.description());
		assertEquals("%9.3f", ROpropertyLongSeq.format());
		assertEquals("A", ROpropertyLongSeq.units());
		assertEquals(65535, ROpropertyLongSeq.resolution());

		assertEquals(10000, ROpropertyLongSeq.min_timer_trigger());
		assertEquals(0, ROpropertyLongSeq.default_value(), 0);
		
		assertEquals(1526, ROpropertyLongSeq.min_delta_trigger(), 0);
		assertEquals(100, ROpropertyLongSeq.graph_min(), 0);
		assertEquals(2000, ROpropertyLongSeq.graph_max(), 0);
		assertEquals(19, ROpropertyLongSeq.min_step(), 0);

		assertEquals(20, ROpropertyLongSeq.alarm_low_on(), 0);
		assertEquals(30, ROpropertyLongSeq.alarm_low_off(), 0);
		assertEquals(890, ROpropertyLongSeq.alarm_high_on(), 0);
		assertEquals(880, ROpropertyLongSeq.alarm_high_off(), 0);
			
	}
	
	
	
	public void testCharacteristicsLongRW() throws Throwable
	{
		
		// RW
		assertEquals(0, RWpropertyLongSeq.min_value(), 0);
		assertEquals(3000, RWpropertyLongSeq.max_value(), 0);

		assertEquals("longRW", RWpropertyLongSeq.name());
		assertEquals(COMPONENT_NAME, RWpropertyLongSeq.characteristic_component_name());

		assertEquals("long RW", RWpropertyLongSeq.description());
		assertEquals("%9.4f", RWpropertyLongSeq.format());
		assertEquals("A", RWpropertyLongSeq.units());
		assertEquals(65535, RWpropertyLongSeq.resolution());

		assertEquals(50000, RWpropertyLongSeq.min_timer_trigger());
		assertEquals(5, RWpropertyLongSeq.default_value(), 0);
		
		assertEquals(4, RWpropertyLongSeq.min_delta_trigger(), 0);
		assertEquals(10, RWpropertyLongSeq.graph_min(), 0);
		assertEquals(1000, RWpropertyLongSeq.graph_max(), 0);
		assertEquals(2, RWpropertyLongSeq.min_step(), 0);


			
	}

	//Double Property
	public void testCharacteristicsDoubleRO() throws Throwable
	{
		// RO

		assertEquals("doubleRO", ROpropertyDoubleSeq.name());
		assertEquals(COMPONENT_NAME, ROpropertyDoubleSeq.characteristic_component_name());

		assertEquals("double RO", ROpropertyDoubleSeq.description());
		assertEquals("%9.4f", ROpropertyDoubleSeq.format());
		assertEquals("A", ROpropertyDoubleSeq.units());
		assertEquals(65535, ROpropertyDoubleSeq.resolution());

		assertEquals(10000, ROpropertyDoubleSeq.min_timer_trigger());
		assertEquals(0.0, ROpropertyDoubleSeq.default_value(), 0.0);
		
		assertEquals(0.01526, ROpropertyDoubleSeq.min_delta_trigger(), 0.0);
		assertEquals(0.0, ROpropertyDoubleSeq.graph_min(), 0.0);
		assertEquals(1000.0, ROpropertyDoubleSeq.graph_max(), 0.0);
		assertEquals(0.01526, ROpropertyDoubleSeq.min_step(), 0.0);

		assertEquals(10.0, ROpropertyDoubleSeq.alarm_low_on(), 0.0);
		assertEquals(20.0, ROpropertyDoubleSeq.alarm_low_off(), 0.0);
		assertEquals(990.0, ROpropertyDoubleSeq.alarm_high_on(), 0.0);
		assertEquals(980.0, ROpropertyDoubleSeq.alarm_high_off(), 0.0);
	
	}
	
	public void testCharacteristicsDoubleRW() throws Throwable
	{
		// RW
		assertEquals(0.0, RWpropertyDoubleSeq.min_value(), 0.0);
		assertEquals(1000.0, RWpropertyDoubleSeq.max_value(), 0.0);

		assertEquals("doubleRW", RWpropertyDoubleSeq.name());
		assertEquals(COMPONENT_NAME, RWpropertyDoubleSeq.characteristic_component_name());

		assertEquals("double RW", RWpropertyDoubleSeq.description());
		assertEquals("%9.4f", RWpropertyDoubleSeq.format());
		assertEquals("A", RWpropertyDoubleSeq.units());
		assertEquals(65535, RWpropertyDoubleSeq.resolution());

		assertEquals(10000, RWpropertyDoubleSeq.min_timer_trigger());
		assertEquals(0.0, RWpropertyDoubleSeq.default_value(), 0.0);
		
		assertEquals(0.01526, RWpropertyDoubleSeq.min_delta_trigger(), 0.0);
		assertEquals(0.0, RWpropertyDoubleSeq.graph_min(), 0.0);
		assertEquals(1000.0, RWpropertyDoubleSeq.graph_max(), 0.0);
		assertEquals(0.01526, RWpropertyDoubleSeq.min_step(), 0.0);

	
	}

	//Float Property
	public void testCharacteristicsFloatRO() throws Throwable
	{
		//RO
		assertEquals("floatRO", ROpropertyFloatSeq.name());
		assertEquals(COMPONENT_NAME, ROpropertyFloatSeq.characteristic_component_name());

		assertEquals("float RO", ROpropertyFloatSeq.description());
		assertEquals("%9.4f", ROpropertyFloatSeq.format());
		assertEquals("A", ROpropertyFloatSeq.units());
		assertEquals(65535, ROpropertyFloatSeq.resolution());

		assertEquals(10000, ROpropertyFloatSeq.min_timer_trigger());
		assertEquals(0.0, ROpropertyFloatSeq.default_value(), 0.00001);
		
		assertEquals(0.01526, ROpropertyFloatSeq.min_delta_trigger(), 0.00001);
		assertEquals(0.0, ROpropertyFloatSeq.graph_min(), 0.00001);
		assertEquals(1000.0, ROpropertyFloatSeq.graph_max(), 0.00001);
		assertEquals(0.01526, ROpropertyFloatSeq.min_step(), 0.00001);

		assertEquals(10.0, ROpropertyFloatSeq.alarm_low_on(), 0.00001);
		assertEquals(20.0, ROpropertyFloatSeq.alarm_low_off(), 0.00001);
		assertEquals(990.0, ROpropertyFloatSeq.alarm_high_on(), 0.00001);
		assertEquals(980.0, ROpropertyFloatSeq.alarm_high_off(), 0.00001);
	
	}
	
	public void testCharacteristicsFloatRW() throws Throwable
	{
		// RW
		assertEquals(0.0, RWpropertyFloatSeq.min_value(), 0.00001);
		assertEquals(1000.0, RWpropertyFloatSeq.max_value(), 0.00001);

		assertEquals("floatRW", RWpropertyFloatSeq.name());
		assertEquals(COMPONENT_NAME, RWpropertyFloatSeq.characteristic_component_name());

		assertEquals("float RW", RWpropertyFloatSeq.description());
		assertEquals("%9.4f", RWpropertyFloatSeq.format());
		assertEquals("A", RWpropertyFloatSeq.units());
		assertEquals(65535, RWpropertyFloatSeq.resolution());

		assertEquals(10000, RWpropertyFloatSeq.min_timer_trigger());
		assertEquals(0.0, RWpropertyFloatSeq.default_value(), 0.00001);
		

		assertEquals(0.01526, RWpropertyFloatSeq.min_delta_trigger(), 0.00001);
		assertEquals(0.0, RWpropertyFloatSeq.graph_min(), 0.00001);
		assertEquals(1000.0, RWpropertyFloatSeq.graph_max(), 0.00001);
		assertEquals(0.01526, RWpropertyFloatSeq.min_step(), 0.00001);

	
	}
	
	
	public void testNewSubscriptionAlarm() {
		try
		{
			
			CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
			ROpropertyDoubleSeq.new_subscription_Alarm(null, descIn);
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
		ROpropertyLongSeq.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);
		
		//RWlong
		completionHolder = new CompletionHolder();
		// TODO check value
		RWpropertyLongSeq.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);
		
		//RODouble
		completionHolder = new CompletionHolder();
		// TODO check value
		ROpropertyDoubleSeq.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);	
		
		//RWDouble
		completionHolder = new CompletionHolder();
		// TODO check value
		RWpropertyDoubleSeq.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);	
		
		//ROFloat
		completionHolder = new CompletionHolder();
		// TODO check value
		ROpropertyFloatSeq.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);	
		
		//RWFloat
		completionHolder = new CompletionHolder();
		// TODO check value
		RWpropertyFloatSeq.get_sync(completionHolder);
		//assertEquals(property.default_value(), value, 0.0);
		assertEquals(0, completionHolder.value.code);
		assertEquals(0, completionHolder.value.type);
		assertEquals(0, completionHolder.value.previousError.length);	
		// less than 5		
		assertTrue((UTCUtility.utcJavaToOmg(System.currentTimeMillis())-completionHolder.value.timeStamp)<50000000);	
		
		
	}

	public void testGetAsyncDoubleRO() {
		
		
		CBdoubleSeqImpl cb = new CBdoubleSeqImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			ROpropertyDoubleSeq.get_async(cb._this(orb), descIn);
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
		
		
		CBdoubleSeqImpl cb = new CBdoubleSeqImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			RWpropertyDoubleSeq.get_async(cb._this(orb), descIn);
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
		
		
		CBlongSeqImpl cb = new CBlongSeqImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			ROpropertyLongSeq.get_async(cb._this(orb), descIn);
			try
			{
				// wait for 5s
				cb.wait(5000);
			}
			catch (InterruptedException ie) {}
		}
			
		// only 1 response is expected
		assertEquals(1, cb.getResponseQueue().size());
		CBResponseLongSeq response = (CBResponseLongSeq)cb.getResponseQueue().firstElement();
		
		// check reponse type
		assertEquals(CBResponseLongSeq.DONE_TYPE, response.type);
		
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
		
		
		CBlongSeqImpl cb = new CBlongSeqImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
		synchronized(cb)
		{
			RWpropertyLongSeq.get_async(cb._this(orb), descIn);
			try
			{
				// wait for 5s
				cb.wait(5000);
			}
			catch (InterruptedException ie) {}
		}
			
		// only 1 response is expected
		assertEquals(1, cb.getResponseQueue().size());
		CBResponseLongSeq response = (CBResponseLongSeq)cb.getResponseQueue().firstElement();
		
		// check reponse type
		assertEquals(CBResponseLongSeq.DONE_TYPE, response.type);
		
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
	
	
	CBfloatSeqImpl cb = new CBfloatSeqImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
	synchronized(cb)
	{
		ROpropertyFloatSeq.get_async(cb._this(orb), descIn);
		try
		{
			// wait for 5s
			cb.wait(5000);
		}
		catch (InterruptedException ie) {}
	}
		
	// only 1 response is expected
	assertEquals(1, cb.getResponseQueue().size());
	CBResponseFloatSeq response = (CBResponseFloatSeq)cb.getResponseQueue().firstElement();
	
	// check reponse type
	assertEquals(CBResponseFloatSeq.DONE_TYPE, response.type);
	
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
	
	
	CBfloatSeqImpl cb = new CBfloatSeqImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
	synchronized(cb)
	{
		RWpropertyFloatSeq.get_async(cb._this(orb), descIn);
		try
		{
			// wait for 5s
			cb.wait(5000);
		}
		catch (InterruptedException ie) {}
	}
		
	// only 1 response is expected
	assertEquals(1, cb.getResponseQueue().size());
	CBResponseFloatSeq response = (CBResponseFloatSeq)cb.getResponseQueue().firstElement();
	
	// check reponse type
	assertEquals(CBResponseFloatSeq.DONE_TYPE, response.type);
	
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
			RWpropertyLongSeq.set_async(new int[]{500,600},cb._this(orb), descIn);
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
		int[] value = RWpropertyLongSeq.get_sync(completionHolder);
		assertEquals(500, value[0], 0);
		assertEquals(600, value[1], 0);
		
		
	}

public void testSetAsyncDoubleRW() {
	
	
	CBvoidImpl cb = new CBvoidImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
	synchronized(cb)
	{
		RWpropertyDoubleSeq.set_async(new double[]{500.0,600.0},cb._this(orb), descIn);
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
	double[] value = RWpropertyDoubleSeq.get_sync(completionHolder);
	assertEquals(500.0, value[0], 0);
	assertEquals(600.0, value[1], 0);
	
	
}

public void testSetAsyncFloatRW() {
	
	
	CBvoidImpl cb = new CBvoidImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);
	synchronized(cb)
	{
		RWpropertyFloatSeq.set_async( new float[]{(float)500.0,(float)600.0},cb._this(orb), descIn);
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
	float[] value = RWpropertyFloatSeq.get_sync(completionHolder);
	assertEquals(500.0, value[0], 0.001);
	assertEquals(600.0, value[1], 0.001);
	
	
}



	public void testGetHistoryDouble() {
		
		// wait until history fills
		try
		{
			// 7 sec
			Thread.sleep(7000);
		}
		catch (InterruptedException ie) {}

		doubleSeqSeqHolder dsh = new doubleSeqSeqHolder();
		TimeSeqHolder tsh = new TimeSeqHolder();
		int len = ROpropertyDoubleSeq.get_history(5, dsh, tsh);
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

		longSeqSeqHolder lsh = new longSeqSeqHolder();
		TimeSeqHolder tsh = new TimeSeqHolder();
		int len = ROpropertyLongSeq.get_history(5, lsh, tsh);
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

		floatSeqSeqHolder fsh = new floatSeqSeqHolder();
		TimeSeqHolder tsh = new TimeSeqHolder();
		int len = ROpropertyFloatSeq.get_history(5, fsh, tsh);
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
		CBlongSeqImpl cb = new CBlongSeqImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitorlong monitor = ROpropertyLongSeq.create_monitor(cb._this(orb), descIn);
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
		CBdoubleSeqImpl cb = new CBdoubleSeqImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitordouble monitor = ROpropertyDoubleSeq.create_monitor(cb._this(orb), descIn);
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
	CBfloatSeqImpl cb = new CBfloatSeqImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

	Monitorfloat monitor = ROpropertyFloatSeq.create_monitor(cb._this(orb), descIn);
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
		CBlongSeqImpl cb = new CBlongSeqImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitorlong monitor = ROpropertyLongSeq
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
		ROpropertyLongSeq.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// disable test
		monitor.set_value_trigger(0, false);

		ROpropertyLongSeq.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		monitor.set_value_trigger(0, true);

		ROpropertyLongSeq.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		System.out.println("------");

		// disable test
		monitor.suspend();

		ROpropertyLongSeq.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// / ... this should revive it
		monitor.resume();

		System.out.println("------");

		ROpropertyLongSeq.alarm_high_on();

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
		CBdoubleSeqImpl cb = new CBdoubleSeqImpl();
		CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

		Monitordouble monitor = ROpropertyDoubleSeq
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
		ROpropertyDoubleSeq.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// disable test
		monitor.set_value_trigger(0, false);

		ROpropertyDoubleSeq.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		monitor.set_value_trigger(0, true);

		ROpropertyDoubleSeq.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		System.out.println("------");

		// disable test
		monitor.suspend();

		ROpropertyDoubleSeq.alarm_high_on();

		try {
			Thread.sleep(3000);
		} catch (InterruptedException ie) {
		}

		// / ... this should revive it
		monitor.resume();

		System.out.println("------");

		ROpropertyDoubleSeq.alarm_high_on();

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
	CBfloatSeqImpl cb = new CBfloatSeqImpl();
	CBDescIn descIn = new CBDescIn(50000, 50000, 1208);

	Monitorfloat monitor = ROpropertyFloatSeq
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
	ROpropertyFloatSeq.alarm_high_on();

	try {
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	// disable test
	monitor.set_value_trigger(0, false);

	ROpropertyFloatSeq.alarm_high_on();

	try {
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	monitor.set_value_trigger(0, true);

	ROpropertyFloatSeq.alarm_high_on();

	try {
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	System.out.println("------");

	// disable test
	monitor.suspend();

	ROpropertyFloatSeq.alarm_high_on();

	try {
		Thread.sleep(3000);
	} catch (InterruptedException ie) {
	}

	// / ... this should revive it
	monitor.resume();

	System.out.println("------");

	ROpropertyFloatSeq.alarm_high_on();

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
			ROpropertyLongSeq.create_postponed_monitor(0, null, descIn);
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
			ROpropertyLongSeq.get_characteristic_by_name("format");
			RWpropertyLongSeq.get_characteristic_by_name("format");
			ROpropertyDoubleSeq.get_characteristic_by_name("format");
			RWpropertyDoubleSeq.get_characteristic_by_name("format");
			ROpropertyFloatSeq.get_characteristic_by_name("format");
			RWpropertyFloatSeq.get_characteristic_by_name("format");
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
		
	}

	public void testFindCharacteristic() {
		
		try
		{
			ROpropertyLongSeq.find_characteristic("format");
			RWpropertyLongSeq.find_characteristic("format");
			ROpropertyDoubleSeq.find_characteristic("format");
			RWpropertyDoubleSeq.find_characteristic("format");
			ROpropertyFloatSeq.find_characteristic("format");
			RWpropertyFloatSeq.find_characteristic("format");
		}
		catch (NO_IMPLEMENT ex)
		{
			// OK
		}
		
	}

	public void testGetAllCharacteristics() {
		
		try
		{
			ROpropertyLongSeq.get_all_characteristics();
			RWpropertyLongSeq.get_all_characteristics();
			ROpropertyDoubleSeq.get_all_characteristics();
			RWpropertyDoubleSeq.get_all_characteristics();
			ROpropertyFloatSeq.get_all_characteristics();
			RWpropertyFloatSeq.get_all_characteristics();
			
			
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
