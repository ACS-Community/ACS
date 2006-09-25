package alma.alarmsystem.source.test;

import alma.alarmsystem.source.ACSFaultStateImpl;

import java.util.Random;
import java.util.Calendar;

public class FaultStateTest extends junit.framework.TestCase {
	
	String FF; // FaultFamily
	String FM; // FaultState
	int FC; // faultCode
	String descriptor; // Descriptor

	public FaultStateTest(String str) {
		super(str);
	}
	
	/**
	 * This execute for each test and we want to have
	 * a cache with some logs 
	 * 
	 * @see junit.framework.TestCase
	 */ 
	protected void setUp() throws Exception
	{ 
		FF = "TestFaultFamily";
		FM = "MemberFailing";
		Calendar cal = Calendar.getInstance();
		Random rnd = new Random(cal.getTimeInMillis());
		FC = rnd.nextInt();
		
		descriptor = "The description of the fault";
	}
	
	/** 
	 * @see junit.framework.TestCase
	 *
	 */
	protected void tearDown() {
	}
	
	/**
	 * Test the empty constructor.
	 * 1. Build a FS with the empty constructor
	 * 2. Assign FF, FM, FC
	 * 3. Check the value of FM, FS, FC
	 * 
	 * @throws Exception
	 */
	public void testEmptyConstructor() throws Exception {
		ACSFaultStateImpl fs = new ACSFaultStateImpl();
		assertNotNull("Error creating the ACSFaultStateImpl",fs);
		
		fs.setFamily(FF);
		fs.setMember(FM);
		fs.setCode(FC);
		
		assertEquals("FF set/get error",fs.getFamily(),FF);
		assertEquals("FM set/get error",fs.getMember(),FM);
		assertEquals("FC set/get error",fs.getCode(),FC);
	}

	/**
	 * Check the FS created with the non-empty constructor
	 * 1. Build a FS with a FM, FS, FC
	 * 2. Check the value of FM, FS, FC
	 * 
	 * @throws Exception
	 */
	public void testConstructor() throws Exception {
		ACSFaultStateImpl fs = new ACSFaultStateImpl(FF,FM,FC);
		assertNotNull("Error creating the ACSFaultStateImpl",fs);
		
		assertEquals("FF set/get error",fs.getFamily(),FF);
		assertEquals("FM set/get error",fs.getMember(),FM);
		assertEquals("FC set/get error",fs.getCode(),FC);
	}
	
	/**
	 * Check the descriptor
	 * 1. Create a FS
	 * 2. Assign a value to the descriptor
	 * 3. Check the value of the descriptor
	 *
	 * @throws Exception
	 */
	public void testDescriptor() throws Exception {
		ACSFaultStateImpl fs = new ACSFaultStateImpl(FF,FM,FC);
		assertNotNull("Error creating the ACSFaultStateImpl",fs);
		
		fs.setDescriptor(descriptor);
		
		assertEquals("Descriptor set/get error",fs.getDescriptor(),descriptor);
	}
}
