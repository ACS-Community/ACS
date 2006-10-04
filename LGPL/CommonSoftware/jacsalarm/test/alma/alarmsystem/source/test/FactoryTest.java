package alma.alarmsystem.source.test;

import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

import si.ijs.maci.Manager;
import com.cosylab.CDB.JDAL;

public class FactoryTest extends junit.framework.TestCase {
	
	
			
	// The current directory
	String curDir=System.getProperty("user.dir");
	
	// The reference to the DAL (to make a clear cache)
	JDAL jdal; 
	
	public FactoryTest(String str) {
		super(str);
		// Get the reference to the DAL
		jdal = TestUtil.getDAL(TestUtil.getManager());
	}
	
	/**
	 * This execute for each test and we want to have
	 * a cache with some logs 
	 * 
	 * @see junit.framework.TestCase
	 */ 
	protected void setUp() throws Exception
	{ 
	}
	
	/** 
	 * @see junit.framework.TestCase
	 *
	 */
	protected void tearDown() {
		ACSAlarmSystemInterfaceFactory.done();
	}
	
	/**
	 * Check if the ACS implementation of the AS is choosen when
	 * there is no Alarm branch in the CDB
	 * 
	 * @throws Exception
	 */
	public void testNoALarmBranch() throws Exception {
		TestUtil.deleteAlarmBranch(curDir);
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(TestUtil.getORB(),TestUtil.getManager(),0,TestUtil.getLogger(this.getClass().getName()));
		assertTrue("Wrong implementation in use (no Alarms in CDB case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Check if the ACS implementation of the AS is choosen when
	 * there ACS is in the CDB
	 * 
	 * @throws Exception
	 */
	public void testACSAS() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"ACS");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(TestUtil.getORB(),TestUtil.getManager(),0,TestUtil.getLogger(this.getClass().getName()));
		assertTrue("Wrong implementation in use (ACS case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Check if the CERN implementation of the AS is choosen when
	 * there CERN is in the CDB
	 * 
	 * @throws Exception
	 */
	public void testCERNAS() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"CERN");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(TestUtil.getORB(),TestUtil.getManager(),0,TestUtil.getLogger(this.getClass().getName()));
		assertFalse("Wrong implementation in use (CERN case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Check if the ACS implementation is used when the Implementation property is wrong
	 * @throws Exception
	 */
	public void testWrongImplementationProp() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"Wrong property");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(TestUtil.getORB(),TestUtil.getManager(),0,TestUtil.getLogger(this.getClass().getName()));
		assertTrue("Wrong implementation in use (wrong prop case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Test the creation of a FaultState
	 * 
	 * @throws Exception
	 */
	public void testFaultStateCreation() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"ACS");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(TestUtil.getORB(),TestUtil.getManager(),0,TestUtil.getLogger(this.getClass().getName()));
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState("Family","Member",0);
		assertNotNull("Error creating a FS",fs);
	}
	
	/**
	 * Test the creation of a source (proxy)
	 * 
	 * @throws Exception
	 */
	public void testAlarmSourceCreation() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"ACS");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(TestUtil.getORB(),TestUtil.getManager(),0,TestUtil.getLogger(this.getClass().getName()));
		ACSAlarmSystemInterface proxy = ACSAlarmSystemInterfaceFactory.createSource("SourceName");
		assertNotNull("Error creating an alarm source",proxy);
	}
	
}
