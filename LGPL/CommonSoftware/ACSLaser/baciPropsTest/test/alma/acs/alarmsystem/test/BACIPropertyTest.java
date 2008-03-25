package alma.acs.alarmsystem.test;

import alma.acs.component.client.ComponentClientTestCase;

/**
 * A class to test if baci sends BACI property alarms.
 * 
 * the test is performed by getting TEST_COMPONENT component and executing
 * CORBA calls to change the value of its property. 
 * The alarms received are written in the stdout and checked by tat
 * 
 * @author acaproni
 *
 */
public class BACIPropertyTest extends ComponentClientTestCase {
	
	/**
	 * Constructor
	 */
	public BACIPropertyTest() throws Exception {
		super("BACIPropertyTest");
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * Placeholder
	 * @throws Exception
	 */
	public void testEmpty() throws Exception {
		
	}

}
