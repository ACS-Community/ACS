package alma.alarmsystem.clients.test;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.clients.CategoryClient;

public class CategoryClientChildren extends ComponentClientTestCase implements AlarmSelectionListener {
	/**
	 *  The categoryClient to test
	 */
	private CategoryClient categoryClient;
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public CategoryClientChildren() throws Exception {
		super(CategoryClientThreshold.class.getName());
	}
	
	/**
	 * @see extends ComponentClientTestCase
	 */
	public void setUp() throws Exception {
		super.setUp();
		
		categoryClient = new CategoryClient(getContainerServices());
		assertNotNull(categoryClient);
	}
	
	/**
	 * @see extends ComponentClientTestCase
	 */
	public void teraDown() throws Exception {
		categoryClient.close();
		super.tearDown();
	}

	/**
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onAlarm(cern.laser.client.data.Alarm)
	 */
	@Override
	public void onAlarm(Alarm alarm) {	}

	/**
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onException(cern.laser.client.services.selection.LaserSelectionException)
	 */
	@Override
	public void onException(LaserSelectionException e) {}
	
	/**
	 * Test getParents for NODE and MULTIPLICITY reduction
	 * 
	 * @throws Exception
	 */
	public void testGetNodeChildren() throws Exception {
		categoryClient.connect(this);
		
		// Multiplicity
		alma.alarmsystem.Alarm[] alarms = categoryClient.getChildren("TEST:MCAUSE:1", false);
		assertEquals(5, alarms.length);
		
		alarms = categoryClient.getChildren("TEST:MULTI3:2", false);
		assertEquals(0, alarms.length);
		
		// Node
		alarms= categoryClient.getChildren("TEST:NODE1:1", true);
		assertEquals(1, alarms.length);
		assertEquals("TEST:NODE2:1", alarms[0].alarmId);
		
		alarms= categoryClient.getChildren("TEST:NODE2:1", true);
		assertEquals(1, alarms.length);
		assertEquals("TEST:NODE3:1", alarms[0].alarmId);
		
		alarms= categoryClient.getChildren("TEST:NODE3:1", true);
		assertEquals(0, alarms.length);
	}
}
