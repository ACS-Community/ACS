/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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
	public void tearDown() throws Exception {
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
		Alarm[] alarms = categoryClient.getChildren("TEST:MCAUSE:1", false);
		assertEquals(5, alarms.length);
		
		alarms = categoryClient.getChildren("TEST:MULTI3:2", false);
		assertEquals(0, alarms.length);
		
		// Node
		alarms= categoryClient.getChildren("TEST:NODE1:1", true);
		assertEquals(1, alarms.length);
		assertEquals("TEST:NODE2:1", alarms[0].getAlarmId());
		
		alarms= categoryClient.getChildren("TEST:NODE2:1", true);
		assertEquals(1, alarms.length);
		assertEquals("TEST:NODE3:1", alarms[0].getAlarmId());
		
		alarms= categoryClient.getChildren("TEST:NODE3:1", true);
		assertEquals(0, alarms.length);
	}
}
