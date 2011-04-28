/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */
package alma.acs.alarmsystem.test.alarmsource;

import alma.acs.alarmsystem.source.AlarmSource;
import alma.acs.alarmsystem.source.AlarmsMap;
import alma.acs.component.client.ComponentClientTestCase;

public class AlarmsMapTest extends ComponentClientTestCase {
	
	/**
	 * The object to test
	 */
	private AlarmsMap alarmsMap;
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public AlarmsMapTest() throws Exception {
		super(AlarmsMapTest.class.getName());
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		alarmsMap=new AlarmsMap();
		assertNotNull(alarmsMap);
	}

	@Override
	protected void tearDown() throws Exception {
		alarmsMap.close();
		super.tearDown();
	}
	
	/**
	 * Test the clearing of alarms
	 */
	public void testClearMap() throws Exception {
		String keyBase="ClearListFF:ClearListFM:";
		for (int t=0; t<10; t++) {
			alarmsMap.raise(keyBase+t);
		}
		assertEquals(10, alarmsMap.size());
		alarmsMap.clear();
		assertEquals(0, alarmsMap.size());
	}
	
	/**
	 * Check if the map removes the items older then 
	 * {@link AlarmsMap#ALARM_ACTIVITY_TIME}
	 * 
	 * @throws Exception
	 */
	public void testAutoRemoval() throws Exception {
		// First test is simply done by adding elements and waiting 
		// enough time to see if they are removed
		String keyBase="RemovalFF:RemovalFM:";
		for (int t=0; t<10; t++) {
			alarmsMap.raise(keyBase+t);
		}
		assertEquals(10, alarmsMap.size());
		Thread.sleep(AlarmsMap.ALARM_ACTIVITY_TIME+1000);
		assertEquals(0, alarmsMap.size());
		// Now check if the timer selectively removes items
		for (int t=0; t<10; t++) {
			alarmsMap.raise(keyBase+t);
		}
		assertEquals(10, alarmsMap.size());
		Thread.sleep(AlarmsMap.ALARM_ACTIVITY_TIME/2+1000);
		assertEquals(10, alarmsMap.size());
		for (int t=10; t<20; t++) {
			alarmsMap.raise(keyBase+t);
		}
		assertEquals(20, alarmsMap.size());
		Thread.sleep(AlarmsMap.ALARM_ACTIVITY_TIME/2+1000);
		assertEquals(10, alarmsMap.size());
		Thread.sleep(AlarmsMap.ALARM_ACTIVITY_TIME/2+1000);
		assertEquals(0, alarmsMap.size());
	}
	
	/**
	 * Test the raising of alarms.
	 * <P>
	 * All the keys are different 
	 * 
	 * @throws Exception
	 */
	public void testRaise() throws Exception{
		alarmsMap.clear();
		// Raise one alarm and check if it is present in the list
		String keyBase="RaiseFF:RaiseFM:";
		alarmsMap.raise(keyBase+0);
		assertTrue("Key not in the map", alarmsMap.containsKey(keyBase+0));
		assertEquals(1, alarmsMap.size());
		// Adds 10 more keys
		for (int t=1; t<=10; t++) {
			alarmsMap.raise(keyBase+t);
		}
		for (int t=0; t<=10; t++) {
			assertTrue("Key not in the map", alarmsMap.containsKey(keyBase+t));
		}
		assertEquals(11, alarmsMap.size());
	}
	
	/**
	 * Test the clearing of alarms.
	 * <P>
	 * All the keys are different.
	 * 
	 * @throws Exception
	 */
	public void testClear() throws Exception {
		alarmsMap.clear();
		// Clear one alarm and check if it is present in the list
		String keyBase="ClearFF:ClearFM:";
		alarmsMap.clear(keyBase+0);
		assertTrue("Key not in the map", alarmsMap.containsKey(keyBase+0));
		assertEquals(1, alarmsMap.size());
		// Adds 10 more keys
		for (int t=1; t<=10; t++) {
			alarmsMap.clear(keyBase+t);
		}
		for (int t=0; t<=10; t++) {
			assertTrue("Key not in the map", alarmsMap.containsKey(keyBase+t));
		}
		assertEquals(11, alarmsMap.size());
	}

}
