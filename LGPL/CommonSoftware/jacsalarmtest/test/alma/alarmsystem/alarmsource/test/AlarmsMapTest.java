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
package alma.alarmsystem.alarmsource.test;

import java.util.Collection;
import java.util.Date;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Logger;

import alma.acs.alarmsystem.source.AlarmsMap;
import alma.acs.alarmsystem.source.AlarmsMap.AlarmInfo;
import alma.acs.component.client.ComponentClientTestCase;

/**
 * The test for the {@link AlarmsMap}.
 * <P>
 * The test is done entirely by junit by
 * means of assert.
 * <P>
 * <B>Note</B>: this test create and test an instrumented {@link AlarmsMap},
 * i.e. a {@link AlarmsMapWithDump}.
 * The {@link ComponentClientTestCase} initialize the alarm system
 * and during the initialization, it creates a {@link AlarmsMap} too, but this
 * is not the instance that this test checks.
 * 
 * @author acaproni
 *
 */
public class AlarmsMapTest extends ComponentClientTestCase {
	
	/**
	 * Extends {@link AlarmsMap} with the dump method 
	 * for debugging
	 * 
	 * @author acaproni
	 *
	 */
	private class AlarmsMapWithDump extends  AlarmsMap {
		/**
		 * Constructor
		 * 
		 * @param threadFactory
		 * @param logger
		 */
		public AlarmsMapWithDump(ThreadFactory threadFactory, Logger logger) {
			super(threadFactory, logger);
		}
		
		public StringBuilder dump() {
			StringBuilder ret = new StringBuilder("\nAlarmsMap content:\n");
			for (String key: alarmsMap.keySet()) {
				ret.append(key);
				ret.append('\n');
			}
			return ret;
		}
	}
	
	/**
	 * The object to test
	 */
	private AlarmsMapWithDump alarmsMap;
	
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
		alarmsMap=new AlarmsMapWithDump(this.getContainerServices().getThreadFactory(),this.m_logger);
		assertNotNull(alarmsMap);
		alarmsMap.start();
	}

	@Override
	protected void tearDown() throws Exception {
		alarmsMap.shutdown();
		alarmsMap=null;
		super.tearDown();
	}
	
	/**
	 * Check if the map removes the items older then 
	 * {@link AlarmsMap#ALARM_ACTIVITY_TIME}
	 * 
	 * @throws Exception
	 */
	public void testAutoRemoval() throws Exception {
		System.out.println("testAutoRemoval");
		// First test is simply done by adding elements and waiting 
		// enough time to see if they are removed
		String keyBase="RemovalFF:RemovalFM:";
		for (int t=0; t<10; t++) {
			alarmsMap.raise(keyBase+t);
		}
		Date d = new Date(System.currentTimeMillis());
		assertEquals(alarmsMap.dump().toString(),10, alarmsMap.size());
		// This sleep must be so long otherwise there is a unlucky case
		// that some alarms are still in the queue
		Thread.sleep(2*AlarmsMap.ALARM_ACTIVITY_TIME*1000+1000);
		d = new Date(System.currentTimeMillis());
		assertEquals(d.toString()+"The map should be empty at this time....",0, alarmsMap.size());
		// Now check if the timer selectively removes items
		for (int t=0; t<10; t++) {
			alarmsMap.raise(keyBase+t);
		}
		assertEquals(alarmsMap.dump().toString(),10, alarmsMap.size());
		Thread.sleep(AlarmsMap.ALARM_ACTIVITY_TIME*1000/2+1000);
		assertEquals(alarmsMap.dump().toString(),10, alarmsMap.size());
		for (int t=10; t<20; t++) {
			alarmsMap.raise(keyBase+t);
		}
		assertEquals(alarmsMap.dump().toString(),20, alarmsMap.size());
		Thread.sleep(2*AlarmsMap.ALARM_ACTIVITY_TIME*1000+3000);
		assertEquals(alarmsMap.dump().toString(),0, alarmsMap.size());
		System.out.println("testAutoRemoval done");
	}
	
	/**
	 * Test the raising of alarms.
	 * <P>
	 * All the keys are different 
	 * 
	 * @throws Exception
	 */
	public void testRaise() throws Exception{
		System.out.println("testRaise");
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
		System.out.println("testRaise done");
	}
	
	/**
	 * Test the clearing of alarms.
	 * <P>
	 * All the keys are different.
	 * 
	 * @throws Exception
	 */
	public void testClear() throws Exception {
		System.out.println("testClear");
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
		System.out.println("testClear done");
	}
	
	/**
	 * Activate and deactivate an alarm to check if its state
	 * has been updated
	 * @throws Exception
	 */
	public void testAlarmUpdate() throws Exception {
		System.out.println("testAlarmUpdate");
		// Clear one alarm and check if it is present in the list
		String keyBase="UpdateFF:UpdateFM:1";
		alarmsMap.clear(keyBase);
		assertEquals(1, alarmsMap.size());
		AlarmInfo info=alarmsMap.get(keyBase);
		assertNotNull(info);
		assertFalse("The alarm shoud not be active",info.active);
		Thread.sleep(500);
		// Set the alarm
		alarmsMap.raise(keyBase);
		assertEquals(1, alarmsMap.size());
		AlarmInfo info2=alarmsMap.get(keyBase);
		assertNotNull(info2);
		assertTrue("The alarm shoud be active",info2.active);
		assertTrue("time not updated", info.time!=info2.time);
		System.out.println("testAlarmUpdate done");
	}
	
	/**
	 * Check if the alarms raised/cleared are correctly replaced
	 */
	public void testAlarmReplacement() {
		System.out.println("testAlarmReplacement");
		// Clear one alarm and check if it is present in the list
		String keyBase="ReplaceFF:replaceFM:";
		// Adds 10 alarms
		for (int t=1; t<=10; t++) {
			alarmsMap.raise(keyBase+t);
		}
		assertEquals(10, alarmsMap.size());
		// Clear 5 alarms
		for (int t=2; t<=10; t+=2) {
			alarmsMap.clear(keyBase+t);
		}
		assertEquals(10, alarmsMap.size());
		Collection<String> actives=alarmsMap.getActiveAlarms();
		for (String active: actives) {
			assertTrue(alarmsMap.get(active).active);
		}
		System.out.println("testAlarmReplacement done");
	}

}
