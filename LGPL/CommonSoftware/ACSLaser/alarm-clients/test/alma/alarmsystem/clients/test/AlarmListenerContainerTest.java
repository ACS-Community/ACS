/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.alarmsystem.clients.test;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import junit.framework.TestCase;
import alma.alarmsystem.clients.alarm.AlarmFilter;
import alma.alarmsystem.clients.alarm.AlarmListenersContainer;
import alma.alarmsystem.clients.alarm.AlarmCategoryStatListener;
import alma.alarmsystem.clients.alarm.AlarmListenersContainer.AlarmListener;
import alma.alarmsystem.clients.alarm.AlarmStatistics;
import alma.alarmsystem.clients.test.utils.AlarmForTesting;

/**
 * Test if the filtering of alarms by their triplets work.
 * 
 * @see AlarmFilter
 * @author acaproni
 * @since ACS-12.2
 */
public class AlarmListenerContainerTest  extends TestCase {
	
	/**
	 * Alarm listener for this test
	 * 
	 * @author acaproni
	 * @since ACS-12.2
	 *
	 */
	private class AlarmListenerForTesting implements AlarmSelectionListener {
		
		public int numAlarms=0;
		public int numExceptions=0;
		
		/**
		 * A name associated to the listener so that we can distinguish
		 * wich listener prints messages
		 */
		public final String name;
		
		/**
		 * Constructor
		 * 
		 * @param name The name of the listener
		 */
		public AlarmListenerForTesting(String name) {
			this.name=name;
		}

		@Override
		public void onAlarm(Alarm alarm) {
			numAlarms++;
			System.out.println(name+": alarm received ["+alarm.getAlarmId()+"]");
		}

		@Override
		public void onException(LaserSelectionException e) {
			numExceptions++;
			System.out.println(name+": exception received. Code is "+e.getCode());
		}
		
	}
	
	/**
	 * Alarm statistics listener for this test
	 * 
	 * @author acaproni
	 * @since ACS-12.2
	 *
	 */
	private class AlrmStatListener implements AlarmCategoryStatListener {
		
		public int numActivationNoifies=0;
		public int numReductionNofies=0;

		@Override
		public void activationAlarmsStats(Integer active, Integer priority1,
				Integer priority2, Integer priority3, Integer priority4) {
			numActivationNoifies++;
		}

		@Override
		public void reductionAlarmsStat(Integer reduced, Integer masked,
				Integer multiplicityParent, Integer nodeParent,
				Integer multiplicityChild, Integer nodeChield) {
			numReductionNofies++;
		}
		
	}
	
	/**
	 * The container to test
	 */
	private AlarmListenersContainer container;

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		container = new AlarmListenersContainer();
		assertNotNull(container);
	}
	
	@Override
	protected void tearDown() throws Exception {
		container.close();
		super.tearDown();
	}
	
	/**
	 * Test add.remove alarm listeners
	 * 
	 * @throws Exception
	 */
	public void testAddRemoveAlarmListeners() throws Exception {
		assertEquals(0, container.getStatListenersSize());
		assertEquals(0, container.getAlarmListenersSize());
		
		AlarmListenerForTesting alarmL1 = new AlarmListenerForTesting("AlarmListenerName");
		
		AlarmListener listener1=container.addAlarmListener(alarmL1);
		assertNotNull(listener1);
		assertEquals(1, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
		
		AlarmListenerForTesting alarmL2 = new AlarmListenerForTesting("AlarmListenerName");
		AlarmFilter filter= new AlarmFilter("TheFF", ".*FM.*", -1,9999);
		AlarmListener listener2=container.addAlarmListener(alarmL2, filter);
		assertNotNull(listener2);
		assertEquals(2, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
		
		AlarmFilter filter2= new AlarmFilter("TheFF2", "FM", -1,9999);
		AlarmListenerForTesting alarmL3 = new AlarmListenerForTesting("AlarmListenerName");
		AlarmListener listener3 = container.new AlarmListener(alarmL3, filter2);
		assertTrue(container.addAlarmListener(listener3));
		assertEquals(3, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
		
		assertTrue(container.removeAlarmListener(listener2));
		assertEquals(2, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
		
		assertTrue(container.removeAlarmListener(listener1));
		assertEquals(1, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
		
		AlarmListenerForTesting alarmL4 = new AlarmListenerForTesting("AlarmListenerName");
		AlarmListener notExistentListener= container.new AlarmListener(alarmL4, null);
		assertFalse(container.removeAlarmListener(notExistentListener));
		assertEquals(1, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
		
		assertTrue(container.removeAlarmListener(listener3));
		assertEquals(0, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
	}
	
	/**
	 * Test add.remove statistics listeners
	 * 
	 * @throws Exception
	 */
	public void testAddRemoveStatListeners() throws Exception {
		assertEquals(0, container.getStatListenersSize());
		assertEquals(0, container.getAlarmListenersSize());
		
		AlrmStatListener stat1 = new AlrmStatListener();
		assertTrue(container.addStatsListener(stat1));
		assertEquals(0, container.getAlarmListenersSize());
		assertEquals(1, container.getStatListenersSize());
		
		AlrmStatListener stat2 = new AlrmStatListener();
		assertTrue(container.addStatsListener(stat2));
		assertEquals(0, container.getAlarmListenersSize());
		assertEquals(2, container.getStatListenersSize());
		
		assertTrue(container.removeStatListener(stat2));
		assertEquals(0, container.getAlarmListenersSize());
		assertEquals(1, container.getStatListenersSize());
		
		AlrmStatListener notExistsentStatL = new AlrmStatListener();
		assertFalse(container.removeStatListener(notExistsentStatL));
		assertEquals(0, container.getAlarmListenersSize());
		assertEquals(1, container.getStatListenersSize());
		
		assertTrue(container.removeStatListener(stat1));
		assertEquals(0, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
	}
	
	/**
	 * Test the dispatching of statistics
	 */
	public void testDispatchStats() throws Exception {
		AlarmStatistics stats= new AlarmStatistics();
		assertNotNull(stats);
		
		AlrmStatListener stat1 = new AlrmStatListener();
		assertTrue(container.addStatsListener(stat1));
		
		AlrmStatListener stat2 = new AlrmStatListener();
		assertTrue(container.addStatsListener(stat2));
		
		assertEquals(2, container.getStatListenersSize());
		
		assertEquals(0, stat1.numActivationNoifies);
		assertEquals(0, stat1.numReductionNofies);
		assertEquals(0, stat2.numActivationNoifies);
		assertEquals(0, stat2.numReductionNofies);
		
		final int numNotifies=10; 
		for (int t=1; t<numNotifies+1; t++) {
			container.dispatchStatistics(stats);
			assertEquals(t, stat1.numActivationNoifies);
			assertEquals(t, stat1.numReductionNofies);
			assertEquals(t, stat2.numActivationNoifies);
			assertEquals(t, stat2.numReductionNofies);
		}
		
	}
	
	/**
	 * Test the dispatching of exception
	 * 
	 * 
	 */
	public void testDispatchException() throws Exception {
		assertEquals(0, container.getAlarmListenersSize());
		AlarmListenerForTesting alarmL1 = new AlarmListenerForTesting("AlarmListenerName");
		AlarmListener listener1=container.addAlarmListener(alarmL1);
		assertNotNull(listener1);
		assertEquals(1, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
		
		int numExceptions=25;
		for (int t=1; t<numExceptions+1; t++ ) {
			container.dispatchException(new LaserSelectionException("Code_"+t));
			assertEquals(t,alarmL1.numExceptions);
			assertEquals(0,alarmL1.numAlarms);
		}
		
	}
	
	/**
	 * Test the dispatching of alarms
	 * 
	 * {@link AlarmListenerForTesting} prints received alarms on the stdout.
	 * 
	 * This test checks the number of alarms received by 2 listeners:
	 * <OL>
	 * 	<LI>The un-filtered listener must receive all the notifications
	 *  <LI>The filtered listener must be notified only for alarms that pass the filter
	 * </OL>
	 * 
	 * This test checks if the filter is applied. 
	 * The correctness of the filtering is tested by {@link AlarmFilterTest}
	 */
	public void testDispatchAlarms() throws Exception {
		// Instantiates 2 listeners one un-filtered and one filtered
		assertEquals(0, container.getAlarmListenersSize());
		AlarmListenerForTesting alarmL1 = new AlarmListenerForTesting("Unfiltered-listener");
		AlarmListener listener1=container.addAlarmListener(alarmL1); // Unfiltered
		assertNotNull(listener1);
		AlarmFilter filter= new AlarmFilter(null, null, 10, 25);
		AlarmListenerForTesting alarmL2 = new AlarmListenerForTesting("Filtered-listener");
		AlarmListener listener2=container.addAlarmListener(alarmL2,filter); // Filtered
		assertNotNull(listener2);
		assertEquals(2, container.getAlarmListenersSize());
		assertEquals(0, container.getStatListenersSize());
		
		int numOfAlarmsToDispatch=50;
		for (int t=1; t<numOfAlarmsToDispatch+1; t++) {
			AlarmForTesting alarm = new AlarmForTesting("faultFamily", "faultMember", t, 2, true, false, true);
			container.dispatchAlarm(alarm);
			assertEquals(t, alarmL1.numAlarms);
		}
		// Filtered listeners is supposed to receive less alarms
		// i.e. all the alarms whose FC is in [10,15]
		assertEquals(16, alarmL2.numAlarms);
	}
	
}
