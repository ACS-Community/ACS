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
package alma.acs.alarm.test.panel;

import java.util.Collection;
import java.util.Vector;

import cern.laser.client.data.Alarm;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableEntry;
import alma.acsplugins.alarmsystem.gui.table.AlarmsContainer;
import alma.acsplugins.alarmsystem.gui.table.AlarmsContainer.AlarmContainerException;

/**
 * <code>AlarmContainerTest</code> tests {@link AlarmsContainer}
 * <P>
 * After building an <code>AlarmsContainer</code>, the test stresses
 * each of its methods in both situations i.e. with and without
 * reductions.
 * 
 * @author acaproni
 *
 */
public class AlarmContainerTest extends ComponentClientTestCase {
	
	/**
	 * The max number of alarms in the container
	 */
	private final int CONTAINER_SIZE = 10000;
	
	/**
	 * The container to test
	 */
	private AlarmsContainer container;

	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public AlarmContainerTest() throws Exception {
		super(AlarmContainerTest.class.getName());
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		TestAlarm.alarm_generator_id=0;
		container = new AlarmsContainer(CONTAINER_SIZE);
		assertNotNull(container);
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		container.clear();
		super.tearDown();
	}
	
	/**
	 * Check the size of the container
	 * 
	 * @throws Exception
	 */
	public void testContainerSize() throws Exception {
		System.out.println("testContainerSize");
		// Check the size of the empty container
		assertEquals(0,container.size(false));
		assertEquals(0,container.size(true));
		// Add half of the max number of alarms
		int reduced=populateContainer(CONTAINER_SIZE/2, "TEST",null,null);
		assertEquals("Wrong not reduced size",CONTAINER_SIZE/2, container.size(false));
		assertEquals("Wrong reduced size",reduced, container.size(true));

		// Add the other alarms until the container is full
		reduced+=populateContainer(CONTAINER_SIZE/2, "TEST", null,null);
		assertEquals("Wrong not reduced size",CONTAINER_SIZE, container.size(false));
		assertEquals("Wrong reduced size",reduced, container.size(true));
		
		// Adding one more item, an exception should be thrown
		boolean exceptionThrown=false;
		try {
			container.add(new AlarmTableEntry(TestAlarm.generateRndAlarm("TEST")));
		} catch (Exception e) {
			exceptionThrown=true;
		}
		assertTrue(exceptionThrown);
		
		// Finally clear the container
		container.clear();
		assertEquals(0,container.size(false));
		assertEquals(0,container.size(true));
	}
	
	/**
	 * Test the removal of alarms
	 * 
	 * @throws Exception
	 */
	public void testAlarmsRemove() throws Exception {
		System.out.println("testAlarmsRemove");
		// Add some alarms
		int reduced=0;
		Vector<Alarm> alarms = new Vector<Alarm>();
		reduced = populateContainer(CONTAINER_SIZE/2, "TEST", alarms,null);
		assertEquals("Wrong not reduced size",CONTAINER_SIZE/2, container.size(false));
		assertEquals("Wrong reduced size",reduced, container.size(true));
		
		// Trying to remove an alarm not in the container throws an exception
		Alarm al = TestAlarm.generateRndAlarm("EXCEPTION");
		boolean exceptionOccurred=false;
		try {
			container.remove(al);
		} catch (AlarmContainerException e) {
			exceptionOccurred=true;
		}
		assertTrue(exceptionOccurred);
		
		// Remove all the alarms checking the number of alarms in the container
		int sz = CONTAINER_SIZE/2;
		int reducedToRemove=reduced;
		for (Alarm removedAlarm: alarms) {
			container.remove(removedAlarm);
			sz--;
			if (removedAlarm.getStatus().isReduced()) {
				reducedToRemove--;
			}
			assertEquals(sz, container.size(false));
			assertEquals(reducedToRemove, container.size(true));
		}
		// The container is empty
		assertEquals(0, container.size(false));
		assertEquals(0, container.size(true));
	}
	
	
	/**
	 * Populate the container with the passed number of randomly generated alarms.
	 * 
	 * @param size The number of alarms to add to the container 
	 * @param fm The fault family of the added alarms
	 * @param generatedAlarms A <code>Collection</code> containing the generated alarms;
	 * 						it can be <code>null</code>; if it is not null, the newly generated
	 * 						alarms are added to the collection.
	 * @param entries The <code>AlarmtableEntry</code> generated while adding alarms to the container
	 * 				it can be <code>null</code>; if it is not <code>null</code>, the newly generated
	 * 						entries are added to the collection.
	 * 					
	 * @return The number of reduced alarms in the collection
	 */
	private int populateContainer(
			int size, 
			String fm, 
			Collection<Alarm> generatedAlarms,
			Collection<AlarmTableEntry> entries) throws Exception {
		int reduced=0;
		int originalCollectionAlarmsSize=0;
		if (generatedAlarms!=null) {
			originalCollectionAlarmsSize=generatedAlarms.size();
		}
		int originalCollectionEntriesSize=0;
		if (entries!=null) {
			originalCollectionEntriesSize=entries.size();
		}
		for (int t=0; t<size; t++) {
			Alarm alarm = TestAlarm.generateRndAlarm(fm);
			if (generatedAlarms!=null) {
				generatedAlarms.add(alarm);
			}
			if (alarm.getStatus().isReduced()) {
				reduced++;
			}
			container.add(new AlarmTableEntry(alarm));
		}
		if (generatedAlarms!=null) {
			assertEquals(size, generatedAlarms.size()-originalCollectionAlarmsSize);
		}
		if (entries!=null) {
			assertEquals(size, entries.size()-originalCollectionEntriesSize);
		}
		return reduced;
	}
	
}
