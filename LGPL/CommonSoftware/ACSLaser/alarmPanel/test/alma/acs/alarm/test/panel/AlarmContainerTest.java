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

import javax.swing.SwingWorker;

import junit.framework.TestCase;
import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableEntry;
import alma.acsplugins.alarmsystem.gui.table.AlarmsContainer;
import alma.acsplugins.alarmsystem.gui.table.AlarmsContainer.AlarmContainerException;
import alma.acsplugins.alarmsystem.gui.table.AlarmsReductionContainer;
import cern.laser.client.data.Alarm;
import alma.acs.gui.util.threadsupport.EDTExecutor;

/**
 * <code>AlarmContainerTest</code> tests {@link AlarmsContainer}
 * <P>
 * After building an <code>AlarmsContainer</code>, the test stresses
 * each of its methods in both situations i.e. with and without
 * reductions.
 * <P>
 * Alarms container must be accessed inside the EDT because the synchronization
 * is done through thread confinement. The alarm containers modify their data
 * inside the EDT but reading must be done from inside the EDT.
 * 
 * @author acaproni
 *
 */
public class AlarmContainerTest extends TestCase {
	
	/**
	 * MyAlarmContainer extends {@link AlarmsReductionContainer} to allow testing
	 * protected methods.
	 * 
	 * @author acaproni
	 *
	 */
	private class MyAlarmContainer extends AlarmsReductionContainer {
		
		/**
		 * Constructor
		 * @see AlarmsReductionContainer#AlarmsReductionContainer(int)
		 */
		public MyAlarmContainer(int max) {
			super(max);
		}

		/**
		 * @see AlarmsContainer#removeInactiveAlarms(AlarmGUIType type)
		 */
		@Override
		public int removeInactiveAlarms(AlarmGUIType type)
				throws AlarmContainerException {
			return super.removeInactiveAlarms(type);
		}
		
	}
	
	/**
	 * The max number of alarms in the container
	 */
	private final int CONTAINER_SIZE = 10000;
	
	/**
	 * The container to test
	 */
	private MyAlarmContainer container;

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
		container = new MyAlarmContainer(CONTAINER_SIZE);
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
		// Check the size of the empty container
		EDTExecutor.instance().executeSync(new Runnable() {
			@Override
			public void run() {
				assertEquals(0,container.size(false));
				assertEquals(0,container.size(true));
			}
		});
		// Add half of the max number of alarms
		int notReduced=populateContainer(CONTAINER_SIZE/2, "TEST",null,null);
		
		class EDTReader implements Runnable {
			// The value to check after reading from EDT
			private final int value;
			
			EDTReader(int val) {
				value=val;
			}
			
			public void run() {
				assertEquals("Wrong not reduced size",CONTAINER_SIZE/2, container.size(false));
				assertEquals("Wrong reduced size",value, container.size(true));
			}
			
		}
		EDTExecutor.instance().executeSync(new EDTReader(notReduced));

		// Add the other alarms until the container is full
		notReduced+=populateContainer(CONTAINER_SIZE/2, "TEST", null,null);
		class EDTReader2 implements Runnable {
			// The value to check after reading from EDT
			private final int value;
			
			EDTReader2(int val) {
				value=val;
			}
			
			public void run() {
				assertEquals("Wrong not reduced size",CONTAINER_SIZE, container.size(false));
				assertEquals("Wrong reduced size",value, container.size(true));
			}
			
		}
		EDTExecutor.instance().executeSync(new EDTReader2(notReduced));
		
		// Adding one more item, an exception should be thrown
		boolean exceptionThrown=false;
		try {
			container.add(new AlarmTableEntry(TestAlarm.generateRndAlarm("TEST")));
		} catch (Exception e) {
			exceptionThrown=true;
		}
		assertTrue(exceptionThrown);
		
		// Finally clear the container
		EDTExecutor.instance().executeSync(new Runnable() {
			@Override
			public void run() {
				container.clear();
				assertEquals(0,container.size(false));
				assertEquals(0,container.size(true));
			}
		});
	}
	
	/**
	 * Test the removal of alarms
	 * 
	 * @throws Exception
	 */
	public void testAlarmsRemove() throws Exception {
		// Add some alarms
		int notReduced=0;
		Vector<Alarm> alarms = new Vector<Alarm>();
		notReduced = populateContainer(CONTAINER_SIZE/2, "TEST", alarms,null);
		class EDTReader implements Runnable {
			// The value to check after reading from EDT
			private final int value;
			
			EDTReader(int val) {
				value=val;
			}
			
			public void run() {
				assertEquals("Wrong not reduced size",CONTAINER_SIZE/2, container.size(false));
				assertEquals("Wrong reduced size",value, container.size(true));
			}
			
		}
		EDTExecutor.instance().executeSync(new EDTReader(notReduced));
		
		// Trying to remove an alarm not in the container throws an exception
		Alarm al = TestAlarm.generateRndAlarm("EXCEPTION");
		boolean exceptionOccurred=false;
		try {
			container.remove(new AlarmTableEntry(al));
		} catch (AlarmContainerException e) {
			exceptionOccurred=true;
		}
		assertTrue(exceptionOccurred);
		
		// Remove all the alarms checking the number of alarms in the container
		int sz = CONTAINER_SIZE/2;
		int reducedToRemove=notReduced;
		for (Alarm removedAlarm: alarms) {
			container.remove(new AlarmTableEntry(removedAlarm));
			sz--;
			if (!removedAlarm.getStatus().isReduced()) {
				reducedToRemove--;
			}
			class EDTReader2 implements Runnable {
				// The values to check after reading from EDT
				private final int sz;
				private final int reducedToRemove;
				
				EDTReader2(int sz,int reducedToRemove) {
					this.sz=sz;
					this.reducedToRemove=reducedToRemove;
				}
				
				public void run() {
					assertEquals(sz, container.size(false));
					assertEquals(reducedToRemove, container.size(true));
				}
				
			}
			EDTExecutor.instance().executeSync(new EDTReader2(sz,reducedToRemove));
			
		}
		// The container is empty
		EDTExecutor.instance().executeSync(new Runnable() {
			@Override
			public void run() {
				assertEquals(0,container.size(false));
				assertEquals(0,container.size(true));
			}
		});
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
	 * @return The number of not reduced alarms in the collection
	 */
	private int populateContainer(
			int size, 
			String fm, 
			Collection<Alarm> generatedAlarms,
			Collection<AlarmTableEntry> entries) throws Exception {
		int notReduced=0;
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
			if (!alarm.getStatus().isReduced()) {
				notReduced++;
			}
			container.add(new AlarmTableEntry(alarm));
		}
		if (generatedAlarms!=null) {
			assertEquals(size, generatedAlarms.size()-originalCollectionAlarmsSize);
		}
		if (entries!=null) {
			assertEquals(size, entries.size()-originalCollectionEntriesSize);
		}
		return notReduced;
	}
	
	/**
	 * Test <code>removeOldest()</code>
	 * 
	 * @throws Exception
	 */
	public void testRemoveOldest() throws Exception {
		// Add some alarms
		final Vector<Alarm> alarms = new Vector<Alarm>();
		int notReduced = populateContainer(CONTAINER_SIZE/2, "TEST", alarms,null);
		class EDTReader implements Runnable {
			// The value to check after reading from EDT
			private final int value;
			
			EDTReader(int val) {
				value=val;
			}
			
			public void run() {
				assertEquals("Wrong not reduced size",CONTAINER_SIZE/2, container.size(false));
				assertEquals("Wrong reduced size",value, container.size(true));
			}
			
		}
		EDTExecutor.instance().executeSync(new EDTReader(notReduced));
		
		final AlarmTableEntry removedEntry = container.removeOldest();
		assertNotNull(removedEntry);
		// Check the sizes
		class EDTReader2 implements Runnable {
			// The value to check after reading from EDT
			private final int notReduced;
			
			EDTReader2(int notReduced) {
				this.notReduced=notReduced;
			}
			
			public void run() {
				assertEquals(alarms.size()-1, container.size(false));
				if (removedEntry.isReduced()) {
					assertEquals(notReduced, container.size(true));
				} else {
					assertEquals(notReduced-1, container.size(true));
				}
			}
			
		}
		EDTExecutor.instance().executeSync(new EDTReader2(notReduced));
		
		// Check if removed alarms was the oldest alarm
		assertEquals(alarms.get(0).getAlarmId(), removedEntry.getAlarmId());
	}
	
	/**
	 * Test <code>replaceAlarm()</code>
	 * 
	 * @throws Exception
	 */
	public void testReplaceAlarm() throws Exception {
		// Add some alarms
		Vector<Alarm> alarms = new Vector<Alarm>();
		int notReduced = populateContainer(CONTAINER_SIZE/2, "TEST", alarms,null);
		class EDTReader implements Runnable {
			// The value to check after reading from EDT
			private final int value;
			
			EDTReader(int val) {
				value=val;
			}
			
			public void run() {
				assertEquals("Wrong not reduced size",CONTAINER_SIZE/2, container.size(false));
				assertEquals("Wrong reduced size",value, container.size(true));
			}
			
		}
		EDTExecutor.instance().executeSync(new EDTReader(notReduced));
		
		// replace the first alarm with another one changing the
		// active attribute
		Alarm alarmToReplace = alarms.get(0);
		assertNotNull(alarmToReplace);
		Alarm newAlarm = new TestAlarm(
				alarmToReplace.getAlarmId(),
				alarmToReplace.isNodeChild(),
				alarmToReplace.isNodeParent(),
				!alarmToReplace.getStatus().isActive(),
				alarmToReplace.getStatus().isMasked(),
				alarmToReplace.getStatus().isReduced());
		class ReplaceAndCheck implements Runnable {
			
			private final Alarm newAlarm;
			
			ReplaceAndCheck(Alarm newAlarm) {
				this.newAlarm=newAlarm;
			}
			
			public void run() {
				try {
					container.replace(new AlarmTableEntry(newAlarm));
				} catch (Throwable t) {
					System.err.println("Error replacing alarm "+newAlarm.getAlarmId());
					t.printStackTrace();
				}
				AlarmTableEntry entry = container.get(newAlarm.getAlarmId());
				assertEquals(newAlarm.getAlarmId(), entry.getAlarmId());
				assertEquals(newAlarm.getStatus().isActive(), entry.getStatus().isActive());
			}
		}
		EDTExecutor.instance().executeSync(new ReplaceAndCheck(newAlarm));
		
		// replace the LAST alarm with another one changing the
		// active attribute
		alarmToReplace = alarms.get(alarms.size()-1);
		assertNotNull(alarmToReplace);
		newAlarm = new TestAlarm(
				alarmToReplace.getAlarmId(),
				alarmToReplace.isNodeChild(),
				alarmToReplace.isNodeParent(),
				!alarmToReplace.getStatus().isActive(),
				alarmToReplace.getStatus().isMasked(),
				alarmToReplace.getStatus().isReduced());
		EDTExecutor.instance().executeSync(new ReplaceAndCheck(newAlarm));
		
		// replace a middle-list alarm with another one changing the
		// active attribute
		alarmToReplace = alarms.get(alarms.size()/2);
		assertNotNull(alarmToReplace);
		newAlarm = new TestAlarm(
				alarmToReplace.getAlarmId(),
				alarmToReplace.isNodeChild(),
				alarmToReplace.isNodeParent(),
				!alarmToReplace.getStatus().isActive(),
				alarmToReplace.getStatus().isMasked(),
				alarmToReplace.getStatus().isReduced());
		EDTExecutor.instance().executeSync(new ReplaceAndCheck(newAlarm));
	}
	
	/**
	 * Test the removal of inactive alarms
	 * 
	 * @throws Exception
	 */
	public void testRemoveInactivAls() throws Exception {
		final Vector<Alarm> alarms= new Vector<Alarm>();
		int notReduced;
		// Stores for each priority the number of inactive alarms
		final int[] priorities =new int[AlarmGUIType.values().length-1];
		// The test is done for each AlarmGUIType corresponding to a priority
		for (AlarmGUIType alarmType: AlarmGUIType.values()) {
			// Add some alarms
			alarms.clear();
			assertEquals(0, alarms.size());
			EDTExecutor.instance().executeSync(new Runnable() {
				@Override
				public void run() {
					container.clear();
					assertEquals(0, container.size(false));
				}
			});
			
			for (int t=0; t<4; t++) {
				priorities[t]=0;
			}
			notReduced = populateContainer(CONTAINER_SIZE/2, "TEST", alarms,null);
			class EDTReader implements Runnable {
				// The value to check after reading from EDT
				private final int value;
				
				EDTReader(int val) {
					value=val;
				}
				
				public void run() {
					assertEquals("Wrong not reduced size",CONTAINER_SIZE/2, container.size(false));
					assertEquals("Wrong reduced size",value, container.size(true));
				}
				
			}
			EDTExecutor.instance().executeSync(new EDTReader(notReduced));
			
			for (Alarm al: alarms) {
				if (!al.getStatus().isActive()) {
					priorities[al.getPriority()]++;
				}
			}
			
			class Checker implements Runnable {
				
				private final AlarmGUIType alarmType;
				
				public Checker(AlarmGUIType alarmType) {
					this.alarmType=alarmType;
				}
				
				public void run() {
					try {
						container.removeInactiveAlarms(alarmType);
					} catch (Throwable t) {
						System.out.println("Error removing inactive alarms for "+alarmType);
						t.printStackTrace();
						return;
					}
					if (alarmType!=AlarmGUIType.INACTIVE) {
						assertEquals(alarms.size()-priorities[alarmType.ordinal()], container.size(false));
					} else {
						int newSize = alarms.size()-priorities[0]-priorities[1]-priorities[2]-priorities[3];
						assertEquals(newSize, container.size(false));
					}		
				}
			}
			EDTExecutor.instance().executeSync(new Checker(alarmType));
		}
	}
	
}
