/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
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
package alma.acs.lasercore.test;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Random;

import cern.laser.source.alarmsysteminterface.FaultState;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.lasercore.test.stress.CategoryClient;
import alma.acs.lasercore.test.stress.category.AlarmView;
import alma.acs.lasercore.test.stress.category.CategoryListener;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * This test sends a lot of fault states and check if they are published in the
 * categories as expected.
 * To avoid defining a great number of alarms in the CDB, a default alarm
 * has been set in the TEST family
 * 
 * Sources are not tested here because they have already been tested in laser-source
 * 
 * NOTE: the test is performed without using the CategoryClient that at this level
 *       is not available (it requires modules compiled later in the build
 *       sequence)
 *       
 *    
 * @author acaproni
 *
 */
public class StressTest extends ComponentClientTestCase implements CategoryListener {
	
	/**
	 * The relevant fields of a fault state to publish
	 * 
	 * @author acaproni
	 *
	 */
	private class MiniFaultState {
		// The fields of the fault state
		public final String FF, FM;
		public final int FC;
		public final long msec;
		public final String description; // ACTIVE/Terminate
		public final Timestamp timestamp;
		
		public MiniFaultState() {
			FF=StressTest.FF;
			//FM=StressTest.FM+Math.abs(rnd.nextInt());
			FM=StressTest.FM+(count++);
			FC = Math.abs(rnd.nextInt() % 2) + 1;
			msec=System.currentTimeMillis();
			timestamp=new Timestamp(msec);
			if (rnd.nextInt()%2==0) {
				description=FaultState.ACTIVE;
				StressTest.this.activeFS++;
			} else {
				description=FaultState.TERMINATE;
			}
			assertNotNull(FF);
			assertNotNull(FM);
			assertNotNull(description);
			assertNotNull(timestamp);
		}
	}
	
	/**
	 * FF of the published states
	 */
	private static final String FF = "TEST";
	
	/**
	 * FM of the published states
	 */
	private static final String FM = "Member";
	
	/**
	 *  The number of alarms to publish
	 */
	private static final int NUM_ALARMS_TO_SEND = 1000;
	
	/**
	 * The random number generator to create FMs
	 */
	private static Random rnd = new Random(System.currentTimeMillis());
	
	/**
	 *  The source
	 */
	private ACSAlarmSystemInterface alarmSource;
	
	/**
	 *  The category client
	 */
	private CategoryClient categoryClient;
	
	/**
	 * The fault states to publish
	 */
	private MiniFaultState[] statesToPublish;
	
	/**
	 *  Container services
	 */
	private ContainerServices contSvcs;
	
	/**
	 * The number of active FS published
	 */
	private int activeFS=0;
	
	/**
	 * Used to generated the FM of the {@link MiniFaultState}'s to send.
	 */
	private static int count=0;
	
	/**
	 * The vector with the alarms received from the categories.
	 * The key is the alarm ID
	 */
	private HashMap<String,AlarmView> alarms = new HashMap<String,AlarmView>();
	
	/**
	 * Constructor
	 * @throws Exception
	 */
	public StressTest() throws Exception {
		super("StressTest");
	}
	
	 

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		
		contSvcs = getContainerServices();
		assertNotNull(contSvcs);
		
		alarmSource = ACSAlarmSystemInterfaceFactory.createSource();
		assertNotNull("Error instantiating the source",alarmSource);
		
		// Connect the categories
		categoryClient= new CategoryClient(contSvcs);
		assertNotNull(categoryClient);
		categoryClient.addAlarmListener(this);
		categoryClient.connect();
		
		// generate the alarms
		statesToPublish=new MiniFaultState[NUM_ALARMS_TO_SEND];
		for (int t=0; t<statesToPublish.length; t++) {
			MiniFaultState mfs = new MiniFaultState();
			assertNotNull(mfs);
			statesToPublish[t]=mfs;
		}
	}

	@Override
	protected void tearDown() throws Exception {
		cleanActiveAlarms();
		categoryClient.disconnect();
		super.tearDown();
	}
	
	/**
	 * @see {@link CategoryListener}
	 */
	@Override
	public void alarmReceived(AlarmView alarm) {
		synchronized (alarms) {
			if (alarm.active && alarm.alarmID.startsWith(FF+":"+FM)) {
				alarms.put(alarm.alarmID,alarm);
			}
		}
	}
	
	/**
	 * publish a FaultState
	 * 
	 * @param mfs The fault state to publish
	 */
	private void send(MiniFaultState mfs) throws Exception {
		assertNotNull(mfs);
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(mfs.FF, mfs.FM, mfs.FC);
		assertNotNull("Error instantiating the FS",fs);
		fs.setDescriptor(mfs.description);
		fs.setUserTimestamp(mfs.timestamp);
		alarmSource.push(fs);
	}
	
	/**
	 * Publishes all the alarms at once then wait until all of them are published by the
	 * alarm service.
	 * Finally check if the received alarms match with the published fault states.
	 * 
	 * The test is based on active alarms only and discards all the terminated ones.
	 * All the published fault states are then compared with alarms received by the
	 * alarm service
	 * 
	 * <B>NOTE</B>: <em>The alarm service publishes an active alarm only if its state
	 * changes</em>
	 * This test works only if the set of alarms it sends is not already present in 
	 * the alarm service.
	 * If it is not the case, the alarm published by the AS depend on the alarms
	 * already present in the service.
	 */
	public void testStress() throws Exception {
		// Send the alarms
		for (MiniFaultState state: statesToPublish) {
			send(state);
			try {
				Thread.sleep(250);
			} catch (Exception e) {}
		}
		
		int timeout = 60; // timeout in secs
		int cnt=0;
		int old=0; // The number of items read in the previous iteration
		// Wait for all the alarms to be in the vector
		while (alarms.size()<activeFS && cnt<2*timeout) {
			if (old!=alarms.size()) {
				cnt=0;
				old=alarms.size();
			}
			try {
				Thread.sleep(500);
				cnt++;
			} catch (Exception e) {}
		}
		assertEquals("Wrong number of alarms received",activeFS, alarms.size());
		
		// Check the correctness of the alarms against the fault states
		for (MiniFaultState mfs: statesToPublish) {
			if (mfs.description.equals(FaultState.TERMINATE)) {
				continue;
			}
			AlarmView alarm = alarms.get(mfs.FF+":"+mfs.FM+":"+mfs.FC);
			assertNotNull("Alarm not published",alarm);
		}
	}
	
	/**
	 * Clean all the active alarms published by the test.
	 * This method must be called in the <code>cleanUp</code> in order
	 * to terminate all the active alarms and be ready for a new test.
	 */
	private void cleanActiveAlarms() throws Exception {
		for (MiniFaultState mfs: statesToPublish) {
			if (mfs.description.equals(FaultState.ACTIVE)) {
				ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(mfs.FF, mfs.FM, mfs.FC);
				assertNotNull("Error instantiating the FS",fs);
				fs.setDescriptor(FaultState.TERMINATE);
				fs.setUserTimestamp(mfs.timestamp);
				alarmSource.push(fs);
			}
		}
	}
}
