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

import java.sql.Timestamp;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.source.alarmsysteminterface.FaultState;
import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.clients.AlarmCategoryClient;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.clients.alarm.AlarmFilter;
import alma.alarmsystem.clients.test.utils.AlarmListenerForTesting;
import alma.alarmsystem.clients.test.utils.AlrmStatListenerForTesting;;

/**
 * This test is basically an extension of {@link CategoryClientTest} to test AlarmCategoryClient
 * <P>
 * The test is done using 2 listener2, one that gets only the alarms having a defined fault code ({@link #filteredLister})
 * and another one that gets all the alarms (the object itself).
 * <P>
 * The alarms received by the filtered listener are printed to the stdout (@see {@link AlarmListenerForTesting#onAlarm(Alarm)})
 * <P>
 * Notification of statistics are printed in the stdout (@see {@link AlrmStatListenerForTesting}).
 * @author acaproni
 * @since ACS-12.2
 *
 */
public class AlarmCategoryClientTest extends ComponentClientTestCase implements AlarmSelectionListener {
	
	/**
	 * The definition of the alarms as we expect they arrive from
	 * the alarm system
	 * 
	 * @author acaproni
	 *
	 */
	public enum AlarmsFromCDB {
		M1_1A("TEST", "TEST_MEMBER1", 1, true,2,"The cause","Run and fix quickly","Test alarm 1","A disaster"),
		M1_2A("TEST", "TEST_MEMBER1", 2, true,3,null,null,"Test alarm 2",null),
		M2_1A("TEST", "TEST_MEMBER2", 1, true,2,"The cause","Run and fix quickly","Test alarm 1","A disaster"),
		M2_2A("TEST", "TEST_MEMBER2", 2, true,3,null,null,"Test alarm 2",null),
		M1_1I("TEST", "TEST_MEMBER1", 1, false,2,"The cause","Run and fix quickly","Test alarm 1","A disaster"),
		M1_2I("TEST", "TEST_MEMBER1", 2, false,3,null,null,"Test alarm 2",null),
		M2_1I("TEST", "TEST_MEMBER2", 1, false,2,"The cause","Run and fix quickly","Test alarm 1","A disaster"),
		M2_2I("TEST", "TEST_MEMBER2", 2, false,3,null,null,"Test alarm 2",null);
		
		public final String FF;
		public final String FM;
		public final Integer FC;
		public boolean state;
		public final String cause;
		public final String action;
		public final String consequence;
		public final String description;
		public final Integer priority;
		
		private AlarmsFromCDB(
				String FF, 
				String FM, 
				int code,
				boolean status,
				int pri,
				String cause,
				String action,
				String desc,
				String consequence) {
			this.FF=FF;
			this.FM=FM;
			this.FC=code;
			this.state=status;
			this.priority=pri;
			this.consequence=consequence;
			this.description=desc;
			this.cause=cause;
			this.action=action;
		}
		
		/**
		 * Return the alarm with the given triplet 
		 * 
		 * @param FF Fault Family
		 * @param FM Fault Member
		 * @param code Fault Code
		 * @return The alarm with the given triplet
		 *         null if the triplet does not exist
		 */
		public static AlarmsFromCDB getCDBAlarm(String FF, String FM, int code) {
			for (AlarmsFromCDB alarm: AlarmsFromCDB.values()) {
				if (alarm.FF.equals(FF) && alarm.FM.equals(FM) && alarm.FC==code) {
					return alarm;
				}
			}
			return null;
		}
	}
	
	/**
	 *  The categoryClient to test
	 */
	private AlarmCategoryClient categoryClient;

	
	/**
	 * A filtered listener of Alarms
	 */
	private AlarmListenerForTesting filteredLister;
	
	/**
	 * 
	 */
	private AlrmStatListenerForTesting statListener;
	
	/**
	 * The vector with the alarms received
	 */
	private Vector<Alarm> alarmsReceived;
	
	/**
	 * <code>latch</code> signals when all the alarms have been received
	 */
	private CountDownLatch latch = new CountDownLatch(8);
	
	/**
	 *  Max number of seconds to wait for the messages
	 */
	private static final int MAX_TIMEOUT = 120;
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public AlarmCategoryClientTest() throws Exception{
		super("AlarmCategoryClientTest");
	}
	
	/**
	 * @see extends ComponentClientTestCase
	 */
	public void setUp() throws Exception {
		super.setUp();
		categoryClient = new AlarmCategoryClient(getContainerServices());
		assertNotNull(categoryClient);
		
		AlarmFilter filter = new AlarmFilter(null, null, 2, 2);
		filteredLister = new AlarmListenerForTesting("Filtered_listener");
		assertNotNull(filteredLister);
		
		categoryClient.addAlarmListener(this);
		categoryClient.addAlarmListener(filteredLister, filter);
		
		statListener = new AlrmStatListenerForTesting();
		assertNotNull(statListener);
		categoryClient.addStatsListener(statListener);
		
		alarmsReceived=new Vector<Alarm>();
	}
	
	/**
	 * @see extends ComponentClientTestCase
	 */
	public void tearDown() throws Exception {
		categoryClient.close();
		alarmsReceived.clear();
		super.tearDown();
	}
	
	/**
	 * @see AlarmSelectionListener
	 */
	public void onAlarm(Alarm alarm) {
		synchronized (alarmsReceived) {
			alarmsReceived.add(alarm);
			latch.countDown();
		}
	}
	
	/**
	 * @see AlarmSelectionListener
	 */
	public void onException(LaserSelectionException e) {
		System.err.println("onException: "+e.getMessage());
		e.printStackTrace(System.err);
	}

	/**
	 * Sends a couple of alarms and check if they arrive from the client
	 * @throws Exception
	 */
	public void testAlarmReception() throws Exception {
		categoryClient.connect();
		
		// Send the alarms
		send_alarm("TEST", "TEST_MEMBER1", 1, true);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		send_alarm("TEST", "TEST_MEMBER1", 2, true);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		send_alarm("TEST", "TEST_MEMBER2", 1, true);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		send_alarm("TEST", "TEST_MEMBER2", 2, true);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		send_alarm("TEST", "TEST_MEMBER1", 1, false);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		send_alarm("TEST", "TEST_MEMBER1", 2, false);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		send_alarm("TEST", "TEST_MEMBER2", 1, false);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		send_alarm("TEST", "TEST_MEMBER2", 2, false);
		
		// Wait for all the messages
		assertTrue("TIMEOUT: not all the alarms have been received",waitForMessages(latch));
		
		// Check the correctness of the messages
		for (Alarm alarm: alarmsReceived) {
			assertEquals("Alex", alarm.getResponsiblePerson().getFirstName());
			assertEquals("123456", alarm.getResponsiblePerson().getGsmNumber());
			assertEquals("http://tempuri.org", alarm.getHelpURL().toString());
			
			AlarmsFromCDB cdbAlarm = AlarmsFromCDB.getCDBAlarm(
					alarm.getTriplet().getFaultFamily(), 
					alarm.getTriplet().getFaultMember(), 
					alarm.getTriplet().getFaultCode());
			
			String alarmDesc = "<"+alarm.getTriplet().getFaultFamily()+", "+alarm.getTriplet().getFaultMember()+", "+alarm.getTriplet().getFaultCode()+ "> ";
			assertEquals(alarmDesc+"Priority",cdbAlarm.priority,alarm.getPriority());
			assertEquals(alarmDesc+"Cause",cdbAlarm.cause,alarm.getCause());
			assertEquals(alarmDesc+"Description",cdbAlarm.description,alarm.getProblemDescription());
			assertEquals(alarmDesc+"Consequence",cdbAlarm.consequence,alarm.getConsequence());
			assertEquals(alarmDesc+"Action",cdbAlarm.action,alarm.getAction());
		}
	}
	
	/**
	 * Push an alarm
	 * 
	 * @param active If true the alarm is active
	 */
	private void send_alarm(String family, String member, int code, boolean active) throws Exception {
		ACSAlarmSystemInterface alarmSource;
		alarmSource = ACSAlarmSystemInterfaceFactory.createSource(member);
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(family, member, code);
		if (active) {
			fs.setDescriptor(FaultState.ACTIVE);
		} else {
			fs.setDescriptor(FaultState.TERMINATE);
		}
		fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

		alarmSource.push(fs);
	}
	
	/**
	 * Wait for the messages from the alarm system.
	 * 
	 * @param signal The {@link CountDownLatch} to know when all the alarms
	 * 				 have been received
	 * @return <code>true</code> if all the messages are received;
	 *         <code>false</code> in case of timeout (i.e. not all the messages received
	 *               in MAX_TIMEOUT seconds)
	 * @throws InterruptedException 
	 */
	private boolean waitForMessages(CountDownLatch signal) throws InterruptedException {
		if (!signal.await(MAX_TIMEOUT, TimeUnit.SECONDS)) {
			return false;
		}
		int timeout =0; 
		// Wait 10 more senconds until all alarms for filtered arrive
		// It can happen that the signal arrived before the filtered listener receives the last 
		// alarm
		while (filteredLister.numAlarms!=4 && timeout<20) {
			Thread.sleep(500);
			timeout++;
		}
		assertEquals("Not all the filtered alarms have been received", 4,filteredLister.numAlarms );
		return true;
	}
}


