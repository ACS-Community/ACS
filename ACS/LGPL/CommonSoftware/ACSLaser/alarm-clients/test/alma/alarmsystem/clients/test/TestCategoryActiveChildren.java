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

import java.sql.Timestamp;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.source.alarmsysteminterface.FaultState;
import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * Test getChildren method of the <code>CategoryClient</code>.
 * 
 * @author acaproni
 *
 */
public class TestCategoryActiveChildren extends ComponentClientTestCase implements AlarmSelectionListener  {
	
	/**
	 *  The categoryClient to test
	 */
	private CategoryClient categoryClient;
	
	/**
	 * The number of received alarms
	 */
	private volatile int alarmsReceived;
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public TestCategoryActiveChildren() throws Exception {
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
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onAlarm(cern.laser.client.data.Alarm)
	 */
	@Override
	public void onAlarm(Alarm alarm) {	
		System.out.println(alarm.getAlarmId());
		alarmsReceived++;
	}

	/**
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onException(cern.laser.client.services.selection.LaserSelectionException)
	 */
	@Override
	public void onException(LaserSelectionException e) {}
	
	/**
	 * Test the getting of active children.
	 * 
	 * @throws Exception
	 */
	public void testActiveChildren() throws Exception {
		categoryClient.connect(this);
		
		// Clear all the alarms involved in the test
		//
		// This could be needed if some of the previous tests
		// forgot to clear alarms
		alarmsReceived=0;
		send_alarm("TEST", "NODE1", 1, false);
		send_alarm("TEST", "NODE2", 1, false);
		send_alarm("TEST", "NODE3", 1, false);
		
		send_alarm("TEST", "MULTI1", 2, false);
		send_alarm("TEST", "MULTI2", 2, false);
		send_alarm("TEST", "MULTI3", 2, false);
		send_alarm("TEST", "MULTI4", 2, false);
		send_alarm("TEST", "MULTI5", 2, false);
		// We can't use waitAlarm because the alarm service only send us
		// active alarms ;)
		try {
			Thread.sleep(10000);
		} catch (Exception e) {}
		
		// NODE with only NODE1 active
		alarmsReceived=0;
		send_alarm("TEST", "NODE1", 1, true);
		waitAlarm(1);
		
		Alarm[] alarms = categoryClient.getActiveChildren("TEST:NODE1:1", true);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:NODE2:1", true);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:NODE3:1", true);
		assertEquals(0, alarms.length);
		
		// NODE with NODE1 and NODE2 active
		alarmsReceived=0;
		send_alarm("TEST", "NODE2", 1, true);
		waitAlarm(1);
		
		alarms = categoryClient.getActiveChildren("TEST:NODE1:1", true);
		assertEquals(1, alarms.length);
		assertEquals("TEST:NODE2:1", alarms[0].getAlarmId());
		alarms = categoryClient.getActiveChildren("TEST:NODE2:1", true);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:NODE3:1", true);
		assertEquals(0, alarms.length);
		
		// NODE with NODE1 NODE2 and NODE3 active
		alarmsReceived=0;
		send_alarm("TEST", "NODE3", 1, true);
		waitAlarm(1);
		
		alarms = categoryClient.getActiveChildren("TEST:NODE1:1", true);
		assertEquals(1, alarms.length);
		assertEquals("TEST:NODE2:1", alarms[0].getAlarmId());
		alarms = categoryClient.getActiveChildren("TEST:NODE2:1", true);
		assertEquals(1, alarms.length);
		assertEquals("TEST:NODE3:1", alarms[0].getAlarmId());
		alarms = categoryClient.getActiveChildren("TEST:NODE3:1", true);
		assertEquals(0, alarms.length);
		
		// MULTIPLICITY (no alarm active)
		alarms = categoryClient.getActiveChildren("TEST:MCAUSE:1", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI1:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI2:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI3:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI4:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI5:2", false);
		assertEquals(0, alarms.length);
		
		// MULTIPLICITY (only MULTI1 active)
		alarmsReceived=0;
		send_alarm("TEST", "MULTI1", 2, true);
		waitAlarm(1);
		
		alarms = categoryClient.getActiveChildren("TEST:MCAUSE:1", false);
		assertEquals(1, alarms.length);
		assertEquals("TEST:MULTI1:2", alarms[0].getAlarmId());
		alarms = categoryClient.getActiveChildren("TEST:MULTI1:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI2:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI3:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI4:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI5:2", false);
		assertEquals(0, alarms.length);
		
		// MULTIPLICITY (all the MULTIx active)
		alarmsReceived=0;
		send_alarm("TEST", "MULTI2", 2, true);
		send_alarm("TEST", "MULTI3", 2, true);
		send_alarm("TEST", "MULTI4", 2, true);
		send_alarm("TEST", "MULTI5", 2, true);
		waitAlarm(5); // There is the alarm generated by the multiplicity reduction too
		alarms = categoryClient.getActiveChildren("TEST:MCAUSE:1", false);
		assertEquals(5, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI1:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI2:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI3:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI4:2", false);
		assertEquals(0, alarms.length);
		alarms = categoryClient.getActiveChildren("TEST:MULTI5:2", false);
		assertEquals(0, alarms.length);
		
		// Turn off all the alarms
		send_alarm("TEST", "NODE1", 1, false);
		send_alarm("TEST", "NODE2", 1, false);
		send_alarm("TEST", "NODE3", 1, false);
		
		send_alarm("TEST", "MULTI1", 2, false);
		send_alarm("TEST", "MULTI2", 2, false);
		send_alarm("TEST", "MULTI3", 2, false);
		send_alarm("TEST", "MULTI4", 2, false);
		send_alarm("TEST", "MULTI5", 2, false);
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {}
	}
	
	/**
	 * Wait until n alarms are received
	 * 
	 * @param n The number of alarms to wait for
	 */
	private void waitAlarm(int n) {
		while (alarmsReceived<n) {
			try {
				Thread.sleep(250);
			} catch (InterruptedException e) {
				continue;
			}
		}
	}
}
