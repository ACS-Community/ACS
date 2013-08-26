/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.lasercore.test.reductions;

import java.sql.Timestamp;

import cern.laser.source.alarmsysteminterface.FaultState;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.lasercore.test.stress.CategoryClient;
import alma.acs.lasercore.test.stress.category.AlarmView;
import alma.acs.lasercore.test.stress.category.CategoryListener;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * Check the functioning of RR with default FM.
 * 
 * @author acaproni
 *
 */
public class RRWithDefaultFM extends ComponentClientTestCase implements CategoryListener{
	
	/**
	 * The category client to listen to alarms
	 */
	private CategoryClient categoryClient;
	
	/**
	 * The alarm source
	 */
	private ACSAlarmSystemInterface alarmSource;

	/**
	 * Constructor 
	 * 
	 * @param name The name of the test
	 * @throws Exception
	 */
	public RRWithDefaultFM() throws Exception {
		super(RRWithDefaultFM.class.getName());
		System.out.println("Test built");
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		categoryClient = new CategoryClient(this.getContainerServices());
		assertNotNull(categoryClient);
		categoryClient.connect();
		categoryClient.addAlarmListener(this);
		alarmSource = ACSAlarmSystemInterfaceFactory.createSource();
		assertNotNull(alarmSource);
		System.out.println("setUp done");
	}

	@Override
	protected void tearDown() throws Exception {
		alarmSource.close();
		categoryClient.close();
		super.tearDown();
		System.out.println("tearDown done");
	}

	/**
	 * @param alarm
	 * @see {@link AlarmSelectionListener}
	 */
	@Override
	public void alarmReceived(AlarmView alarm) {
		System.out.print("Alarm received: "+alarm.alarmID+" active="+alarm.active);
		System.out.print(", nodeC="+alarm.nodeChild);
		System.out.print(", nodeP="+alarm.nodeParent);
		System.out.print(", multiC="+alarm.multiplicityChild);
		System.out.print(", multiP="+alarm.multiplicityParent);
		System.out.print(", reduced="+alarm.reduced);
		System.out.println(", masked="+alarm.masked);
	}

	/**
	 * Test the default FM reductions 
	 * 
	 * B>Note</B>: initially I wrote 2 test for distinguishing between
	 * 				NODe and MULTILPICITY but there is something going wrong 
	 * 				when the alarms for the second tests are published: SimpleSupplier.publishEvent 
	 * 				hangs probably due to some misuse of static variables.
	 * 				I have decided to condesate the 2 tests in only one.
	 * 
	 * @TODO Split this test in two test when the problem described upon has been fixed.
	 *       Update 2012-02: SimpleSupplier has been removed, so it is worth trying again.
	 * 
	 * @throws Exception
	 */
	public void testReductionRules() throws Exception {
		System.out.println("testMultiplicity");
		// Send the alarms to trigger the reduction
		sendAlarm("MF_DEFAULT", "MF1", 0, true);
		sendAlarm("MF_DEFAULT", "MF2", 0, true);
		sendAlarm("MF_DEFAULT", "MF3", 0, true);
		sendAlarm("MF_DEFAULT", "MF4", 0, true);
		sendAlarm("MF_DEFAULT", "MF5", 0, true);
		// Give time for the reduced alarm to arrive
		try {
			Thread.sleep(10000);
		} catch (InterruptedException i) {}
		// Clear the alarms
		sendAlarm("MF_DEFAULT", "MF1", 0, false);
		sendAlarm("MF_DEFAULT", "MF2", 0, false);
		sendAlarm("MF_DEFAULT", "MF3", 0, false);
		sendAlarm("MF_DEFAULT", "MF4", 0, false);
		sendAlarm("MF_DEFAULT", "MF5", 0, false);
		// Give time for the alarms to arrive
		try {
			Thread.sleep(10000);
		} catch (InterruptedException i) {}
		///////////////////////////////////////////////////////////
		///////
		///////////////////////////////////////////////////////////
		System.out.println("testNode");
		sendAlarm("NODE_DEFAULT", "NODE1", 1, true);
		sendAlarm("NODE_DEFAULT", "NODE2", 1, true);
		sendAlarm("NODE_DEFAULT", "NODE3", 1, true);
		sendAlarm("NODE_DEFAULT", "NODE4", 1, true);
		// Give time for the reduced alarm to arrive
		try {
			Thread.sleep(10000);
		} catch (InterruptedException i) {}
		// Clear the alarms
		sendAlarm("NODE_DEFAULT", "NODE1", 1, false);
		sendAlarm("NODE_DEFAULT", "NODE2", 1, false);
		sendAlarm("NODE_DEFAULT", "NODE3", 1, false);
		sendAlarm("NODE_DEFAULT", "NODE4", 1, false);
		// Give time for the alarms to arrive
		try {
			Thread.sleep(10000);
		} catch (InterruptedException i) {}
	}

	/**
	 * Send an alarm
	 * 
	 * @param FF The fault family
	 * @param FM The fault Member
	 * @param FC The fault code
	 * @param active The state active/terminate
	 */
	private void sendAlarm(String FF, String FM, int FC, boolean active) throws Exception {
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(FF, FM, FC);
		assertNotNull(fs);
		if (active) {
			fs.setDescriptor(FaultState.ACTIVE);
		} else {
			fs.setDescriptor(FaultState.TERMINATE);
		}
		fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

		alarmSource.push(fs);
	}
	
}
