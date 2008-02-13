/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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
package alma.acs.lasercore.test;

import java.sql.Timestamp;
import java.util.Vector;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.source.alarmsysteminterface.FaultState;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.AlarmServiceHelper;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.clients.SourceClient;
import alma.alarmsystem.clients.source.SourceListener;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * This class tests the send/receipt of alarms listening at the NC 
 * 
 * Each alarm/fault state received is pushed into a vector
 * 
 * @author acaproni
 *
 */
public class AlarmSender extends ComponentClientTestCase implements SourceListener, AlarmSelectionListener {
	
	// The triplet
	private static final String FF = "PS";
	private static final String FM = "PS_MEMBER";
	private static final int FC = 1;
	
	// The object waits up to TIMEOUT seconds for the receipt of alarms
	private static final int TIMEOUT = 60;
	
	// The vector of the received alarms and sources
	//
	// For the category we need to remember the number of received alarms
	// because AlarmView redefines equalsTo and the adding of alarms with the
	// same triplet causes a replace in the Vector
	private Vector<FaultState> faultStatesReceived;
	private Vector<Alarm> alarmsReceived;
	private volatile int numOfAlarmsReceived;
	
	// Container services
	private ContainerServices contSvcs;
	
	// The AS component
	private AlarmService alarmService;
	
	/**
	 * Constructor 
	 * 
	 * @throws Exception
	 */
	public AlarmSender() throws Exception {
		super("AlarmSender");
	}
	
	/**
	 * @see TestCase
	 */
	public void setUp() throws Exception {
		super.setUp();
		
		contSvcs = getContainerServices();
		assertNotNull(contSvcs);
		
		ACSAlarmSystemInterfaceFactory.init(contSvcs);
		
		// Get the AS 
		alarmService =AlarmServiceHelper.narrow(contSvcs.getComponent("AlarmService"));
		assertNotNull(alarmService);
		
		faultStatesReceived=new Vector<FaultState>();
		alarmsReceived=new Vector<Alarm>();
		numOfAlarmsReceived=0;
	}
	
	/**
	 * @see TestCase
	 */
	public void tearDown() throws Exception {
		
		contSvcs.releaseComponent(alarmService.name());
		alarmService=null;
		
		contSvcs=null;
		
		faultStatesReceived.clear();
		alarmsReceived.clear();
		
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
	
	public void faultStateReceived(FaultState faultState) {
		synchronized(faultStatesReceived){
			faultStatesReceived.add(faultState);
		}
	}
	
	/**
	 * @see AlarmSelectionListener
	 */
	public void onAlarm(Alarm alarm) {
		synchronized (alarmsReceived) {
			alarmsReceived.add(alarm);
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
	 * Test the alarms received in the categories
	 * @throws Exception
	 */
	public void testCategories() throws Exception {
		CategoryClient categoryClient = new CategoryClient(contSvcs);
		assertNotNull(categoryClient);
		categoryClient.connect(this);
		
		// send 2 alarms active and inactive
		send_alarm(FF, FM, FC, true);
		send_alarm(FF, FM, FC, false);
		
		// wait until 2 alarms are received in the source NC
		long now = System.currentTimeMillis();
		while (numOfAlarmsReceived<2 && now+TIMEOUT*1000>System.currentTimeMillis()) {
			try {
				Thread.sleep(500);
			} catch (Exception e) {}
		}
		assertEquals("Not all the alarms appeared in the category channels", 2, alarmsReceived.size());
		
		// Check the content of the 2 alarms
		Alarm al1 = alarmsReceived.get(0);
		assertNotNull(al1);
		assertEquals(al1.getAlarmId(), FF+":"+FM+":"+FC);
		assertEquals(al1.getProblemDescription(),"PS test alarm");
		assertFalse(al1.getStatus().isActive());
		assertEquals(al1.getPriority(), Integer.valueOf(2));
		
		Alarm al2 = alarmsReceived.get(1);
		assertNotNull(al2);
		assertEquals(al2.getAlarmId(), FF+":"+FM+":"+FC);
		assertEquals(al2.getProblemDescription(),"PS test alarm");
		assertTrue(al2.getStatus().isActive());
		assertEquals(al2.getPriority(), Integer.valueOf(2));
	}
	
	/**
	 * Send some alarms and checks if they appear in the source NC.
	 * This test should better be in the source module.
	 * 
	 * @throws Exception
	 */
	public void testSource() throws Exception {
		SourceClient sourceClient = new SourceClient(contSvcs);
		assertNotNull(sourceClient);
		sourceClient.addAlarmListener(this);
		sourceClient.connect();
		// send 2 alarms active and inactive
		send_alarm(FF, FM, FC, true);
		send_alarm(FF, FM, FC, false);
		
		// wait until 2 alarms are received in the source NC
		long now = System.currentTimeMillis();
		while (faultStatesReceived.size()<2 && now+TIMEOUT*1000>System.currentTimeMillis()) {
			try {
				Thread.sleep(500);
			} catch (Exception e) {}
		}
		assertEquals("Not all the alarms appeared in the source channel", 2, faultStatesReceived.size());
		
		// Check the content of the 2 alarms
		FaultState fs1 = faultStatesReceived.get(0);
		assertNotNull(fs1);
		assertEquals(fs1.getFamily(), FF);
		assertEquals(fs1.getMember(), FM);
		assertEquals(fs1.getCode(), FC);
		
		FaultState fs2 = faultStatesReceived.get(1);
		assertNotNull(fs1);
		assertEquals(fs2.getFamily(), FF);
		assertEquals(fs2.getMember(), FM);
		assertEquals(fs2.getCode(), FC);
	}
}
