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

import cern.laser.source.alarmsysteminterface.FaultState;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.clients.SourceClient;
import alma.alarmsystem.clients.category.AlarmView;
import alma.alarmsystem.clients.category.CategoryListener;
import alma.alarmsystem.clients.source.SourceListener;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * This class tests the send/receipt of alarms listenings at the NC 
 * 
 * Each alarm/fault state received is pushed into a vector
 * 
 * @author acaproni
 *
 */
public class AlarmSender extends ComponentClientTestCase implements SourceListener, CategoryListener {
	
	// The triplet
	private static final String FF = "PS";
	private static final String FM = "PS_MEMBER";
	private static final int FC = 1;
	
	// The object waits up to TIMEOUT seconds for the receipt of alarms
	private static final int TIMEOUT = 60;
	
	// The category client
	private CategoryClient categoryClient;
	
	// The source client
	private SourceClient sourceClient;
	
	// The vector of the received alarms and sources
	private Vector<FaultState> faultStatesReceived;
	private Vector<AlarmView> alarmsReceived;
	
	public AlarmSender() throws Exception
	{
		super("AlarmSender");
	}
	
	public void setUp() throws Exception {
		super.setUp();
		
		ContainerServices contSvcs = getContainerServices();
		assertNotNull(contSvcs);
		
		ACSAlarmSystemInterfaceFactory.init(contSvcs);
		
		categoryClient = new CategoryClient(contSvcs);
		assertNotNull(categoryClient);
		
		sourceClient = new SourceClient(contSvcs);
		assertNotNull(sourceClient);
		
		faultStatesReceived=new Vector<FaultState>();
		alarmsReceived=new Vector<AlarmView>();
		
		// Connects the listeners
		categoryClient.addAlarmListener(this);
		sourceClient.addAlarmListener(this);
	}
	
	public void tearDown() throws Exception {
		sourceClient.removeListener(this);
		categoryClient.removeListener(this);
		
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
	
	public void alarmReceived(AlarmView alarm) {
		synchronized(alarmsReceived) {
			alarmsReceived.add(alarm);
		}
	}
	
	
	/**
	 * Send some alarms and checks if they appear in the source NC.
	 * This test should better be in the source module.
	 * 
	 * @throws Exception
	 */
	public void testSource() throws Exception {
		sourceClient.connect();
		// send 2 alarms active and inactive
		send_alarm(FF, FF, FC, true);
		send_alarm(FF, FF, FC, false);
		
		// wait until 2 alarms are received in the source NC
		long now = System.currentTimeMillis();
		while (faultStatesReceived.size()<2 && now+TIMEOUT*1000>System.currentTimeMillis()) {
			try {
				Thread.sleep(500);
			} catch (Exception e) {}
		}
		assertEquals("Not all the alarms appeared in the source channel", 2, faultStatesReceived.size());
	}
}
