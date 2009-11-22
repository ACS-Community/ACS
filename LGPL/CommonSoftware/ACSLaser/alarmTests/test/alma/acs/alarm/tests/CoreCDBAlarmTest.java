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
package alma.acs.alarm.tests;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.core.alarms.LaserCoreFaultState;
import alma.alarmsystem.core.alarms.LaserCoreFaultState.LaserCoreFaultCodes;

/**
 * Test the sending of the core alarm from the alarm component if a file
 * of the CDB is unreadable
 * <P>
 * The XML file UnReadable.xml is not readable and we expect that the alarm service
 * sends an alarm. 
 * <P>
 * When the alarm component starts, it sends an alarm about a misconfigured CDB.
 * The alarm will be received by the category client.
 * <P>
 * Note that this test does almost nothing but testing if the expected alarm arrives.
 * 
 * @author acaproni
 *
 */
public class CoreCDBAlarmTest extends ComponentClientTestCase implements AlarmSelectionListener {
	
	/**
	 *  The category client
	 */
	private CategoryClient categoryClient;
	
	/**
	 *  Container services
	 */
	private ContainerServices contSvcs;
	
	/**
	 * The logger
	 */
	private Logger logger;
	
	/**
	 * The vector with the alarms received from the categories.
	 * The key is the alarm ID
	 */
	private final List<Alarm> alarms = Collections.synchronizedList(new LinkedList<Alarm>());
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public CoreCDBAlarmTest() throws Exception {
		super(CoreCDBAlarmTest.class.getName());
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		contSvcs = getContainerServices();
		assertNotNull(contSvcs);
		logger=contSvcs.getLogger();
		assertNotNull(logger);
		
		// Connect the categories
		categoryClient= new CategoryClient(contSvcs);
		assertNotNull(categoryClient);
		categoryClient.connect(this);
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		categoryClient.close();
		super.tearDown();
	}
	
	/**
	 * To test if the alarm service send an alarm in case of an error 
	 * in the CDB:
	 * <OL>
	 * 	<LI>shut down the AS
	 * 	<LI>change the reading permission of a file
	 * 	<LI>restart the AS
	 * 	<LI>wait for the alarm or a timeout
	 * </OL>
	 * 
	 * @throws Exception
	 */
	public void testCDBAlarm() throws Exception {
		logger.info("Waiting for alarms");
		long timeout=System.currentTimeMillis()+60*1000;
		do {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				continue;
			}
			if (alarms.size()>0) {
				break;
			}
		} while (System.currentTimeMillis()<timeout); 
		assertEquals(1, alarms.size());
		// Check if the alarm is the right one
		Alarm alarm = alarms.get(0);
		assertNotNull(alarm);
		String expectedID=LaserCoreFaultState.FaultFamily+":"+LaserCoreFaultState.FaultMember+":"+LaserCoreFaultCodes.ALARMS_CDB.faultCode;
		assertEquals(expectedID, alarm.getAlarmId());
		
		
	}

	/**
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onAlarm(cern.laser.client.data.Alarm)
	 */
	@Override
	public void onAlarm(Alarm alarm) {
		// TODO Auto-generated method stub
		if (alarm.getAlarmId().equals(
				LaserCoreFaultState.FaultFamily+":"+
				LaserCoreFaultState.FaultMember+":"+
				LaserCoreFaultCodes.ALARMS_CDB.faultCode)) {
			alarms.add(alarm);
		}
	}

	/**
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onException(cern.laser.client.services.selection.LaserSelectionException)
	 */
	@Override
	public void onException(LaserSelectionException e) {
		System.err.println("Exception for LASER: "+e.getMessage());
		e.printStackTrace();
	}
}
