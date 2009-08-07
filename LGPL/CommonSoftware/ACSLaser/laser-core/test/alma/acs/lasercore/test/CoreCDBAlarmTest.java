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

import java.io.File;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.lasercore.test.stress.CategoryClient;
import alma.acs.lasercore.test.stress.category.AlarmView;
import alma.acs.lasercore.test.stress.category.CategoryListener;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.corbaservice.CernAlarmServiceUtils;
import alma.alarmsystem.core.alarms.LaserCoreFaultState;
import alma.alarmsystem.core.alarms.LaserCoreFaultState.LaserCoreFaultCodes;

/**
 * Test the sending of core alarms from the alarm component.
 * <P>
 * The test checks if the alarm component send an alarm if one file
 * of the CDB (for instance <code>PS.xml</code>) is unreadable.
 * <P>
 * The test works by changing the readability property of <code>PS.xml</code>.
 * When the alarm component starts, it sends an alarm about a misconfigured CDB.
 * The alarm will be received by the category client.
 * 
 * @author acaproni
 *
 */
public class CoreCDBAlarmTest extends ComponentClientTestCase implements CategoryListener {
	
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
	private final List<AlarmView> alarms = Collections.synchronizedList(new LinkedList<AlarmView>());
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public CoreCDBAlarmTest() throws Exception {
		super(CoreCDBAlarmTest.class.getName());
	}

	/**
	 * @see alma.acs.lasercore.test.stress.category.CategoryListener#alarmReceived(alma.acs.lasercore.test.stress.category.AlarmView)
	 */
	@Override
	public void alarmReceived(AlarmView alarm) {
		if (alarm.alarmID.equals(
				LaserCoreFaultState.FaultFamily+":"+
				LaserCoreFaultState.FaultMember+":"+
				LaserCoreFaultCodes.ALARMS_CDB.faultCode)) {
			alarms.add(alarm);
		}
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
		categoryClient.addAlarmListener(this);
		categoryClient.connect();
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		categoryClient.disconnect();
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
		// Shuts down the AS
		shutdownAS();
		// Change the permission of PS.xml
		File f = new File("CDB//Alarms//AlarmDefinitions//PS//PS.xml");
		assertTrue(f.canRead());
		f.setReadable(false);
		assertFalse(f.canRead());
		// Start the AS again
		startAS();
		
		logger.info("Waiting for alarms");
		long timeout=System.currentTimeMillis()+60*1000;;
		do {
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				continue;
			}
			if (alarms.size()>0) {
				break;
			}
		} while (System.currentTimeMillis()<timeout); 
		assertEquals(1, alarms.size());
		// Check if the alarm is the right one
		AlarmView alarm = alarms.get(0);
		assertNotNull(alarm);
		String expectedID=LaserCoreFaultState.FaultFamily+":"+LaserCoreFaultState.FaultMember+":"+LaserCoreFaultCodes.ALARMS_CDB.faultCode;
		assertEquals(expectedID, alarm.alarmID);
		
		// Restore the reading permission of the file
		f = new File("CDB//Alarms//AlarmDefinitions//PS//PS.xml");
		assertFalse(f.canRead());
		f.setReadable(true);
		assertTrue(f.canRead());
	}
	
	/**
	 * Shuts down the alarm service
	 */
	private void shutdownAS() throws Exception {
		logger.info("Shutting down the AS");
		CernAlarmServiceUtils utils = new CernAlarmServiceUtils(contSvcs);
		assertNotNull(utils);
		AlarmService alarmService = utils.getAlarmService();
		assertNotNull(alarmService);
		alarmService.shutdown();
		logger.info("Leaving the AS time to shut down");
		// Leave the AS time to shut down
		try {
			Thread.sleep(30*1000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Launch the alarm service
	 * 
	 * @throws Exception
	 */
	private void startAS() throws Exception {
		logger.info("Restarting the AS");
		File f=new File("../bin/alarmService");
		//assertTrue(f.canExecute());
		Process p = Runtime.getRuntime().exec("alarmService");
		logger.info("Leaving the AS time to startup");
		// Leave the AS time to shut down
		try {
			Thread.sleep(60*1000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

}
