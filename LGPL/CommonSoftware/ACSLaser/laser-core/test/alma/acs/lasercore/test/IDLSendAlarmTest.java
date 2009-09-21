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

import org.omg.CosPropertyService.Property;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.lasercore.test.stress.CategoryClient;
import alma.acs.lasercore.test.stress.category.AlarmView;
import alma.acs.lasercore.test.stress.category.CategoryListener;
import alma.acs.util.UTCUtility;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.CERNAlarmService;
import alma.alarmsystem.CERNAlarmServiceHelper;
import alma.alarmsystem.Timestamp;
import alma.alarmsystem.Triplet;
import alma.alarmsystem.corbaservice.CernAlarmServiceUtils;
import alma.alarmsystem.core.alarms.LaserCoreFaultState;
import alma.alarmsystem.core.alarms.LaserCoreFaultState.LaserCoreFaultCodes;

/**
 * Test the sending of alarms through the <code>submitAlarm</code> IDL method 
 * 
 * @author acaproni
 *
 */
public class IDLSendAlarmTest extends ComponentClientTestCase implements CategoryListener {
	
	/**
	 * The fault family of the test alarm
	 */
	private static final String IDLTestFF="IDLFamily";
	
	/**
	 * The fault member of the test alarm
	 */
	private static final String IDLTestFM="IDLMember";
	
	/**
	 * The fault code of the test alarm
	 */
	private static final int IDLTestFC=1;
	
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
	 * The alarm service
	 */
	private CERNAlarmService alarmService=null;
	
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
	public IDLSendAlarmTest() throws Exception {
		super(IDLSendAlarmTest.class.getName());
	}

	/**
	 * @see alma.acs.lasercore.test.stress.category.CategoryListener#alarmReceived(alma.acs.lasercore.test.stress.category.AlarmView)
	 */
	@Override
	public void alarmReceived(AlarmView alarm) {
		if (alarm.alarmID.equals(
				IDLTestFF+":"+
				IDLTestFM+":"+
				IDLTestFC)) {
			alarms.add(alarm);
		}
		System.out.println("Alarm received: "+alarm.alarmID+", state "+alarm.active);
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
		
		// Get the alarm service
		CernAlarmServiceUtils utils = new CernAlarmServiceUtils(contSvcs);
		assertNotNull(utils);
		alarmService = CERNAlarmServiceHelper.narrow(utils.getAlarmService());
		assertNotNull(alarmService);
		assertFalse(alarmService.isACSAlarmService());
		
		alarms.clear();
		
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
		alarms.clear();
		categoryClient.disconnect();
		super.tearDown();
	}
	
	/**
	 * Send an alarm with the IDL method
	 * 
	 * @throws Exception
	 */
	public void testSendAlarmThroughIDL() throws Exception {
		// Build the alarm
		Triplet triplet = new Triplet(IDLTestFF, IDLTestFM, IDLTestFC);
		long  timestamp = UTCUtility.utcJavaToOmg(System.currentTimeMillis());
		org.omg.CosPropertyService.Property[] props = new org.omg.CosPropertyService.Property[0];
		alarmService.submitAlarm(
				triplet, 
				true, 
				"ThisHostName", 
				this.getClass().getName(), 
				timestamp, 
				props);
		
		logger.info("Alarm sent. Now waiting for alarms");
		long timeout=System.currentTimeMillis()+60*1000;
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
	}
}
