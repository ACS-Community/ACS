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
import java.util.HashMap;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.lasercore.test.stress.CategoryClient;
import alma.acs.lasercore.test.stress.category.AlarmView;
import alma.acs.lasercore.test.stress.category.CategoryListener;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;

/**
 * Test the sending of core alarms from the alarm component.
 * <P>
 * The test checks if the alarm component send an alarm if one file
 * of the CDB (for instance <code>PS.xml</code>) is unreadable.
 * <P>
 * The test works by changing the readability property of <code>PS.xml</code>.
 * When the alarm component starts, it sends an alarm about a misconfigured CDB.
 * The alarm will be received by the category client.
 * <BR>
 * Note that the test does not do a real work: it only change the property of 
 * the file and connect the category client. As a consequence, the test method
 * is empty.
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
	 * The vector with the alarms received from the categories.
	 * The key is the alarm ID
	 */
	private HashMap<String,AlarmView> alarms = new HashMap<String,AlarmView>();
	
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
		synchronized (alarms) {
			alarms.put(alarm.alarmID, alarm);
		}
	}

	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		// Change the permission of PS.xml
		File f = new File("CDB//Alarms//AlarmDefinitions//PS//PS.xml");
		assertTrue(f.canRead());
		f.setReadable(false);
		assertFalse(f.canRead());
		
		contSvcs = getContainerServices();
		assertNotNull(contSvcs);
		
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
		File f = new File("CDB//Alarms//AlarmDefinitions//PS//PS.xml");
		assertFalse(f.canRead());
		f.setReadable(true);
		assertTrue(f.canRead());
		super.tearDown();
	}
	
	/**
	 * Connect the category client and wait until the alarm is received
	 * of e timeout occur.
	 * 
	 * @throws Exception
	 */
	public void testCDBAlarm() throws Exception {
		long timeout=System.currentTimeMillis()+60*1000;;
		do {
			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				continue;
			}
			synchronized (alarms) {
				if (alarms.size()>0) {
					break;
				}
			}
		} while (System.currentTimeMillis()<timeout); 
		assertEquals(1, alarms.size());
	}

}
