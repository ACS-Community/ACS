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

import cern.laser.business.data.Alarm;
import cern.laser.business.data.Triplet;

import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;

import alma.acs.component.client.ComponentClientTestCase;

/**
 * Test the alarm definitions read from the CDB
 * 
 * @author acaproni
 *
 */
public class TestAlarmDAO extends ComponentClientTestCase {
	
	/**
	 * The triplets of the alarms defined in the CDB
	 * 
	 * @author acaproni
	 *
	 */
	private enum AlarmTriplets {
		TEST_TM1_1("TEST:TEST_MEMBER1:1"),
		TEST_TM1_2("TEST:TEST_MEMBER1:2"),
		TEST_TM2_1("TEST:TEST_MEMBER2:1"),
		TEST_TM2_2("TEST:TEST_MEMBER2:2"),
		TEST_DEF_1("TEST:*:1"),
		TEST_DEF_2("TEST:*:2"),
		PS_PSM_1("PS:PS_MEMBER:1");
		
		public final String ID;
		
		private AlarmTriplets(String ID) {
			this.ID=ID;
		}
		
		/**
		 * Check if the passed string is a defined ID
		 * 
		 * @param id The ID to check
		 * @return
		 */
		public static boolean exist(String id) {
			for (AlarmTriplets triplet: AlarmTriplets.values()) {
				if (triplet.ID.equals(id)) {
					return true;
				}
			}
			return false;
		}
		
		/**
		 * Build the triplet for the given alarm
		 */
		public Triplet getTriplet() {
			String[] parts = ID.split(":");
			Triplet ret = new Triplet(parts[0],parts[1],Integer.parseInt(parts[2]));
			return ret;
		}
	}
	
	private ACSAlarmDAOImpl alarmDAO;
	
	/**
	 * Constructor 
	 * 
	 * @throws Exception
	 */
	public TestAlarmDAO() throws Exception {
		super("TestAlarmDAO");
	}
	
	
	/**
	 * @see TestCase
	 */
	public void setUp() throws Exception {
		super.setUp();
		
		ConfigurationAccessor conf;
		conf = ConfigurationAccessorFactory.getInstance(getContainerServices());
		assertNotNull("Got a null ConfigurationAccessor", conf);
		
		alarmDAO=new ACSAlarmDAOImpl(getContainerServices().getLogger());
		assertNotNull("AlarmDAO is null", alarmDAO);
		
		alarmDAO.setConfAccessor(conf);
		alarmDAO.loadAlarms();
	}
	
	/**
	 * @see TestCase
	 */
	public void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * Check the getting of all the alarm IDs
	 * 
	 * The ID of an alarm is given by its triplet
	 * 
	 * @throws Exception
	 */
	public void testAlarmIDs() throws Exception {
		String[] ids = alarmDAO.getAllAlarmIDs();
		assertNotNull(ids);
		
		// There are 7 alarms defined in the CDB 
		// 5 alarms plus 2 defaults for TEST
		assertEquals(7, ids.length);
		
		// Check if all the triplets exist
		for (String id: ids) {
			assertTrue(AlarmTriplets.exist(id));
		}
	}
	
	/**
	 * Test the getting of alarms by their ID
	 */
	public void testGetAlarmID() throws Exception {
		for (AlarmTriplets triplet: AlarmTriplets.values()) {
			if (!triplet.ID.contains("*")) {
				Alarm alarm = alarmDAO.getAlarm(triplet.ID);
				assertNotNull(alarm);
				assertEquals(triplet.ID, alarm.getAlarmId());
				Triplet alarmTriplet = alarm.getTriplet();
				assertNotNull(alarmTriplet);
				Triplet defTriplet = triplet.getTriplet();
				assertEquals(defTriplet.getFaultFamily(), alarmTriplet.getFaultFamily());
				assertEquals(defTriplet.getFaultMember(), alarmTriplet.getFaultMember());
				assertEquals(defTriplet.getFaultCode(), alarmTriplet.getFaultCode());
			}
		}
	}

}
