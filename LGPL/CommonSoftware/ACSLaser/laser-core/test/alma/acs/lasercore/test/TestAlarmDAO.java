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

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import junit.framework.TestCase;
import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.core.alarms.LaserCoreFaultState;
import alma.alarmsystem.core.alarms.LaserCoreFaultState.LaserCoreFaultCodes;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.Source;
import cern.laser.business.data.Triplet;

import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;

/**
 * Test the alarm definitions read from the CDB
 * 
 * @author acaproni
 *
 */
public class TestAlarmDAO extends ComponentClientTestCase {
	
	/**
	 * The triplets of the alarms defined in the CDB.
	 * It should be enough to change this enum if the alarm
	 * definitions in the CDB are modified.
	 * 
	 * @author acaproni
	 *
	 */
	private enum AlarmTriplets {
		TEST_TM1_1("TEST:TEST_MEMBER1:1",2,"Test alarm 1","The cause","Run and fix quickly","A disaster"),
		TEST_TM1_2("TEST:TEST_MEMBER1:2",3,"Test alarm 2",null,null,null),
		TEST_TM2_1("TEST:TEST_MEMBER2:1",2,"Test alarm 1","The cause","Run and fix quickly","A disaster"),
		TEST_TM2_2("TEST:TEST_MEMBER2:2",3,"Test alarm 2",null,null,null),
		TEST_DEF_1("TEST:*:1",2,"Test alarm 1","The cause","Run and fix quickly","A disaster"),
		TEST_DEF_2("TEST:*:2",3,"Test alarm 2",null,null,null),
		PS_PSM_1("PS:PS_MEMBER:1",2,"PS test alarm","A terrible mistake",null,null),
		IDL("IDLFamily:IDLMember:1",0,"This alarm has been sent through an IDL method","Sent an IDL alarm",null,null),
		NODE_DEFAULT1("NODE_DEFAULT:*:1", 2, "NODE test", null, null, null),
		NODE_DEFAULT2("NODE_DEFAULT:*:2", 2, "NODE test", null, null, null),
		NODE_DEFAULT3("NODE_DEFAULT:*:3", 2, "NODE test", null, null, null),
		NODE_NODE1_1("NODE_DEFAULT:NODE1:1", 2, "NODE test", null, null, null),
		NODE_NODE1_2("NODE_DEFAULT:NODE1:2", 2, "NODE test", null, null, null),
		NODE_NODE1_3("NODE_DEFAULT:NODE1:3", 2, "NODE test", null, null, null),
		NODE_NODE4_1("NODE_DEFAULT:NODE4:1", 2, "NODE test", null, null, null),
		NODE_NODE4_2("NODE_DEFAULT:NODE4:2", 2, "NODE test", null, null, null),
		NODE_NODE4_3("NODE_DEFAULT:NODE4:3", 2, "NODE test", null, null, null),
		NODE_MULTI1("NODE_DEFAULT:MULTIPLICITY_REG:1", 2, "NODE test", null, null, null),
		NODE_MULTI2("NODE_DEFAULT:MULTIPLICITY_REG:2", 2, "NODE test", null, null, null),
		NODE_MULTI3("NODE_DEFAULT:MULTIPLICITY_REG:3", 2, "NODE test", null, null, null),
		REGEXP_NODE1("NODE_REGEXP:REGEXP_NODE1:1", 2, "NODE test", null, null, null),
		REGEXP_NODE2("NODE_REGEXP:REGEXP_NODE_2:1", 2, "NODE test", null, null, null),
		REGEXP_NODE3("NODE_REGEXP:REGEXP_NODE_3:1", 2, "NODE test", null, null, null),
		REGEXP_NODE4("NODE_REGEXP:REGEXP_NODE_4:1", 2, "NODE test", null, null, null),
		MF_DEFAULT0("MF_DEFAULT:MULTIPLE_MF_FAILURES:0", 0, "The description for 0",null, null, null),
		MF_DEFAULT1("MF_DEFAULT:MULTIPLE_MF_FAILURES:1", 0, "Description 1",null, null, null),
		MF_DEFAULT2("MF_DEFAULT:MULTIPLE_MF_FAILURES:2", 0, "The description for 2",null, null, null),
		MF_DEFAULT3("MF_DEFAULT:MULTIPLE_MF_FAILURES:3", 0, "The description for 3",null, null, null),
		MF_DEFAULT4("MF_DEFAULT:MULTIPLE_MF_FAILURES:4", 0, "The description for 4",null, null, null),
		MF_DEFAULT5("MF_DEFAULT:MULTIPLE_MF_FAILURES:5", 1, "MR alarm","Multiplicity reduction generated", null, null),
		MF_DEFAULT0_DFM("MF_DEFAULT:*:0", 0, "The description for 0",null, null, null),
		MF_DEFAULT1_DFM("MF_DEFAULT:*:1", 0, "Description 1",null, null, null),
		MF_DEFAULT2_DFM("MF_DEFAULT:*:2", 0, "The description for 2",null, null, null),
		MF_DEFAULT3_DFM("MF_DEFAULT:*:3", 0, "The description for 3",null, null, null),
		MF_DEFAULT4_DFM("MF_DEFAULT:*:4", 0, "The description for 4",null, null, null),
		MF_DEFAULT5_DFM("MF_DEFAULT:*:5", 1, "MR alarm","Multiplicity reduction generated", null, null),
		MF_REGEXP0_REGEXP1("MF_REGEXP:REGEXP1:0", 0, "Regexp for 0",null, null, null),
		MF_REGEXP1_REGEXP1("MF_REGEXP:REGEXP1:1", 0, "Regexp 1",null, null, null),
		MF_REGEXP2_REGEXP1("MF_REGEXP:REGEXP1:2", 0, "Regexp for 2",null, null, null),
		MF_REGEXP3_REGEXP1("MF_REGEXP:REGEXP1:3", 0, "Regexp for 3",null, null, null),
		MF_REGEXP4_REGEXP1("MF_REGEXP:REGEXP1:4", 0, "Regexp for 4",null, null, null),
		MF_REGEXP5_REGEXP1("MF_REGEXP:REGEXP1:5", 1, "MR alarm with regexp","Multiplicity reduction generated", null, null),
		MF_REGEXP0_REGEXP2("MF_REGEXP:REGEXP2:0", 0, "Regexp for 0",null, null, null),
		MF_REGEXP1_REGEXP2("MF_REGEXP:REGEXP2:1", 0, "Regexp 1",null, null, null),
		MF_REGEXP2_REGEXP2("MF_REGEXP:REGEXP2:2", 0, "Regexp for 2",null, null, null),
		MF_REGEXP3_REGEXP2("MF_REGEXP:REGEXP2:3", 0, "Regexp for 3",null, null, null),
		MF_REGEXP4_REGEXP2("MF_REGEXP:REGEXP2:4", 0, "Regexp for 4",null, null, null),
		MF_REGEXP5_REGEXP2("MF_REGEXP:REGEXP2:5", 1, "MR alarm with regexp","Multiplicity reduction generated", null, null),
		MF_REGEXP0_REGEXP3("MF_REGEXP:REGEXP3:0", 0, "Regexp for 0",null, null, null),
		MF_REGEXP1_REGEXP3("MF_REGEXP:REGEXP3:1", 0, "Regexp 1",null, null, null),
		MF_REGEXP2_REGEXP3("MF_REGEXP:REGEXP3:2", 0, "Regexp for 2",null, null, null),
		MF_REGEXP3_REGEXP3("MF_REGEXP:REGEXP3:3", 0, "Regexp for 3",null, null, null),
		MF_REGEXP4_REGEXP3("MF_REGEXP:REGEXP3:4", 0, "Regexp for 4",null, null, null),
		MF_REGEXP5_REGEXP3("MF_REGEXP:REGEXP3:5", 1, "MR alarm with regexp","Multiplicity reduction generated", null, null),
		MF_REGEXP0_REGEXP4("MF_REGEXP:REGEXP4:0", 0, "Regexp for 0",null, null, null),
		MF_REGEXP1_REGEXP4("MF_REGEXP:REGEXP4:1", 0, "Regexp 1",null, null, null),
		MF_REGEXP2_REGEXP4("MF_REGEXP:REGEXP4:2", 0, "Regexp for 2",null, null, null),
		MF_REGEXP3_REGEXP4("MF_REGEXP:REGEXP4:3", 0, "Regexp for 3",null, null, null),
		MF_REGEXP4_REGEXP4("MF_REGEXP:REGEXP4:4", 0, "Regexp for 4",null, null, null),
		MF_REGEXP5_REGEXP4("MF_REGEXP:REGEXP4:5", 1, "MR alarm with regexp","Multiplicity reduction generated", null, null),
		MF_REGEXP0_REGEXP5("MF_REGEXP:REGEXP5:0", 0, "Regexp for 0",null, null, null),
		MF_REGEXP1_REGEXP5("MF_REGEXP:REGEXP5:1", 0, "Regexp 1",null, null, null),
		MF_REGEXP2_REGEXP5("MF_REGEXP:REGEXP5:2", 0, "Regexp for 2",null, null, null),
		MF_REGEXP3_REGEXP5("MF_REGEXP:REGEXP5:3", 0, "Regexp for 3",null, null, null),
		MF_REGEXP4_REGEXP5("MF_REGEXP:REGEXP5:4", 0, "Regexp for 4",null, null, null),
		MF_REGEXP5_REGEXP5("MF_REGEXP:REGEXP5:5", 1, "MR alarm with regexp","Multiplicity reduction generated", null, null),
		MF_REGEXP0_MULTIPLE_MF_REGEXP("MF_REGEXP:MULTIPLE_MF_REGEXP:0", 0, "Regexp for 0",null, null, null),
		MF_REGEXP1_MULTIPLE_MF_REGEXP("MF_REGEXP:MULTIPLE_MF_REGEXP:1", 0, "Regexp 1",null, null, null),
		MF_REGEXP2_MULTIPLE_MF_REGEXP("MF_REGEXP:MULTIPLE_MF_REGEXP:2", 0, "Regexp for 2",null, null, null),
		MF_REGEXP3_MULTIPLE_MF_REGEXP("MF_REGEXP:MULTIPLE_MF_REGEXP:3", 0, "Regexp for 3",null, null, null),
		MF_REGEXP4_MULTIPLE_MF_REGEXP("MF_REGEXP:MULTIPLE_MF_REGEXP:4", 0, "Regexp for 4",null, null, null),
		MF_REGEXP5_MULTIPLE_MF_REGEXP("MF_REGEXP:MULTIPLE_MF_REGEXP:5", 1, "MR alarm with regexp","Multiplicity reduction generated", null, null),
		MF_REGEXP0_DFM("MF_REGEXP:*:0", 0, "Regexp for 0",null, null, null),
		MF_REGEXP1_DFM("MF_REGEXP:*:1", 0, "Regexp 1",null, null, null),
		MF_REGEXP2_DFM("MF_REGEXP:*:2", 0, "Regexp for 2",null, null, null),
		MF_REGEXP3_DFM("MF_REGEXP:*:3", 0, "Regexp for 3",null, null, null),
		MF_REGEXP4_DFM("MF_REGEXP:*:4", 0, "Regexp for 4",null, null, null),
		MF_REGEXP5_DFM("MF_REGEXP:*:5", 1, "MR alarm with regexp","Multiplicity reduction generated", null, null),
		HIER_DEF1("HIERARCHICAL:*:1",2,"Generic FS",null,null,null),
		HIER_DEF2("HIERARCHICAL:*:2",2,"MULTI FS",null,null,null),
		HIER_DA41_1("HIERARCHICAL:CONTROL/DA41/HIER:1",2,"Generic FS",null,null,null),
		HIER_DA41_2("HIERARCHICAL:CONTROL/DA41/HIER:2",2,"MULTI FS",null,null,null),
		HIER_MULTI_1("HIERARCHICAL:MULTIPLICITY_HIER:1",2,"Generic FS",null,null,null),
		HIER_MULTI_2("HIERARCHICAL:MULTIPLICITY_HIER:2",2,"MULTI FS",null,null,null);
		

		public final String ID;
		public final int priority;
		public final String description;
		public final String cause;
		public final String action;
		public final String consequence;
		
		private AlarmTriplets(String ID, int priority, String desc,String cause, String action, String consequence) {
			this.ID=ID;
			this.priority=priority;
			this.description=desc;
			this.cause=cause;
			this.action=action;
			this.consequence=consequence;
		}
		
		/**
		 * Check if the passed string is a defined ID.
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
			// Is the ID of a laser core alarm?
			String[] coreIds = new String[LaserCoreFaultCodes.values().length];
			for (int t=0; t<LaserCoreFaultCodes.values().length; t++) {
				coreIds[t]=LaserCoreFaultState.FaultFamily+":"+LaserCoreFaultState.FaultMember+":"+LaserCoreFaultCodes.values()[t].faultCode;
			}
			for (String str: coreIds) {
				if (str.equals(id)) {
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
		
		// There are 8 alarms defined in the CDB 
		// 6 alarms plus 2 defaults for TEST
		// We have to consider the laser core alarms too...
		assertEquals(AlarmTriplets.values().length + LaserCoreFaultCodes.values().length, ids.length);
		
		// Check if all the triplets exist
		for (String id: ids) {
			assertTrue(id+" alarmID does not exist",AlarmTriplets.exist(id));
		}
	}
	
	/**
	 * Test the getting of alarms by their ID.
	 * 
	 * it check the alarm, its ID, its triplet, its priority, its description,
	 * action, cause, consequence
	 */
	public void testGetAlarmID() throws Exception {
		for (AlarmTriplets triplet: AlarmTriplets.values()) {
			if (!triplet.ID.contains("*")) {
				Alarm alarm = alarmDAO.getAlarm(triplet.ID);
				assertNotNull(alarm);
				// CHeck the ID
				assertEquals(triplet.ID, alarm.getAlarmId());
				// Check the triplet
				Triplet alarmTriplet = alarm.getTriplet();
				assertNotNull(alarmTriplet);
				Triplet defTriplet = triplet.getTriplet();
				assertEquals(defTriplet.getFaultFamily(), alarmTriplet.getFaultFamily());
				assertEquals(defTriplet.getFaultMember(), alarmTriplet.getFaultMember());
				assertEquals(defTriplet.getFaultCode(), alarmTriplet.getFaultCode());
				// Check the priority
				assertEquals(Integer.valueOf(triplet.priority), alarm.getPriority());
				// Check the description
				assertEquals(triplet.description, alarm.getProblemDescription());
				// Action
				assertEquals(triplet.action, alarm.getAction());
				// The cause
				assertEquals(triplet.cause, alarm.getCause());
				// Conseuqnces
				assertEquals(triplet.consequence, alarm.getConsequence());
			}
		}
	}
	
	/**
	 * Test the setting of the responsible person
	 * by getting PS and one of the TEST member
	 * 
	 * @throws Exception
	 */
	public void testResponsiblePerson() throws Exception {
		Alarm ps = alarmDAO.getAlarm(AlarmTriplets.PS_PSM_1.ID);
		assertNotNull(ps);
		assertEquals("123456",ps.getResponsiblePerson().getGsmNumber());
		assertEquals("test@eso.org", ps.getResponsiblePerson().getEMail());
		assertEquals("", ps.getResponsiblePerson().getFirstName());
		assertEquals("Alessandro", ps.getResponsiblePerson().getFamilyName());
		
		
		Alarm test = alarmDAO.getAlarm(AlarmTriplets.TEST_TM1_1.ID);
		assertNotNull(test);
		assertEquals("",test.getResponsiblePerson().getGsmNumber());
		assertEquals("", test.getResponsiblePerson().getEMail());
		assertEquals("", test.getResponsiblePerson().getFirstName());
		assertEquals("Alex", test.getResponsiblePerson().getFamilyName());
	}
	
	/**
	 * Test the sources read from CDB (only one at the moment)
	 * 
	 * @throws Exception
	 */
	public void testGetSources() throws Exception {
		ConcurrentHashMap<String,cern.laser.business.data.Source> sources = alarmDAO.getSources();
		assertNotNull(sources);
		assertEquals("There should be only one source and not "+sources.size(),1, sources.size());
		Set<String> keys =sources.keySet();
		assertNotNull(keys);
		assertEquals("Invalid number of keys", 1, keys.size());
		for (String key: keys) {
			Source src = sources.get(key);
			assertNotNull(src);
			// The key is the Source ID
			assertEquals(src.getSourceId(), key);
			// Check the description
			assertEquals("SOURCE", src.getDescription());
			// Check the name
			assertEquals("ALARM_SYSTEM_SOURCES", src.getName());
		}
	}
	
	/**
	 * Find alarm is like getAlarm but throws an exception
	 * if the alarm is not found
	 * 
	 * @throws Exception
	 */
	public void testFindAlarm() throws Exception {
		// Get an alarm that exist
		Alarm alarm = alarmDAO.findAlarm(AlarmTriplets.TEST_TM2_1.ID);
		assertNotNull(alarm);
		assertEquals(AlarmTriplets.TEST_TM2_1.ID, alarm.getAlarmId());
		
		// Get an unknown alarm ==> thorws an exception
		Alarm alarm2=null;
		try {
			alarm2 = alarmDAO.findAlarm("A:b:1");
			alarm2=alarm; // Should not be executed
		} catch (Exception e) {
			// Ok
		}
		assertNull(alarm2);
	}
	
	/**
	 * Test the deletion of alarms
	 */
	public void testDeleteAlarm() {
		int size =alarmDAO.getAllAlarmIDs().length;
		
		Alarm alarmToDelete = alarmDAO.getAlarm(AlarmTriplets.PS_PSM_1.ID);
		assertNotNull(alarmToDelete);
		
		alarmDAO.deleteAlarm(alarmToDelete);
		assertEquals(size-1, alarmDAO.getAllAlarmIDs().length);
		Alarm deletedAlarm=alarmDAO.getAlarm(alarmToDelete.getAlarmId());
		assertNull(deletedAlarm);
		
		// Try to delete an alarm that does not exist
		size =alarmDAO.getAllAlarmIDs().length;
		alarmDAO.deleteAlarm(alarmToDelete);
		assertEquals(size, alarmDAO.getAllAlarmIDs().length);
	}

}
