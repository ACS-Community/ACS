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
package alma.alarmsystem.source.test;

import java.util.Vector;
import java.util.Collection;

import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;

import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

/**
 * A class to test the Alarm Source
 * 
 * @author acaproni
 *
 */
public class ProxyTest extends ComponentClientTestCase {
	
	// The current directory
	private String curDir=System.getProperty("user.dir");
	
	// The reference to the DAL (to make a clear cache)
	private JDAL jdal;
	
	public ProxyTest(String str) throws Exception {
		super(str);
	}
	
	/**
	 * This execute for each test and we want to have
	 * a cache with some logs 
	 * 
	 * @see junit.framework.TestCase
	 */ 
	protected void setUp() throws Exception
	{ 
		super.setUp();
		// Get the reference to the DAL
		org.omg.CORBA.Object cdbObj = m_acsManagerProxy.get_service("CDB", false);
        if (cdbObj==null) {
			throw new Exception("Error getting the CDB from the manager");
		} 
        jdal = JDALHelper.narrow(cdbObj);
        if (jdal==null) {
        	throw new Exception("Error narrowing the DAL");
        }
		// Set the CDB to use ACS
		TestUtil.setupAlarmBranch(curDir,"ACS");
		// Init the Factory
		ACSAlarmSystemInterfaceFactory.init(getContainerServices());
	}
	
	/** 
	 * @see junit.framework.TestCase
	 *
	 */
	protected void tearDown() throws Exception {
		ACSAlarmSystemInterfaceFactory.done();
		super.tearDown();
	}

	/**
	 * Test the push of an alarm
	 * 
	 * @throws Exception
	 */
	public void testPushAlarm() throws Exception {
		String FF = "TestFamily";
		String FM = "TestMember";
		int FC = 1;
		String descriptor = "Alarm for test";
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(FF,FM,FC);
		assertNotNull("Error creating the FaultState",fs);
		fs.setDescriptor(descriptor);
		ACSAlarmSystemInterface source= ACSAlarmSystemInterfaceFactory.createSource();
		assertNotNull("Error creating the Alarm Source",source);
		source.push(fs);
	}
	
	/**
	 * Test the push of a collection of alarms
	 * 
	 * @throws Exception
	 */
	public void testAlarmsCollection() throws Exception {
		String FF1 = "TestFamily - 1";
		String FM1 = "TestMember - 1";
		int FC1 = 0;
		ACSFaultState fs1 = ACSAlarmSystemInterfaceFactory.createFaultState(FF1,FM1,FC1);
		assertNotNull("Error creating a FS",fs1);
		fs1.setDescriptor("Description of Al1");
		String FF2 = "TestFamily - 2";
		String FM2 = "TestMember - 2";
		int FC2 = 1;
		ACSFaultState fs2 = ACSAlarmSystemInterfaceFactory.createFaultState(FF2,FM2,FC2);
		assertNotNull("Error creating a FS",fs2);
		fs2.setDescriptor("Description of Al2");
		String FF3 = "TestFamily - 3";
		String FM3 = "TestMember - 3";
		int FC3 = 2;
		ACSFaultState fs3 = ACSAlarmSystemInterfaceFactory.createFaultState(FF3,FM3,FC3);
		assertNotNull("Error creating a FS",fs3);
		fs3.setDescriptor("Description of Al3");
		Collection alarms = new Vector();
		alarms.add(fs1);
		alarms.add(fs2);
		alarms.add(fs3);
		assertEquals("Wrong collection size",alarms.size(),3);
		ACSAlarmSystemInterface source= ACSAlarmSystemInterfaceFactory.createSource();
		assertNotNull("Error creating the Alarm source",source);
		source.pushActiveList(alarms);
	}
	
}
