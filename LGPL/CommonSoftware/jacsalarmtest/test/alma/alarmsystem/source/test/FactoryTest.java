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

import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;

public class FactoryTest extends ComponentClientTestCase {
	
	
			
	// The current directory
	private String curDir=System.getProperty("user.dir");
	
	// The reference to the DAL (to make a clear cache)
	private JDAL jdal; 
	
	public FactoryTest() throws Exception {
		super(FactoryTest.class.getSimpleName());
	}
	
	protected void setUp() throws Exception {
		super.setUp();
        org.omg.CORBA.Object cdbObj = m_acsManagerProxy.get_service("CDB", false);
        if (cdbObj==null) {
			throw new Exception("Error getting the CDB from the manager");
		} 
        jdal = JDALHelper.narrow(cdbObj);
        if (jdal==null) {
        	throw new Exception("Error narrowing the DAL");
        }
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
	 * Check if the ACS implementation of the AS is chosen when
	 * there is no Alarm branch in the CDB
	 * 
	 * @throws Exception
	 */
	public void testNoALarmBranch() throws Exception {
		TestUtil.deleteAlarmBranch(curDir);
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(getContainerServices());
		assertTrue("Wrong implementation in use (no Alarms in CDB case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Check if the ACS implementation of the AS is choosen when
	 * there ACS is in the CDB
	 * 
	 * @throws Exception
	 */
	public void testACSAS() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"ACS");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(getContainerServices());
		assertTrue("Wrong implementation in use (ACS case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Check if the CERN implementation of the AS is choosen when
	 * there CERN is in the CDB
	 * 
	 * @throws Exception
	 */
	public void testCERNAS() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"CERN");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(getContainerServices());
		assertFalse("Wrong implementation in use (CERN case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Check if the ACS implementation is used when the Implementation property is wrong
	 * @throws Exception
	 */
	public void testWrongImplementationProp() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"Wrong property");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(getContainerServices());
		assertTrue("Wrong implementation in use (wrong prop case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Test the creation of a FaultState
	 * 
	 * @throws Exception
	 */
	public void testFaultStateCreation() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"ACS");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(getContainerServices());
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState("Family","Member",0);
		assertNotNull("Error creating a FS",fs);
	}
	
	/**
	 * Test the creation of a source (proxy)
	 * 
	 * @throws Exception
	 */
	public void testAlarmSourceCreation() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"ACS");
		jdal.clear_cache_all();
		ACSAlarmSystemInterfaceFactory.init(getContainerServices());
		ACSAlarmSystemInterface proxy = ACSAlarmSystemInterfaceFactory.createSource("SourceName");
		assertNotNull("Error creating an alarm source",proxy);
	}
	
}
