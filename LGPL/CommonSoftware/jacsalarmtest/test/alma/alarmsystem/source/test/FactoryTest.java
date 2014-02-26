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

import java.util.logging.Logger;

import junit.framework.TestCase;
import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.logging.ClientLogManager;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;

public class FactoryTest extends TestCase {
			
	/**
	 *  The current directory
	 */
	private final String curDir=System.getProperty("user.dir");
	
	/**
	 * The manager corbaloc
	 */
	private final String managerLoc = System.getProperty("ACS.manager").trim();
	
	// The reference to the DAL (to make a clear cache)
	private JDAL jdal; 
	
	/**
	 * The name of the client
	 */
	private final String clientName = getClass().getName();
	
	/**
	 * The component client will be instantiated after setting up
	 * the type of the alarm system
	 */
	private AdvancedComponentClient client;
	
	protected void setUp() throws Exception {
		super.setUp();
		
		
		assertNull(client);
	}

	/**
	 * Clear the cache of the DAL
	 * @param contSvcs ContainerServices
	 * @throws Exception
	 */
	private void clearDalCache(ContainerServices contSvcs) throws Exception {
		DAL dal=contSvcs.getCDB();
		jdal = JDALHelper.narrow(dal);
        if (jdal==null) {
        	throw new Exception("Error narrowing the DAL");
        }
		jdal.clear_cache_all();
	}
	
	/**
	 * Instantiating a component client trigger the initialization of the 
	 * {@link ACSAlarmSystemInterfaceFactory}.
	 * 
	 * @return The component client
	 */
	private AdvancedComponentClient instantiateComponentClient() throws Exception {
		// Initialize the logger
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(clientName, true);
		assertNotNull(logger);
		client=new AdvancedComponentClient(logger, managerLoc, clientName);
		return client;
	}
	
	/** 
	 * @see junit.framework.TestCase
	 *
	 */
	protected void tearDown() throws Exception {
		try {
			Thread.sleep(5000);
		} catch (InterruptedException ie) {
			ie.printStackTrace(System.err);
		}
		if (client!=null) {
			clearDalCache(client.getContainerServices());
			client.tearDown();
			client=null;
		}
		
		// Restore the same version checked out from repository
		TestUtil.setupAlarmBranch(curDir,"ACS");
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
		instantiateComponentClient();
		assertTrue("Wrong implementation in use (no Alarms in CDB case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Check if the ACS implementation of the AS is chosen when
	 * there ACS is in the CDB
	 * 
	 * @throws Exception
	 */
	public void testACSAS() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"ACS");
		instantiateComponentClient();
		assertTrue("Wrong implementation in use (ACS case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Check if the CERN implementation of the AS is chosen when
	 * there CERN is in the CDB
	 * 
	 * @throws Exception
	 */
	public void testCERNAS() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"CERN");
		instantiateComponentClient();
		assertFalse("Wrong implementation in use (CERN case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Check if the ACS implementation is used when the Implementation property is wrong
	 * @throws Exception
	 */
	public void testWrongImplementationProp() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"Wrong property");
		instantiateComponentClient();
		assertTrue("Wrong implementation in use (wrong prop case)",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	/**
	 * Test the creation of a FaultState
	 * 
	 * @throws Exception
	 */
	public void testFaultStateCreation() throws Exception {
		TestUtil.setupAlarmBranch(curDir,"ACS");
		instantiateComponentClient();
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
		instantiateComponentClient();
		ACSAlarmSystemInterface proxy = ACSAlarmSystemInterfaceFactory.createSource("SourceName");
		assertNotNull("Error creating an alarm source",proxy);
	}
	
}
