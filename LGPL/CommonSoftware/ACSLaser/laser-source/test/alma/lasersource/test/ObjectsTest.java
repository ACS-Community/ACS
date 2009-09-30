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

/** 
 * @author  almadev   
 * @version $Id: ObjectsTest.java,v 1.6 2009/09/30 15:22:29 acaproni Exp $
 * @since    
 */

package alma.lasersource.test;

import java.util.logging.Logger;

import org.omg.CORBA.ORB;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.logging.ClientLogManager;

import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;
import cern.laser.source.alarmsysteminterface.impl.AlarmSystemInterfaceProxy;

import alma.acsErrTypeAlarmSourceFactory.ErrorGettingDALEx;

public class ObjectsTest extends ComponentClientTestCase {
	
	private String faultFamily = "TestFaultFamily";
	private String faultMember = "TestFaultMember";
	private Integer faultCode = 1001;
	
	public ObjectsTest() throws Exception {
		super("ObjectsTest");
	}

	public void setUp() throws Exception {
		super.setUp();
		assertFalse("Using ACS implementation instead of CERN",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	public void tearDown() throws Exception {
		super.tearDown();
	}
	
	public void testFaultStateType() throws Exception {
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(
				faultFamily, faultMember, faultCode);
		String descriptor = "A description for this FS";
		assertNotNull("Error creting the FS",fs);
		assertTrue("The FS has wrong class type",fs instanceof FaultStateImpl);
		assertEquals("Wrong FF",faultFamily,fs.getFamily());
		assertEquals("Wrong FM",faultMember,fs.getMember());
		assertEquals("Wrong FC",faultCode,new Integer(fs.getCode()));
		fs.setDescriptor(descriptor);
		assertEquals("Wrong descriptor",descriptor,fs.getDescriptor());
	}
	
	public void testSourceType() throws Exception{
		ACSAlarmSystemInterface source;
		String sourceName=this.getName();
		try {
			source = ACSAlarmSystemInterfaceFactory.createSource(sourceName);
		} catch (Exception e) {
			System.out.println("Error creating the source: "+e.getMessage());
			e.printStackTrace();
			throw e;
		}
		assertNotNull("Error creating the source",source);
		assertTrue("The source has wrong class type",source instanceof AlarmSystemInterfaceProxy);
	}
	
	
}
