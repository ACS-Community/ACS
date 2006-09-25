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
 * @version $Id: SendTest.java,v 1.2 2006/09/25 10:21:40 acaproni Exp $
 * @since    
 */

package alma.lasersource.test;

import org.omg.CORBA.ORB;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;

import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

import junit.framework.TestCase;

import alma.acs.logging.ClientLogManager;

import java.util.Properties;
import java.util.logging.Logger;

import java.sql.Timestamp;

public class SendTest extends TestCase {
	
	public SendTest() {
		super("SendTest");
	}
	
	public void setUp() throws Exception {
		super.setUp();
		ORB orb = TestUtil.getORB();
		assertNotNull("Error getting the orb",orb);
		Manager manager =TestUtil. getManager();
		assertNotNull("Error getting the manager",manager);
		Logger logger = TestUtil.getLogger(getClass().getName());
		assertNotNull("Error getting the logger",logger);
		ACSAlarmSystemInterfaceFactory.init(orb,manager,logger);
		assertFalse("Using ACS implementation instead of CERN",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
	}
	
	public void tearDown() throws Exception {
		super.tearDown();
		ACSAlarmSystemInterfaceFactory.done();
	}
	
	public void testSend() throws Exception {
		String faultFamily="AlarmSource";
		String faultMember ="ALARM_SOURCE_ANTENNA";
		int faultCode=1;
		String faultState=ACSFaultState.ACTIVE;
		ACSAlarmSystemInterface alarmSource;
		try {
			alarmSource = ACSAlarmSystemInterfaceFactory.createSource();
			ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(
					faultFamily, faultMember, faultCode);
			fs.setDescriptor(faultState);
			fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

			Properties props = new Properties();
			props.setProperty(ACSFaultState.ASI_PREFIX_PROPERTY, "prefix");
			props.setProperty(ACSFaultState.ASI_SUFFIX_PROPERTY, "suffix");
			props.setProperty("TEST_PROPERTY", "TEST_VALUE");
			fs.setUserProperties(props);

			alarmSource.push(fs);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			throw e;
		}
	}

}
