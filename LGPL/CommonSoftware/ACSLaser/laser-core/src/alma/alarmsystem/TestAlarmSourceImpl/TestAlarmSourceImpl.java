/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
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
package alma.alarmsystem.TestAlarmSourceImpl;

import java.sql.Timestamp;
import java.util.Properties;

import cern.laser.source.alarmsysteminterface.ASIException;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterface;
import cern.laser.source.alarmsysteminterface.AlarmSystemInterfaceFactory;
import cern.laser.source.alarmsysteminterface.FaultState;
import alma.acs.component.ComponentImplBase;
import alma.alarmsystem.TestAlarmSourceOperations;

/**
 * A component that acts as a generic source of alarms for testing purposes.
 * 
 * @author Klemen Zagar
 */
public class TestAlarmSourceImpl extends ComponentImplBase implements
		TestAlarmSourceOperations {

	private int count = 1;
	
	public void set_count(int count) {
		this.count = count;
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see alma.alarmsystem.TestAlarmSourceOperations#send_alarm(int,
	 *      java.lang.String, java.lang.String, int)
	 */
	public void active_alarm(String faultFamily, String faultMember, int faultCode) {
		send_alarm(faultFamily, faultMember, faultCode, FaultState.ACTIVE);
	}

	public void terminate_alarm(String faultFamily, String faultMember, int faultCode) {
		send_alarm(faultFamily, faultMember, faultCode, FaultState.TERMINATE);	
	}

	public void instant_alarm(String faultFamily, String faultMember, int faultCode) {
		send_alarm(faultFamily, faultMember, faultCode, FaultState.INSTANT);	
	}

	public void change_alarm(String faultFamily, String faultMember, int faultCode) {
		send_alarm(faultFamily, faultMember, faultCode, FaultState.CHANGE);	
	}

	public void send_alarm(String faultFamily, String faultMember, int faultCode, String faultState) {
		int count = this.count;
		AlarmSystemInterface alarmSource;
		try {
			alarmSource = AlarmSystemInterfaceFactory.createSource(this.name());
			while (count-- > 0) {
				FaultState fs = AlarmSystemInterfaceFactory.createFaultState(
						faultFamily, faultMember, faultCode);
				fs.setDescriptor(faultState);
				fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

				Properties props = new Properties();
				props.setProperty(FaultState.ASI_PREFIX_PROPERTY, "prefix");
				props.setProperty(FaultState.ASI_SUFFIX_PROPERTY, "suffix");
				props.setProperty("TEST_PROPERTY", "TEST_VALUE");
				fs.setUserProperties(props);

				alarmSource.push(fs);
			}
		} catch (ASIException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
