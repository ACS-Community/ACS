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
package alma.alarmsystemdemo.MFImpl;

import java.sql.Timestamp;
import java.util.Properties;

import alma.acs.component.ComponentImplBase;
import alma.alarmsystemdemo.MFOperations;

import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;

import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.FaultStateCreationErrorEx;
import  alma.acsErrTypeAlarmSourceFactory.SourceCreationErrorEx;

public class MFImpl extends ComponentImplBase implements MFOperations {
	
	private final String FF="MF";
	private final String FM="ALARM_SOURCE_MF";
	private final int FC=0;
	
	private ACSAlarmSystemInterface alarmSource=null;
	private ACSFaultState[] faultStates=null;
	
	/**
	 * Init i.e. create the fault state and the source
	 * 
	 * @param family The FaultFamily
	 * @param member The FaultMember
	 * @param code The FaultCode
	 */
	private void init(String family, String member, int code) 
		throws ACSASFactoryNotInitedEx, SourceCreationErrorEx, FaultStateCreationErrorEx {
		if (alarmSource==null) {
				alarmSource=ACSAlarmSystemInterfaceFactory.createSource(this.name());
		}
		if (faultStates==null) {
			faultStates = new ACSFaultState[5];
		}
		for (int t=0; t<faultStates.length; t++) {
			if (faultStates[t]==null) {
				faultStates[t] = ACSAlarmSystemInterfaceFactory.createFaultState(
						family, member, code+t);
				if (faultStates[t]==null) {
					throw new NullPointerException("Error creating the fault state");
				}
				Properties props = new Properties();
				props.setProperty(ACSFaultState.ASI_PREFIX_PROPERTY, "prefix");
				props.setProperty(ACSFaultState.ASI_SUFFIX_PROPERTY, "suffix");
				props.setProperty("TEST_PROPERTY", "TEST_VALUE");
				faultStates[t].setUserProperties(props);
			}
		}
		
		
	}
	
	/**
	 * Send nFaults ACTIVE alarms
	 * 
	 * @param nFaults The number of alarms to send
	 */
	public void multiFault() {
		if (faultStates==null || alarmSource==null) {
			try {
				init(FF,FM,FC);
			} catch (Exception e) {
				System.out.println("Error initing alarm system objects: "+e.getMessage());
				e.printStackTrace();
				throw new IllegalStateException("The alarm system objects are not initialized!",e);
			}
		}
		for (int t=0; t<faultStates.length; t++) {
			faultStates[t].setDescriptor(ACSFaultState.ACTIVE);
			faultStates[t].setUserTimestamp(new Timestamp(System.currentTimeMillis()));
			alarmSource.push(faultStates[t]);
			try {
				Thread.sleep(10000);
			} catch (InterruptedException ie) {}
		}
		

	}
	
	/**
	 * Send nFaults TERMINATE alarms
	 *
	 * @param nFaults The number of alarms to send
	 */
	public void terminate_multiFault() {
		if (faultStates==null || alarmSource==null) {
			try {
				init(FF,FM,FC);
			} catch (Exception e) {
				System.out.println("Error initing alarm system objects: "+e.getMessage());
				e.printStackTrace();
				throw new IllegalStateException("The alarm system objects are not initialized!",e);
			}
		}
		
		for (int t=0; t<faultStates.length; t++) {
			faultStates[t].setDescriptor(ACSFaultState.TERMINATE);
			faultStates[t].setUserTimestamp(new Timestamp(System.currentTimeMillis()));
			alarmSource.push(faultStates[t]);
			try {
				Thread.sleep(10000);
			} catch (InterruptedException ie) {}
		}
	}
}
