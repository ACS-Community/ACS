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

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.alarmsystemdemo.MFOperations;

import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;

import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.FaultStateCreationErrorEx;
import  alma.acsErrTypeAlarmSourceFactory.SourceCreationErrorEx;

/**
 * The component sends 5 alarms with triplets having the same fault family {@link MFImpl#FF} 
 * and fault member {@link MFImpl#FM} and 5 different fault codes {@link MFImpl#FCs} .
 * <P>
 * The purpose of this alarms is to show the effect of the multiplicity reduction rule.
 * 
 * @author acaproni
 *
 */
public class MFImpl extends ComponentImplBase implements MFOperations {
	
	/**
	 * 
	 */
	private final String FF="MF";
	private final String FM="ALARM_SOURCE_MF";
	private final int[] FCs= { 0, 1, 2, 3, 4};
	
	private ACSAlarmSystemInterface alarmSource=null;
	private ACSFaultState[] faultStates=null;
	
	/**
	 * Raise the alarms
	 */
	public void multiFault() {
		for (int fc: FCs) {
			m_containerServices.getAlarmSource().raiseAlarm(FF, FM, fc);
			// Give the user time to see what happens in the alarm panel
			try {
				Thread.sleep(5000);
			} catch (InterruptedException ie) {}
		}
	}
	
	/**
	 * Clear the alarms
	 */
	public void terminate_multiFault() {
		for (int fc: FCs) {
			m_containerServices.getAlarmSource().clearAlarm(FF, FM, fc);
			// Give the user time to see what happens in the alarm panel
			try {
				Thread.sleep(5000);
			} catch (InterruptedException ie) {}
		}
	}
}
