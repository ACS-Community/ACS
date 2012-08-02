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
package alma.alarmsystemdemo.AntennaImpl;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.alarmsystemdemo.AntennaOperations;
import alma.acs.component.ComponentImplBase;

import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

import java.sql.Timestamp;
import java.util.Properties;

class AntennaImpl extends ComponentImplBase implements AntennaOperations
{
	public void faultAntenna() {
		try {
			m_containerServices.getAlarmSource().raiseAlarm("Antenna","ALARM_SOURCE_ANTENNA",1);
		}catch (AcsJContainerServicesEx ex) {
			// This can never happen actually but the exception is still present in 
			// ContainerServices.///
		}
	} 
	public void terminate_faultAntenna() {
		try {
			m_containerServices.getAlarmSource().clearAlarm("Antenna","ALARM_SOURCE_ANTENNA",1);	
		} catch (AcsJContainerServicesEx ex) {
			// This can never happen actually but the exception is still present in 
			// ContainerServices.///
		}
	}
}
