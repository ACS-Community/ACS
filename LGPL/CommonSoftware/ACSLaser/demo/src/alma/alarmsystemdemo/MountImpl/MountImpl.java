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
package alma.alarmsystemdemo.MountImpl;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.ComponentImplBase;
import alma.alarmsystemdemo.MountOperations;
import alma.alarmsystemdemo.Antenna;
import alma.alarmsystemdemo.AntennaOperations;
import alma.alarmsystemdemo.AntennaHelper;

import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import java.sql.Timestamp;
import java.util.Properties;

class MountImpl extends ComponentImplBase implements MountOperations {
	Antenna antenna;
	public void faultMount() {
		m_containerServices.getAlarmSource().raiseAlarm("Mount","ALARM_SOURCE_MOUNT",1);
		try { 
			Thread.sleep(5000);
		} catch (Exception e) {}
		Antenna ant = getAntenna();
		if (ant!=null) {
			ant.faultAntenna();
		}
	}
	public void terminate_faultMount() {
		m_containerServices.getAlarmSource().clearAlarm("Mount","ALARM_SOURCE_MOUNT",1);
		try { 
			Thread.sleep(5000);
		} catch (Exception e) {}
		Antenna ant = getAntenna();
		if (ant!=null) {
			ant.terminate_faultAntenna();
		}
	}
	
	private Antenna getAntenna() {
		if (this.antenna==null) {
			org.omg.CORBA.Object cmp = null;
			try
			{
				cmp = m_containerServices.getComponent("ALARM_SOURCE_ANTENNA");
				antenna = AntennaHelper.narrow(cmp);
			}
			catch (Exception ex)
			{}
		}
		return antenna;
	}
}
