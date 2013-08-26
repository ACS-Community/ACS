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
package alma.acs.alarmsystem.binding;

import java.util.Collection;
import java.util.logging.Logger;

import cern.laser.source.alarmsysteminterface.impl.AlarmSystemInterfaceProxy;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.ASIException;

import alma.acs.logging.AcsLogLevel;
import alma.acs.util.XmlNormalizer;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSFaultState;

public class ACSLaserSource extends AlarmSystemInterfaceProxy implements ACSAlarmSystemInterface {
	
	// The logger
	private Logger logger=null;
	
	public ACSLaserSource(String sourceName, Logger logger) throws ASIException {
		super(sourceName);
		if (logger==null) {
			throw new IllegalArgumentException("Invalid null logger in constructor");
		}
		this.logger=logger;
	}
	/**
	 * @see cern.laser.source.alarmsysteminterface.AlarmSystemInterface
	 */
	@Override
	public synchronized void push(ACSFaultState state){
		try {
			this.push((FaultState)state);
			logFaultState(state);
		} catch (Throwable t) {
			StringBuilder logStr = new StringBuilder("Exception "+t.getMessage()+" throwing alarm <");
			logStr.append(state.getFamily()+','+state.getMember()+','+state.getCode()+">");
			logStr.append(" "+state.getDescriptor());
			logger.log(AcsLogLevel.ERROR,logStr.toString(),t);
		}
	}
	
	@Override
	public synchronized void push(Collection states) {
		try {
			super.push(states);
		} catch (Throwable t) {
			logger.log(AcsLogLevel.ERROR,"Error pushing the collection of alarms",t);
		}
	}
	
	@Override
	public synchronized void pushActiveList(Collection active) {
		try {
			super.pushActiveList(active);
		} catch (Throwable t) {
			logger.log(AcsLogLevel.ERROR,"Error pushing the activeList of alarms",t);
		}
	}
	
	/**
	 * Write the ACSFaultState in the log
	 * 
	 * @param fs The ACSFaultState to log
	 */
	private void logFaultState(ACSFaultState fs) {
		if (fs==null) {
			return;
		}
		StringBuilder sb = new StringBuilder("Alarm sent: <");
		sb.append(fs.getFamily()+','+fs.getMember()+','+fs.getCode()+">");
		sb.append(" "+fs.getDescriptor());
		logger.log(AcsLogLevel.DEBUG,XmlNormalizer.normalize(sb.toString()));
	}
}
