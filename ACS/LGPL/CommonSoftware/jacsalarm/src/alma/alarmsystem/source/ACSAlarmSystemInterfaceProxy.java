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
package alma.alarmsystem.source;

import java.util.Collection;
import java.util.logging.Logger;

import alma.acs.logging.AcsLogLevel;
import alma.acs.util.XmlNormalizer;

/**
 * ACS implementation of the AlarmSystemInterface.
 * The alarms are published in the logging.
 * 
 * @author acaproni
 *
 */
public class ACSAlarmSystemInterfaceProxy implements ACSAlarmSystemInterface {
	
	// The logger to publish the alarms
	private  Logger m_logger;
	
	// The name of the source
	String name;
	
	/**
	 * The basic constructor
	 * 
	 * @param name The name of the source
	 * @param logger The logger to log alarms in
	 */
	public ACSAlarmSystemInterfaceProxy(String name, Logger logger) {
		if (logger==null) {
			throw new IllegalArgumentException("Invalid null logger in constructor");
		}
		this.name=name;
		m_logger = logger;
		
		// This "connected to the logging" just means we are connected to ACS lightweight alarm system based on logs.
		// Better not change / clarify it now, would make lots of tests fail.
		m_logger.fine("Alarm source of "+name+" connected to the logging");
	}
	
	/**
	 * Set the source name.
	 * @param newSourceName the source name.
	 */
	public void setSourceName(String newSourceName) {
		name = newSourceName;
	}
	
	/**
	 * Get the source name.
	 * @return the source name.
	 */
	public String getSourceName() {
		return name;
	}
	
	/**
	 * Close and deallocate resources.
	 */
	public void close() {}
	
	/**
	 * Push a fault state.
	 * @param state the fault state change to push.
	 * @throws ASIException if the fault state can not be pushed.
	 */
	public void push(ACSFaultState state) {
		logFaultState(state);
	}
	
	/**
	 * Push a collection of fault states.
	 * @param states
	 * @throws ASIException if the fault state collection can not be pushed.
	 */
	public void push(Collection<ACSFaultState> states) {
		if (states==null || states.size()==0) {
			return;
		}
		
		for (ACSFaultState acsFaultState : states) {
			push(acsFaultState);
		}
	}
	
	/**
	 * Push the set of active fault states.
	 * @param active the active fault states.
	 * @throws ASIException if the fault state active list can not be pushed.
	 */
	public void pushActiveList(Collection<ACSFaultState> active) {
		if (active==null || active.size()==0) {
			return;
		}
	    push(active);
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
		sb.append(fs.getFamily()+','+fs.getMember()+','+fs.getCode()+'>');
		sb.append(" "+fs.getDescriptor());
		m_logger.log(AcsLogLevel.ALERT,XmlNormalizer.normalize(sb.toString()));
	}
}

