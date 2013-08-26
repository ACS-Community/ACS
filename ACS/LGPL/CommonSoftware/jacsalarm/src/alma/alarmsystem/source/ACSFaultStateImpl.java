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

import java.sql.Timestamp;
import java.util.Properties;

/**
 * The implementation of the ACSFaultState.
 * It is used only in the ACS implementation of the AS so the implementation of 
 * some methods is empty
 * 
 * @see ACSFaultState
 * @author acaproni
 *
 */
public class ACSFaultStateImpl implements ACSFaultState {
	// The triplet FS = <FF, FM, FC>
	int FC; // FaultCode
	String FF; // FaultFamily
	String FM; // FaultMember
	
	// The desription of the alarm
	String descriptor;
	
	public ACSFaultStateImpl() {
		FF=FM=descriptor="";
		FC=0;
	}
	
	public ACSFaultStateImpl(String family, String member, int code) {
		FF=family;
		FM=member;
		FC=code;
		descriptor="";
	}

	/**
	 * @see ACSFaultState
	 */
	public void setCode(int faultCode) {
		FC=faultCode;
	}

	/**
	 * @see ACSFaultState
	 */
	public int getCode() {
		return FC;
	}

	/**
	 * @see ACSFaultState
	 */
	public void setDescriptor(String descriptor) {
		this.descriptor=descriptor;
	}

	/** 
	 * @see ACSFaultState
	 */
	public String getDescriptor() {
		return descriptor;
	}

	/** 
	 * @see ACSFaultState
	 */
	public void setFamily(String faultFamily) {
		FF=faultFamily;
	}

	/** 
	 * @see ACSFaultState
	 */
	public String getFamily() {
		return FF;
	}

	/** 
	 * @see ACSFaultState
	 */
	public void setMember(String faultMember) {
		FM=faultMember;
	}

	/** 
	 * @see ACSFaultState
	 */
	public String getMember() {
		return FM;
	}

	/** 
	 * Not used in ACS
	 * @see ACSFaultState
	 */
	public void setUserProperties(Properties properties) {	}

	/** 
	 * Not used in ACS
	 * @see ACSFaultState
	 */
	public Properties getUserProperties() {
		return null;
	}

	/** 
	 * Not used in ACS
	 * @see ACSFaultState
	 */
	public void setUserTimestamp(Timestamp timestamp) {}

	/** 
	 * Not used in ACS
	 * @see ACSFaultState
	 */
	public Timestamp getUserTimestamp() {
		return null;
	}

	/**
	 * Not used in ACS
	 * @see ACSFaultState
	 */
	public boolean getActivatedByBackup() { 
		return false;
	}

	/**
	 * Not used in ACS
	 * @see ACSFaultState
	 */
	public void setActivatedByBackup(boolean newActivatedByBackup) {}

	/**
	 * Not used in ACS
	 * @see ACSFaultState
	 */
	public boolean getTerminatedByBackup() {
		return true;
	}

	/**
	 * Not used in ACS
	 * @see ACSFaultState
	 */
	public void setTerminatedByBackup(boolean newTerminatedByBackup) {}
	
}
