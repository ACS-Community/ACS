/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
package alma.acs.logging.level;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.AcsLogging.alma.LogLevels.TRACE_VAL;
import alma.AcsLogging.alma.LogLevels.TRACE_NAME;
import alma.AcsLogging.alma.LogLevels.DEBUG_VAL;
import alma.AcsLogging.alma.LogLevels.DEBUG_NAME;
import alma.AcsLogging.alma.LogLevels.INFO_VAL;
import alma.AcsLogging.alma.LogLevels.INFO_NAME;
import alma.AcsLogging.alma.LogLevels.NOTICE_VAL;
import alma.AcsLogging.alma.LogLevels.NOTICE_NAME;
import alma.AcsLogging.alma.LogLevels.WARNING_VAL;
import alma.AcsLogging.alma.LogLevels.WARNING_NAME;
import alma.AcsLogging.alma.LogLevels.ERROR_VAL;
import alma.AcsLogging.alma.LogLevels.ERROR_NAME;
import alma.AcsLogging.alma.LogLevels.CRITICAL_VAL;
import alma.AcsLogging.alma.LogLevels.CRITICAL_NAME;
import alma.AcsLogging.alma.LogLevels.ALERT_VAL;
import alma.AcsLogging.alma.LogLevels.ALERT_NAME;
import alma.AcsLogging.alma.LogLevels.EMERGENCY_VAL;
import alma.AcsLogging.alma.LogLevels.EMERGENCY_NAME;
import alma.AcsLogging.alma.LogLevels.OFF_VAL;
import alma.AcsLogging.alma.LogLevels.OFF_NAME;

/**
 * An enum with the log levels, defined in the IDL. It is a convenience
 * class encapsulating the IDL constants of the log levels.
 * 
 * Each itemo of the enumerated is composed of a name and a value, 
 * both read directly from the IDL definition LogLevels:
 *   -name  comes from <type>_NAME 
 *   -value comes from <type>_VAL.
 * 
 * @author acaproni
 *
 */
public enum AcsLogLevelDefinition {
	TRACE(TRACE_VAL.value,TRACE_NAME.value),
	DEBUG(DEBUG_VAL.value,DEBUG_NAME.value),
	INFO(INFO_VAL.value,INFO_NAME.value),
	NOTICE(NOTICE_VAL.value,NOTICE_NAME.value),
	WARNING(WARNING_VAL.value,WARNING_NAME.value),
	ERROR(ERROR_VAL.value,ERROR_NAME.value),
	CRITICAL(CRITICAL_VAL.value,CRITICAL_NAME.value),
	ALERT(ALERT_VAL.value,ALERT_NAME.value),
	EMERGENCY(EMERGENCY_VAL.value,EMERGENCY_NAME.value),
	OFF(OFF_VAL.value,OFF_NAME.value);
	
	/**
	 * The value of the log (the integer read from the IDL)
	 */
	public int value;
	
	/**
	 * The name of the log (the string read from the IDL)
	 */
	public String name;
	
	/**
	 * Constructor
	 * 
	 * @param val The value of the log level
	 * @param name The name of the log level
	 */
	private AcsLogLevelDefinition(int val, String name) {
		value=val;
		this.name=name;
	}
	
	/**
	 * Return a log level given its integer value
	 * 
	 * @param val The value of the log level
	 * @return The log level having val as its value
	 */
	public static AcsLogLevelDefinition fromInteger(int val) throws AcsJIllegalArgumentEx {
		for (AcsLogLevelDefinition def: AcsLogLevelDefinition.values()) {
			if (def.value==val) {
				return def;
			}
		}
		AcsJIllegalArgumentEx ex = new AcsJIllegalArgumentEx();
		ex.setErrorDesc("Invalid value of log level "+val);
		throw ex;
	}
	
	/**
	 * Return a log level, given its name.
	 * 
	 * @param name The (not null, not empty) name of the log level
	 * @return The log level having the name as its name
	 */
	public static AcsLogLevelDefinition fromName(String name) throws AcsJIllegalArgumentEx {
		if (name==null) {
			AcsJIllegalArgumentEx ex = new AcsJIllegalArgumentEx();
			ex.setErrorDesc("Name of log level can't be null");
			throw ex;
		}
		for (AcsLogLevelDefinition def: AcsLogLevelDefinition.values()) {
			if (def.name.equals(name)) {
				return def;
			}
		}
		AcsJIllegalArgumentEx ex = new AcsJIllegalArgumentEx();
		ex.setErrorDesc("Invalid name of log level "+name);
		throw ex;
	}
	
}
