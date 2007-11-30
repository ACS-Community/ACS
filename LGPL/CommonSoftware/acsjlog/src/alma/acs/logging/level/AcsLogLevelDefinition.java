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
import alma.AcsLogLevels.ALERT_NAME;
import alma.AcsLogLevels.ALERT_VAL;
import alma.AcsLogLevels.CRITICAL_NAME;
import alma.AcsLogLevels.CRITICAL_VAL;
import alma.AcsLogLevels.DEBUG_NAME;
import alma.AcsLogLevels.DEBUG_VAL;
import alma.AcsLogLevels.EMERGENCY_NAME;
import alma.AcsLogLevels.EMERGENCY_VAL;
import alma.AcsLogLevels.ERROR_NAME;
import alma.AcsLogLevels.ERROR_VAL;
import alma.AcsLogLevels.INFO_NAME;
import alma.AcsLogLevels.INFO_VAL;
import alma.AcsLogLevels.NOTICE_NAME;
import alma.AcsLogLevels.NOTICE_VAL;
import alma.AcsLogLevels.OFF_NAME;
import alma.AcsLogLevels.OFF_VAL;
import alma.AcsLogLevels.TRACE_NAME;
import alma.AcsLogLevels.TRACE_VAL;
import alma.AcsLogLevels.WARNING_NAME;
import alma.AcsLogLevels.WARNING_VAL;

/**
 * An enum with the log levels, defined in the IDL. It is a convenience
 * class encapsulating the IDL constants of the log levels.
 * 
 * Each item of the enumerated is composed of a name and a value, 
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
	public final int value;
	
	/**
	 * The name of the log (the string read from the IDL)
	 */
	public final String name;
	
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
	 * Return a log level given its integer value.
	 * <p>
	 * For backward compatibility with CDB entries where often immediateDispatchLevel=31 
	 * and other nonsense is used, we convert values > 11 to OFF instead of throwing the 
	 * exception.
	 * @TODO: Change this with next major release (ACS 8.0) 
	 * 
	 * @param val The value of the log level
	 * @return The log level having val as its value
	 */
	public static AcsLogLevelDefinition fromInteger(int val) throws AcsJIllegalArgumentEx {
		// @TODO remove value fix with next major acs release
		if (val > EMERGENCY.value) {
			val = OFF.value;
		}
		
		for (AcsLogLevelDefinition def : AcsLogLevelDefinition.values()) {
			if (def.value==val) {
				return def;
			}
		}
		AcsJIllegalArgumentEx ex = new AcsJIllegalArgumentEx();
		ex.setVariable("val");
		ex.setValue(Integer.toString(val));
		throw ex;
	}
	
	/**
	 * Return a log level, given its name.
	 * <p>
	 * This method is similar to {@link #valueOf(String)} except that it throws 
	 * an Acs exception instead of a RuntimeException.
	 * @TODO: perhaps implement this method by forwarding to valueOf(name).
	 *  
	 * @param name The (not null, not empty) name of the log level
	 * @return The log level having the name as its name
	 * @throws AcsJIllegalArgumentEx if <code>name</code> is <code>null</code> or not a valid IDL-defined log level name.
	 */
	public static AcsLogLevelDefinition fromName(String name) throws AcsJIllegalArgumentEx {
		for (AcsLogLevelDefinition def : AcsLogLevelDefinition.values()) {
			if (def.name.equals(name)) {
				return def;
			}
		}
		AcsJIllegalArgumentEx ex = new AcsJIllegalArgumentEx();
		ex.setVariable("name");
		ex.setValue(name);			
		throw ex;
	}
	
	/**
	 * <b>Use this method only for testing!</b>
	 * Returns the next log level in the list of enum literals, or <code>null</code>
	 * if there is no next level or the next level would be <code>OFF</code>.
	 */
	public AcsLogLevelDefinition getNextHigherLevel() {
		if (ordinal()+1 < values().length) {
			AcsLogLevelDefinition next = values()[ordinal() + 1];
			if (next != OFF) {
				return next;
			}
		}
		return null;
	}
}
