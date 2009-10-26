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
import alma.AcsLogLevels.DELOUSE_NAME;
import alma.AcsLogLevels.DELOUSE_VAL;
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
import alma.maci.loggingconfig.UnnamedLogger;
import alma.maci.loggingconfig.types.LogLevel;

/**
 * An enum with the log levels, defined in the IDL. It is a convenience
 * class encapsulating the IDL constants of the log levels.
 * <p>
 * Each item of the enumerated is composed of a name and a value, 
 * both read directly from the IDL definition LogLevels: <br>
 *   -name  comes from <type>_NAME <br>
 *   -value comes from <type>_VAL.
 * <p>
 * There are also a number of methods to convert to/from/between the log level definitions
 * in IDL, XSD, and this enum.
 * 
 * @author acaproni
 *
 */
public enum AcsLogLevelDefinition {
	
////////////////////////////////////////////////////////////////////////
//            Enum related code	
////////////////////////////////////////////////////////////////////////
	
	TRACE(TRACE_VAL.value,TRACE_NAME.value),
	DELOUSE(DELOUSE_VAL.value,DELOUSE_NAME.value),
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
	
	/**
	 * Return a log level given its integer value.
	 * @param val The value of the log level
	 * @return The log level having val as its value
	 * @throws AcsJIllegalArgumentEx if <code>val</code> did not match any <code>AcsLogLevelDefinition</code>.
	 */
	public static AcsLogLevelDefinition fromInteger(int val) throws AcsJIllegalArgumentEx {
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
		
	////////////////////////////////////////////////////////////////////////
	//              XSD types	
	////////////////////////////////////////////////////////////////////////
	
	/**
	 * Helper method that converts an xsd defined log level to the matching enum literal.
	 */
	public static AcsLogLevelDefinition fromXsdLogLevel(LogLevel legalLogLevel) throws AcsJIllegalArgumentEx {
		return fromInteger(Integer.parseInt(legalLogLevel.toString()));
	}

	/**
	 * Returns the <code>LogLevel</code> defined in maciidl/ws/config/CDB/schemas/LoggingConfig.xsd 
	 * whose integer value matches this enum's value.
	 * @throws IllegalArgumentException if no matching Xsd-defined level can be found 
	 *            (should never happen as long as IDL and XSD files are aligned)
	 */
	public LogLevel toXsdLevel() {
		return LogLevel.valueOf(Integer.toString(this.value));
	}

	
	public static LogLevel xsdLevelFromInteger(int level) throws AcsJIllegalArgumentEx {
		return fromInteger(level).toXsdLevel();
	}

	/**
	 * Checks if an xsd-defined log level is equal to this IDL-defined level by comparing the underlying integer values.
	 * @throws IllegalArgumentException  if <code>schemaLevel</code> is null.
	 */
	public boolean isEqualXsdLevel(LogLevel schemaLevel) {
		if (schemaLevel == null) {
			throw new IllegalArgumentException("'schemaLevel' must not be null");
		}
		return ( this.value == Integer.parseInt(schemaLevel.toString()) ); 
	}
	
	
	////////////////////////////////////////////////////////////////////////
	//  IDL type
	////////////////////////////////////////////////////////////////////////

	/**
	 * Convenience method that creates the IDL-defined log level struct from 
	 * the XSD-defined levels and the default flag.
	 */
	public static si.ijs.maci.LoggingConfigurablePackage.LogLevels createIdlLogLevelsFromXsd(boolean useDefault, UnnamedLogger xsdLevels) {
		return new si.ijs.maci.LoggingConfigurablePackage.LogLevels(
				useDefault,
				Short.parseShort(xsdLevels.getMinLogLevel().toString()),
				Short.parseShort(xsdLevels.getMinLogLevelLocal().toString()));
	}

	public static UnnamedLogger createXsdLogLevelsFromIdl(si.ijs.maci.LoggingConfigurablePackage.LogLevels idlLevels) throws AcsJIllegalArgumentEx {
		UnnamedLogger xsdLevels = new UnnamedLogger();
		xsdLevels.setMinLogLevel(AcsLogLevelDefinition.xsdLevelFromInteger(idlLevels.minLogLevel));
		xsdLevels.setMinLogLevelLocal(AcsLogLevelDefinition.xsdLevelFromInteger(idlLevels.minLogLevelLocal));
		return xsdLevels;
	}
}
