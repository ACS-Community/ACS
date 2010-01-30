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
package com.cosylab.logging.engine.log;

import com.cosylab.logging.engine.log.LogField;

import alma.acs.logging.level.AcsLogLevelDefinition;

/**
 * @author acaproni
 *
 * This class contains all the definitions needed for the log type.
 * It means the constants, the strings describing the fields
 * and the icons.
 * This enum extends the AcsLogLevelDefinition with informations
 * needed by the GUI
 * 
 */
public enum LogTypeHelper {
	/**
	 * The types of the logs.
	 * 
	 * We do want to reuse these Integers instead of creating new Integers 
	 * with the same value
	 * We need that to reduce the amount of memory used by the software.
	 * 
	 */
    TRACE(AcsLogLevelDefinition.TRACE,"Trace"),
    DELOUSE(AcsLogLevelDefinition.DELOUSE,"Delouse"),
    DEBUG(AcsLogLevelDefinition.DEBUG,"Debug"),
    INFO(AcsLogLevelDefinition.INFO,"Info"),
    NOTICE(AcsLogLevelDefinition.NOTICE,"Notice"),
    WARNING(AcsLogLevelDefinition.WARNING,"Warning"),
    ERROR(AcsLogLevelDefinition.ERROR,"Error"),
    CRITICAL(AcsLogLevelDefinition.CRITICAL,"Critical"),
    ALERT(AcsLogLevelDefinition.ALERT,"Alert"),
    EMERGENCY(AcsLogLevelDefinition.EMERGENCY,"Emergency");
	
	// The string describing this entry type
	public final String logEntryType;
	
	// The ACSLogLevelDefinition
	public final AcsLogLevelDefinition acsCoreLevel;
    
    /**
     * Constructor
     * 
     * @param AcsCoreLevel The ACS log level definition
     * @param logEntryType The entry type
     * @param iconName The name of the icon
     */
    private LogTypeHelper(AcsLogLevelDefinition acsCoreLvl, String logEntryType) {
    	this.acsCoreLevel=acsCoreLvl;
    	this.logEntryType=logEntryType;
    }
    
    /**
     * Return the type of the log of a LogEntryXML
     * 
     * @param log The LogEntryXML we need the type
     * 
     * @return A String describing the type of the log 
     *         ("Undeclared" if the type is unknown)
     */
    public static String getLogTypeDescription(LogEntryXML log) {
        if (log==null) {
            throw new IllegalArgumentException("Impossible to get the type of a null log");
        }
        Integer type = (Integer)log.getField(LogField.ENTRYTYPE);
        if (type==null || type.intValue()<0 || type.intValue()>LogTypeHelper.values().length) {
            return "Undeclared";
        } else {
            return LogTypeHelper.values()[type.intValue()].logEntryType;
        }
    }
    
    /**
     * Return an enumerated with the description equal to the parameter
     * 
     * @param logDescr The description of a log as a String
     * 
     * @return The LogTypeHelper described by the parameter 
     *         (null if the string does not match with any log description)
     */
    public static LogTypeHelper fromLogTypeDescription(String logDescr) {
        for (LogTypeHelper logType: LogTypeHelper.values()) {
            if (logDescr.equals(logType.logEntryType)) {
                return logType;
            }
        }
        // Description not found!
        return null; 
    }
    
    /**
     * Return an enumerated with the ACS log level equal to the parameter
     * 
     * @param acsLevel The ACS log level
     * 
     * @return The LogTypeHelper with the passed ACS log level 
     *         (null if the string does not match with any log description)
     */
    public static LogTypeHelper fromAcsCoreLevel(AcsLogLevelDefinition acsLevel) {
        for (LogTypeHelper logType: LogTypeHelper.values()) {
            if (acsLevel==logType.acsCoreLevel) {
                return logType;
            }
        }
        // ACS log level not found!
        return null; 
    } 
    
    public AcsLogLevelDefinition getAcsCoreLevel() {
    	return acsCoreLevel;
    }
    
    public String toString() {
    	return logEntryType;
    }
}
