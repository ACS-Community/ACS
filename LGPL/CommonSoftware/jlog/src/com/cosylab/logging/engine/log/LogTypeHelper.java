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

import javax.swing.ImageIcon;

import com.cosylab.logging.engine.log.ILogEntry.Field;

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
    TRACE(AcsLogLevelDefinition.TRACE,"Trace","/trace.gif"),
    DEBUG(AcsLogLevelDefinition.DEBUG,"Debug","/debug.gif"),
    INFO(AcsLogLevelDefinition.INFO,"Info","/information.gif"),
    NOTICE(AcsLogLevelDefinition.NOTICE,"Notice","/notice.gif"),
    WARNING(AcsLogLevelDefinition.WARNING,"Warning","/warning.gif"),
    ERROR(AcsLogLevelDefinition.ERROR,"Error","/error.gif"),
    CRITICAL(AcsLogLevelDefinition.CRITICAL,"Critical","/critical.gif"),
    ALERT(AcsLogLevelDefinition.ALERT,"Alert","/alert.gif"),
    EMERGENCY(AcsLogLevelDefinition.EMERGENCY,"Emergency","/emergency.gif");
	
	// The string describing this entry type
	public String logEntryType;
	
	// The icon to show in the GUI for a given log type
	public ImageIcon icon;
	
	// The ACSLogLevelDefinition
	public AcsLogLevelDefinition acsCoreLevel;
    
    /**
     * Constructor
     * 
     * @param AcsCoreLevel The ACS log level definition
     * @param logEntryType The entry type
     * @param iconName The name of the icon
     */
    private LogTypeHelper(AcsLogLevelDefinition acsCoreLvl, String logEntryType, String iconName) {
    	this.acsCoreLevel=acsCoreLvl;
    	this.logEntryType=logEntryType;
    	this.icon=new ImageIcon(LogTypeHelper.this.getClass().getResource(iconName));
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
        Integer type = (Integer)log.getField(Field.ENTRYTYPE);
        if (type==null || type.intValue()<0 || type.intValue()>LogTypeHelper.values().length) {
            return "Undeclared";
        } else {
            return LogTypeHelper.values()[type.intValue()].logEntryType;
        }
    }
    
    /**
     * Parse the parameter to check which is its type
     * 
     * @param logDescr The description of a log as a String
     * 
     * @return The ENTRYTYPE described by the parameter 
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
     * Evaluates and returns the highest vertical dimension
     * of the icons (it is needed to optimize the display of some widgets)
     *  
     * @return The max vertical dimension of the icons 
     */
    public static int getIconsVSize() {
        int vDim=0;
        for (LogTypeHelper logType: LogTypeHelper.values()) {
            // Load the Images if they are null
            if (logType.icon.getIconHeight()>vDim) vDim=logType.icon.getIconHeight();
        }
        return vDim;
    }
}
