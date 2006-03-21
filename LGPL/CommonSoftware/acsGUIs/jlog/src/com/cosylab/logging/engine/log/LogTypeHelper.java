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

import com.cosylab.logging.engine.log.LogEntryXML;

/**
 * @author acaproni
 *
 * This class contains all the definitions needed for the log type.
 * It means the constants, the strings describing the fields
 * and the icons.
 * 
 * This class is trivial of course but I prefer a centralized management
 * of these constants to avoid to build the same constants everywhere
 * in the code especially to avoid to instantiated and load the same
 * array of icons in a lot of different places
 */
public class LogTypeHelper {
    public static final short ENTRYTYPE_TRACE = 0;
    public static final short ENTRYTYPE_DEBUG = 1;
    public static final short ENTRYTYPE_INFO = 2;
    public static final short ENTRYTYPE_NOTICE = 3;
    public static final short ENTRYTYPE_WARNING = 4;
    public static final short ENTRYTYPE_ERROR = 5;
    public static final short ENTRYTYPE_CRITICAL = 6;
    public static final short ENTRYTYPE_ALERT = 7;
    public static final short ENTRYTYPE_EMERGENCY = 8;
    
    // The total amount of different log types
    private static short NUMBER_OF_ENTRYTYPES = 9;
    
    /**
     * The description of each log type
     * 
     * The length of this array must be the same of logTypeIcons
     * and equal to NUMBER_OF_ENTRYTYPES
     */
    private static final String[] logEntryTypes = { 
            "Trace", 
            "Debug", 
            "Info", 
            "Notice", 
            "Warning", 
            "Error", 
            "Critical", 
            "Alert", 
            "Emergency" 
    };
    
    /**
     * The name of the icons
     * The will be loaded by the getIcon method
     */
    private static String[] logIconNames= {
        "/trace.gif",
        "/debug.gif",
        "/information.gif",
        "/notice.gif",
        "/warning.gif",
        "/error.gif",
        "/critical.gif",
        "/alert.gif",
        "/emergency.gif"   
    };
    
    /**
     * The icons for each type of log
     * Null at the beginning, they are loaded when needed by getIcon
     * 
     * In this way it is possible to execute the test without
     * X-Window running.
     * This is quite inefficent ad force us to test the value of the icons
     * in the getIconsVSize and in the getAllIcons methods...
     * 
     * 
     * The length of this array must be the same of logEntryTypes
     * and equal to NUMBER_OF_ENTRYTYPES
     */
    private static ImageIcon[] logTypeIcons = {
            null, // trace.gif
            null, // debug.gif
            null, // information.gif
            null, // notice.gif
            null, // warning.gif"
            null, // error.gif
            null, // critical.gif
            null, // alert.gif
            null  // emergency.gif
    };
     
    /**
     * Return the number of different log types defined
     * 
     * @return The total number of log types
     */
    public static int getNumberOfTypes() {
        return NUMBER_OF_ENTRYTYPES;
    }
    
    /**
     * Return the type (int) of the log as a String
     * 
     * @param logType The type of the log
     *  
     * @return The Sttring describing the type of the log 
     */
    public static String getLogTypeDescription(int logType) {
        if (logType<0 || logType>=NUMBER_OF_ENTRYTYPES) {
            throw new IllegalArgumentException("The parameter is not a valid log type number");
        }
        return logEntryTypes[logType];
    }
    
    /**
     * Return the type of the log as a String
     * 
     * @param logType The type (Integer) of the log
     *  
     * @return The Sttring describing the type of the log 
     */
    public static String getLogTypeDescription(Integer logType) {
        return getLogTypeDescription(logType.intValue());
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
        Integer type = (Integer)log.getField(LogEntryXML.FIELD_ENTRYTYPE);
        if (type==null || type.intValue()<0 || type.intValue()>NUMBER_OF_ENTRYTYPES) {
            return "Undeclared";
        } else {
            return logEntryTypes[type.intValue()];
        }
    }
    
    /**
     * Parse the parameter to check wich is its type
     * 
     * @param logDescr The description of a log as a String
     * 
     * @return The ENTRYTYPE described by the parameter 
     *         (null if the string does not match with any log description)
     */
    public static Integer parseLogTypeDescription(String logDescr) {
        for (int t=0; t<logEntryTypes.length; t++) {
            if (logDescr.equals(logEntryTypes[t])) {
                return new Integer(t);
            }
        }
        // Description not found!
        return null; 
    }
    
    /**
     * Return an array containing the description of each type
     * of log
     * 
     * @return An array with all the types of logs
     * 
     */
    public static String[] getAllTypesDescriptions() {
        return logEntryTypes;
    }
    
    /**
     * Return the icon related to a specific type
     * 
     * @param type The type of the icon
     * 
     * @return An ImageIcon of the specified type
     */
    public static ImageIcon getIcon(int type) {
        if (type<0 || type>=NUMBER_OF_ENTRYTYPES) {
            throw new IllegalArgumentException("The parameter is not a valid log type number");
        }
        // Each icon is loaded the first time someone need it
        if (logTypeIcons[type]==null) {
            logTypeIcons[type]=new ImageIcon(LogTypeHelper.class.getResource(logIconNames[type]));
        }
        return logTypeIcons[type];
    }
    
    /**
     * Return the array with all the icons
     * 
     * @return The array of the icons
     */
    public static ImageIcon[] getAllIcons() {
        // While the icons are static and null at the beginning
        // we force the load of the icons
        for (int t=0; t<logIconNames.length; t++) {
            getIcon(t);
        }
        return logTypeIcons;
    }
    
    /**
     * Evaluates and returns the highest vertical dimension
     * of the icons (it is needed to optimize the display of some widgets)
     *  
     * @return The max vertical dimension of the icons 
     */
    public static int getIconsVSize() {
        int vDim=0;
        for (int t=0; t<logTypeIcons.length; t++) {
            // Load the Images if they are null
            if (logTypeIcons[t]==null) {
                getIcon(t);
            }
            if (logTypeIcons[t].getIconHeight()>vDim) vDim=logTypeIcons[t].getIconHeight();
        }
        return vDim;
    }
}
