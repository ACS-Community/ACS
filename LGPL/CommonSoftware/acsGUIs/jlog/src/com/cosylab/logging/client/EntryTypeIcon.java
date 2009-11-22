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
package com.cosylab.logging.client;

import javax.swing.ImageIcon;

import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The icons shown for each log entry type
 * 
 * The icon was part of {@link com.cosylab.logging.engine.log.LogTypeHelper} but has been moved
 * here because it caused a process using the engine to crash if executed
 * without X running or when started with sudo
 * 
 * @author acaproni
 *
 */
public enum EntryTypeIcon {
	
	/**
	 * The icon of each type of the logs.
	 * 
	 */
    TRACE_ICON(LogTypeHelper.TRACE,"/trace.gif"),
    DELOUSE_ICON(LogTypeHelper.DELOUSE,"/delouse.png"),
    DEBUG_ICON(LogTypeHelper.DEBUG,"/debug.gif"),
    INFO_ICON(LogTypeHelper.INFO,"/information.gif"),
    NOTICE_ICON(LogTypeHelper.NOTICE,"/notice.gif"),
    WARNING_ICON(LogTypeHelper.WARNING,"/warning.gif"),
    ERROR_ICON(LogTypeHelper.ERROR,"/error.gif"),
    CRITICAL_ICON(LogTypeHelper.CRITICAL,"/critical.gif"),
    ALERT_ICON(LogTypeHelper.ALERT,"/alert.gif"),
    EMERGENCY_ICON(LogTypeHelper.EMERGENCY,"/emergency.gif");
	
	// The icon
	public final ImageIcon icon;
	
	// The log definition
	public final LogTypeHelper logLevel;
	
	/**
	 * Constructor 
	 * 
	 * @param logLevel The log type
	 * @param fileName The name of the file containing the icon
	 * 
	 * @see {@link com.cosylab.logging.engine.log.LogTypeHelper}
	 */
	private EntryTypeIcon(LogTypeHelper logLevel, String fileName) {
		if (logLevel==null) {
			throw new IllegalArgumentException("The log type can't be null");
		}
		if (fileName==null || fileName.isEmpty()) {
			throw new IllegalArgumentException("Invalid file name");
		}
   		icon=new ImageIcon(this.getClass().getResource(fileName));
    	this.logLevel = logLevel;
	}
	
	/**
     * Evaluates and returns the highest vertical dimension
     * of the icons (it is needed to optimize the display size of some widgets)
     *  
     * @return The max vertical dimension of the icons 
     */
    public static int getIconsVSize() {
        int vDim=0;
        for (EntryTypeIcon typeIcon: EntryTypeIcon.values()) {
        	int height = (typeIcon.icon==null)?10:typeIcon.icon.getIconHeight();
            if (height>vDim) vDim=height;
        }
        return vDim;
    }
    
    /**
     * Return the icon for a given log type.
     * 
     * @param logLevel The not <code>null</code> log type whose icon must ne returned
     * @return The icon for the given log type
     */
    public static ImageIcon getIcon(LogTypeHelper logLevel) {
    	if (logLevel==null) {
    		throw new IllegalArgumentException("The log type can't be null");
    	}
    	for (EntryTypeIcon typeIcon: EntryTypeIcon.values()) {
    		if (typeIcon.logLevel==logLevel) {
    			return typeIcon.icon;
    		}
    	}
    	throw new IllegalStateException("No icon found for type "+logLevel);
    }
    
}
