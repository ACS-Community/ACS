/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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
/*
 * Created on May 2, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.logging.settings;

import java.awt.Component;
import java.awt.Dimension;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

import com.cosylab.logging.client.EntryTypeIcon;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The renderer for the item in the listbox
 * It is needed in order to show the labels together with
 * the icons.
 * The labels are the descrition of the log type (read from LogTypeHelper)
 * 
 * @author acaproni
 */
public class LogTypeRenderer extends JLabel  implements ListCellRenderer {
    
    /**
     * The constructor build the item to show 
     * for each log entry type
     * 
     * @param labels
     */
    public LogTypeRenderer () {
        super();
        setPreferredSize(new Dimension(100,EntryTypeIcon.getIconsVSize()));
        setOpaque(true);
        setVerticalTextPosition(CENTER);
        setHorizontalTextPosition(TRAILING);
        setHorizontalAlignment(LEFT);
        setVerticalAlignment(CENTER);
    }
    
    public Component getListCellRendererComponent(
            JList list, 
            Object value, 
            int index, 
            boolean isSelected, 
            boolean cellHasFocus) {
        if (isSelected) {
            setBackground(list.getSelectionBackground());
            setForeground(list.getSelectionForeground());
        } else {
            setBackground(list.getBackground());
            setForeground(list.getForeground());
        }
        
        // Set the text
        setText(value.toString());
        setFont(list.getFont());
        
        // If the type of log is known, set the icon
        if (value==null) {
        	setIcon(null);
        } else if (value instanceof LogTypeHelper) {
        	setIcon(EntryTypeIcon.getIcon((LogTypeHelper)value));
        } else if (!value.toString().equals("None")) {
	        LogTypeHelper logType = LogTypeHelper.fromLogTypeDescription(value.toString());
	        if (logType!=null) {
	            setIcon(EntryTypeIcon.getIcon(logType));
	        } 
        } else {
        	setIcon(null);
        }

        return this;
    }
}
