/*
 * Created on May 2, 2005
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.logging.settings;

import java.awt.Component;
import java.awt.Dimension;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

import com.cosylab.logging.LogTypeHelper;

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
        setPreferredSize(new Dimension(100,LogTypeHelper.getIconsVSize()));
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
        
        // It log is known, set the icon
        Integer logType = LogTypeHelper.parseLogTypeDescription(value.toString());
        if (logType!=null) {
            setIcon(LogTypeHelper.getIcon(logType.intValue()));
        } 

        return this;
    }
}
