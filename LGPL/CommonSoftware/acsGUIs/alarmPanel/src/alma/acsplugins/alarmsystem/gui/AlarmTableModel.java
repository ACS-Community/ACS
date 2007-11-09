/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */

/** 
 * @author  caproni   
 * @version $Id: AlarmTableModel.java,v 1.6 2007/11/09 10:43:56 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import javax.swing.table.AbstractTableModel;

import alma.alarmsystem.clients.category.AlarmView;
import alma.alarmsystem.clients.category.CategoryListener;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

/** 
 * 
 * The table model for the table alarms
 *
 */
public class AlarmTableModel extends AbstractTableModel implements CategoryListener {
	
		
		
	
	/**
	 * The max number of alarms in the table
	 * When the max has been reach, the oldest alarm is removed 
	 * before adding a new one
	 */
	private final int MAX_ALARMS=10000;
	
	/**
	 * The colors used to show alarms in the table
	 */
	private final String[] colors = {
			"<HTML><FONT Color=\"#0000FF\">", // Priority 0
			"<HTML><FONT Color=\"#FF8050\">", // Priority 1
			"<HTML><FONT Color=\"#FF00FF\">", // Priority 2
			"<HTML><FONT Color=\"#FF0000\">", // Priority 3
			"<HTML><FONT Color=\"#00FF00\">" // Inactive
	};
	private final String endStr="</FONT>";
	
	
	
	 // The names of the cols of the table
	private String headerNames[] = new String[] {
		"Time",
		"Triplet",
		"Priority",
		"Description",
		"Cause",
		"Host"
		
	};
	
	// The alarms in the table
	private Vector<AlarmView> items = new Vector<AlarmView>(); 

	public int getRowCount() {
		return items.size();
	}

	public int getColumnCount() {
		return headerNames.length;
	}
	
	/**
	 * Return the text to display in a cell as it is read by the alarm
	 * without any formatting (the table add some formatting for
	 * example the color)
	 * 
	 * @param rowIndex The row of the cell
	 * @param columnIndex The col of the cell
	 * @return The string to display in the cell
	 */
	public String getCellContent(int rowIndex, int columnIndex) {
		AlarmView alarm = items.get(rowIndex);
		String ret="";
		switch (columnIndex) {
		case 0: {
			// Timestamp
			ret=alarm.getTimestamp();
			break;
		}
		case 1: {
			// Triplet
			ret=alarm.alarmID;
			break;
		}
		case 2: {
			// Priority
			ret=alarm.priority.toString();
			break;
		}
		case 3: {
			// Description
			ret=alarm.description;
			break;
		}
		case 4: {
			ret=alarm.cause;
			break;
		}
		case 5: {
			ret=alarm.hostName;
			break;
		}
		default: {
				return "";
			}
		}
		return ret;
	}

	/**
	 * @see javax.swing.table.AbstractTableModel
	 */
	public Object getValueAt(int rowIndex, int columnIndex) {
		AlarmView alarm = items.get(rowIndex);
		String ret=getCellContent(rowIndex, columnIndex);
		if (!alarm.active || alarm.priority==null || alarm.priority<0 || alarm.priority>3) {
			return colors[4]+ret+endStr;
		} else {
			return colors[alarm.priority]+ret+endStr;
		}
	}

	/**
	 * Add an alarm in the table.
	 * If an alarm with the same triplet is already in the table it is replaced.
	 * 
	 * @param alarm The alarm to show in the table.
	 */
	public synchronized void alarmReceived(AlarmView alarm) {
		if (items.size()>MAX_ALARMS && items.indexOf(alarm)>=0) {
			items.remove(items.size()-1); // Remove the last one
		}
		if (items.indexOf(alarm)>=0) {
			items.setElementAt(alarm,items.indexOf(alarm));
		} else {
			items.add(0,alarm); 
		}
		fireTableDataChanged();
	}
	
	@Override
	public String getColumnName(int col) {
		if (col>=0 && col<headerNames.length) {
			return headerNames[col];
			
		} else {
			return"?????????????????????";
		}
	}
	
}
