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
 * @version $Id: AlarmTableModel.java,v 1.1.1.1 2007/09/21 09:10:11 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import javax.swing.table.AbstractTableModel;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

/** 
 * 
 * The table model for the table alarms
 *
 */
public class AlarmTableModel extends AbstractTableModel {
	/**
	 * 
	 * The view of an alarm for the table model containing
	 * only the fields that will be dispalayed in the table
	 *
	 */
	public class AlarmView {
		public String alarmID=null; // Triplet
		public Integer priority = null; // Priority
		public Date sourceTimestamp=null; // Source timestamp
		public String description=null; // Problem description
		public String cause=null; // The cause
		public Boolean active=null; // Active
		
		/**
		 * Constructor
		 * 
		 * @param id The ID (triplet)
		 * @param pri Priority
		 * @param timestamp Timestamp
		 * @param desc Problem description
		 * @param act Active
		 */
		public AlarmView(String id, String pri, String timestamp, String desc, String cause,String act) {
			if (id==null || id.length()==0) {
				throw new IllegalArgumentException("Invalid triplet in constructor");
			}
			alarmID=id;
			description=desc;
			this.cause=cause;
			// Get the date
			try {
				long nsec=Long.parseLong(timestamp);
				 sourceTimestamp=new Date(nsec);
			} catch (Exception e) {
				// Invalid timestamp
				sourceTimestamp=null;
				System.err.println("Invalid timestamp for "+alarmID+": "+timestamp);
			}
			// Get the status
			try {
				active=Boolean.parseBoolean(act);
			} catch (Exception e) {
				System.err.println("Invalid priority for "+alarmID+": "+pri);
				active=true;
			}
			// Get the priority
			try {
				priority=new Integer(pri);
			} catch (Exception e) {
				System.err.println("Invalid priority for "+alarmID+": "+pri);
				priority=0;
			}
		}
		
		/**
		 * Override the Object.equals because we know
		 * that 2 alarms are equals if they have the same ID
		 *
		 * This method is used to check if a given alarm is already
		 * in the vector of the alarms in the table.
		 * The same alarm is shown only once but its color must change
		 * depending of its stata (active/inactive)
		 * 
		 * @param obj The objct to comapre
		 * @return true if the 2 objects are equal
		 */
		@Override
		public boolean equals(Object obj) {
			if (obj==null || !(obj instanceof AlarmView)) {
				return false;
			} else {
				AlarmView alarm = (AlarmView)obj;
				return alarmID.equals(alarm.alarmID);
			}
		}
		
		/**
		 * Dump the alarm
		 * @see Object.toString()
		 */
		@Override
		public String toString() {
			StringBuilder ret = new StringBuilder("Triplet: ");
			ret.append(alarmID);
			ret.append(", Time: ");
			if (sourceTimestamp!=null) {
				ret.append(dateFormat.format(sourceTimestamp));
			} else {
				ret.append("undefined");
			}
			ret.append(", Priority: ");
			ret.append(priority);
			ret.append(", Status: ");
			ret.append(active);
			ret.append(", Description: ");
			ret.append(description);
			return ret.toString();
		}
	}
	
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
			"<HTML><FONT Color=\"#FF0000\">", // Priority 0
			"<HTML><FONT Color=\"#FF8050\">", // Priority 1
			"<HTML><FONT Color=\"#FFFF00\">", // Priority 2
			"<HTML><FONT Color=\"#0000FF\">", // Priority 3
			"<HTML><FONT Color=\"#00FF00\">" // Inactive
	};
	private final String endStr="</FONT>";
	
	// The format for the timestamp
	private final String TIME_FORMAT = "yyyy'-'MM'-'dd'T'HH':'mm':'ss";
	private final SimpleDateFormat dateFormat = new SimpleDateFormat(TIME_FORMAT);
	
	 // The names of the cols of the table
	private String headerNames[] = new String[] {
		"Time",
		"Triplet",
		"Priority",
		"Description",
		"Cause"
	};
	
	// The alarms in the table
	private Vector<AlarmView> items = new Vector<AlarmView>(); 

	public int getRowCount() {
		return items.size();
	}

	public int getColumnCount() {
		return headerNames.length;
	}

	public Object getValueAt(int rowIndex, int columnIndex) {
		AlarmView alarm = items.get(rowIndex);
		String ret="";
		switch (columnIndex) {
		case 0: {
			// Timestamp
			if (alarm.sourceTimestamp!=null) {
				ret=dateFormat.format(alarm.sourceTimestamp);
				break;
			}
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
		default: {
				return "";
			}
		}
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
	public synchronized void addAlarm(String id, String pri, String timestamp, String desc, String cause, String act) {
		AlarmView alarm=null;
		try {
			alarm = new AlarmView(id,pri,timestamp,desc,cause,act);
		} catch (Exception e) {
			System.err.println("Invalid alarm definition: "+id+", "+pri+", "+timestamp+", "+desc+", "+act);
			System.err.println("Alarm discarded");
			return;
		}
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
