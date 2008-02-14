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
 * @author  acaproni   
 * @version $Id: AlarmTableModel.java,v 1.13 2008/02/14 01:57:37 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import javax.swing.table.AbstractTableModel;

import alma.acs.util.IsoDateFormat;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;

import java.text.SimpleDateFormat;
import java.util.Vector;

/** 
 * 
 * The table model for the table alarms
 *
 */
public class AlarmTableModel extends AbstractTableModel implements AlarmSelectionListener {
	
	/**
	 * The title of each column.
	 * 
	 * To change the initial order of the columns, change the order of the
	 * declaration of this enumerated.
	 * 
	 * @author acaproni
	 *
	 */
	public enum AlarmTableColumn {
		TIME("Time",true),
		COMPONENT("Component",true),
		CODE("Code",true),
		PRIORITY("Priority",true),
		DESCRIPTION("Description",true),
		CAUSE("Cause",true),
		ACTION("Action",false),
		CONSEQUENCE("Consequence",false),
		URL("URL",false),
		CONTACT("Contact",false),
		EMAIL("email",false),
		GSM("GSM",false);
		
		// The title of the header
		public final String title;
		
		// If true the column is shown at startup
		public boolean visible;
		
		/**
		 * Constructor: set the final properties
		 * 
		 */
		private AlarmTableColumn(String title, boolean initiallyVisible) {
			this.title=title;
			this.visible=initiallyVisible;
		}
			
	};
	
	private SimpleDateFormat dateFormat = new IsoDateFormat();
	
	/**
	 * Add an alarm in the table.
	 * If an alarm with the same triplet is already in the table it is replaced.
	 * 
	 * @param alarm The alarm to show in the table.
	 * @see AlarmSelectionListener
	 */
	public void onAlarm(Alarm alarm) {
		
		synchronized (items) {
			if (items.size()>MAX_ALARMS && items.indexOf(alarm)>=0) {
				items.remove(items.size()-1); // Remove the last one
			}
			if (items.indexOf(alarm)>=0) {
				items.setElementAt(alarm,items.indexOf(alarm));
			} else {
				items.add(0,alarm); 
			}
		}
		fireTableDataChanged();
	}

	@Override
	public void onException(LaserSelectionException e) {
		System.err.println("Exception: "+e.getMessage());
		e.printStackTrace(System.err);		
	}

	/**
	 * The max number of alarms in the table
	 * When the max has been reach, the oldest alarm is removed 
	 * before adding a new one
	 */
	private static final int MAX_ALARMS=10000;
	
	// The alarms in the table
	private Vector<Alarm> items = new Vector<Alarm>(); 

	public int getRowCount() {
		synchronized (items) {
			return items.size();
		}
	}

	public int getColumnCount() {
		return AlarmTableColumn.values().length;
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
	public Object getCellContent(int rowIndex, int columnIndex) {
		Alarm alarm;
		synchronized (items) {
			alarm = items.get(rowIndex);
		}
		
		AlarmTableColumn col = AlarmTableColumn.values()[columnIndex];
		switch (col) {
		case TIME: {
			return dateFormat.format(alarm.getStatus().getSourceTimestamp());
		}
		case COMPONENT: {
			return alarm.getTriplet().getFaultFamily();
		}
		case CODE: {
			return alarm.getTriplet().getFaultCode();
		}
		case PRIORITY: {
			return alarm.getPriority().toString();
		}
		case DESCRIPTION: {
			return alarm.getProblemDescription();
		}
		case CAUSE: {
			return alarm.getCause();
		}
		case ACTION: {
			return alarm.getAction();
		}
		case CONSEQUENCE: {
			return alarm.getConsequence();
		}
		case GSM: {
			return alarm.getResponsiblePerson().getGsmNumber();
		}
		case CONTACT: {
			return alarm.getResponsiblePerson().getFirstName();
		}
		case EMAIL: {
			return alarm.getResponsiblePerson().getEMail();
		}
		case URL: {
			return alarm.getHelpURL();
		}
		default: {
				return "";
			}
		}
	}

	/**
	 * @see javax.swing.table.AbstractTableModel
	 */
	public Object getValueAt(int rowIndex, int columnIndex) {
		return getCellContent(rowIndex, columnIndex);
	}
	
	@Override
	public String getColumnName(int col) {
		return AlarmTableColumn.values()[col].title;
	}
	
	/**
	 * Return the alarm whose content fills the given row
	 * 
	 * @param row The given showing the alarm
	 * @return The alarm shown in the row row
	 */
	public Alarm getRowAlarm(int row) {
		synchronized (items) {
			if (row<0 || row>=items.size()) {
				throw new IllegalArgumentException("Invalid row: "+row+" not in [0,"+items.size()+"[");
			}
			return items.get(row);
		}
	}	
}
