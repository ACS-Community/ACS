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
package alma.acs.gui.loglevel.leveldlg;

import javax.swing.JOptionPane;
import javax.swing.table.DefaultTableModel;

import si.ijs.maci.LoggingConfigurablePackage.LogLevels;

/**
 * The table model for the log levels table
 * 
 * @author acaproni
 *
 */
public class LogLevelModel extends DefaultTableModel {
	
	// The name of the columns
	private String[] colNames = new String[] {
		"Logger name",
		"Use deafult",
		"Local",
		"Global"
	};
	
	private Class[] colClasses = new Class[] {
		String.class,
		Boolean.class,
		Integer.class,
		Integer.class
	};
	
	// The log levels.
	// Each log level is an entry in the table
	private LogLevelHelper[] levels;
	
	/**
	 * Constructor
	 * 
	 * @param levels The levels i.e. the row of the table
	 */
	public LogLevelModel(LogLevelHelper[] levels) {
		if (levels==null) {
			throw new IllegalArgumentException("Invalid null array of log levels in constructor");
		}
		this.levels=levels;
		initialize();
	}
	
	/**
	 * Init the table model data structures
	 *
	 */
	private void initialize() {
		for (String col: colNames) {
			addColumn(col);
		}
	}
	
	/**
	 * @see DefaultTableModel
	 */
	public Class getColumnClass(int column) {
		return colClasses[column];
	}
	
	/**
	 * @see DefaultTableModel
	 */
	public int getRowCount() {
		if (levels==null) {
			return 0;
		}
		return levels.length;
	}
	
	/**
	 * @see DefaultTableModel
	 */
	public boolean isCellEditable(int row, int col) {
		return col!=0; 
	}
	
	public void setValueAt(Object aValue, int row, int column) {
		if (aValue!=null) {
			System.out.println("Setting class "+aValue.getClass().getName());
		}
		if (row>levels.length) {
			throw new IllegalStateException("Trying to set value for ["+row+", "+column+"] but the table has "+levels.length+" rows");
		}
		switch (column) {
		case 0: {
			// Field not editable
			break;
		}
		case 1: {
			try {
				levels[row].setUseDefault((Boolean)aValue);
			} catch (Throwable t) {
				System.err.println("Error setting a value:" +t.getMessage());
				t.printStackTrace(System.err);
				JOptionPane.showMessageDialog(
						null,
						"Error setting default: "+aValue+"\n"+t.getMessage(), 
						"Error", 
						JOptionPane.ERROR_MESSAGE);
			}
			break;
		}
		case 2: {
			try {
				levels[row].setLocalLevel((Integer)aValue);
			} catch (Throwable t) {
				System.err.println("Error setting a value:" +t.getMessage());
				t.printStackTrace(System.err);
				JOptionPane.showMessageDialog(
						null, 
						"Error setting local level: "+aValue+"\n"+t.getMessage(), 
						"Error", 
						JOptionPane.ERROR_MESSAGE);
			}
			break;
		}
		case 3: {
			try {
				levels[row].setGlobalLevel((Integer)aValue);
			} catch (Throwable t) {
				System.err.println("Error setting a value:" +t.getMessage());
				t.printStackTrace(System.err);
				JOptionPane.showMessageDialog(
						null, 
						"Error setting global level: "+aValue+"\n"+t.getMessage(), 
						"Error", 
						JOptionPane.ERROR_MESSAGE);
			}
			break;
		}
		default: {
			throw new IllegalStateException("trying to set value for ["+row+", "+column+"]");
		}
		}
		fireTableRowsUpdated(row, row);
	}
	
	/**
	 * All the changes made by the user to the log levels have been
	 * applied.
	 * This method reset the "modified" status of all the LogLevels
	 *
	 */
	public void changesApplied() {
		for (LogLevelHelper lvl: levels) {
			lvl.resetChanges();
		}
	}
	
	/**
	 * @see DefaultTableModel
	 */
	public Object getValueAt(int row, int column) {
		switch (column) {
		case 0: {
			return levels[row].getName();
		}
		case 1: {
			return levels[row].isUsingDefault();
		}
		case 2: {
			return levels[row].getLocalLevel();
		}
		case 3: {
			return levels[row].getGlobalLevel();
		}
		default: {
			return "N/A";
		}
		}
			
	}

	/**
	 * Getter
	 * 
	 * @return The LogLevels[] set by the user
	 */
	public LogLevelHelper[] getLevels() {
		return levels;
	}
	
}
