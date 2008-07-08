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

import alma.acs.logging.level.AcsLogLevelDefinition;

import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The table model for the log levels table
 * 
 * @author acaproni
 *
 */
public class LogLevelModel extends DefaultTableModel {
	
	public enum Column {
		NAME("<HTML><B>Logger name</B></HTML>",String.class),
		DEFAULT("<HTML><B>Use default</B></HTML>",Boolean.class),
		LOCAL("<HTML><B>Local</B></HTML>",Integer.class),
		GLOBAL("<HTML><B>Global</B></HTML>",Integer.class);
		
		/**
		 * The string to show in the header
		 */
		public final String name;
		
		/**
		 * The class of the items in the column
		 */
		public final Class colClass;
		
		/**
		 * Constructor
		 * 
		 * @param name The string to show in the  table header
		 * @param cl The class of the items in the column
		 */
		private Column(String name, Class cl) {
			this.name=name;
			this.colClass=cl;
		}
	}
	
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
		for (Column col: Column.values()) {
			addColumn(col.name);
		}
	}
	
	/**
	 * @see DefaultTableModel
	 */
	public Class<?> getColumnClass(int column) {
		return Column.values()[column].colClass;
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
	
	/**
	 * Set the value of the cell in the given row and col.
	 * 
	 * @param aValue The new value to set
	 * @param row The row of the cell
	 * @param column The column of the cell
	 */
	public void setValueAt(Object aValue, int row, int column) {
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
				levels[row].setLocalLevel(getLevelFromObject(aValue));
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
				levels[row].setGlobalLevel(getLevelFromObject(aValue));
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
	 * Get the level represented by the object.
	 * There are two cases:
	 *  - obj is an integer representing the log level (for example 3)
	 *  - obj is a string describing the log (LogTypeHelper.logEntryTypes for example Info)
	 *  The reason we have two cases is that the value can be set from the LogLevelHelper
	 *  of from the Combobox of the editor. The former is reprsented by an int and the
	 *  latter by a String
	 *  
	 * @param obj The object representing the log type
	 * @return The log type
	 *         null if a log type for the given parameter is not found
	 */
	private LogTypeHelper getLevelFromObject(Object obj) {
		if (obj==null) {
			throw new IllegalArgumentException("Invalid null object");
		}
		LogTypeHelper logType;
		if (obj instanceof Integer) {
			try {
				logType=LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.fromInteger((Integer)obj));
			} catch (Exception e) {
				return null;
			}
		} else {
			try {
				logType=LogTypeHelper.fromLogTypeDescription(obj.toString());
			} catch (Exception e) {
				return null;
			}
		}
		return logType;
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
