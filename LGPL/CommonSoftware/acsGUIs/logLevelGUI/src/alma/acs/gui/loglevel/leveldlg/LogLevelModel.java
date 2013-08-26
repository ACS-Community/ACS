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

	private static final long serialVersionUID = 6512293636952955954L;

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
		public final Class<?> colClass;
		
		/**
		 * Constructor
		 * 
		 * @param name The string to show in the  table header
		 * @param cl The class of the items in the column
		 */
		private Column(String name, Class<?> cl) {
			this.name=name;
			this.colClass=cl;
		}
		
		public static Column getColumn(int index) {
			return values()[index];
		}
		
		public static int getColumnCont() {
			return values().length;
		}
	}
	
	// The log levels.
	// Each log level is an entry in the table
	private LogLevelHelper[] levels;
	
	private LogTypeHelper commonLocalLevel;
	private LogTypeHelper commonGlobalLevel;
	
	/**
	 * Constructor
	 * 
	 * @param levels The levels i.e. the row of the table
	 */
	public LogLevelModel(LogLevelHelper[] levels) {
		if (levels==null) {
			throw new IllegalArgumentException("Invalid null array of log levels in constructor");
		}
		setLevels(levels);
		initialize();
	}
	
	/**
	 * @param levels
	 */
	public void setLevels(LogLevelHelper[] levels) {
		this.levels = levels;
	}
	
	/**
	 * Sets the common local log level and apply it to the loggers
	 * which use the default level.
	 * 
	 * @param commonLocalLevel
	 */
	public void setCommonLocalLevel(LogTypeHelper commonLocalLevel) {
		int colDefault = Column.DEFAULT.ordinal();
		for (int row = 0; row < getRowCount(); row++) {
			if ((Boolean)getValueAt(row, colDefault)) 
				levels[row].setLocalLevel(commonLocalLevel);
		}
		this.commonLocalLevel = commonLocalLevel;
		fireTableDataChanged();
	}
	
	/**
	 * Sets the common global log level and apply it to the loggers
	 * which use the default level.
	 * 
	 * @param commonGlobalLevel
	 */
	public void setCommonGlobalLevel(LogTypeHelper commonGlobalLevel) {
		int colDefault = Column.DEFAULT.ordinal();
		for (int row = 0; row < getRowCount(); row++) {
			if ((Boolean)getValueAt(row, colDefault)) 
				levels[row].setGlobalLevel(commonGlobalLevel);
		}
		this.commonGlobalLevel = commonGlobalLevel;
		fireTableDataChanged();
	}
	
	/**
	 * Forces all the loggers to use the common levels
	 */
	public void setAllToCommonLevels() {
		for (int row = 0; row < getRowCount(); row++) {
			LogLevelHelper level = levels[row];
			level.setUseDefault(true);
			level.setLocalLevel(commonLocalLevel);
			level.setGlobalLevel(commonGlobalLevel);
		}
		fireTableDataChanged();
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
		Column column = Column.getColumn(col);
		return (column != Column.NAME); 
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
		Column col = Column.getColumn(column);
		LogLevelHelper level = levels[row];
		
		switch (col) {
		case NAME: {
			// Field not editable
			break;
		}
		
		case DEFAULT: {
			Boolean isSelected = (Boolean)aValue;
			level.setUseDefault(isSelected);
			if (isSelected) {
				level.setLocalLevel(commonLocalLevel);
				level.setGlobalLevel(commonGlobalLevel);
			}
			break;
		}

		case LOCAL: {
			try {
				LogTypeHelper lev = getLevelFromObject(aValue);
				if (level.getLocalLevel() != lev.acsCoreLevel.value) {
					level.getLogLevels().useDefault = false;
					level.setLocalLevel(lev);
				}
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
		case GLOBAL: {
			try {
				LogTypeHelper lev = getLevelFromObject(aValue);
				if (level.getGlobalLevel() != lev.acsCoreLevel.value) {
					level.getLogLevels().useDefault = false;
					level.setGlobalLevel(lev);
				}
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
		Column col = Column.getColumn(column);
		LogLevelHelper level = levels[row];
		switch (col) {
		case NAME: {
			return level.getName();
		}
		case DEFAULT: {
			return level.getLogLevels().useDefault;
		}
		case LOCAL: {
			return level.getLocalLevel();
		}
		case GLOBAL: {
			return level.getGlobalLevel();
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
