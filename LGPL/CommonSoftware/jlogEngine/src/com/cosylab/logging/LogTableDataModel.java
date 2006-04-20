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
package com.cosylab.logging;

import java.net.URL;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileNotFoundException;

import javax.swing.table.AbstractTableModel;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import com.cosylab.logging.client.VisibleLogsVector;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.IOLogsHelper;

import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.client.cache.LogCacheException;


/**
 * Defines a table model that extends the AbstractTableModel class interface and implements 
 * getRowCount, getColumnCount, getValueAt. Additionally, it implements 
 * methods for appending log entries to a list, manipulating log entries, 
 * applying filters to log entries as well as loading from files and saving into files.
 * 
 * Creation date: (11/11/2001 13:46:06)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class LogTableDataModel extends AbstractTableModel 
{
	private static final String BLANK_STRING = "";

	//private final LogEntryComparator sortComparator = new LogEntryComparator(LogEntryXML.FIELD_STACKID, true);
		
	// Stores all the logs received.
	// private final AbstractList allLogs = new ArrayList();
	private LogCache allLogs = null ;
	
	// Stores the references to visible logs after the filters are applied
	// If no filters are defined or the filters do not hide any items,
	// allLogs provides the items and filteredLogs is not valid.
	private final VisibleLogsVector visibleLogs;
	
	// Data model maintains the list of all the available filters
	private final FiltersVector filters = new FiltersVector();
    
    // The list of the filters not defined by the user as for example the log level
    // defined in the toolbar
    private final FiltersVector systemFilters = new FiltersVector();
    
	// Contains references to the filters that are currently applied to logs.
	// Actual filters are stored in filters.
	//private final Vector appliedFilters = new Vector();

	// Internal row cache information. See getValueCache(int).
	private int rowCacheIndex = -1;
	private ILogEntry rowCacheLogEntry = null;
	
	// Stores the current directory which is being accessed.	
	public java.io.File currentDir = null;
	
	private boolean isSuspended = true;
	
	/**
	 * An object to load and save logs
	 */
	private IOLogsHelper ioHelper=null;
		
	/**
	 * Return number of columns in table
	 */
	public final int getColumnCount() {
		return ILogEntry.NUMBER_OF_FIELDS+2;
	}
	
	/**
	 * Replace a log entry with another
	 * 
	 * @param pos The position in the cache of the log to replace 
	 * @param newEntry The new LogEntryXML
	 */
	public synchronized void replaceLog(int pos, ILogEntry newEntry) {
		// Replace the entry in the list of all the logs (allLogs)
		allLogs.replaceLog(pos,newEntry);
		return;
	}
	
	/**
	 * Returns whether the saving/loading of the file has been cancelled or not that reflects on the
	 * status of the JToggleButton of the GUI. If canceled, then the button should be released. 
	 */
	public final boolean getSuspended() {
	
		return isSuspended;	
	}
	
	/**
	 * Returns number of rows in the table. This value returns number of visible rows
	 * after filtering.
	 * @return int
	 */
	public final int getRowCount() {
		return visibleLogs.size();
	}
	
	/**
	 * Returns an item according to the row and the column of its position.
	 * @return java.lang.Object
	 * @param row int
	 * @param column int
	 */
	public Object getValueAt(int row, int column) {
		if (column == 0) {
			return new Integer(0);
		} else if (column==1) {
				ILogEntry log = getVisibleLogEntry(row);
				return new Boolean(log.hasDatas());
		} else {
	
			column=column-2;
			
			ILogEntry log = getVisibleLogEntry(row);
		
			if (log != null) {
				return log.getField(column);
			}
			
		
			return null;
		}
	
	}
	
	
	/**
	 * Returns the LogEntryXML at specified row. The value is based on current visibility
	 * of entries as implied by filtering. This method is used for GUI entry presentation.
	 * To access the logs use the <code>getStoredLogEntry</code> method.
	 * Creation date: (11/11/2001 14:20:44)
	 * @return com.cosylab.logging.engine.LogEntryXML
	 * @param row int
	 */
	public final ILogEntry getVisibleLogEntry(int row) {
		return getValueCache(row);	
	}
	
	/**
	 * Performance method to reduce the number of table lookups during table display.
	 * Returns <code>LogEntryXML</code> based on last accessed entry. Will always produce
	 * valid result regardless of actual cached value.
	 * Creation date: (11/16/2001 10:46:00)
	 * @return com.cosylab.logging.engine.LogEntryXML
	 * @param row int
	 */
	private final ILogEntry getValueCache(int row) {
	/*
		if ((cacheHits+cacheMisses)%500 == 0) {
		    System.out.println("LogEntryDataModel: Hits: "+cacheHits+", Misses: "+cacheMisses+", H/M ratio = "+(100*cacheHits/(cacheHits+cacheMisses+1)));
		}
	*/   
		if (row == rowCacheIndex) {
	//    	cacheHits++;
			return rowCacheLogEntry;
		} else {
			if ((row >= 0) && (row < visibleLogs.size())) {
	//	        cacheMisses++;
				ILogEntry log = visibleLogs.get(row);
				rowCacheIndex = row;
				rowCacheLogEntry = log;
				return log;
			} else {
				invalidateRowCache();
				return null;
			}
		}
	}
	
	/**
	 * LCLogTableDataModel constructor comment. Gets updated logs.
	 */
	public LogTableDataModel() {
		super();
		try {
			allLogs = new LogCache();
		} catch (LogCacheException lce) {
			System.err.println("Exception instantiating the cache: "+lce.getMessage());
			lce.printStackTrace(System.err);
			String msg="Unrecoverable error instantiating the cache:\n<I>"+lce.getMessage()+"</I>";
			JOptionPane.showMessageDialog(null,msg,"I/O error",JOptionPane.ERROR_MESSAGE);
			// The program exit because it can't store the logs!
			System.exit(-1);
		} 
		visibleLogs = new VisibleLogsVector(allLogs,this);
		//visibleLogs.setChangeCallback(this);
	} 
	
	
	/**
	 * Adds the log to the list. Logs are always appended at the end of the list.
	 * 
	 * @param log The log to add
	 */
	public void appendLog(ILogEntry log) {
		try {
			int pos=allLogs.add(log);
			visibleLogInsert(log,pos);
		} catch (LogCacheException lce) {
			System.err.println("Exception caught while inserting a new log entry in cache:");
			System.err.println(lce.getLocalizedMessage());
			lce.printStackTrace(System.err);
		}
	}
	
	/**
	 * Decreases the list of visible logs.
	 * Creation date: (12/1/2001 14:33:50)
	 * @param index int
	 */
	public final void collapse(int index) {
		//visibleLogs.collapse(index);
	}
	
	/**
	 * Increases the list of visible logs.
	 * Creation date: (11/30/2001 22:48:56)
	 * @param index int
	 */
	public final void expand(int index) {
		//visibleLogs.expand(index);
	}
	
	/**
	 * Returns default class for column.
	 * Creation date: (12/1/2001 14:18:53)
	 * @return java.lang.Class
	 * @param column int
	 */
	public final Class getColumnClass(int column) {
		if (column == 0) {
			return Integer.class;
		} else if (column==1) {
			return Boolean.class;
		} else {
			column=column-2;
		
			if (column>=0 && column<ILogEntry.NUMBER_OF_FIELDS) {
				return ILogEntry.fieldClasses[column];
			}
			return String.class;
		}
	}
	
	/**
	 * Returns name of the column based LogEntryXML fields.
	 * If the specified index does not return a valid column, blank string is returned.
	 * Creation date: (11/11/2001 13:50:16)
	 * @return java.lang.String
	 * @param columnIndex int
	 */
	public final String getColumnName(int columnIndex) {
	
		if (columnIndex == 0 || columnIndex==1)
			return BLANK_STRING;
	
		columnIndex=columnIndex-2;
		
		return (columnIndex>=0 && columnIndex<ILogEntry.NUMBER_OF_FIELDS) ? 
					ILogEntry.fieldNames[columnIndex] :
					BLANK_STRING;
	}
	
	/** 
	 * 
	 * @return The number of the field of the logs used to order the table
	 *         -1 means no ordering for field
	 */
	public int getFieldSortNumber() {
		return visibleLogs.getFieldNumForOrdering();
	}
	
	public boolean sortedAscending() {
		return visibleLogs.isSortAscending();
	}

	/**
	 * Empties the row cache.
	 * Creation date: (11/16/2001 11:22:24)
	 */
	protected final void invalidateRowCache() {
		rowCacheIndex = -1;
		rowCacheLogEntry = null;
	}
	
		public void loadFromURL() {
			java.io.BufferedReader in = null;
			String urlStr=null;
			try {
				urlStr = "http://websqa.hq.eso.org/alma/snapshotRHE/ACS-Reports/TestCoverage-Linux/ACS/LGPL/CommonSoftware/jcont/test/all_logs.xml";
				
				urlStr = (String) JOptionPane.showInputDialog(
	                    null,
	                    "URL for log file:",
	                    urlStr);
				
				if (urlStr != null && urlStr.trim().length()>0) {
					URL url = new URL(urlStr);
					in = new java.io.BufferedReader(new java.io.InputStreamReader(url.openStream()),16384);
					
					isSuspended = true;
					clearAll();
			
					getIOHelper().loadLogs(
							in,
							LoggingClient.getInstance(),
							allLogs,
							true,
							0);
				}
			} catch (Exception ex) {
				ex.printStackTrace();
				JOptionPane.showInternalMessageDialog(null, "Exception reading "+ex.getMessage(),"Error reading "+urlStr,JOptionPane.ERROR_MESSAGE);
			}
			isSuspended = false;
		}
	
	
	/**
	 * Loads logs from a file.
	 * If the name of the file is null, a dialog to choose the file is shown.
	 * 
	 * @param filename The name of the file to load
	 */
	public void loadFromFile(String fileName) {
		if (fileName==null) {
			JFileChooser fc = new JFileChooser(currentDir);
			if (fc.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
				try {
					 // Assigns to filename the name of the selected file			
					fileName = fc.getSelectedFile().getAbsolutePath();
					
					// Assigns the current directory which 
					// the file chooser has accessed for getting the file
					currentDir = fc.getCurrentDirectory(); // if curDir is declared as File
					isSuspended	= true;
		            	
		            	
				} catch (Exception e) {
					System.err.println("Exception while loading: "+e.getMessage());
					e.printStackTrace(System.err);
				}
			} else {
				// Load aborted
				isSuspended = false;
				return;
			}
		}
		BufferedReader br=null;
		int len=0;
		try {
			File f = new File(fileName);
			len = (int)f.length();
			br = new BufferedReader(new FileReader(f),32768);
		} catch (FileNotFoundException fnfe) {
			System.err.println("File not found: "+fileName);
			return;
		}
		getIOHelper().loadLogs(
				br,
				LoggingClient.getInstance(),
				allLogs,
				true,
				len);
		
	}

	/**
	 * Saves input logs into a file.
	 * Creation date: (4/14/2002 17:21:49)
	 * @param s java.lang.String
	 */
	
	public void saveFile() {
		JFileChooser fc = new JFileChooser(currentDir);
		if (fc.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
			try {
				String filename = fc.getSelectedFile().getAbsolutePath();	
					
				currentDir = fc.getCurrentDirectory();
				
				// Cancels saving
				if (fc.getSelectedFile() == null) 
					return;		
				
				// Checks whether the selected file exists
				if (fc.getSelectedFile().exists()) {
					int act = javax.swing.JOptionPane.showConfirmDialog(null, filename + " exists.  Overwrite?");
					
					// Checks whether a file exists
					while (act == javax.swing.JOptionPane.NO_OPTION) {
						fc.showSaveDialog(null);
						act = javax.swing.JOptionPane.showConfirmDialog(null, filename + " exists.  Overwrite?");
					}
		
					// Canceled saving action
					if (act == javax.swing.JOptionPane.CANCEL_OPTION) {
						filename = null;
						isSuspended = false;
						return;
					}
				}
				isSuspended	= true;		
				saveFile(filename);
			} catch (Exception e) {
				System.out.println("Exception "+e.getMessage());
				e.printStackTrace(System.err);
			};
			
		} else {
			isSuspended = false;
		}
	}
	
	public void saveFile(String fileName) {
		try {
			getIOHelper().saveLogs(fileName,allLogs,true);
	 	} catch (Exception e) {
	 		JOptionPane.showMessageDialog(null, "Exception saving the file: "+e.getMessage(),"Error saving "+fileName,JOptionPane.ERROR_MESSAGE);
	 		
	 	};
	}
	
	// clears all	
	public void clearAll() {
			    if (allLogs != null) {
					allLogs.clear();
			    }
			    if (visibleLogs != null) {
					visibleLogs.clear();
			    }
	}
	
	/**
	* Erases and rebuilds visible logs considering filters.
	* Creation date: (11/16/2001 11:26:32)
	*/
	public final void invalidateVisibleLogs() {
	
		visibleLogs.clear();
	
		for (int i = 0; i < allLogs.getSize(); i++) {
			visibleLogInsert(allLogs.getLog(i),i);
		}
	}
	
	/**
	 * Insert the method's description here.
	 * <p>
	 * Creation date: (12/23/2001 13:01:44)
	 * @param index int
	 */
	public void toggleExpand(int index) {
		/*if (visibleLogs.isExpanded(index)) 
			visibleLogs.collapse(index);
		else
			visibleLogs.expand(index);*/
	}
	/**
	 * If the log is not filtered then it is inserted
	 * in the list of the visible logs (i.e. the logs that
	 * appear in the window)
	 * 
	 * @param log The log to show or hide
	 * @param pos The position of this log in the cache
	 */
	public void visibleLogInsert(ILogEntry log, int pos) {
		if (filters.applyFilters(log) && systemFilters.applyFilters(log)) {
			visibleLogs.add(pos,log);
		}
	}
	
	/** 
	 * Return the filters defined by the user
	 * 
	 * @return The user defined filters
	 */
	public FiltersVector getFilters() {
		return filters;
	}
	
	/** 
	 * Return the filters defined by the system (for example the log level in the toolbar)
	 * 
	 * @return The system filters
	 */
	public FiltersVector getSystemFilters() {
	    return systemFilters;
	}
	
	/**
	 * Return true if an async load/save is in progress
	 * @return
	 */
	public boolean IOInProgress() {
		if (ioHelper==null) {
			return false;
		} else {
			return ioHelper.isPerformingIO();
		}
	}
	
	/**
	 * Insert the method's description here.
	 * Creation date: (1/24/02 10:48:29 AM)
	 * @param comparator com.cosylab.logging.client.LogEntryComparator
	 */
	public void setSortComparator(int logField, boolean ascending) {
		visibleLogs.setLogsOrder(logField,ascending);
	}
	
	/**
	 * A getter method that created the helper only when needed
	 * 
	 * @return The IOLogsHelper object
	 */
	private IOLogsHelper getIOHelper() {
		if (ioHelper==null) {
			ioHelper = new IOLogsHelper();
		}
		return ioHelper;
	}
	
	/**
	 * 
	 * @return The number of logs in cache
	 */
	public long totalLogNumber() {
		return allLogs.getSize();
	}

}
