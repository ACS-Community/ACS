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
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileNotFoundException;

import javax.swing.table.AbstractTableModel;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import alma.acs.logging.dialogs.LoadURLDlg;

import com.cosylab.logging.client.VisibleLogsVector;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.IOLogsHelper;

import com.cosylab.logging.engine.log.LogTypeHelper;

import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.client.cache.LogCacheException;
import com.cosylab.logging.client.CustomFileChooser;

/**
 * Defines a table model that extends the AbstractTableModel class interface and implements 
 * getRowCount, getColumnCount, getValueAt. Additionally, it implements 
 * methods for appending log entries to a list, manipulating log entries, 
 * applying filters to log entries as well as loading from files and saving into files.
 * 
 * Creation date: (11/11/2001 13:46:06)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class LogTableDataModel extends AbstractTableModel implements Runnable
{
	/**
	 * The class contains a thread to delete asynchronously
	 * the logs running at low priority.
	 * The thread runs once awhile as specified by the TIME_INTERVAL 
	 * local variable.
	 * 
	 * @author acaproni
	 *
	 */
	public class LogDeleter extends Thread {
		// The time interval between two iteration of the thread
		private final int TIME_INTERVAL=15*1000;
		
		// The queue with the keys of the logs to delete
		private LinkedBlockingQueue<Integer> logsToDelete = new LinkedBlockingQueue<Integer>();
		
		/**
		 * Constructor
		 *
		 */
		public LogDeleter() {
			super("LogDeleter");
		}
		
		/**
		 * Add the key of a log to delete
		 * 
		 * @param key The key of the log to delete
		 */
		private void addLogToDelete(Integer key) {
			if (logsToDelete.contains(key)) {
				System.out.println("Rejected "+key);
				return; 
			}
			try {
				logsToDelete.put(key);
			} catch (InterruptedException ie) {}
			System.out.println("Added "+key);
		}
		
		/** 
		 * The thread to delete logs
		 */
		public void run() {
			ArrayList<Integer> keysToDelete = new ArrayList<Integer>();
			int sz;
			while (true) {
				try {
					Thread.sleep(TIME_INTERVAL);
				} catch (InterruptedException e) {
					continue;
				}
				// Do not do anything if the application is paused
				if (LoggingClient.getInstance().isPaused()) {
					continue;
				}
				sz =allLogs.getSize();
				if (maxLog>0 && sz>maxLog) {
					keysToDelete.clear();
					int nKeys=allLogs.getFirstLogs(sz-maxLog,keysToDelete);
					deleteLogs(keysToDelete);
				}
			}
		}
		
		/**
		 * Delete a log with the given key.
		 *  
		 * @param key The key of the log to delete
		 */
		private synchronized void deleteLog(Integer key) {
			int posInTable=visibleLogs.deleteLog(key);
			fireTableRowsDeleted(posInTable,posInTable);
			try {
				allLogs.deleteLog(key);
			} catch (LogCacheException e) {
				System.out.println("Error deleting a log from thread: "+e.getMessage());
				e.printStackTrace();
			}
		}
		
		/**
		 * Delete all the logs whoise keys are in the Collection
		 * 
		 * @param keys The collection of logs to delete
		 */
		private void deleteLogs(Collection<Integer> keys) {
			if (keys==null) {
				throw new IllegalArgumentException("The collection can't be null");
			}
			visibleLogs.deleteLogs(keys);
			fireTableDataChanged();
			try {
				allLogs.deleteLogs(keys);
			} catch (LogCacheException e) {
				System.out.println("Error deleting a collection of logs from thread: "+e.getMessage());
				e.printStackTrace();
			}
		}
		
	}
	
	private static final String BLANK_STRING = "";

	//private final LogEntryComparator sortComparator = new LogEntryComparator(LogEntryXML.FIELD_STACKID, true);
		
	// Stores all the logs received.
	private LogCache allLogs = null ;
	
	// Stores the references to visible logs after the filters are applied
	// If no filters are defined or the filters do not hide any items,
	// allLogs provides the items and filteredLogs is not valid.
	private final VisibleLogsVector visibleLogs;
	
	// Data model maintains the list of all the available filters
	private final FiltersVector filters = new FiltersVector();
    
	/**
	 * The level of the log to show in the table
	 */
    private int logLevel;
    
	// Contains references to the filters that are currently applied to logs.
	// Actual filters are stored in filters.
	//private final Vector appliedFilters = new Vector();
	
	// Stores the current directory which is being accessed.	
	public File currentDir = null;
	
	private boolean isSuspended = true;
	
	/**
	 * An object to load and save logs
	 */
	private IOLogsHelper ioHelper=null;
	
	/**
	 * The thread to invalidate the list of logs
	 * i.e. to regenerate the list of visible logs 
	 */
	private Thread invalidateThread = new Thread(this);
	
	/**
	 * The max number of logs in cache 
	 * This limit is not for the buffer but for the size of the whole cache
	 * A value of 0 means unlimited 
	 */
	private int maxLog=0;
	
	/**
	 * The time frame of the logs to keep in cache
	 * This limit is not for the buffer but for the timeframe of the whole cache
	 * A value of 0 means unlimited
	 */
	private long timeFrame=0;
	
	/**
	 * The thread to delete the logs asynchronously
	 */
	private LogDeleter logDeleter;
	
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
		try {
			allLogs.replaceLog(pos,newEntry);
		} catch (LogCacheException e) {
			System.err.println("Error replacing log "+pos);
			e.printStackTrace();
		}
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
	public synchronized Object getValueAt(int row, int column) {
		ILogEntry log = getVisibleLogEntry(row);
		if (log==null) {
			return null;
		} else {
			if (column == 0) {
				return new Integer(0);
			} else if (column==1) {
					return new Boolean(log.hasDatas());
			} else {
				column=column-2;
				return log.getField(column);
			}
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
	public synchronized final ILogEntry getVisibleLogEntry(int row) {
		ILogEntry ret= null;
		try {
			ret=visibleLogs.get(row);
		} catch (Exception e) {
			System.out.println("Exception caught "+e.getMessage());
			e.printStackTrace();
		}
		return ret;
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
		logDeleter=new LogDeleter();
		logDeleter.setPriority(Thread.MIN_PRIORITY);
		logDeleter.start();
	}
	
	/**
	 * Set the max number of logs to keep in cache
	 * This is the max number of logs stored in cache
	 * (the visible logs can be less)
	 * 
	 * @param max The max number of logs
	 *            0 means unlimited
	 */
	public void setMaxLog(int max) {
		if (max<0) {
			throw new IllegalArgumentException("Impossible to set the max log to "+max);
		}
		maxLog=max;
	}
	
	/**
	 * Set the time frame of the logs in the cache
	 * The time frame if the amount of time we want to keep in the table 
	 * for example the last 2hr
	 * (the visible logs can be less)
	 * 
	 * @param timeframe The time frame in milliseconds
	 *                  0 means unlimited
	 */
	public void setTimeFrame(long timeframe) {
		if (timeframe<0) {
			throw new IllegalArgumentException("Impossible to set the time frame to "+timeframe);
		}
		timeFrame=timeframe;
		checkTimeFrame();
	}
	
	/**
	 * Adds the log to the list. Logs are always appended at the end of the list.
	 * 
	 * @param log The log to add
	 */
	public synchronized void appendLog(ILogEntry log) {
		try {
			//checkLogNumber();
			int key=allLogs.add(log);
			visibleLogInsert(log,key);
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

	public void loadFromURL() {
		LoadURLDlg urlDlg = new LoadURLDlg("http://websqa.hq.eso.org/alma/snapshotRHE/ACS-Reports/TestCoverage-Linux/ACS/LGPL/CommonSoftware/jcont/test/tmp/all_logs.xml");
		urlDlg.setVisible(true);
		URL url = urlDlg.getURL();
		if (url==null) {
			// The user pressed Cancel
			return;
		}
		System.out.println("URL: "+url.toString());
		
		java.io.BufferedReader in = null;
		try {
			in = new java.io.BufferedReader(new java.io.InputStreamReader(url.openStream()), 16384);

			isSuspended = true;
			clearAll();

			getIOHelper().loadLogs(in, LoggingClient.getInstance(),
				allLogs, true, 0);
		} catch (Exception ex) {
			ex.printStackTrace();
			JOptionPane.showInternalMessageDialog(null, "Exception reading "
					+ ex.getMessage(), "Error reading " + url.toString(),
					JOptionPane.ERROR_MESSAGE);
		}
		isSuspended = false;
	}
	
	
	/**
	 * Loads logs from a file. If the name of the file is null, a dialog to
	 * choose the file is shown.
	 * 
	 * @param filename
	 *            The name of the file to load
	 */
	public void loadFromFile(String fileName) {
		if (fileName==null) {
			CustomFileChooser fc = new CustomFileChooser(currentDir,"Load");
			File f = fc.getSelectedFile();
			if (f!=null) {
				try {
					 // Assigns to filename the name of the selected file			
					fileName = f.getAbsolutePath();
					
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
			    	try {
			    		allLogs.clear();
			    	} catch (LogCacheException e) {
			    		System.err.println("Error clearing the cache: "+e.getMessage());
			    		e.printStackTrace(System.err);
			    		JOptionPane.showMessageDialog(null, "Exception clearing the cache: "+e.getMessage(),"Error clearing the cache",JOptionPane.ERROR_MESSAGE);
			    	}
			    }
			    if (visibleLogs != null) {
					visibleLogs.clear();
			    }
	}
	
	/**
	 * Erases and rebuilds visible logs considering filters.
	 */
	public void run() {
		LoggingClient logging=LoggingClient.getInstance();
		// Store the status of the application (paused/unpaused) before this rebuilding
		boolean logClientWasPaused=logging.isPaused();
		logging.setEnabledGUIControls(false);
		try {
			logging.pause();
		} catch (Exception e) {}
		int key=allLogs.getFirstLog();
		visibleLogs.clear();
		visibleLogs.setRefreshInterval(3000);
		logging.animateProgressBar("Regenerating",key,allLogs.getLastLog());
		
		try {
			while (key <= allLogs.getLastLog()) {
				try {
					if (allLogs.getLogType(key) >= logLevel) {
						visibleLogInsert(allLogs.getLog(key), key);
					}
				} catch (LogCacheException e) {
					// This can happen if the log has been removed by a separate
					// thread
					// It is not an error and the exception can be ignored
				}
				if (key % 50 == 0) {
					logging.moveProgressBar(key);
				}
				key++;
			}
		} catch (Throwable t) {
			System.err.println("Got a throwble " + t.getMessage());
			t.printStackTrace();
			System.out.println("Exiting");
			System.exit(-1);			
		}
		logging.freezeProgressBar();
		visibleLogs.setRefreshInterval(null);
		logging.setEnabledGUIControls(true);
		visibleLogs.setRefreshInterval(null);
		// Upause the application if it was unpaused before this rebuilding
		if (!logClientWasPaused) {
			try {
				LoggingClient.getInstance().resume();
			} catch (Exception e) {}
		}
	}
	
	/**
	*	Invalidate the visible logs launching a worker thread 
	* 
	*/
	public final void invalidateVisibleLogs() {
		if (invalidateThread.isAlive()) {
			throw new IllegalStateException("Trying to start an already running thread");
		}
		if (allLogs.getSize()>1) {
			invalidateThread=new Thread(this);
			invalidateThread.setName("LogTableDataModel");
			invalidateThread.start();
		}
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
		if (log.getType()>=logLevel && filters.applyFilters(log)) {
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
	
	public void setLogLevel(int newLevel) {
		if (newLevel<0 || newLevel>LogTypeHelper.ENTRYTYPE_EMERGENCY) {
			throw new IllegalArgumentException(""+newLevel+" is not valid");
		}
		logLevel=newLevel;
		invalidateVisibleLogs();
	}
	
	/**
	 * @return The time frame of the log in cache
	 * @see com.cosylab.logging.client.cache.LogCache
	 */
	public Calendar getTimeFrame() {
		return allLogs.getTimeFrame();
	}
	
	/**
	 * Check if the time frame of the logs in cache exceeds the limit and
	 * if it is the case, deletes the oldest logs
	 */
	private void checkTimeFrame() {
		if (timeFrame==0) {
			return;
		}
		Collection<Integer> logsToDelete = allLogs.getLogExceedingTimeFrame(timeFrame);
		Collections.sort((List<Integer>)logsToDelete);
		Collections.reverse((List<Integer>)logsToDelete);
		
		System.out.println("*** Checking time frame ****");
		System.out.println("===> "+logsToDelete.size()+" logs to delete, cache size="+allLogs.getSize());
		System.out.println("===> "+timeFrame+" timeframe");
		if (logsToDelete!=null && logsToDelete.size()>0) {
			for (Integer logNumber: logsToDelete) {
				//deleteLog(logNumber);
			}
		}
		System.out.println("cahche size after deletion "+allLogs.getSize());
	}
	
	/**
	 * check if each logs in the table has the same date of the related log in the cache
	 *
	 */
	private void checkConsistency() {
		System.out.print("Checking consistency... ");
		for (int t=0; t<allLogs.getSize(); t++) {
			int posInTable =visibleLogs.getRowOfEntry(t);
			if (posInTable==-1) { 
				continue;
			}
			// Get the date from the tale (visibleLogs)
			ILogEntry logInTable;
			try {
				logInTable=visibleLogs.get(posInTable);
			} catch (Exception e) {
				System.out.println("Exception trying to get log in cache pos "+t);
				System.out.println(e.getMessage());
				e.printStackTrace();
				continue;
			}
			long dateInTable = ((java.util.Date)logInTable.getField(ILogEntry.FIELD_TIMESTAMP)).getTime();
			
			// Get the date from the cache (allLogs)
			ILogEntry cacheLog=null;
			try {
				cacheLog = allLogs.getLog(t);
			} catch (LogCacheException le) {
				System.out.println("checkConsistency: Error geting log "+t);
			}
			long cacheDate=((java.util.Date)cacheLog.getField(ILogEntry.FIELD_TIMESTAMP)).getTime();
			
			if (cacheDate!=dateInTable) {
				System.out.println("The date in cache (allLogs.get..) and that in the table (visibleLogs) differ:");
				System.out.println("\tdate in cache: "+cacheDate+", date in table: "+dateInTable);
			}
			try {
				if (dateInTable!=allLogs.getLogTimestamp(t)) {
					System.out.println("Values differ in "+t);
					return;
				}
			} catch (LogCacheException e) {
				System.err.println("Error getting tyhe time stamp of "+t);
				e.printStackTrace();
			}
			if (t%10000==0) {
				System.out.print('.');
			}
		}
		System.out.println("Ok and checking order!" );
		checkOrder();
	}
	
	private void checkOrder() {
		System.out.print("Checking order (visibleLogs size "+visibleLogs.size()+")... ");
		for (int t=0; t<visibleLogs.size()-1; t++) {
			ILogEntry log1=visibleLogs.get(t);
			ILogEntry log2=visibleLogs.get(t+1);
			long date1 = ((java.util.Date)log1.getField(ILogEntry.FIELD_TIMESTAMP)).getTime();
			long date2 = ((java.util.Date)log2.getField(ILogEntry.FIELD_TIMESTAMP)).getTime();
			if (date1<date2) {
				System.out.println(" Misaligned at "+t+" date@pos0="+date1+" date@pos1="+date2);
				int start = (t-5<0)?0:t-5;
				int end=(t+5<visibleLogs.size())?t+5:visibleLogs.size();
				System.out.println("visibleLogs dump:");
				for (int j=start; j<end; j++) {
					ILogEntry l =visibleLogs.get(j);
					long d =((java.util.Date)l.getField(ILogEntry.FIELD_TIMESTAMP)).getTime();
					System.out.println("\t"+j+": "+d);
				}
				System.exit(-1);
			}
			if (t%10000==0) {
				System.out.print('*');
			}
		}
		System.out.println("Ok");
	}

}
