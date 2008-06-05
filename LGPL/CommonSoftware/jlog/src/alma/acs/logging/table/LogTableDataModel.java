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
package alma.acs.logging.table;

import java.net.URL;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.LinkedBlockingQueue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileNotFoundException;

import javax.swing.table.AbstractTableModel;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import alma.acs.logging.dialogs.LoadURLDlg;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.IOLogsHelper;

import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;

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
public class LogTableDataModel extends AbstractTableModel {
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
		private final int TIME_INTERVAL=30;
		
		// The queue with the keys of the logs to delete
		private LinkedBlockingQueue<Integer> logsToDelete = new LinkedBlockingQueue<Integer>();
		
		// Signal the thread to terminate
		private volatile boolean terminateThread=false;
		
		/**
		 * Constructor
		 *
		 */
		public LogDeleter() {
			super("LogDeleter");
		}
		
		/**
		 * Terminate the thread
		 * 
		 * @param sync If it is true wait the termination of the thread before returning
		 */
		public void close(boolean sync) {
			terminateThread=true;
			interrupt();
			if (sync) {
				while (this.isAlive()) {
					try {
						Thread.sleep(125);
					} catch (InterruptedException ie) {
						continue;
					}
				}
			}
		}
		
		/** 
		 * The thread to delete logs
		 */
		public void run() {
			int sz;
			while (!terminateThread) {
				for (int t=0; t<TIME_INTERVAL && !terminateThread; t ++) {
					try {
						Thread.sleep(1000);
					} catch (InterruptedException e) {
						break;
					}
				}
				if (terminateThread) {
					return;
				}
				// Do not do anything if the application is paused
				if (loggingClient.isPaused()) {
					continue;
				}
				sz =rows.size();
				if (maxLog>0 && sz>maxLog) {
					deleteLogs();
				}
			}
		}
		
		/**
		 * Delete all the logs exceeding the max number of logs
		 * 
		 */
		private void deleteLogs() {
			synchronized (LogTableDataModel.this) {
				int numOfLogsToRemove = rows.size()-maxLog;
				try {
					Vector<Integer> removed = new Vector<Integer>(numOfLogsToRemove);
					for (int t=0; t<numOfLogsToRemove; t++) {
						removed.add(rows.remove(0));
					}
					System.out.println("removing "+removed.size()+" logs");
					fireTableRowsDeleted(0, numOfLogsToRemove);
					allLogs.deleteLogs(removed);
					removed.removeAllElements();
				} catch (Exception e) {
					System.out.println("Error deleting a collection of logs from thread: "+e.getMessage());
					e.printStackTrace();
				}
			}
		}
		
	}
	
	private static final String BLANK_STRING = "";

	// Stores all the logs received.
	private LogCache allLogs = null ;
    
    // The LoggingClient that owns this table model
    private LoggingClient loggingClient=null;
    
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
	 * To avoid refreshing the table too often, <code>lastFireEventTime</code> records
	 * the last time such an operation has been performed.
	 * <P>
	 * The next refresh will be done only after <code>TABLE_UPDATE_TIME</code> msecs
	 * have been elapsed
	 * 
	 * @see <code>TABLE_UPDATE_TIME</code>
	 */
	private long lastFireEventTime=0;
	
	/**
	 * The time interval (in msec) between 2 refreshes of the table
	 */
	private static final long TABLE_UPDATE_TIME=1000;
	
	/**
	 * The thread to delete the logs asynchronously
	 */
	private LogDeleter logDeleter;
	
	/**
	 * Each row shows a log identified by a key returned by the cache.
	 * <P>
	 * This vector stores the key of each log shown in the table.
	 */
	private Vector<Integer> rows = new Vector<Integer>(10000,2048);
	
	/**
	 * Return number of columns in table
	 */
	public final int getColumnCount() {
		return Field.values().length+1;
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
	 * Returns an item according to the row and the column of its position.
	 * @return java.lang.Object
	 * @param row int
	 * @param column int
	 */
	public synchronized Object getValueAt(int row, int column) {
		switch (column) {
		case 1: {// TIMESTAMP
				try {
				return new Date(allLogs.getLogTimestamp(row));
				} catch (Exception e) {
					return null;
				}
		}
		case 2: { // ENTRYTYPE
			try {
				return allLogs.getLogType(row);
			} catch (Exception e) {
				return null;
			}
		}
		default: {
			ILogEntry log=getVisibleLogEntry(row); 
			if (log==null) {
				return null;
			} 
			if (column == 0) {
				return new Boolean(log.hasDatas());
			} else {
				return log.getField(Field.values()[column-1]);
			}
		}
		}
	}
	
	/**
	 * LCLogTableDataModel constructor comment. Gets updated logs.
	 */
	public LogTableDataModel(LoggingClient client) throws Exception {
		super();
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient");
		}
		this.loggingClient=client;
		try {
			allLogs = new LogCache();
		} catch (LogCacheException lce) {
			System.err.println("Exception instantiating the cache: "+lce.getMessage());
			lce.printStackTrace(System.err);
			throw new Exception("Exception instantiating the cache: ",lce);
		} 
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
			synchronized (rows) {
				rows.add(key);
			}
			if (System.currentTimeMillis()-lastFireEventTime>TABLE_UPDATE_TIME) {
				fireTableDataChanged();
				lastFireEventTime=System.currentTimeMillis();
			}
		} catch (LogCacheException lce) {
			System.err.println("Exception caught while inserting a new log entry in cache:");
			System.err.println(lce.getLocalizedMessage());
			lce.printStackTrace(System.err);
		}
	}
	
	/**
	 * Returns default class for column.
	 * Creation date: (12/1/2001 14:18:53)
	 * @return java.lang.Class
	 * @param column int
	 */
	public final Class<?> getColumnClass(int column) {
		if (column == 0) {
			return Boolean.class;
		} else {
			int col=column-1;
		
			if (col>=0 && col<Field.values().length) {
				return Field.values()[col].getType();
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
	
		if (columnIndex == 0 ) {
			return BLANK_STRING;
		}
	
		columnIndex=columnIndex-1;
		
		return (columnIndex>=0 && columnIndex<Field.values().length) ? 
			Field.values()[columnIndex].getName() :
			BLANK_STRING;
	}
	
	public void loadFromURL() {
		LoadURLDlg urlDlg = new LoadURLDlg("http://websqa.hq.eso.org/alma/snapshotRHE/ACS-Reports/TestCoverage-Linux/ACS/LGPL/CommonSoftware/jcont/test/tmp/all_logs.xml",loggingClient);
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

			getIOHelper().loadLogs(in, loggingClient, loggingClient, allLogs, 0);
		} catch (Exception ex) {
			ex.printStackTrace();
			JOptionPane.showMessageDialog(null, "Exception reading "
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
			CustomFileChooser fc = new CustomFileChooser(currentDir,"Load",loggingClient);
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
		getIOHelper().loadLogs(br,loggingClient,loggingClient,allLogs,len);
		
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
	public synchronized void clearAll() {
			    if (allLogs != null) {
			    	try {
			    		synchronized(rows) {
			    			rows.removeAllElements();
			    		}
			    		allLogs.clear();
			    		fireTableDataChanged();
			    	} catch (LogCacheException e) {
			    		System.err.println("Error clearing the cache: "+e.getMessage());
			    		e.printStackTrace(System.err);
			    		JOptionPane.showMessageDialog(null, "Exception clearing the cache: "+e.getMessage(),"Error clearing the cache",JOptionPane.ERROR_MESSAGE);
			    	}
			    }
	}
	
	/**
	 * Return true if an async load/save is in progress
	 * @return
	 */
	public boolean IOInProgress() {
		if (ioHelper==null) {
			return false;
		}
		return ioHelper.isPerformingIO();
	}
	
	/**
	 * A getter method that created the helper only when needed
	 * 
	 * @return The IOLogsHelper object
	 */
	private IOLogsHelper getIOHelper() {
		if (ioHelper==null) {
			ioHelper = new IOLogsHelper(loggingClient);
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
	
	/**
	 * Set the new log level i.e. the level of the logs shown in the table.
	 * 
	 * @param newLevel
	 */
	public void setLogLevel(LogTypeHelper newLevel) {
		
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
		
		if (logsToDelete!=null && logsToDelete.size()>0) {
			for (Integer logNumber: logsToDelete) {
				//deleteLog(logNumber);
			}
		}
	}
	
	/**
	 * Closes all the threads and frees the resources
	 * This is the last method to call before closing the application
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		if (logDeleter!=null) {
			logDeleter.close(sync);
			logDeleter=null;
		}
		if (ioHelper!=null) {
			ioHelper.done();
			ioHelper=null;
		}
	}

	/**
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		return allLogs.getSize();
	}
	
	 public ILogEntry getVisibleLogEntry(int row) {
		 assert row>=0 && row<rows.size();
		 try {
			 ILogEntry ret;
			 synchronized (rows) {
				 ret=allLogs.getLog(rows.get(row));
			 }
			 return ret;
		 } catch (Exception e) {
			 e.printStackTrace(System.err);
			 return null;
		 }
	 }
	 
	 public int getFieldSortNumber() {
		 return 0;
	 }

	 public boolean sortedAscending() {
		 return true;
	 }
	 
	 public void setSortComparator(int index, boolean ascending) {
	 }
	 
	 /**
	  * Return the key of the log in the given position of the
	  * vector of keys.
	  * <P>
	  * There are several cases that forbids to retrieve the key in the given position,
	  * in such a situations the method return <code>null</code>.
	  * One typical situation is when the entry has been deleted by the <code>LogDeleter</code>.
	  *   
	  * @param index The position in the model of the key
	  * @return The key in the passed position or
	  *         <code>null</code> if it is not possible to return the key
	  *         
	  *         @see findKeyPos(Integer key)
	  */
	 public synchronized Integer getLogKey(int index) {
		try {
			return rows.get(index);
		} catch (Throwable t) {
			return null;
		}
	 }
	 
	 /**
	  * Return the position of the key in the vector.
	  * <P>
	  * There are cases when the key is not anymore in the vector and in such situations 
	  * this method return <code>null</code>.
	  * <BR>For example it could happen if the log has been deleted by the <code>LogDeleter</code>. 
	  * 
	  * @param key The key whose position in the vector has to be found
	  * @return The position of the key in the vector of logs or 
	  * 	    <code>-1</code> if the key is not in the vector
	  */
	 public synchronized int findKeyPos(Integer key) {
		 return rows.indexOf(key);
	 }
	 
}
