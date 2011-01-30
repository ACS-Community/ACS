/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
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

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.Vector;

import javax.swing.JOptionPane;
import javax.swing.table.AbstractTableModel;

import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.client.cache.LogCacheException;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

/**
 * The table model with basic functionalities.
 * <P>
 * This model can be reused by log tables with reduced functionalities like the error
 * browsers. 
 * 
 * 
 * @author acaproni
 *
 */
public class LogEntryTableModelBase extends AbstractTableModel {
	
	/**
	 * To reduce the overload refreshing the content of the table when a lot of logs
	 * have been added or removed, the refresh is triggered only after a certain amount
	 * of time.
	 * <P>
	 * Objects from this class check if there have been changes and fire the event
	 * 
	 * @author acaproni
	 *
	 */
	public class TableUpdater extends Thread {
		
		/**
		 * Signal the thread to terminate
		 */
		private volatile boolean terminateThread=false;
		
		/**
		 * The interval (msec) between two refreshes of the content
		 * of the table
		 */
		private static final int UPDATE_INTERVAL=2000; 
		
		/**
		 * Terminate the thread and frees the resources
		 * 
		 * @param sync If it is true wait the termination of the threads before returning
		 */
		public void close(boolean sync) {
			terminateThread=true;
			interrupt();
			while (sync && isAlive()) {
				try {
					Thread.sleep(125);
				} catch (InterruptedException e) {}
			}
		}
		
		/**
		 * The thread updating the content of the table
		 */
		public void run() {
			while (!terminateThread) {
				try {
					Thread.sleep(UPDATE_INTERVAL);
				} catch (InterruptedException ie) {
					continue;
				}
				// Refresh the content of the table before refreshing
				try {
					updateTableEntries();
				} catch (Throwable t) {
					// This thread never fails!
					System.err.println("Error in thread "+getName()+": "+t.getMessage());
					t.printStackTrace(System.err);
				}
			}
		}
	}
	
	/** 
	 * The cache of all the logs received.
	 */
	protected LogCache allLogs = null ;
	
	/**
	 * Each row shows a log identified by a key returned by the cache.
	 * <P>
	 * This vector stores the key of each log shown in the table.
	 */
	protected RowEntries rows = new RowEntries(10000);
	
	/**
	 * The vector of logs to add in the rows.
	 * <P>
	 * Newly arrived logs are added to this vector and flushed into 
	 * <code>rows</code> by the <code>TableUpdater</code> thread.
	 */
	protected Vector<Integer> rowsToAdd = new Vector<Integer>();
	
	/**
	 * The thread to refresh the content of the table
	 */
	protected TableUpdater tableUpdater;
	
	/**
	 * <code>true</code> if the model has been closed
	 */
	private boolean closed=false;
	
	/**
	 * Constructor
	 */
	public LogEntryTableModelBase() throws Exception {
		try {
			allLogs = new LogCache();
		} catch (LogCacheException lce) {
			System.err.println("Exception instantiating the cache: "+lce.getMessage());
			lce.printStackTrace(System.err);
			throw new Exception("Exception instantiating the cache: ",lce);
		} 
		
		tableUpdater = new TableUpdater();
		tableUpdater.setName("TableUpdater");
		tableUpdater.setDaemon(true);
		tableUpdater.start();
	}
	

	/**
	 * Return number of columns in table
	 */
	@Override
	public final int getColumnCount() {
		return LogField.values().length+1;
	}
	
	/**
	 * @return The number of files used by the cache
	 */
	public int numberOfUsedFiles() {
		return allLogs.getNumberOfCacheFiles();
	}
	
	/**
	 * 
	 * @return The amount of disk space used by the files of the cache
	 * @throws IOException In case of error getting the size of at least 
	 * 					   one of the used cache files
	 */
	public long usedDiskSpace() throws IOException {
		return allLogs.getFilesSize();
	}

	/**
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public synchronized int getRowCount() {
		synchronized (rows) {
			return rows.size();	
		}
	}

	/**
	 * Returns an item according to the row and the column of its position.
	 * @return java.lang.Object
	 * @param row int
	 * @param column int
	 */
	@Override
	public synchronized Object getValueAt(int row, int column) {
		switch (column) {
		case 1: {// TIMESTAMP
				try {
					synchronized(rows) {
						return new Date(allLogs.getLogTimestamp(rows.get(row)));
					}
				} catch (Exception e) {
					// This can happen because deletion of logs is done asynchronously
					return null;
				}
		}
		case 2: { // ENTRYTYPE
			try {
				synchronized(rows) {
					return allLogs.getLogType(rows.get(row));
				}
			} catch (Exception e) {
				// This can happen because deletion of logs is done asynchronously
				return null;
			}
		}
		default: {
			ILogEntry log=getVisibleLogEntry(row); 
			if (log==null) {
				return null;
			} 
			if (column == 0) {
				return Boolean.valueOf(log.hasDatas());
			} else {
				return log.getField(LogField.values()[column-1]);
			}
		}
		}
	}
	
	/**
	 * Return the log shown in the passed row.
	 * 
	 * @param row The row of the table containing the log
	 * @return The log in the passed row
	 */
	public synchronized ILogEntry getVisibleLogEntry(int row) {
		 if (closed) {
			 return null;
		 }
		 try {
			 ILogEntry ret;
			 synchronized (rows) {
				 ret=allLogs.getLog(rows.get(row));
			 }
			 return ret;
		 } catch (Exception e) {
			 // This can happen because deletion/adding of logs is asynchronous
			 // We can return null and safely ignore this exception
			return null;
		 }
	 }
	
	/**
	 * Remove all the logs	
	 */
	public synchronized void clearAll() {
	    if (allLogs != null) {
	    	try {
	    		synchronized (rowsToAdd) {
		    		synchronized(rows) {
		    			if (!rows.isEmpty()) {
		    				int sz =rows.size();
		    				rows.clear();
		    				fireTableRowsDeleted(0, sz-1);
		    			}
		    		}
		    		rowsToAdd.removeAllElements();
	    		}
	    		allLogs.clear();
	    	} catch (LogCacheException e) {
	    		System.err.println("Error clearing the cache: "+e.getMessage());
	    		e.printStackTrace(System.err);
	    		JOptionPane.showMessageDialog(null, "Exception clearing the cache: "+e.getMessage(),"Error clearing the cache",JOptionPane.ERROR_MESSAGE);
	    	}
	    }
	}
	
	/**
	 * 
	 * @return The number of logs in cache
	 */
	public long totalLogNumber() {
		return allLogs.getSize();
	}
	
	/**
	 * @return The time frame of the log in cache
	 * @see com.cosylab.logging.client.cache.LogCache
	 */
	public Calendar getTimeFrame() {
		return allLogs.getTimeFrame();
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
		 if (key==null) {
			 throw new IllegalArgumentException("The key can't be null");
		 }
		 return rows.indexOf(key);
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
			return "";
		}
	
		columnIndex=columnIndex-1;
		
		return (columnIndex>=0 && columnIndex<LogField.values().length) ? 
			LogField.values()[columnIndex].getName() : "";
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
		
			if (col>=0 && col<LogField.values().length) {
				return LogField.values()[col].getType();
			}
			return String.class;
		}
	}
	
	/**
	 * Adds the log to the table.
	 * <P>
	 * To avoid updating the table very frequently, the logs to add are immediately 
	 * inserted in the <code>LogCache</code> but their insertion in the table is delayed 
	 * and done by the <code>TableUpdater</code> thread.
	 * <BR>
	 * For this reason each log is inserted in the temporary vector <code>rowsToAdd</code> 
	 * that will be flushed into <code>rows</code> by the thread.
	 * 
	 * @param log The log to add
	 */
	public synchronized void appendLog(ILogEntry log) {
		if (closed) {
			return;
		}
		try {
			//checkLogNumber();
			Integer key=Integer.valueOf(allLogs.add(log));
			synchronized (rowsToAdd) {
				rowsToAdd.insertElementAt(key,0);
			}
		} catch (LogCacheException lce) {
			System.err.println("Exception caught while inserting a new log entry in cache:");
			System.err.println(lce.getLocalizedMessage());
			lce.printStackTrace(System.err);
		}
	}
	
	/**
	 * Update the table entries before refreshing the table
	 * 
	 * @return <code>true</code> If the model has been changed and the table needs to be refreshed
	 */
	protected synchronized void updateTableEntries() {
		flushLogs();
	}
	
	/**
	 * Flush the logs from the temporary vector into the table.
	 * <P>
	 * New logs are appended in the temporary vector <code>rowsToAdd</code> to limit 
	 * the frequency of updating the table model.
	 * This method flushes the logs from the temporary vector into the model vector 
	 * (<code>rows</code>).
	 * 
	 * @return <code>true</code> if at least one log has been added to the model
	 */
	private void flushLogs() {
		int added=0;
		synchronized (rowsToAdd) {
			added=rowsToAdd.size();
			if (added>0) {
				synchronized (rows) {
					rows.addAll(0,rowsToAdd);
				}
				rowsToAdd.clear();
			} else {
				return;
			}
		}
		fireTableDataChanged();
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
	 * Closes all the threads and frees the resources
	 * This is the last method to call before closing the application
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		closed=true;
		if (tableUpdater!=null) {
			tableUpdater.close(sync);
			tableUpdater=null;
		}
		clearAll();
	}
}
