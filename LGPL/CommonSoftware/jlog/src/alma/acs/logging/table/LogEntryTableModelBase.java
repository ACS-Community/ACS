package alma.acs.logging.table;

import java.util.Calendar;
import java.util.Date;
import java.util.Vector;

import javax.swing.JOptionPane;
import javax.swing.table.AbstractTableModel;

import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.client.cache.LogCacheException;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;

/**
 * The table model with basic functionalities.
 * <P>
 * This model can be reused by log tables with reduced functionalities like the error
 * browsers. 
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
		 * <code>true</code> if some logs have been added/removed from the model 
		 */
		public volatile boolean changed=false;
		
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
				if (changed) {
					changed=false;
					try {
						fireTableDataChanged();
					} catch (Throwable t) {
						// This exception could happen while deleting asynchronously
					}
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
	protected Vector<Integer> rows = new Vector<Integer>(10000,2048);
	
	/**
	 * The thread to refresh the content of the table
	 */
	protected TableUpdater tableUpdater;
	
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
		return Field.values().length+1;
	}

	/**
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		return rows.size();
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
					return new Date(allLogs.getLogTimestamp(rows.get(row)));
				} catch (Exception e) {
					// This can happen because deletion of logs is done asynchronously
					return null;
				}
		}
		case 2: { // ENTRYTYPE
			try {
				return allLogs.getLogType(rows.get(row));
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
				return new Boolean(log.hasDatas());
			} else {
				return log.getField(Field.values()[column-1]);
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
	public ILogEntry getVisibleLogEntry(int row) {
		 assert row>=0 && row<rows.size();
		 try {
			 ILogEntry ret;
			 synchronized (rows) {
				 ret=allLogs.getLog(rows.get(row));
			 }
			 return ret;
		 } catch (Exception e) {
			// This can happen because deletion of logs is done asynchronously
			 return null;
		 }
	 }
	
	/**
	 * Remove all the logs	
	 */
	public synchronized void clearAll() {
	    if (allLogs != null) {
	    	try {
	    		synchronized(rows) {
	    			int sz =rows.size();
	    			rows.removeAllElements();
	    			fireTableRowsDeleted(0, sz-1);
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
		
		return (columnIndex>=0 && columnIndex<Field.values().length) ? 
			Field.values()[columnIndex].getName() : "";
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
		} catch (LogCacheException lce) {
			System.err.println("Exception caught while inserting a new log entry in cache:");
			System.err.println(lce.getLocalizedMessage());
			lce.printStackTrace(System.err);
		}
		tableUpdater.changed=true;
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
		if (tableUpdater!=null) {
			tableUpdater.close(sync);
			tableUpdater=null;
		}
	}
}
