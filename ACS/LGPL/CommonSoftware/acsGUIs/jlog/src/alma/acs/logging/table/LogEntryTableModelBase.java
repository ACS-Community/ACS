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
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.ScheduledThreadPoolExecutor;

import javax.swing.JOptionPane;
import javax.swing.table.AbstractTableModel;

import alma.acs.gui.util.threadsupport.EDTExecutor;
import alma.acs.logging.table.reduction.LogProcessor;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.client.cache.LogCacheException;
import com.cosylab.logging.engine.audience.Audience.AudienceInfo;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

/**
 * The table model with basic functionalities like add logs and remove all the logs.
 * The model can be bounded to a max number of logs (by default unlimited).
 * <P>
 * Each log in the model is identified by a integer key; the association
 * between keys and logs is done by the {@link #allLogs}.
 * {@link #rows} represents a entry in the table i.e. a log identified by its key.
 * So to get a log in a row, we must first get its key and then get the log from the cache. 
 * <BR> This indirection allows to avoid keeping all the logs in memory
 * The list of keys ({@link #rows}) and the map ({@link #allLogs}) must be consistent
 * in the sense that the map must always contain the keys of all the logs in the table 
 * but it can have more keys. For example if a log is removed from the table
 * its key can be removed from the map at a latter time.
 * <P>
 * The model is modified inside the EDT. The consistency between
 * ({@link #rows}) and the map ({@link #allLogs}) is also obtained by
 * modifying their contents inside the EDT.
 * {@link EDTExecutor} is used to ensure to access the model from inside the EDT.
 * <P>
 * To reduce the overload refreshing the content of the table when a lot of logs
 * have been added or removed, the refresh is triggered only after a certain amount
 * of time by a dedicated thread (see {@link #run()}).
 * <P>
 * When a log is deleted, it key is immediately removed from the List of logs ({@link #rows} 
 * but the key is still in the cache ({@link #allLogs}). A dedicated thread, {@link KeysDeleter}, 
 * is in charge of removing unused keys. This operation can be safely done in a non-EDT
 * thread.
 * <P>
 * <CODE>LogEntryTableModelBase</CODE> can be reused by log tables with basic functionalities 
 * like the error browsers.
 * <P> 
 * TODO: manage threads with a {@link ScheduledThreadPoolExecutor} 
 * 
 * @author acaproni
 *
 */
public class LogEntryTableModelBase extends AbstractTableModel implements Runnable {
	
	/**
	 * The class to delete the keys in a dedicated thread.
	 * <P>
	 * Keys to remove are added only by {@link LogEntryTableModelBase#removeExceedingLogs()} inside the EDT.
	 * 
	 * @author acaproni
	 *
	 */
	private class KeysDeleter  implements Runnable {
		
		/**
		 * If closed then new keys are rejected and the thread terminates
		 */
		private volatile boolean closed=false;
		
		/**
		 * The cache of keys to be deleted by the thread.
		 * <P>
		 * {@link ArrayList} seems the best choice considering that we need only 3
		 * operations: add, get and clear.
		 */
		private final List<Integer> keysToDelete = Collections.synchronizedList(new ArrayList<Integer>());
		
		/**
		 * Invalidate the list that will be cleared.
		 * <P>
		 * This method is needed when the cache is cleared to avoid removing the
		 * same keys more then once
		 * 
		 * @see LogEntryTableModelBase#clearAll()
		 */
		public void invalidate() {
			keysToDelete.clear();
		}
		
		/**
		 * Add a key to the collection of keys to delete.
		 * 
		 * @param key The not <code>null</code> key to delete
		 */
		public void scheduleForDeletion(Integer key) {
			if (key==null) {
				throw new IllegalArgumentException("Adding a null key is not allowed");
			}
			if (closed) {
				return;
			}
			keysToDelete.add(key);
		}
		
		/**
		 * Add the keys of the passed List 
		 * to the collection of keys to delete.
		 * 
		 * @param keys The not <code>null</code> keys to delete
		 */
		public void scheduleForDeletion(List<Integer> keys) {
			if (keys==null) {
				throw new IllegalArgumentException("Adding a null key is not allowed");
			}
			if (closed) {
				return;
			}
			keysToDelete.addAll(keys);
		}

		/**
		 * The thread to remove the keys
		 */
		@Override
		public void run() {
			while (!closed) {
				try {
					Thread.sleep(KEY_DELETION_INTERVAL);
				} catch (InterruptedException ie) {
					continue;
				}
				if (keysToDelete.isEmpty()) {
					continue;
				}
				// Make a local copy to avoid blocking the EDT
				List<Integer> temp;
				synchronized(keysToDelete) {
					temp= new ArrayList<Integer>(keysToDelete);
					keysToDelete.clear();
				}
				for (Integer keyToRemove: temp) {
					try {
						allLogs.deleteLog(keyToRemove);
					} catch (Throwable t) {
						// An exception removing the key:
						// there is nothing to do so we print out a message and continue
						System.err.println("Error deleting a key from the cache of logs "+keyToRemove);
						t.printStackTrace(System.err);
					}
				}
				temp.clear();
			}
			keysToDelete.clear();
		}
		
		/**
		 * Terminates the computation
		 */
		public void close() {
			closed=true;
			
		}
	}
	
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
	 * The interval of time to remove the keys
	 * from the cache (msec)
	 */
	private static final int KEY_DELETION_INTERVAL = 60000; 
	
	/**
	 * The vector of logs to add in the rows.
	 * <P>
	 * Newly arrived logs are appended to this vector and flushed into 
	 * <code>rows</code> by the <code>TableUpdater</code> thread.
	 * <P>Newest logs are in the tail; oldest logs in the head.
	 */
	private final List<ILogEntry> rowsToAdd = Collections.synchronizedList(new Vector<ILogEntry>());
		
	/**
	 * The processor to reduce the logs
	 */
	private final LogProcessor logProcessor = new LogProcessor();
	
	/** 
	 * The cache of all the logs received.
	 */
	protected LogCache allLogs;
	
	/**
	 * Each row of the table shows a log identified by a key returned by the cache.
	 * <P>
	 * This vector stores the key of each log in the table.
	 * 
	 */
	protected final RowEntries rows = new RowEntries();
	
	/**
	 * The thread to refresh the content of the table
	 */
	protected Thread tableUpdater;
	
	/**
	 * It deletes unused key from the cache 
	 */
	private final KeysDeleter keysDeleter = new KeysDeleter();
	
	/**
	 * The thread to delete unused keys from the cache
	 * every {@link #KEY_DELETION_INTERVAL} time interval
	 * 
	 * @see KeysDeleter
	 */
	private Thread keysDeleterThread=null;
	
	/** 
     * The LoggingClient that owns this table model
     */
    protected final LoggingClient loggingClient;
	
	
	/**
	 * <code>true</code> if the model has been closed
	 */
	private volatile boolean closed=false;
	
	/**
	 * HTML open TAG for the string in the table
	 */
	private static final String htmlStartTAG="<html>";
	
	/**
	 * HTML close TAG for the string in the table
	 */
	private static final String htmlCloseTAG="</html>";
	
	/**
	 * The empty string is returned when a object is <code>null</code>
	 */
	private static final String emptyString="";
	
	/**
	 * The max number of logs in the table.
	 * <P>
	 * A value of 0 means unlimited.
	 */
	private volatile int maxLog=0;
	
	/**
	 * Constructor
	 */
	public LogEntryTableModelBase(LoggingClient client) throws Exception {
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
	}
	
	/**
	 * Start the thread to update the table model.
	 * 
	 * @see {@link LogEntryTableModelBase#tableUpdater}
	 */
	public void start() {
		tableUpdater=new Thread(this,"LogEntryTableModelBase.TableUpdaterThread");
		tableUpdater.setDaemon(true);
		tableUpdater.start();
		
		keysDeleterThread = new Thread(keysDeleter,"LogEntryTableModelBase.KeysDeleter");
		keysDeleterThread.setDaemon(true);
		keysDeleterThread.start();
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
	public int getRowCount() {
		return rows.size();	
	}

	/**
	 * Returns an item according to the row and the column of its position.
	 * 
	 * @return java.lang.Object
	 * @param row int
	 * @param column int
	 */
	@Override
	public Object getValueAt(int row, int column) {
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
				return Boolean.valueOf(log.hasDatas());
			} else {
				Object val=log.getField(LogField.values()[column-1]);
				if (val==null || val.toString()==null || val.toString().isEmpty()) {
					return emptyString;
				}
				// Format the string as HTML
				String ret=val.toString().replaceAll("<","&lt;");
				ret=ret.replaceAll(">","&gt;");
				ret=ret.replaceAll("\n", "<BR>");
				return htmlStartTAG+ret+htmlCloseTAG;
			}
		}
		}
	}
	
	/**
	 * Return the log shown in the passed row.
	 * <P>
	 * This must be called inside the EDT.
	 * 
	 * @param row The row of the table containing the log
	 * @return The log in the passed row
	 */
	public ILogEntry getVisibleLogEntry(int row) {
		 if (closed) {
			 return null;
		 }
		 try {
			 ILogEntry ret;
			 ret=allLogs.getLog(rows.get(row));
			 return ret;
		 } catch (LogCacheException e) {
			 e.printStackTrace();
			return null;
		 }
	 }
	
	/**
	 * Remove all the logs.
	 */
	public void clearAll() {
		// Remove the logs waiting to be inserted
		EDTExecutor.instance().execute(new Runnable() {
			public void run() {
				rowsToAdd.clear();
				rows.clear();
				keysDeleter.invalidate();
				try {
					allLogs.clear();		
				} catch (Throwable t) {
					System.err.println("Exception caught clearing the cache: "+t.getMessage());
					t.printStackTrace(System.err);
					JOptionPane.showInternalMessageDialog(loggingClient, "Error clearing the cache.", "Error clearing the cache", JOptionPane.ERROR_MESSAGE);
				}
				fireTableDataChanged();
			}
		});
	}
	
	/**
	 * Return the number of logs in cache 
	 * without those waiting to be added in {@link TableUpdater#rowsToAdd}.
	 * 
	 * @return The number of logs in cache
	 */
	public long totalLogNumber() {
		return rows.size();
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
	 public Integer getLogKey(int index) {
		return rows.get(index);
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
	 public int findKeyPos(Integer key) {
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
	 * Replace a log entry with another.
	 * 
	 * @param pos The position in the cache of the log to replace 
	 * @param newEntry The new LogEntryXML
	 */
	public void replaceLog(final int pos, final ILogEntry newEntry) {
		EDTExecutor.instance().execute(new Runnable() {
			@Override
			public void run() {
				// Replace the entry in the list of all the logs (allLogs)
				try {
					allLogs.replaceLog(pos,newEntry);
				} catch (LogCacheException e) {
					System.err.println("Error replacing log "+pos);
					e.printStackTrace();
				}
			}
		});
	}
	
	/**
	 * Closes all the threads and frees the resources
	 * This is the last method to call before closing the application
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		terminateThread=true;
		keysDeleter.close();
		
		closed=true;
		if (tableUpdater!=null) {
			tableUpdater.interrupt();
			if (sync) {
				try {
					// We do not want to wait forever..
					tableUpdater.join(10000);
					if (tableUpdater.isAlive()) {
						System.err.println("LogEntryTableModelBase.TableUpdater thread still alive!");
					}
				} catch (InterruptedException ie) {}
			}
		}
		clearAll();
		if (keysDeleterThread!=null) {
			keysDeleterThread.interrupt();
			if (sync) {
				try {
					// We do not want to wait forever..
					keysDeleterThread.join(10000);
					if (keysDeleterThread.isAlive()) {
						System.err.println("LogEntryTableModelBase.KeysDeleter thread still alive!");
					}
				} catch (InterruptedException ie) {}
			}
		}
	}
	
	/**
	 * Adds a log to {@link TableUpdater#rowsToAdd} ready to be flushed
	 * in the table at the next iteration.
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
	public void appendLog(ILogEntry log) {
		if (log==null) {
			throw new IllegalArgumentException("Can't append a null log to the table model");
		}
		if (!closed) {
			rowsToAdd.add(log);
		}
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
		
		// rowsToAdd is reduced then copied into temp:
		// it is possible to append logs again without waiting for the flush
		// i.e. modification of the model inside EDT do not block rowsToAdd
		final List<ILogEntry> temp;
		synchronized (rowsToAdd) {
			if (rowsToAdd.isEmpty()) {
				return;
			}
			// try to apply reduction rules only in OPERATOR mode
			try {
				if (loggingClient.getEngine().getAudience().getInfo()==AudienceInfo.OPERATOR) {
					logProcessor.reduce(rowsToAdd);
				}
			} catch (Throwable t) {
				System.err.println("Exception caught ("+t.getMessage()+")while reducing logs: reduction disabled this time");
				t.printStackTrace(System.err);
			}
			temp=new Vector<ILogEntry>(rowsToAdd);
			rowsToAdd.clear();
		}
			
		// Add the reduced logs into the model (from inside the EDT)
		try {
			EDTExecutor.instance().executeSync(new Runnable() {
				@Override
				public void run() {
					for (int t=temp.size()-1; t>=0; t--) {
						ILogEntry log=temp.get(t);
						Integer key;
						try {
							key=Integer.valueOf(allLogs.add(log));
						} catch (LogCacheException lce) {
							System.err.println("Exception caught while inserting a new log entry in cache:");
							System.err.println(lce.getLocalizedMessage());
							lce.printStackTrace(System.err);
							continue;
						}
						rows.add(key);
					}
					// Finally notify the change
					fireTableRowsInserted(0, temp.size()-1);
				}
			});
		} catch (InvocationTargetException e) {
			System.out.println("!!! Exception: "+e.getMessage()+"; model size="+rows.size());
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
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
	 * Check if there are too many logs in the table and if it is the case
	 * then remove the oldest.
	 * <P>
	 * This method must be executed inside the EDT.
	 * 
	 * @see #maxLog
	 */
	private void removeExceedingLogs() {
		try {
			EDTExecutor.instance().executeSync(new Runnable() {
				@Override
				public void run() {
					int sz=rows.size();
					if (maxLog<=0 || sz<=maxLog) {
						// The model is unbounded or there is still enough room in the model
						return;
					}
					keysDeleter.scheduleForDeletion(rows.removeLastEntries(rows.size()-maxLog));
					fireTableDataChanged();
				}
			});
		} catch (InvocationTargetException e) {
			System.out.println("!!! Exception: "+e.getMessage()+"; model size="+rows.size());
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		while (!terminateThread) {
			try {
				Thread.sleep(UPDATE_INTERVAL);
			} catch (InterruptedException ie) {
				continue;
			}
			// Flush the logs waiting to be added in the model
			try {
				flushLogs();
			} catch (Throwable t) {
				// This thread never fails!
				System.err.println("Error in thread "+Thread.currentThread().getName()+": "+t.getMessage());
				t.printStackTrace(System.err);
			}
			// Too many logs? Remove the oldest!
			removeExceedingLogs();
		}
		
	}
}
