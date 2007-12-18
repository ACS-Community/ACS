package com.cosylab.logging.client;

import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;

import com.cosylab.logging.LogTableDataModel;

import com.cosylab.logging.LogTableDataModel.LogDeleter;
import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.client.cache.LogCacheException;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;

import com.cosylab.logging.LoggingClient;

/**
 * The array of visible logs (i.e. all and only the logs shown in the table)
 * The logs filtered out are not present in this object (while they are 
 * in the list of logs, LogCache).
 * This class manages the ordering/sorting of logs.
 * Long lasting operations are executed asynchronously. 
 * While long operations are in progress, each new log to be inserted will be  
 * cached (into the buffer Vector) and added asynchronously by the thread.
 * If a log arrives and no async operation is in progress the log is added 
 * directly to the GUI.
 *  
 * @author acaproni
 *
 */
public class VisibleLogsVector extends Thread {
	
	/**
	 * NewLogGUIRefresher keep track of the logs inserted and
	 * show all the logs at once in a separate thread by firinig
	 * the event to the table
	 */
	public class NewLogGUIRefresher extends Thread {
		public static final int REFRESH_INTERVAL=1500;
		/**
		 * The minimum index of the logs inserted
		 */
		private int min;
		/**
		 * The minimum index of the logs inserted
		 */
		private int max;
		
		/**
		 * The interval in microsecond between two refresh of the table
		 */
		private int refreshInterv;
		
		private volatile boolean terminateRefresherThread=false;
		
		/**
		 * Constructor
		 *
		 */
		public NewLogGUIRefresher() {
			super("NewLogGUIRefresher");
			refreshInterv=REFRESH_INTERVAL;
			clear();
			start();
		}
		
		/**
		 * Close the threads and free all the resources
		 * @param sync If it is true wait the termination of the threads before returning
		 */
		public void close(boolean sync) {
			// Terminate the VisibleLogsVector thread
			terminateRefresherThread=true;
			if (sync) {
				while (isAlive()) {
					try {
						Thread.sleep(250);
					} catch (InterruptedException ie) {
						continue;
					}
				}
			}
		}
		
		/**
		 * Avoid updating the GUI
		 *
		 */
		public synchronized void clear() {
			min=max=-1;
		}
		
		/**
		 * Notify about the insertion of the new log in the given position
		 * 
		 * @param pos The position where the new log has been inserted
		 */
		public synchronized void update(int pos) {
			if (min==-1 || min>pos) {
				min=pos;
			}
			if (max==-1 || pos>max) {
				max=pos;
			}
		}
		
		/**
		 * Set the new interval for refreshing.
		 * 
		 * @param newInterval The new interval in microsecond
		 *        If null, then set the default interval (REFRESH_INTERVAL)
		 */
		public void setRefreshInterval(Integer newInterval) {
			if (newInterval==null) {
				refreshInterv=REFRESH_INTERVAL;
			} else {
				refreshInterv=newInterval;
			}
		}
		
		/**
		 * The thread to notify the GUI about new insertion of logs.
		 * It is a loop that notifies the table about all the changes that
		 * happened in the REFRESH_INTERVAL time.
		 */
		public void run() {
			while (!terminateRefresherThread) {
				try {
					Thread.sleep(refreshInterv);
				} catch (InterruptedException ie) {}
				if (terminateRefresherThread) {
					return;
				}
				if (min>=0 && max>=0) {
					tableModel.fireTableRowsInserted(min,max);
					min=max=-1;
				}
			}
		}
	}
	
	/**
	 * The comparator.
	 * It compares two logs starting from their indexes in the LogCache
	 * 
	 * The purpose of this class is to map indexes to logs. The real
	 * comparison is made by the compareLogs method
	 */
	private final class VisibleLogsComparator implements Comparator<Integer> {
		
		/**
		 * The field of the logs to compare
		 * -1 means that the comparison is disabled (the logs are not ordered)
		 */
		private int fieldIndex=-1;

		/**
		 * Ascending or descending way to compare (defaults to acsending)
		 */
		private boolean sortAscending = true;

		public VisibleLogsComparator() {
		}
		
		/**
		 * Compare the entries with the given position in the cache
		 * It works only for TIMESTAMP and ENTRYTYPE because the cache has
		 * two arrays and there is no need to load the log
		 * 
		 * @param fisrt The index of the first item to compare
		 * @param second The index of the second item to compare
		 * @return 
		 */
		private int fastCompare(int first, int second) {
			long firstVal=0;
			long secondVal=0;
			if (fieldIndex==Field.TIMESTAMP.ordinal()) {
				try {
					firstVal=(Long)cache.getLogTimestamp(first);
				} catch (LogCacheException e) {
					System.err.println("Error getting the time stamp of "+first);
					e.printStackTrace();
				}
				try {
					secondVal=(Long)cache.getLogTimestamp(second);
				} catch (LogCacheException e) {
					System.err.println("Error getting the time stamp of "+second);
					e.printStackTrace();
				}
			} else {
				try {
					firstVal=cache.getLogType(first).ordinal();
				} catch (LogCacheException e) {
					System.err.println("Error getting the time stamp of "+first);
					e.printStackTrace();
				}
				try {
					secondVal=cache.getLogType(second).ordinal();
				} catch (LogCacheException e) {
					System.err.println("Error getting the time stamp of "+second);
					e.printStackTrace();
				}
			}
			if (firstVal==secondVal) {
				return 0;
			} 
			int ret=(secondVal>firstVal)?1:-1;
			if (sortAscending) {
				return -1*ret;
			} else {
				return ret;
			}
		}

		/**
		 * Compare two log entries by their index in the cache
		 * 
		 * @param firtsItem The index in the LogCache of the first log entry
		 * @param secondItem The index in the LogCache of the second log entry
		 * @return a negative integer, zero, or a positive integer as the first 
		 *         argument is less than, equal to, or greater than the second
		 * 
		 * @see java.util.Comparable
		 */
		public final int compare(Integer firtsItem, Integer secondItem) {
			if (fieldIndex==Field.TIMESTAMP.ordinal() || fieldIndex==Field.ENTRYTYPE.ordinal()) {
				return fastCompare(firtsItem,secondItem);
			}
			ILogEntry log1 = null;
			try {
				log1=VisibleLogsVector.this.cache.getLog(firtsItem);
			} catch (Exception e) {
				System.err.println("Exception caught: "+e.getMessage());
				e.printStackTrace(System.err);
			}
			
			return compare(log1,secondItem);
		}
		
		/**
		 * Compare a log with another log knowing the index of this last one
		 * 
		 * @param log The first log to compare
		 * @param secondItem The index in the LogCache of the second log entry
		 * @return a negative integer, zero, or a positive integer as the first 
		 *         argument is less than, equal to, or greater than the second
		 * 
		 * @see java.util.Comparable
		 */
		public final int compare(ILogEntry log, Integer secondItem) {
			ILogEntry log2 = null;
			try {
				log2=VisibleLogsVector.this.cache.getLog(secondItem);
			} catch (Exception e) {
				System.err.println("Exception caught: "+e.getMessage());
				e.printStackTrace(System.err);
			}
			
			return compareLogs(log, log2);
		}
		
		/**
		 * Compare 2 logs
		 * 
		 * @param log1 The first log to compare
		 * @param log2 The second log to compare
		 * @return
		 */
		public int compareLogs(ILogEntry log1, ILogEntry log2) {
			if (fieldIndex<0 || fieldIndex>=Field.values().length) {
				throw new IllegalStateException("Trying to compare with comparison disabled");
			}
			if ((log1 == null) || (log2 == null))
				return 0;
				
			Comparable item1 = (Comparable)(log1).getField(Field.values()[fieldIndex]);
			Comparable item2 = (Comparable)(log2).getField(Field.values()[fieldIndex]);
		
			int returnValue = 0;
		
			int nulltest = 0;
			if (item1 == null) nulltest += 1;
			if (item2 == null) nulltest += 2;
		
			switch (nulltest) {
				case 0 : returnValue = item1.compareTo(item2); break;
				case 1 : returnValue = -1; break;
				case 2 : returnValue = 1; break;
				case 3 : returnValue = 0; break;
			}
			return (sortAscending ? returnValue : -returnValue);
		}
		
		/**
		 * 
		 * @return true if the sort is enabled
		 */
		public boolean sortEnabled() {
			return fieldIndex!=-1;
		}
		
		/**
		 * @return The current order for comparison
		 */
		public boolean isSortAscending() {
			return sortAscending;
		}
		
		/** 
		 * 
		 * @return The number of the field used to sort the logs
		 *         -1 means no ordering
		 */
		public int getSortField() {
			return fieldIndex;
		}
		
		/**
		 * Set the new comparator parameters 
		 * 
		 * @param field The field of the logs to compare
		 *              If it is -1 the comparison is disabled
		 * @param isAscending Set ascending/descening order of comparison
		 */
		public void setComparingParams(int field, boolean isAscending) {
			if (field<-1 || field>Field.values().length) {
				throw new IllegalArgumentException("Invalid comparator field "+field);
			}
			this.fieldIndex=field;
			this.sortAscending=isAscending;
		}
		
		/**
		 * Set the order of sorting
		 *  
		 * @param newSortAscending The order for sorting
		 */
		public void setSortAscending(boolean newSortAscending) {
			sortAscending = newSortAscending;
		}
	}
	
	/**
	 * The request for an async operation
	 * 
	 * Async operations must be executed with the application paused
	 * @see VisibleLogsVector.buffer
	 * @see VisibleLogsVector.asyncOps
	 * 
	 * @author acaproni
	 *
	 */
	public class LogOperationRequest {
		public static final int TERMINATE =0; // Stops the thread
		public static final int SETORDER = 1; // Reverse/reorder the logs
		public static final int FLUSH_QUEUE = 2; // Add the logs in the queue 
		
		private int opType;
		private int field; // Field for ordering
		private boolean ascending; // Direction of ordering
		
		/**
		 * Build  request for termination or to flush the logs
		 * 
		 * @param type The type of reuest.
		 *             It can be TERMINATE or FLUSH_QUEUE
		 */
		public LogOperationRequest(int type) {
			if (type!=FLUSH_QUEUE && type!=TERMINATE) {
				throw new IllegalArgumentException("Illegal operation requested");
			}
			opType=type;
		}
		
		/**
		 * Build an ordering/sorting  request
		 * 
		 * @param fld The field for ordering/sorting
		 * @param direction The ascending/descending way to order
		 */
		public LogOperationRequest(int fld, boolean direction) {
			opType= SETORDER;
			this.field=fld;
			this.ascending = direction;
		}
		
		/**
		 * Getter method 
		 * 
		 * @return The type of operation to perform 
		 */
		public int getType() {
			return opType;
		}
		
		/**
		 * Getter method 
		 * 
		 * @return The field for ordering 
		 */
		public int getOrderingField() {
			return field;
		}
		
		/**
		 * Getter method 
		 * @return The direction (true means ascending) 
		 */
		public boolean orderDirection() {
			return ascending;
		}
	}
	
	/**
	 * An entry in the buffer of the logs to add asynchronously
	 * 
	 * @author acaproni
	 *
	 */
	public class AddLogItem {
		public int index;
		public ILogEntry log;
		
		public AddLogItem(int idx, ILogEntry logEntry) {
			this.log=logEntry;
			this.index=idx;
		}
	}
	
	/**
	 * The vector of visible logs (i.e. the logs shown in the table)
	 * The elements are odered.
	 * 
	 * visibleLogs[r]=c
	 * means that the row number r of the table contains the log
	 * in position c in the cache
	 * 
	 * The vector contains the index of each log in the LogCache.
	 */
	private Vector<Integer> visibleLogs;
	
	private NewLogGUIRefresher guiRefresher;
	
	// Signal the thread to terminate
	private volatile boolean terminateThread=false;;
	
	/**
	 * Usually new logs are directly added in the GUI.
	 * They are cached if a long lasting async operation is in progress.
	 * 
	 * The thread checks if there are logs to add when it terminates
	 * other operations like a sort/reorder.
	 * 
	 * This buffer has limited size but it is intended to store logs
	 * that accidentally are added to the vector while async ops are
	 * in progress.
	 * The async ops must me executed with the appication paused so the logs
	 * are keeped on a file when they are received from the NC.
	 * Sometimes a few logs are added between the moment the application is
	 * requested to pause and the moment it is effectively paused 
	 * i.e. all the threads acknowledge the situation.
	 * 
	 * @see VisibleLogsVector.LogOperationRequest
	 */
	private Vector<AddLogItem> buffer = new Vector<AddLogItem>(16);
	
	/** The vector with the request for async operations.
	 *  If it is empty then each new log can be immediately added
	 *  in the GUI, otherwise it has to be cached in the buffer
	 *
	 *   To avoid out of memories it is better to execute async ops with 
	 *   the application paused to reduce the number of logs added in the  buffer vector.
	 *   @see documentation for VisibleLogsVector.buffer for further details
	 */
	private Vector<LogOperationRequest> asyncOps = new Vector<LogOperationRequest>();
	
	
	/**
	 * The cache with all the logs
	 */
	private LogCache cache;
	
	/**
	 * The comparator to order the logs
	 */
	private VisibleLogsComparator comparator;
	
	/**
	 * The table model that owns this object (needed to notify changes)
	 */
	private LogTableDataModel tableModel;
	
	// The logging client
	private LoggingClient loggingClient=null;
	
	/**
	 * Build a VisibleLogsVector object 
	 * 
	 * @param theCache The cache of all the logs
	 * @param model The table model that owns this object
	 */
	public VisibleLogsVector(LogCache theCache, LogTableDataModel model, LoggingClient client) {
		super("VisibleLogsVector");
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		loggingClient=client;
		this.cache=theCache;
		this.comparator = new VisibleLogsComparator();
		this.comparator.setComparingParams(Field.TIMESTAMP.ordinal(),false);
		this.tableModel=model;
		visibleLogs = new Vector<Integer>(256,32);
		// Start the thread for async operations
		terminateThread=false;
		start();
		// Instantiate and start the refresher
		guiRefresher=new NewLogGUIRefresher();
	}

	/**
	 * Add the log in the vector of the visible logs.
	 * If there asynchronous operation in progress (ordering/sorting of logs)
	 * then the log is in buffered and will be added by the thread when the
	 * ordering terminates.
	 * The same happens when the interface is paused and in that case
	 * the flush is forced when the interface is unpaused (see suspendRefresh)
	 * 
	 * @param key The key of the log in the cache
	 * @param log The log to add
	 */
	public synchronized void add(Integer key, ILogEntry log) {
		synchronized (asyncOps) {
			if (asyncOps.size()==0) {
				// Add the log to the array of visible logs 
				// i.e. it will be shown in the table at the next refresh 
				// iteration
				addLogToVector(key, log);
				return;
			} 
		}
		// There are async ops in progress: the log is buffered in the temporary vector 
		synchronized(buffer) {
			buffer.add(new AddLogItem(key,log));
		}

	}
	
	/**
	 * Remove the log with the given key from the vector
	 * of visible ligs
	 * 
	 * @param key The key of the log to delete
	 * @return The position in the table of the removed log
	 *         -1 if the log is not visible
	 */
	public synchronized int  deleteLog(Integer key) {
		if (key==null || key<0 || key>=cache.getSize()) {
			throw new IllegalArgumentException("Trying to remve a log ("+key+") that is not in the cache [0,"+cache.getSize()+'[');
		}
		int posInVisibleLogs;
		synchronized (visibleLogs) {
			posInVisibleLogs = visibleLogs.indexOf(key);
			if (posInVisibleLogs>=0) {
				if (!visibleLogs.remove(key)) {
					System.err.println("Error removing "+key+" from "+posInVisibleLogs);
				}
			}
		}
		return posInVisibleLogs;
	}
	
	/**
	 * Delete the rows containg the log whose keys are
	 * in the collection
	 * Not all the logs in keys are in the visible logs vector becuase
	 * they can be filtered out by LogLevel or user defined filters
	 * 
	 * @param keys The keys of the logs to delete
	 */
	public synchronized void deleteLogs(Collection<Integer> keys) {
		if (keys==null) {
			throw new IllegalArgumentException("Invalid null collection");
		}
		Iterator<Integer> iter = keys.iterator();
		while (iter.hasNext()) {
			Integer key = iter.next();
			synchronized (visibleLogs) {
				// The remove does not always return true (success).
				// It happens if the log repsented by the key is not visible
				// due, for example, to some filters or the log level.
				visibleLogs.remove(key);
			}
		}
	}
	
	/**
	 * Add the log to the vector of visible logs
	 *
	 */
	private void addLogToVector(Integer index, ILogEntry log) {
		if (!comparator.sortEnabled()) {
			visibleLogs.add(index);
			guiRefresher.update(index);
		} else {
			// Find the position where the log has to be inserted
			int pos = findPosLogarithmic(log);
			visibleLogs.insertElementAt(index,pos);
			guiRefresher.update(pos);
		}
	}
	
	/**
	 * Find the position in the ordered list where the log has
	 * to be inserted.
	 * The algoritm has linear complexity
	 * 
	 * @param log The log to insert in the visible logs
	 * @return The position where the log has to be inserted
	 */
	private int findPosLinear(ILogEntry log) {
		int t=0;
		for (t=0; t<visibleLogs.size(); t++) {
			if (comparator.isSortAscending() && comparator.compare(log,visibleLogs.get(t))>0) {
				return t;
			} else if (!comparator.isSortAscending() && comparator.compare(log,visibleLogs.get(t))<0) {
				return t;
			} 
		}
		return t;
	}
	
	/**
	 * Find the position in the ordered list where the log has
	 * to be inserted.
	 * The algoritm has logaritmic complexity
	 * 
	 * @param log The log to insert in the visible logs
	 * @return The position where the log has to be inserted
	 */
	private int findPosLogarithmic(ILogEntry log) {
		if (visibleLogs.size()==0) {
			return 0;
		}
		if (comparator.getSortField()==Field.TIMESTAMP.ordinal()) {
			return findPosLogarthmicDate(((Date)log.getField(Field.TIMESTAMP)).getTime());
		}
		if (comparator.getSortField()==Field.ENTRYTYPE.ordinal()) {
			return findPosLogarthmicType(((LogTypeHelper)log.getField(Field.ENTRYTYPE)).ordinal());
		}
		//int ret=-1;
		int minInter = 0;
		int maxInter = visibleLogs.size()-1;
		int middle=0;
		ILogEntry maxLog=null;
		ILogEntry minLog=null;
		do {
			maxLog=null;
			try {
				maxLog = cache.getLog(visibleLogs.get(maxInter));
			}  catch (Exception e) {
				System.err.println("Exception caught: "+e.getMessage());
				e.printStackTrace(System.err);
			}
			if (maxInter==minInter) {
				if (comparator.compareLogs(log,maxLog)>=0) {
					return minInter+1;
				} else {
					return minInter;
				}
			}
			minLog=null;
			try {
				minLog = cache.getLog(visibleLogs.get(minInter));
			}  catch (Exception e) {
				System.err.println("Exception caught: "+e.getMessage());
				e.printStackTrace(System.err);
			}
			if (maxInter-minInter==1) {
				if (comparator.compareLogs(log,minLog)<0) {
					return minInter;
				} else if (comparator.compareLogs(log,maxLog)<=0){
					return maxInter;
				} else {
					return maxInter+1;
				}
			}
			// Check if we are lucky and the log has to be inserted in the head
			// or in the tail
			if (comparator.compareLogs(log,minLog)<=0) {
				return minInter; 
			} else if (comparator.compareLogs(log,maxLog)>=0) {
				return maxInter+1;
			}
			
			middle = minInter+(maxInter - minInter)/2;
			int middleCmp = comparator.compare(log,visibleLogs.get(middle)); 
			if (middleCmp>0) {
					minInter = middle;
					continue;
			} else if (middleCmp<0) {
					maxInter = middle;
					continue;
			} else  { // middleCmp==0
					return middle;
			}
		} while (true);
	}
	
	/**
	 * When the logs are ordered by entry type, it finds the position
	 *  in the ordered list where the log has to be inserted.
	 * The algoritm has logaritmic complexity
	 * 
	 * @param The entry type of the log to be inserted
	 * @return The position log to insert in the visible logs
	 */
	private int findPosLogarthmicDate(long date) {
		int minInter = 0;
		int maxInter = visibleLogs.size()-1;
		int middle=0;
		long maxDate=0;
		long minDate=0;
		boolean sortAscending = comparator.isSortAscending();
		int iter = 0;
		do {
			try {
				maxDate = cache.getLogTimestamp(visibleLogs.get(maxInter));
			} catch (LogCacheException e) {
				System.err.println("Error getting the time stamp of "+visibleLogs.get(maxInter));
				e.printStackTrace();
			}
			try {
				minDate = cache.getLogTimestamp(visibleLogs.get(minInter));
			} catch (LogCacheException e) {
				System.err.println("Error getting the time stamp of "+visibleLogs.get(minInter));
				e.printStackTrace();
			}
			
			if (maxInter-minInter<=1) {
				if (sortAscending) {
					if (date>=maxDate) {
						return maxInter+1;
					} else if (date<minDate) {
						return minInter;
					} else {
						return maxInter;
					}
				} else {
					if (date>=minDate) {
						return minInter;
					} else if (date>=maxDate) {
						return maxInter;
					} else {
						return maxInter+1;
					}
				}
			}
			
			middle = minInter+(maxInter - minInter)/2;
			long dateShift=0;
			try {
				dateShift= date-cache.getLogTimestamp(visibleLogs.get(middle));
			} catch (LogCacheException e) {
				System.err.println("Error getting the time stamp of "+visibleLogs.get(middle));
				e.printStackTrace();
			}
			if (sortAscending) {
				if (dateShift>=0) {
						minInter = middle;
						continue;
				} else {
						maxInter = middle;
						continue;
				} 
			} else {
				if (dateShift>=0) {
					maxInter = middle;
					continue;
				} else {
					minInter = middle;
					continue;
				} 
			}
		} while (true);
	}
	
	
	/**
	 * When the logs are ordered by timestamp, it finds the position
	 *  in the ordered list where the log has to be inserted.
	 * The algoritm has logaritmic complexity
	 * 
	 * param The timestamp of the log to be inserted
	 * @return The position log to insert in the visible logs
	 */
	/*private int findPosLogarthmicDate(long date) {
		int minInter = 0;
		int maxInter = visibleLogs.size()-1;
		int middle=0;
		long maxDate;
		long minDate;
		int iter=0;
		//System.out.println("\n\n#########################################");
		//System.out.println("#########################################");
		//System.out.println("Finding pos for "+date+"\n");
		do {
			//System.out.println("\n***Iter. "+(++iter));
			//System.out.println("maxInter="+maxInter+", minInter="+minInter);
			maxDate = cache.getLogTimestamp(maxInter);
			minDate = cache.getLogTimestamp(minInter);
			//System.out.println("minDate = "+minDate);
			//System.out.println("maxDate = "+maxDate);
			

			if (maxInter-minInter<=1) {
				if (date<maxDate) {
					return maxInter+1;
				} else if (date>minDate) {
					return minInter;
				} else {
					return maxInter;
				}
			}
			
			middle = minInter+(maxInter - minInter)/2;
			long middleDate=cache.getLogTimestamp(middle);
			//System.out.println("midDate = "+middleDate);
			long dateShift = date-cache.getLogTimestamp(middle);
			//System.out.println("shft="+dateShift);
			if (dateShift<=0) {
					minInter = middle;
					continue;
			} else {
					maxInter = middle;
					continue;
			} 
		} while (true);
	}*/
	
	/**
	 * When the logs are ordered by entry type, it finds the position
	 *  in the ordered list where the log has to be inserted.
	 * The algoritm has logaritmic complexity
	 * 
	 * @param The entry type of the log to be inserted
	 * @return The position log to insert in the visible logs
	 */
	private int findPosLogarthmicType(int type) {
		int minInter = 0;
		int maxInter = visibleLogs.size()-1;
		int middle=0;
		int maxType=0;
		int minType=0;
		boolean sortAscending = comparator.isSortAscending();
		do {
			try {
				maxType = cache.getLogType(visibleLogs.get(maxInter)).ordinal();
			} catch (LogCacheException e) {
				System.err.println("Error getting the type of "+visibleLogs.get(maxInter));
				e.printStackTrace();
			}
			try {
				minType = cache.getLogType(visibleLogs.get(minInter)).ordinal();
			} catch (LogCacheException e) {
				System.err.println("Error getting the type of "+visibleLogs.get(minInter));
				e.printStackTrace();
			}
			if (maxInter-minInter<=1) {
				if (sortAscending) {
					if (type>maxType) {
						return maxInter+1;
					} else if (type<minType) {
						return minInter;
					} else {
						return maxInter;
					}
				} else {
					if (type>minType) {
						return minInter;
					} else if (type<maxType) {
						return maxInter+1;
					} else {
						return maxInter;
					}
				}
			}
			
			middle = minInter+(maxInter - minInter)/2;
			int typeShift=0;
			try {
				typeShift = type-cache.getLogType(visibleLogs.get(middle)).ordinal();
			} catch (LogCacheException e) {
				System.err.println("Error getting the type of "+visibleLogs.get(middle));
				e.printStackTrace();
			}
			if (sortAscending) {
				if (typeShift>=0) {
						minInter = middle;
						continue;
				} else {
						maxInter = middle;
						continue;
				} 
			} else {
				if (typeShift>=0) {
					maxInter = middle;
					continue;
				} else if (typeShift<0) {
					minInter = middle;
					continue;
				} 
			}
		} while (true);
	}
	
	/**
	 * @return The number of logs in the vector
	 */
	public int size() {
		return visibleLogs.size();
	}
	
	/**
	 * 
	 * @param pos The index in the vector of the log
	 *            It is the row number of the table
	 *            
	 * @return The ILogEntry in the given row/pos
	 */
	public synchronized ILogEntry get(int pos) {
		if (pos<0 || pos>=visibleLogs.size()) {
			throw new IndexOutOfBoundsException("Index out of bounds: "+pos);
		} else {
			ILogEntry log=null;
			try {
				log=cache.getLog(visibleLogs.get(pos));
			} catch (Exception e) {
				System.err.println("Exception caught getting log (visibleLogs position: "+pos+", cache pos:"+visibleLogs.get(pos)+"): "+e.getMessage());
				e.printStackTrace(System.err);
			}
			return log;
		}
	}
	
	/** 
	 * Empty the vector of visible logs
	 *
	 */
	public synchronized void clear() {
		visibleLogs.clear();
		buffer.clear();
		tableModel.fireTableDataChanged(); 
	}
	
	/**
	 * Set the comparison field and order.
	 * It creates an action for the thread that will end 
	 * in a call to the sort method
	 * 
	 * @param field The ILogENtry field to compare
	 * @param ascending true for ascending order
	 * 
	 * @see VisibleLogsVector.sort
	 */
	public synchronized void setLogsOrder(int field, boolean ascending) {
		synchronized (asyncOps) {
			asyncOps.add(new LogOperationRequest(field,ascending));
		}
		synchronized(this) {
			notifyAll();
		}
	}
	
	/**
	 * Sort the logs in the table with the given criteria
	 * 
	 * @param field
	 * @param ascending
	 */
	private void sort(int field, boolean ascending) {
		loggingClient.animateProgressBar("Sorting");
		int prevField = comparator.getSortField();
		comparator.setComparingParams(field,ascending);
		// Do we have to update the vector?
		if (visibleLogs.size()>1) {
			if (prevField==field) {
				// It is faster then resorting (linear)
				Collections.reverse(visibleLogs);
			} else {
				// Complexity is n*n*log(n)
				Collections.sort(visibleLogs,comparator);
			}
			tableModel.fireTableDataChanged();
		}
		loggingClient.freezeProgressBar();
	}
	
	/**
	 * @return If logs are ordered ascending
	 */
	public boolean isSortAscending() {
		return comparator.isSortAscending();
	}
	
	/**
	 * 
	 * @return The number of the fields f the log for comparison
	 *         -1 means no comparison activated
	 */
	public int getFieldNumForOrdering() {
		return comparator.getSortField();
	}
	
	/**
	 * The thread for async long lasting ops.
	 * At the end of each operation, it check if there are
	 * new logs to add in the buffer Vector.
	 * The ordering of logs is not performed in mutual exclusion i.e.
	 * other methods like add and setLogOrder will not hang.
	 *
	 */
	public void run() {
		LogOperationRequest request = null;
		AddLogItem item = null;
		int flushLimit=0;
		while (!terminateThread) {
			synchronized (asyncOps) {
				if (asyncOps.size()>0) {
					// The request will be removed later to avoid a critical
					// race in add 
					request = asyncOps.get(0);
				} else {
					request = null;
				}
			}
			if (terminateThread) {
				return;
			}
			if (request!=null) {
				if (request.getType()==LogOperationRequest.TERMINATE) {
							return;
				} else if (request.getType()==LogOperationRequest.SETORDER) {
					// Store the status of the application (paused/unpaused) before this rebuilding
					boolean logClientWasPaused=loggingClient.isPaused(); 
					loggingClient.setEnabledGUIControls(false);
					loggingClient.getLogEntryTable().getTableHeader().setEnabled(false);
					try {
						loggingClient.pause();
					} catch (Exception e) {}
					sort(request.getOrderingField(),request.orderDirection());
					loggingClient.getLogEntryTable().getTableHeader().setEnabled(true);
					if (!logClientWasPaused) {
						try {
							loggingClient.resume();
						} catch (Exception e) {}
					}
					loggingClient.setEnabledGUIControls(true);
					loggingClient.getLogEntryTable().getTableHeader().resizeAndRepaint();
				}
			}
			
			// If the queue of logs is not empty, flush the logs
			// This is done by the  LogOperationRequest.FLUSH_QUEUE request but
			// in the queue there are also logs added during slow operation like
			// a sort.
			// For this reason it is not a good idea to execute the flush only
			// after a LogOperationRequest.FLUSH_QUEUE is issued.
			// It mean that LogOperationRequest.FLUSH_QUEUE is used to force the
			// flush of the queue asynchronously (for example when the interface
			// is unpaused)
			flushLimit=0;
			while (buffer.size()>0) {
				synchronized(buffer) {
					item = buffer.remove(0);
				}
				addLogToVector(item.index,item.log);
				// Sleep a little bit to give other threads a chance to run
				if (++flushLimit%5000==0) {
					try {
						Thread.sleep(250);
					} catch (InterruptedException e) {}
				}
			}
			
			if (request!=null) {
				synchronized (asyncOps) {
					asyncOps.remove(0);
				}
			} 
			synchronized(this) {
				if (asyncOps.size()==0 && buffer.size()==0) {
					try {
						wait();
					} catch (InterruptedException e) {}
				}
			}
		}
	}
	
	/**
	 * Set the interval between two refreshes of the table
	 * 
	 * @param newInterv The new Interval of refreshing in microsecond
	 *                  If null the default interval is set 
	 * 
	 */
	public void setRefreshInterval(Integer newInterv) {
		guiRefresher.setRefreshInterval(newInterv);
	}
	
	/**
	 * Return the row containing a log with the given index in the cache
	 * (i.e. it is the way to know in which row a log is visible)
	 * 
	 * @param entry The position of a log in the cache
	 * @return The position (row) of the log in the table
	 *         -1 if the entry is not in the table
	 */
	public int getRowOfEntry(Integer entry) {
		return visibleLogs.indexOf(entry);
	}
	
	/**
	 * Close the threads and free all the resources
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		// Terminate the VisibleLogsVector thread
		terminateThread=true;
		synchronized (asyncOps) {
			asyncOps.add(new LogOperationRequest(LogOperationRequest.TERMINATE));
		}
		synchronized(this) {
			notifyAll();
		}
		if (sync) {
			while (isAlive()) {
				try {
					Thread.sleep(250);
				} catch (InterruptedException ie) {
					continue;
				}
			}
		}
		// Terminate the refresher thread
		if (guiRefresher!=null) {
			guiRefresher.close(sync);
		}
	}
}
