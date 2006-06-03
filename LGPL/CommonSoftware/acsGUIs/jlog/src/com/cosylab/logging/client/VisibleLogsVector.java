package com.cosylab.logging.client;

import java.util.Vector;
import java.util.Collections;
import java.util.Comparator;

import com.cosylab.logging.LogTableDataModel;

import com.cosylab.logging.client.cache.LogCache;

import com.cosylab.logging.engine.log.ILogEntry;

import com.cosylab.logging.LoggingClient;

/**
 * The array of visible logs (i.e. all and only the logs shown in the table)
 * The logs filtered out are not present in this object (while they are 
 * in the list of logs, LogCache).
 * This class manage the ordering/sorting of logs.
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
	
	public class NewLogGUIRefresher extends Thread {
		private int min;
		private int max;
		
		public NewLogGUIRefresher() {
			clear();
			start();
		}
		
		public synchronized void clear() {
			min=max=-1;
		}
		
		public synchronized void update(int pos) {
			if (min==-1 || min>pos) {
				min=pos;
			}
			if (max==-1 || pos>max) {
				max=pos;
			}
		}
		
		public void run() {
			while (true) {
				try {
					Thread.sleep(1000);
				} catch (InterruptedException ie) {}
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
			if (fieldIndex<0 || fieldIndex>=ILogEntry.NUMBER_OF_FIELDS) {
				throw new IllegalStateException("Trying to compare with comparison disabled");
			}
			if ((log1 == null) || (log2 == null))
				return 0;
				
			Comparable item1 = (Comparable)(log1).getField(fieldIndex);
			Comparable item2 = (Comparable)(log2).getField(fieldIndex);
		
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
			if (field<-1 || field>ILogEntry.NUMBER_OF_FIELDS) {
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
	 * @author acaproni
	 *
	 */
	public class LogOperationRequest {
		public static final int TERMINATE =0; // Stops the thread
		public static final int SETORDER = 1; // Reverse/reorder the logs
		public static final int ADDLOG = 2; // Add a log
		
		private int opType;
		private int field; // Field for ordering
		private boolean ascending; // Direction of ordering
		
		/**
		 * Build an a termination request
		 */
		public LogOperationRequest() {
			opType=TERMINATE;
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
	 * The vector contains the index of each log in the LogCache.
	 */
	private Vector<Integer> visibleLogs;
	
	private NewLogGUIRefresher guiRefresher;
	
	/**
	 * Usually new logs are directly added in the GUI.
	 * They are cached if a long lasting async operation is in progress.
	 * The thread will check if there are logs to add when it terminates
	 * other operations
	 */
	private Vector<AddLogItem> buffer = new Vector<AddLogItem>(16);
	
	// The vector with the request for async operations.
	// If it is empty then each new log can be immediately added
	// in the GUI, otherwise it has to be caches in the buffer
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

	/**
	 * Build a VisibleLogsVector object 
	 * 
	 * @param theCache The cache of all the logs
	 * @param model The table model that owns this object
	 */
	public VisibleLogsVector(LogCache theCache, LogTableDataModel model) {
		super();
		this.cache=theCache;
		this.comparator = new VisibleLogsComparator();
		this.comparator.setComparingParams(ILogEntry.FIELD_TIMESTAMP,false);
		this.tableModel=model;
		visibleLogs = new Vector<Integer>(256,32);
		// Start the thread for async operations
		start();
		// Instantiate and start the refresher
		guiRefresher=new NewLogGUIRefresher();
	}

	/**
	 * Add the log in the vector of the visible logs.
	 * If there asynchronous operation in progress (ordering/sorting of logs)
	 * then the log is in buffer and will be added by the thread when the
	 * ordering terminates.
	 * 
	 * @param index The index of the log to add
	 * @param log The log to add
	 */
	public synchronized void add(Integer index, ILogEntry log) {
		synchronized (asyncOps) {
			if (asyncOps.size()==0) {
				addLogToVector(index, log);
				return;
			} 
		} 
		synchronized(buffer) {
			buffer.add(new AddLogItem(index,log));
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
			// Find the position where the log has to be inserted
		} else {
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
		//int ret=-1;
		int minInter = 0;
		int maxInter = visibleLogs.size()-1;
		int middle=0;
		ILogEntry maxLog=null;
		ILogEntry minLog=null;
		int iter =0;
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
				System.err.println("Exception caught: "+e.getMessage());
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
	
	private void sort(int field, boolean ascending) {
		LoggingClient.getInstance().animateProgressBar("Sorting");
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
		LoggingClient.getInstance().freezeProgressBar();
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
	 * The thread for async long lasting op.
	 * At the end of each operation, it check if there are
	 * new logs to add in the buffer Vector.
	 * The ordering of logs is not performed in mutual exclusion i.e.
	 * other methods like add and setLogOrder will not hang.
	 *
	 */
	public void run() {
		LogOperationRequest request = null;
		AddLogItem item = null;
		while (true) {
			synchronized (asyncOps) {
				if (asyncOps.size()>0) {
					// The request will be removed later to avoid a critical
					// race in add 
					request = asyncOps.get(0);
				} else {
					request = null;
				}
			}
			if (request!=null) {
				if (request.getType()==LogOperationRequest.TERMINATE) {
							return;
				} else {
					sort(request.getOrderingField(),request.orderDirection());
				}
			}
			// There are new logs to add?
			synchronized(buffer) {
				while (buffer.size()>0) {
					item = buffer.remove(0);
					addLogToVector(item.index,item.log);
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
	
}
