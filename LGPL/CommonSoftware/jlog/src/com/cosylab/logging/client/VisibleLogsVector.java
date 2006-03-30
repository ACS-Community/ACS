package com.cosylab.logging.client;

import java.util.Vector;
import java.util.Collections;
import java.util.Comparator;


import com.cosylab.logging.LogTableDataModel;

import com.cosylab.logging.client.cache.LogCache;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * The array of visible logs (i.e. all and only the logs shown in the table)
 * The logs filtered out are not present in this object (while they are 
 * in the list of logs, LogCache).
 * This class manage the ordering/sorting of logs.
 *  
 * @author acaproni
 *
 */
public class VisibleLogsVector {
	
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
			ILogEntry log1 = VisibleLogsVector.this.cache.getLog(firtsItem);
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
			ILogEntry log2 = VisibleLogsVector.this.cache.getLog(secondItem);
			
			
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
	 * The vector of visible logs (i.e. the logs shown in the table)
	 * The elements are odered.
	 * 
	 * The vector contains the index of each log in the LogCache.
	 */
	private Vector<Integer> visibleLogs;
	
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
		this.comparator.setComparingParams(ILogEntry.FIELD_ENTRYTYPE,false);
		this.tableModel=model;
		visibleLogs = new Vector<Integer>(256,32);
	}

	/**
	 * Add the log in the vector of the visible logs
	 * 
	 * @param index The index of the log to add
	 * @param log The log to add
	 */
	public void add(Integer index, ILogEntry log) {
		if (!comparator.sortEnabled()) {
			visibleLogs.add(index);
			tableModel.fireTableRowsInserted(index,index);
			// Find the position where the log has to be inserted
		} else {
			int pos = findPos2(log);
			visibleLogs.insertElementAt(index,pos);
			tableModel.fireTableRowsInserted(pos,pos);
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
	private int findPos(ILogEntry log) {
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
	private int findPos2(ILogEntry log) {
		if (visibleLogs.size()==0) {
			return 0;
		}
		//int ret=-1;
		int minInter = 0;
		int maxInter = visibleLogs.size()-1;
		int middle=0;
		ILogEntry maxLog=null;
		ILogEntry minLog=null;
		do {
			maxLog = cache.getLog(visibleLogs.get(maxInter));
			if (maxInter==minInter) {
				if (comparator.compareLogs(log,maxLog)>=0) {
					return minInter+1;
				} else {
					return minInter;
				}
			}
			minLog = cache.getLog(visibleLogs.get(minInter));
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
			switch (comparator.compare(log,visibleLogs.get(middle))) {
				case +1: {
					minInter = middle;
					continue;
				}
				case -1: {
					maxInter = middle;
					continue;
				}
				case 0: {
					return middle;
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
	public ILogEntry get(int pos) {
		if (pos<0 || pos>=visibleLogs.size()) {
			throw new IndexOutOfBoundsException("Index out of bounds: "+pos);
		} else {
			return cache.getLog(visibleLogs.get(pos));
		}
	}
	
	/** 
	 * Empty the vector of visible logs
	 *
	 */
	public void clear() {
		visibleLogs.clear();
	}
	
	/**
	 * Set the comparison field and order
	 * 
	 * @param field The ILogENtry field to compare
	 * @param ascending true for ascending order
	 */
	public void setLogsOrder(int field, boolean ascending) {
		System.out.println("Set sortting to ["+field+","+ascending+"]");
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
}
