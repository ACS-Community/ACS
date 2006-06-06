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
package com.cosylab.logging.client.cache;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.ArrayBlockingQueue;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * The class extends the cache on file implemented in LogBufferedFileCache
 * keeping a cache of logs in memory to avoid to access the file
 * for the most frequently accessed logs
 * 
 * The cache stores the logs into an HashMap using their index as key
 * 
 * @author acaproni
 *
 */
public class LogCache extends LogBufferedFileCache {

	public static final String CACHESIZE_PROPERTY_NAME = "jlog.cache.size";
	public static final int DEFAULT_CACHESIZE = 16384;

	private int actualCacheSize;
	
	/**
	 * The logs are stored into an HashMap.
	 * The key is the index of the log
	 */
	private HashMap<Integer,ILogEntry> cache;
	
	/**
	 * The following list used to keep ordered the indexes
	 * in such a way it is fast to insert/remove logs in the cache.
	 * The indexes contained in this object are the indexes in the cache hashmap
	 * and the size of the HashMap and the LinkedList is always the same.
	 * 
	 * The functioning is the following:
	 *  - new elements are added in the tail
	 *  - old elemnts are removed from the head
	 *  - whenever an elements is accessed it is moved of one position
	 *    toward the tail reducing the canche to be removed
	 *    The moving operation is performed swapping the accessed elements
	 *    with its neighbor 
	 */
	private ArrayBlockingQueue<Integer> manager = null;
	
	/** 
	 * The aray with the level of each log in the cache
	 * (useful for setting the log level)
	 */
	private ArrayList<Integer> logTypes = new ArrayList<Integer>(256);
	
	/**
	 * The array with the timestamp of each log in the cache
	 * (useful for sorting)
	 */
	private ArrayList<Long> logTimes = new ArrayList<Long>(256);
		
	/**
	 * Build a LogCache object
	 * 
	 * @throws IOException The exception is thrown if the base class
	 *                     is not able to create the cache on a file
	 */
	public LogCache() throws LogCacheException {
		this(getDefaultCacheSize()); 
	}
	
	/** 
	 * Adds a log in the cache.
	 * It does nothing because the adding is done by its parent class.
	 * What it does is to store the level of the log in the array
	 * 
	 * @param log The log to add in the cache
	 * @return The position of the added log in the cache
	 * @throws LogCacheException If an error happened while adding the log
	 */
	public int add(ILogEntry log) throws LogCacheException {
		int pos = super.add(log);
		if (pos!=logTypes.size()) {
			throw new LogCacheException("Inconsistent state of the logLevels array");
		}
		synchronized (logTypes) {
			logTypes.add((Integer)log.getField(ILogEntry.FIELD_ENTRYTYPE));
		}
		synchronized (logTimes) {
			logTimes.add(((Date)log.getField(ILogEntry.FIELD_TIMESTAMP)).getTime());
		}
		return pos;
	}

	/** 
	 * 
	 * @param pos The position of the log
	 * @return The type of the log in the given position
	 */
	public int getLogType(int pos) {
		if (pos<0 || pos>=logTypes.size()) {
			throw new IndexOutOfBoundsException(""+pos+" is not in the array ["+0+","+logTypes.size()+"]");
		}
		return logTypes.get(pos);
	}
	
	/** 
	 * 
	 * @param pos The position of the log
	 * @return The timestamp of the log in the given position
	 */
	public long getLogTimestamp(int pos) {
		if (pos<0 || pos>=logTypes.size()) {
			throw new IndexOutOfBoundsException(""+pos+" is not in the array ["+0+","+logTypes.size()+"]");
		}
		return logTimes.get(pos);
	}
	
	/**
	 * Build a logCache object of the given size
	 * 
	 * @param size The size of the cache
	 * @throws LogCacheException
	 */
	public LogCache(int size) throws LogCacheException {
		super();
		if (size<=0) {
			throw new LogCacheException("Invalid initial size: "+size);
		}
		actualCacheSize = size;
		System.out.println("Jlog will use cache for " + actualCacheSize + " log records.");		
		cache = new HashMap<Integer,ILogEntry>(actualCacheSize);
		manager = new ArrayBlockingQueue<Integer>(actualCacheSize, true);
		clear();
	}
	
	/**
	 * Gets the default cache size, which comes either from the system property
	 * <code>jlog.cache.size</code> (see {@link #CACHESIZE_PROPERTY_NAME}) 
	 * or, if this property is undefined or invalid, from the fixed size given by 
	 * {@link #DEFAULT_CACHESIZE}. 
	 * @return the default cache size to be used if none is given in the constructor
	 */
	private static int getDefaultCacheSize() {
		Integer cacheSizeFromProperty = Integer.getInteger(CACHESIZE_PROPERTY_NAME);
		if (cacheSizeFromProperty != null) {
			return cacheSizeFromProperty.intValue();
		}
		else {
			return DEFAULT_CACHESIZE;
		}
	}
	
	/**
	 * Return the log in the given position.
	 * The method is synchronized because both the HashMap and
	 * the LinkedList must be synchronized if there is a chance
	 * to acces these objects from more then one thread in the same 
	 * time
	 * @see java.util.LinkedList
	 * @see java.util.HashMap
	 *  
	 * @param pos The position of the log
	 * @return The LogEntryXML or null in case of error
	 */
	public synchronized ILogEntry getLog(int pos) throws LogCacheException {
		Integer position = new Integer(pos);
		ILogEntry log = cache.get(position);
		if (log!=null) {
			// Hit! The log is in the cache
			return log;
		} else {
			// Oops we need to read a log from disk!
			return loadNewLog(position);
		}
	}
	
	/**
	 * Get a log from the cache on disk updating all the 
	 * internal lists
	 * 
	 * @param idx The position of the log
	 * @return The log read from the cache on disk
	 */
	private synchronized ILogEntry loadNewLog(Integer idx) throws LogCacheException {
		// Read the new log from the cache on disk
		ILogEntry log = super.getLog(idx);
		
		// There is enough room in the lists?
		if (cache.size()==actualCacheSize) {
			// We need to create a room for the new element
			try {
				Integer itemToRemove = manager.take();
				cache.remove(itemToRemove);
			}
			catch(InterruptedException ex) {
				throw new LogCacheException ("Interrupted while waiting to take from the Queue!");
			}
		}
		
		// Add the log in the cache
		cache.put(idx,log);
		
		// Append the index in the manager list
		manager.add(idx);
		
		return log;
	}
	
	
	/**
	 * Empty the cache
	 * 
	 */
	public synchronized void clear() throws LogCacheException {
		cache.clear();
		manager.clear();
		logTypes.clear();
		logTimes.clear();
		super.clear();
	}
	
	/**
	 * Gets the actual cache size, which may come from {@link #getDefaultCacheSize()} or
	 * may be given in the constructor.
	 * @return
	 */
	public int getCacheSize() {
		return actualCacheSize;
	}
}
