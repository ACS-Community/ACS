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

import java.util.HashMap;
import java.util.LinkedList;

import com.cosylab.logging.engine.log.ILogEntry;



/**
 * The class extends the cache on file implemented in LogFileCache
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
	private LinkedList<Integer> manager = new LinkedList<Integer>();
	
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
			hitLog(position);
			return log;
		} else {
			// Ops we need to read a log from disk!
			return loadNewLog(position);
		}
	}
	
	/**
	 * Get a log from the cache on disk updating all the 
	 * internal lists
	 * 
	 * @param index The position of the log
	 * @return The log read from the cache on disk
	 */
	private synchronized ILogEntry loadNewLog(Integer index) throws LogCacheException {
		// A little check: each index must appear only once in the list
		//
		// This check can cause a scansion of the list
		// so it is probably better to remove after debugging
		if (manager.contains(index)) {
			throw new LogCacheException (""+index+" is already in the list!");
		}
		
		// Read the new log from the cache on disk
		ILogEntry log = super.getLog(index);
		
		// There is enough room in the lists?
		if (cache.size()==actualCacheSize) {
			// We need to create a room for the new element
			Integer itemToRemove = manager.removeFirst();
			cache.remove(itemToRemove);
		}
		
		// Add the log in the cache
		cache.put(index,log);
		
		// Append the index in the manager list
		manager.add(index);
		
		return log;
	}
	
	/**
	 * Update the lists when a log has been read from the cache
	 * The key of the hitted log must be moved toward the end of the 
	 * manager list (as the Indexes are removd from the head of the
	 * list, the probability to remove the most accessed logs is reduced)
	 * 
	 * The hashMap does not need any maintenance
	 * 
	 * @param idx 
	 */
	private synchronized void hitLog(Integer index) {
		// Find the position of the item in the manager list
		int pos = manager.indexOf(index);
		if (pos==-1) {
			// Why this element is not here?
			throw new IllegalArgumentException(""+index+" is not in the list!");
		}
		
		// If the element is in the last position then there is nothing
		// to do because it can't do anything better then that!
		// Remember: we remove the items from the head, the first position.
		if (pos==manager.size()-1) {
			return;
		}
		// Move the hitted index one position toward the end of the list
		Integer temp=manager.get(pos+1);
		manager.set(pos+1,index);
		manager.set(pos,temp);
	}
	
	/**
	 * Empty the cache
	 * 
	 */
	public synchronized void clear() throws LogCacheException {
		cache.clear();
		manager.clear();
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
