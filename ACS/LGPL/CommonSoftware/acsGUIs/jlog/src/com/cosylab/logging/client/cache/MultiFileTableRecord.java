/*    ALMA - Atacama Large Millimiter Array
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
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * This class defines a data set providing information on a single 
 * LogBufferedFileCache object. Each time a new file is created we
 * also create an instance of this class.
 * 
 * LogFileTableRecord objects are organized in a vector to form the
 * log file table used to retrieve the logs according to the user's
 * defined keyword.
 * 
 * @author mcomin
 */

public class MultiFileTableRecord {
	
	/**
	 * Build a new object
	 * 
	 * @throws LogCacheException
	 */
	public MultiFileTableRecord() throws LogCacheException {
		minLogIdx=Integer.MAX_VALUE;
		maxLogIdx=Integer.MIN_VALUE; 
		lbfc= new LogBufferedFileCache();
	}
	
	// Reference to an LogBufferedFileCache object
	private LogBufferedFileCache lbfc;
	
	/** Minimum and maximum log keys stored in the LogBufferedFileCache
	 * These two numbers are used to locate a given log by its keyword.
	 * <P>
	 * <B>NOTE</B>: These numbers are updated only while adding logs
	 *              i.e. if a log is deleted it can happen that is not in the record even if
	 *              its key is in the range <code>[minLogIdx,maxLogIdx]</code>
	 */
	private int minLogIdx;
	private int maxLogIdx;
	
	/**
	 * 
	 * @return The number of logs in this record
	 */
	public int getNumOfLogs() {
		return lbfc.getSize();
	}
	
	/**
	 * return the size of the file on disk
	 * 
	 * @return The size of the file
	 * @throws IOException In case of error getting the size of the file
	 */
	public long getFileSize() throws IOException {
		return lbfc.getFileSize();
	}
	
	/**
	 * Add a log to the cache and updates the max and min indexes
	 * 
	 * @param log The log to add to the cache
	 * @param key The key of the new log
	 * @throws LogCacheException If an error happens while adding the log
	 */
	public void addLog(ILogEntry log, int key) throws LogCacheException {
		if (log==null) {
			throw new IllegalArgumentException("The log can't be null");
		}
		lbfc.add(log);
		if (key<minLogIdx) {
			minLogIdx=key;
		}
		if (key>maxLogIdx) {
			maxLogIdx=key;
		}
	}

	/**
	 * @return the minLogIdx
	 */
	public int getMinLogIdx() {
		return minLogIdx;
	}

	/**
	 * @return the maxLogIdx
	 */
	public int getMaxLogIdx() {
		return maxLogIdx;
	}
	
	/**
	 * Return the log with the given key
	 * 
	 * @param key The not <code>null</code> key of identifying the log
	 * @return the log with the given key
	 */
	public ILogEntry getLog(Integer key) throws LogCacheException {
		if (key==null) {
			throw new IllegalArgumentException("The key can't be null");
		}
		if (key<minLogIdx || key>maxLogIdx) {
			throw new LogCacheException("Key "+key+" out of range ["+minLogIdx+", "+maxLogIdx+"]");
		}
		return lbfc.getLog(key-minLogIdx);
	}
	
	/**
	 * Flush the buffer on disk delegating to the <code>LogBufferedFileCache</code>
	 * 
	 * @throws LogCacheException In case of error flushing on disk
	 */
	public void flushBuffer() throws LogCacheException {
		lbfc.flushBuffer();
	}
	
	/**
	 * Delete the log with the given key
	 * 
	 * @param key The not <code>null</code> key of identifying the log
	 * @throws <code>LogCacheException</code> in case of error deleting the log
	 * 
	 */
	public void deleteLog(Integer key) throws LogCacheException {
		if (key==null) {
			throw new IllegalArgumentException("The key can't be null");
		}
		lbfc.deleteLog(key-minLogIdx);
	}
	
	/**
	 * Remove all the logs and free the resources by
	 * delegating to the <code>LogBufferedFileCache</code>
	 * 
	 * @throws <code>LogCacheException</code> in case of error clearing the buffer
	 */
	public void clear() throws LogCacheException {
		minLogIdx=Integer.MAX_VALUE;
		maxLogIdx=Integer.MIN_VALUE; 
		lbfc.clear();
	}
	
	/**
	 * @return <code>true</code> if the record contains no log
	 */
	public boolean isEmpty() {
		return lbfc.getSize()==0;
	}
	
	/**
	 * Return the key of the first log in this record
	 * 
	 * @return The key of the first log in this record
	 * @see ILogMap
	 */
	public Integer getFirstLog() {
		if (isEmpty()) {
			return null;
		}
		return lbfc.getFirstLog()+minLogIdx;
	}
	
	/**
	 * Return the key of the last log in this record
	 * 
	 * @return The key of the last log in this record
	 * @see ILogMap
	 */
	public Integer getLastLog() {
		if (isEmpty()) {
			return null;
		}
		return lbfc.getLastLog()+minLogIdx;
	}
	
	/**
	 * Append at most n keys from the first valid logs to the collection.
	 * First here means first in the FIFO policy.
	 * 
	 * The number of added keys can be less then n if the cache doesn't
	 * contain enough logs.
	 * 
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public int getFirstLogs(int n, Collection<Integer> keys) {
		return lbfc.getFirstLogs(n, keys);
	}
	
	/**
	 * The keys in the map
	 * 
	 * @return The key in the map
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public Set<Integer> keySet() {
		Set<Integer> keys = lbfc.keySet();
		// Pad the keys by adding the min  
		Set<Integer> ret = new HashSet<Integer>();
		for (Integer key: keys) {
			ret.add(key+minLogIdx);
		}
		return ret;
	}
	
	/**
	 * Replace the log with the given key
	 
	 * @param key The key of the log to replace
	 * @param log The new log
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public void replaceLog(Integer key, ILogEntry log) throws LogCacheException {
		lbfc.replaceLog(key-minLogIdx, log);
	}
}

