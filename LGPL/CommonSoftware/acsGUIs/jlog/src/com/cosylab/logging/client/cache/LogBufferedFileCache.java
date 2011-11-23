/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package com.cosylab.logging.client.cache;

import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * This class extends the LogFileCache adding the buffering of the logs
 * that must be written on disk.
 * 
 * It uses a WriteBuffer to store the logs to write on disk. 
 * The purpose of this class is to write several logs at once reducing
 * the write operations on disk.
 * 
 * The buffer stores the logs when  they are added. 
 * It flushes the buffer on disk when the number of logs in memory 
 * exceeds the maximum size. This means that the number of chars written on
 * the file cache at once is not fixed but depends on the lengths of the
 * logs stored in this buffer.
 * To enhance performance and to have the size of the file always available,
 * the buffer stores the log together with other info in Map of BufferedCacheItem    
 *  
 * @author acaproni
 */
public class LogBufferedFileCache extends LogFileCache implements ILogMap {
	
	/**
	 * Objects of this class represent an entry of the buffer.
	 * For each log, the string to write on disk is also stored
	 * This allows
	 * 1 to know the size of the file on disk taking in account the size
	 *   of the buffer
	 * 2 to convert only once the log in a cache string
	 * 
	 * @author acaproni
	 *
	 */
	private class BufferedCacheItem {
		private ILogEntry logEntry;
		private String logCacheString;
		public BufferedCacheItem(ILogEntry log, String cacheString) {
			if (log==null) {
				throw new IllegalArgumentException("Invalid null log entry");
			}
			if (cacheString==null || cacheString.length()==0) {
				throw new IllegalArgumentException("Invalid log cache string "+cacheString);
			}
			logEntry=log;
			logCacheString= cacheString;
		}

		/**
		 * Getter 
		 * 
		 * @return The log entry
		 */
		public ILogEntry getLogEntry() {
			return logEntry;
		}
		
		/**
		 * Getter 
		 * 
		 * @return The string to write on disk
		 */
		public String getLogCacheString() {
			return logCacheString;
		}
	}
	
	public static final String WRITEBUFFERSIZE_PROPERTY_NAME = "jlog.cache.writebuffersize";
	public static final int DEFAULT_WRITEBUFFERSIZE = 8192;
	
	// The buffer of logs is a TreeMap having has key the identifier
	// of the log and a BufferedCacheItem as value
	private TreeMap<Integer,BufferedCacheItem> buffer= new TreeMap<Integer,BufferedCacheItem>();
	
	// The capacity of the buffer (measured as number of logs):
	// when the buffer is full it is flushed on disk 
	private int size;
	
	// The size of the chars in the buffer
	// 
	// It is the length of the string to write on disk containing all the logs
	// translated in the cache format.
	// This allows to know the size of the file on disk taking into account
	// the size of the logs in the buffer
	private volatile long bufferFileSize=0;
	
	/**
	 * Build a LogBufferedFileCache with the given size for the cache and the
	 * write buffer.
	 * If there isn't enough memory for the cache, tries with a smmaller
	 * size.
	 * 
	 * @param cacheSize The size of the cache
	 * @param writeBufferSize The size of the write buffer
	 * @throws LogCacheException If there is'nt enough memory for a buffer of at least 32 logs
	 * 
	 */
	public LogBufferedFileCache(int writeBufferSize) throws LogCacheException {
		if (writeBufferSize<=0) {
			throw new IllegalArgumentException("Invalid size for the buffer "+writeBufferSize);
		}
		size = writeBufferSize;
	}
	
	/**
	 * Build a LogBufferedFileCache with the default sizes for
	 * the write buffer
	 * 
	 * @throws LogCacheException
	 * 
	 * @see LogBufferedFileCache.getDefaultWriteCacheSize
	 */
	public LogBufferedFileCache() throws LogCacheException {
		this(getDefaultWriteCacheSize());
	}
	
	/**
	 * Empty the cache 
	 * 
	 * @throws IOException
	 */
	public synchronized void clear() throws LogCacheException {
		super.clear();
		synchronized(buffer) {
			buffer.clear();
		}
		bufferFileSize=0;
	}

	/**
	 * Return the log with the given key
	 *  
	 * @param key The key of the log
	 * @return The LogEntryXML or null in case of error
	 */
	public synchronized ILogEntry getLog(Integer key) throws LogCacheException {
		BufferedCacheItem item = buffer.get(key);
		if (item==null) {
			return super.getLog(key);
		}
		return  item.getLogEntry();
	}
	
	/**
	 * Delete a log with the given key
	 * 
	 * @param pos The key of the log to delete
	 */
	public synchronized void deleteLog(Integer key) throws LogCacheException {
		BufferedCacheItem itemToDelete = buffer.get(key);
		if (itemToDelete!=null) {
			synchronized (buffer) {
				buffer.remove(key);
			}
			bufferFileSize-=itemToDelete.getLogCacheString().length();
		} else {
			super.deleteLog(key);
		}
	}
	
	/**
	 * Delete a collection of logs 
	 * 
	 * @param keys The keys of the logs to remove from the cache
	 */
	public synchronized void deleteLogs(Collection<Integer> keys) throws LogCacheException {
		if (keys==null) {
			throw new IllegalArgumentException("Illegal null collection of logs to delete");
		}
		Iterator<Integer> iter =keys.iterator();
		while (iter.hasNext()) {
			Integer key = iter.next();
			BufferedCacheItem cacheItem;
			synchronized(buffer) {
				cacheItem=buffer.remove(key);
			}
			if (cacheItem!=null) {
				iter.remove();
				bufferFileSize-=cacheItem.getLogCacheString().length();
			}
		}
		if (keys.size()>0) {
			super.deleteLogs(keys);
		}
	}
	
	/**
	 * Append a log to the cache
	 * 
	 * @param log The log to append in the cache
	 * @return The key of the added log
	 */
	public synchronized int add(ILogEntry log) throws LogCacheException {
		if (log==null) {
			throw new LogCacheException("Error: trying to add a null log to the buffer");
		}
		int logInBuffer;
		BufferedCacheItem cacheItem = new BufferedCacheItem(log,toCacheString(log));
		synchronized (buffer) {
			buffer.put(logID,cacheItem);
			logInBuffer=buffer.size();
			bufferFileSize+=cacheItem.getLogCacheString().length();
		}
		if (logInBuffer==size) {
			flushBuffer();
		}
		return logID++;
	}
	
	/**
	 * Gets the default size ot the write buffer, which comes either from the system property
	 * <code>jlog.cache.writebuffersize</code> (see {@link #WRITEBUFFERSIZE_PROPERTY_NAME}) 
	 * or, if this property is undefined or invalid, from the fixed size given by 
	 * {@link #DEFAULT_WRITEBUFFERSIZE}.
	 *  
	 * @return the default size of the write buffer to be used if none is given in the constructor
	 */
	private static int getDefaultWriteCacheSize() {
		Integer cacheSizeFromProperty = Integer.getInteger(WRITEBUFFERSIZE_PROPERTY_NAME);
		if (cacheSizeFromProperty != null) {
			return cacheSizeFromProperty.intValue();
		}
		return DEFAULT_WRITEBUFFERSIZE;
	}
	
	/**
	 * Flush all the logs on file
	 *
	 */
	public void flushBuffer() throws LogCacheException {
		// str is the buffer of logs to write on disk at once
		StringBuilder str = new StringBuilder();
		if (file==null) {
			try {
				initCache();
			} catch (IOException e) {
				throw new LogCacheException("Error flushing buffer",e);
			}
		}
		
		// The position of the first log to add on disk
		long startingPos;
		try {
			startingPos = file.length();
		} catch (IOException ioe) {
			throw new LogCacheException("Error getting the length of the file ",ioe);
		}
		// Prepare the buffer and the index
		LogFileCache.LogCacheInfo info;
		for (Integer key: buffer.keySet()) {
			BufferedCacheItem item = buffer.get(key);
			info = new LogFileCache.LogCacheInfo();
			info.start=startingPos+str.length();
			str.append(item.getLogCacheString());
			info.len=item.getLogCacheString().length();
			if (buffer.containsKey(key)) {
				index.put(key,info);
			} 
		}
		
		// Write the buffer on disk
		synchronized (file) {
			try {
				file.seek(startingPos);
				file.writeBytes(str.toString());
			} catch (IOException ioe) {
				throw new LogCacheException("Error writing the buffer on disk",ioe);
			}
		}
		// Clear the buffer
		synchronized (buffer) {
			buffer.clear();
		}
		bufferFileSize=0;
	}
	
	/**
	 *  Return the number of logs in cache
	 *  
	 *  @return The number of logs in cache
	 */
	public synchronized int getSize() {
		int sz;
		synchronized (buffer) {
			sz=buffer.size();
		}
		return super.getSize()+sz;
	}
	
	/**
	 * Return the length of the file on disk taking into account
	 * the length of the string to write on the disk for the logs in the
	 * buffer.
	 * 
	 * @return The size of the file cache
	 * 
	 * @throws IOException in case of I/O error
	 * @see java.io.RandomAccessFile
	 */
	public long getFileSize() throws IOException{
		return super.getFileSize()+bufferFileSize;
	}
	
	/**
	 * 
	 * @return The number of logs actually in the buffer
	 */
	public synchronized final int getBufferSize() {
		return buffer.size();
	}
	
	/**
	 * Return the key of the first valid log (FIFO).
	 * The key of the first log is 0 but it can change if the log 0 has
	 * been deleted.
	 * 
	 * @return The key of the first log
	 *         null if the cache is empty
	 */
	public Integer getFirstLog() {
		Integer cacheFirstLog = super.getFirstLog();
		Integer bufferFirstLog;
		synchronized(buffer) {
			if (buffer.isEmpty()) {
				return cacheFirstLog;
			} else {
				bufferFirstLog=buffer.firstKey();
			}
		}
		
		if (cacheFirstLog==null) {
			return bufferFirstLog;
		} else {
			return Math.min(cacheFirstLog,bufferFirstLog);
		}
	}
	
	/**
	 * Append at most n keys from the first valid logs to the collection.
	 * First here means first in the FIFO policy.
	 * 
	 * The number of added keys can be less then n if the cache doesn't
	 * contain enough logs.
	 * 
	 * @param n The desired number of keys of first logs
	 * @param keys The collection to add they keys to
	 * @return The number of keys effectively added
	 */
	public int getFirstLogs(int n, Collection<Integer> keys) {
		if (n<=0 || keys==null) {
			throw new IllegalArgumentException("Invalid number of requested keys or null collection");
		}
		int ret=super.getFirstLogs(n,keys);
		if (ret<n) {
			Set<Integer> allTheKeys = buffer.keySet();
			Iterator<Integer> iter = allTheKeys.iterator();
			while (iter.hasNext() && ret<n) {
				keys.add(iter.next());
				ret++;
			}
		}
		
		return ret;
	}
	
	/**
	 * Return the key of the last valid log (FIFO)
	 * The key of the last log is the key of the last inserted log
	 * but it can cheang if such log has been deleted
	 * 
	 * @return The key of the last inserted log
	 *         null if th cache is empty
	 */
	public Integer getLastLog() {
		Integer bufferLastLog;
		Integer cacheLastLog = super.getLastLog();
		synchronized(buffer) {
			if (buffer.isEmpty()) {
				return cacheLastLog;
			} else {
				bufferLastLog=buffer.lastKey();
			}
		}
		
		if (cacheLastLog==null) {
			return bufferLastLog;
		} else {
			return Math.max(cacheLastLog,bufferLastLog);
		}
	}
	
	/**
	 * Return a set with all the keys of the logs in cache
	 * 
	 * @return The keys of the logs in cache
	 */
	public Set<Integer> keySet() {
		HashSet<Integer> ret = new HashSet<Integer>();
		ret.addAll(super.keySet());
		ret.addAll(buffer.keySet());
		return ret;
	}
	
	/**
	 * Return an iterator over the logs in cache
	 */
	public Iterator<ILogEntry> iterator() {
		return new LogIterator(this);
	}
	
	/**
	 * Replace the log in the given position with the new one
	 * <P>
	 * If the log to replace is not in the buffer, <code>LogBufferedFileCache</code>
	 * delegated to <code>LogFileCache</code>
	 
	 * @param position The position of the log to replace
	 * @param log The key (identifier) of the log
	 */
	@Override
	public synchronized void replaceLog(Integer key, ILogEntry log) throws LogCacheException {
		synchronized (buffer) {
			if (buffer.containsKey(key)) {
				BufferedCacheItem oldItem=buffer.get(key);
				BufferedCacheItem cacheItem = new BufferedCacheItem(log,toCacheString(log));
				buffer.put(key,cacheItem);
				bufferFileSize=bufferFileSize+cacheItem.getLogCacheString().length()-oldItem.getLogCacheString().length();
				return;
			}
		}
		super.replaceLog(key, log);
	}
	
}
