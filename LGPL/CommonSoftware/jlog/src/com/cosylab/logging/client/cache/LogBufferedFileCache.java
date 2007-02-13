package com.cosylab.logging.client.cache;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;
import java.util.Vector;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * This class extends the LogFileCache adding the buffering of the logs
 * that must be written on disk.
 * 
 * It uses a WriteBuffer to store the logs to write on disk. 
 * The purpose of this class is to write several logs at once reducing
 * the write operations on disk.
 *  
 * @author acaproni
 */
public class LogBufferedFileCache extends LogFileCache implements ILogMap {
	
	public static final String WRITEBUFFERSIZE_PROPERTY_NAME = "jlog.cache.writebuffersize";
	public static final int DEFAULT_WRITEBUFFERSIZE = 8192;
	
	// The buffer of logs is a TreeMap having has key the identifier
	// of the log and the log itself as value
	private TreeMap<Integer,ILogEntry> buffer= new TreeMap<Integer,ILogEntry>();
	
	// The capacity of the buffer: when the buffer is full it is flushed on disk  
	private int size;
	
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
	 * Empty the cache.
	 * 
	 */
	public synchronized void clear() throws LogCacheException {
		super.clear();
		synchronized (buffer){
			buffer.clear();
		}
	}
	
	/**
	 * Empty the cache 
	 * 
	 * @param newFile If true the cache allocates a new file for storing the logs
	 * @param keepOldFile If true the old file for the cache is not deleted
	 * 
	 * @throws IOException
	 */
	public synchronized void clear(boolean newFile, boolean keepOldFile) throws LogCacheException {
		super.clear(newFile,keepOldFile);
		synchronized(buffer) {
			buffer.clear();
		}
	}

	/**
	 * Return the log with the given key
	 *  
	 * @param key The key of the log
	 * @return The LogEntryXML or null in case of error
	 */
	public synchronized ILogEntry getLog(Integer key) throws LogCacheException {
		ILogEntry log = buffer.get(key);
		if (log==null) {
			return super.getLog(key);
		}
		return log;
	}
	
	/**
	 * Delete a log with the given key
	 * 
	 * @param pos The key of the log to delete
	 */
	public synchronized void deleteLog(Integer key) throws LogCacheException {
		if (!buffer.containsKey(key)) {
			super.deleteLog(key);
		}
		ILogEntry log;
		synchronized (buffer) {
			log = buffer.remove(key);
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
			ILogEntry log;
			synchronized(buffer) {
				log=buffer.remove(key);
			}
			if (log!=null) {
				iter.remove();
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
		synchronized (buffer) {
			buffer.put(logID,log);
			logInBuffer=buffer.size();
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
	private void flushBuffer() throws LogCacheException {
		if (buffer.size()!=size) {
			throw new IllegalStateException("Error: trying to flush but the buffer is not full");
		}
		// str is the buffer of logs to write on disk at once
		StringBuilder str = new StringBuilder();
		
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
			ILogEntry log = buffer.get(key);
			info = new LogFileCache.LogCacheInfo();
			info.start=startingPos+str.length();
			String cacheLogStr=toCacheString(log);
			str.append(cacheLogStr);
			info.len=cacheLogStr.length();
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
		Set<Integer> keys = super.keySet();
		keys.addAll(buffer.keySet());
		return keys;
	}
	
	/**
	 * Return an iterator over the logs in cache
	 */
	public Iterator<ILogEntry> iterator() {
		return new LogIterator(this);
	}
}
