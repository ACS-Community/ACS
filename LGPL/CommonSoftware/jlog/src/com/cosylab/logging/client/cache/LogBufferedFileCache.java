package com.cosylab.logging.client.cache;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.HashMap;
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
public class LogBufferedFileCache extends LogFileCache {
	
	public static final String WRITEBUFFERSIZE_PROPERTY_NAME = "jlog.cache.writebuffersize";
	public static final int DEFAULT_WRITEBUFFERSIZE = 8192;
	
	/**
	 * The buffer of logs to write on disk
	 */
	private WriteBuffer wBuffer;
	
	/** 
	 * Buffers the logs to write block of logs at once
	 * 
	 * The buffer conatins an HashMap of logs with their indexes as keys
	 * (the same as replaced logs)
	 * 
	 * @author acaproni
	 *
	 */
	private class WriteBuffer {
		/**
		 * The max number of logs in the buffer
		 */
		private int writeBufferSize;
		/**
		 * The map of buffered logs 
		 */
		private HashMap<Integer,ILogEntry> writeBuffer;
		
		/**
		 * The position of each XML in the buffer. These values will be added
		 * to the LogFileCache.index vector.
		 * 
		 * (it is zero base i.e. these numbers must be updated when they are flushed)
		 */
		private Vector<Long> bufferIndex;
		
		private StringBuffer charBuffer = new StringBuffer();
		
		/**
		 * The file of logs 
		 * @see LogFileCache.file
		 */
		private RandomAccessFile fileOfLogs;
		
		/**
		 * The vectors with the positions of the logs in cache
		 * @see LogFileCache.index
		 */
		private Vector<Long> indexes;
		
		/**
		 * Constructor 
		 * 
		 * @param sz The max num of logs in the buffer (the size of the buffer)
		 */
		public WriteBuffer(RandomAccessFile theFile, Vector<Long> theIndex, int sz) {
			if (theFile==null || theIndex==null || sz<=0) {
				throw new IllegalArgumentException("Illegal argument in constructor");
			}
			writeBufferSize=sz;
			fileOfLogs=theFile;
			indexes=theIndex;
			writeBuffer = new HashMap<Integer,ILogEntry>(writeBufferSize);
			bufferIndex = new Vector<Long>(writeBufferSize);
		}
		
		/**
		 * Add a log in the buffer
		 * If the buffer is full then it is flushed on disk
		 * @param log
		 */
		public synchronized int addLog(ILogEntry log) throws LogCacheException {
			// Add the log in the buffer
			bufferIndex.add((long)charBuffer.length());
			charBuffer.append(log.toXMLString());
			writeBuffer.put(bufferIndex.size()-1,log);
			if (writeBuffer.size()>=writeBufferSize) {
				flushBuffer();
			}
			return indexes.size()+bufferIndex.size()-1;
		}
		
		/**
		 * Flush the buffer on disk
		 *
		 */
		private void flushBuffer() throws LogCacheException {
			// Get all the logs
			String logsStr=charBuffer.toString();
			// The length of the file
			long pos;
			// Write the charBuffer on disk
			synchronized(fileOfLogs) {
				try {
					pos=fileOfLogs.length();
					fileOfLogs.seek(fileOfLogs.length());
					fileOfLogs.writeBytes(logsStr);
				} catch (IOException ioe) {
					throw new LogCacheException("Error flushing the buffer of logs",ioe);
				}
			}
			// Add the indexes in the vector 
			synchronized(index) {
				for (int t=0; t<bufferIndex.size(); t++) {
					index.add(pos+bufferIndex.get(t));
				}
			}
			// Clean up the buffer
			charBuffer.delete(0,charBuffer.length());
			bufferIndex.clear();
			writeBuffer.clear();
		}
		
		/**
		 * Return a log in the buffer
		 * 
		 * @param pos The position of the log
		 * @return The log
		 */
		public synchronized ILogEntry getLog(int pos)  throws LogCacheException {
			// Check if the log is in the buffer
			int lastLog = indexes.size()-1;
			if (pos<=lastLog) {
				throw  new LogCacheException("The log is not in the buffer!");
			}
			int posInBuffer = pos-lastLog-1;
			ILogEntry log = writeBuffer.get(posInBuffer);
			if (log==null) {
				throw new LogCacheException("The log is in the buffer but is null!");
			}
			return log;
		}
		
		/**
		 * Return the number of logs in buffer
		 * 
		 * @return the number of logs in buffer
		 */
		public synchronized int getSize() {
			return writeBuffer.size();
		}
		
		/**
		 * Clear the buffer maintaining the same file and cache
		 *
		 */
		public synchronized void clear() {
			bufferIndex.clear();
			writeBuffer.clear();
			charBuffer.delete(0,charBuffer.length());
		}
		
		/**
		 * Clear the cache setting a new file and vector index
		 * @param newFile The new cache file
		 * @param theIndex The new vector of positions
		 */
		public synchronized void clear(RandomAccessFile newFile, Vector<Long> newIndex) {
			fileOfLogs=newFile;
			indexes=newIndex;
			clear();
		}
	}
	
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
		while (true) {
			try {
				wBuffer = new WriteBuffer(file,index,writeBufferSize);
				System.out.println("The WriteBuffer stores "+writeBufferSize+" logs");
				break;
			} catch (OutOfMemoryError e) {
				writeBufferSize=writeBufferSize/2;
				if (writeBufferSize<32) {
					throw new LogCacheException("Out of memory creating the buffer for writing ops",e);
				}
			}
		}
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
	public void clear() throws LogCacheException {
		super.clear();
		wBuffer.clear(file,index);
	}
	
	/**
	 * Empty the cache 
	 * 
	 * @param newFile If true the cache allocates a new file for storing the logs
	 * @param keepOldFile If true the old file for the cache is not deleted
	 * 
	 * @throws IOException
	 */
	public void clear(boolean newFile, boolean keepOldFile) throws LogCacheException {
		super.clear(newFile,keepOldFile);
		wBuffer.clear(file,index);
	}

	/**
	 * Return the log in the given position
	 *  
	 * @param pos The position of the log
	 * @return The LogEntryXML or null in case of error
	 */
	public ILogEntry getLog(int pos) throws LogCacheException {
		// Check if the log is present in the list of the replaced logs
		if (replacedLogs.containsKey(new Integer(pos))) {
			return replacedLogs.get(new Integer(pos));
		}
		if (pos>=index.size()) {
			return wBuffer.getLog(pos);
		} else return super.getLog(pos);
	}
	
	/**
	 * Append a log to the end of the cache
	 * 
	 * @param log The log to append in the cache
	 * @return The position in the cache of the added log
	 */
	public int add(ILogEntry log) throws LogCacheException {
		return wBuffer.addLog(log);
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
		else {
			return DEFAULT_WRITEBUFFERSIZE;
		}
	}
	
	/**
	 *  Return the number of logs in ache
	 *  
	 *  @return The number of logs in cache
	 */
	public int getSize() {
		return super.getSize()+wBuffer.getSize();
	}
}
