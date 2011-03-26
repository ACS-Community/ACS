/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package com.cosylab.logging.engine.cache;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import com.cosylab.logging.engine.LogEngineException;

/**
 * Objects from this class implement a FIFO cache of String objects. 
 * The strings are written on disk by using several files: a new file is created whenever
 * the dimension of the current file becomes greater then a fixed size.
 * For each entry in cache, a record is created and kept in a in-memory list. 
 * <P>
 * The logs are stored in a set of files and their ending position saved.
 * When all the logs in a file have been red, the file is deleted optimizing 
 * the usage disk space.
 * The deletion of unused files is done by a thread. 
 * <P>
 * The length of each file of cache can be specified by a parameter in the constructor
 * or by a java property. If both those values are not given, a default length is used.
 * <P> 
 * <code>files</code> contains all the files used by the cache, identified by a key.
 * When a file does not contain unread entries then its key is pushed into <code>filesToDelete</code> 
 * and deleted. 
 * The thread that deletes the files from disk, removes the {@link CacheFile} object from
 * <code>files</code> too.
 * 
 * <P>
 * <code>entries</code> contains all the entries of in cache.
 *  
 * @author acaproni
 *
 */
public class EngineCache extends Thread {

	/**
	 * Each file of the cache is identified by a key.
	 * <P>
	 * The key is always positive.
	 */
	private int fileKey = 0;
	
	/**
	 * The file used to write the strings into.
	 * When the size of this file is greater then <code>maxSize</code> then a new file
	 * is created for output.
	 */
	private volatile CacheFile outCacheFile=null;
	
	/**
	 * The file used to read the previous record.
	 * It is used to know when all the records in a file have been read.
	 */
	private volatile CacheFile inCacheFile=null;
	
	/**
	 * The entries in the cache.
	 * <P>
	 * The items of the list are organized  in a FIFO policy.
	 * This is particularly important because this order is used to know when a file
	 * is not used anymore and can be deleted.
	 * 
	 * @see {@link EngineCache.files}
	 */
	private CacheEntriesQueue entries = new CacheEntriesQueue();
	
	/**
	 * A list of keys of unused files to delete.
	 */
	private LinkedBlockingQueue<CacheFile> filesToDelete = new LinkedBlockingQueue<CacheFile>();
	
	/**
	 * The files used by the cache.
	 * 
	 * The entries in this vector have the same order of the creation of the files:
	 * the last created file is in the last position of the vector.
	 * 
	 * This property can be used to verify for the correctness of the computation because
	 * every time we have to delete a file, it must be the first item of this vector 
	 * 
	 * @see {@link EngineCache.entries}
	 */
	private LinkedHashMap<Integer,CacheFile> files = new LinkedHashMap<Integer,CacheFile>();
	
	/** 
	 * <code>true</code> if the cache is closed.
	 * It signals the thread to terminate.
	 */
	private volatile boolean closed=false;
	
	/**
	 * The handler to create and delete the file of the this cache.
	 */
	private final ILogQueueFileHandler fileHandler;
	
	/**
	 * <code>true</code> if the string in the cache are in binary format
	 * and <code>false</code> if XML.
	 */
	private final boolean binary;
	
	/**
	 * Build a cache.
	 * 
	 * @param binary <code>true</code> if the string of logs are in binary format
	 *               and <code>false</code> if XML strings
	 */
	public EngineCache(boolean binary) {
		super("EngineCache");
		this.binary=binary;
		fileHandler=new LogQueueFileHandlerImpl();
		setDaemon(true);
		setPriority(MIN_PRIORITY);
		start();
	}
	
	/**
	 * Build the cache with the passed maximum size for each file of the cache
	 * 
	 * @param size The max size of each file of the cache
	 * @param binary <code>true</code> if the string of logs are in binary format
	 *               and <code>false</code> if XML strings
	 */
	public EngineCache(long size, boolean binary) {
		super("EngineCache");
		if (size<=1024) {
			throw new IllegalArgumentException("The size can't be less then 1024");
		}
		this.binary=binary;
		fileHandler=new LogQueueFileHandlerImpl(size);
		setDaemon(true);
		setPriority(MIN_PRIORITY);
		start();
	}
	
	/**
	 * Build the cache by setting the size of the files and the handler to 
	 * create and delete the files.
	 * 
	 * @param handler The handler to create and delete the files
	 * @param binary <code>true</code> if the string of logs are in binary format
	 *               and <code>false</code> if XML strings
	 */
	public EngineCache(ILogQueueFileHandler handler, boolean binary) {
		super("EngineCache");
		if (handler==null) {
			throw new IllegalArgumentException("The ILogQueueFileHandler can't be null");
		}
		this.binary=binary;
		fileHandler=handler;
		setDaemon(true);
		setPriority(MIN_PRIORITY);
		start();
	}
	
	/**
	 * Attempts to create the file for the strings in several places
	 * before giving up.
	 * 
	 * @return A new temporary file
	 *          <code>null</code> if it was not possible to create a new file
	 * @throws If it was not possible to create a temporary file
	 */
	private File getNewFile() throws IOException {
		File ret= fileHandler.getNewFile();
		return ret;
	}
	
	/**
	 * Close and delete a file.
	 * 
	 * @param itemToDel The item to delete
	 * @return true if the file is deleted
	 */
	private void releaseFile(CacheFile itemToDel) {
		if (itemToDel==null) {
			throw new IllegalArgumentException("The item to delete can't be null");
		}
		itemToDel.close();
		File f=null;
		try {
			f = itemToDel.getFile();
		} catch (FileNotFoundException fnfe) {
			System.err.println("Error deleting "+itemToDel.fileName+" (key "+itemToDel.key+")");
			fnfe.printStackTrace(System.err);
			return ;
		}
		fileHandler.fileProcessed(f,itemToDel.minDate(), itemToDel.maxDate());
	}
	
	/**
	 * The method to get and delete unused files
	 */
	public void run() {
		while (!closed) {
			CacheFile cacheFile;
			try {
				cacheFile = filesToDelete.poll(1, TimeUnit.HOURS);
			} catch (InterruptedException ie) {
				continue;
			}
			if (cacheFile==null) {
				// timeout elapsed
				continue;
			}
			releaseFile(cacheFile);
		}
		// release all the files in cache before exiting
		CacheFile cf=null;
		while ((cf=filesToDelete.poll())!=null) {
			releaseFile(cf);
		}
	}
	
	/**
	 * Return the number of entries in cache.
	 * @return the number of entries in cache
	 */
	public int size() {
		return entries.size();
	}
	
	/**
	 * Generate a new key for a file.
	 * <P>
	 * Each new key is generated by increasing the value of the current key.
	 * If the max integer value is reached then the key rests to the min value.
	 * 
	 * @return A new key for a file
	 */
	private Integer getNextFileKey() {
		if (fileKey<Integer.MAX_VALUE) {
			fileKey++;
		} else {
			// Ops we need to reset the key...
			fileKey=0;
		}
		Integer ret = Integer.valueOf(fileKey);
		// Check if the key is already used
		synchronized (files) {
			if (files.containsKey(fileKey)) {
				throw new IllegalStateException("No more room in cache");
			}
		}
		return ret;
	}
	
	/**
	 * 
	 * @return The number of files used by the cache
	 */
	public int getActiveFilesSize() {
		synchronized (files) {
			return files.size();
		}
		
	}
	
	/**
	 * Push an entry in the cache.
	 * If the current file is <code>null</code> or its size is greater then <code>maxSize</code>,
	 * then a new file is created.
	 * 
	 * @param string The string to write in the cache
	 * @throws IOException In case of error writing the string on disk
	 */
	public synchronized void push(String string) throws IOException, LogEngineException {
		if (string==null || string.length()==0) {
			throw new IllegalArgumentException("The string can't be null nor empty");
		}
		if (closed) {
			return;
		}
		// Check if a new file must be created
		if (outCacheFile==null || outCacheFile.getFileLength()>=fileHandler.getMaxFileSize()) {
			File f = getNewFile();
			if (f==null) {
				throw new IOException("Error creating a cache file");
			}
			if (outCacheFile!=null) {
				outCacheFile.setWritingMode(false);
			}
			String name = f.getAbsolutePath();
			RandomAccessFile raF = new RandomAccessFile(f,"rw");
			outCacheFile = new CacheFile(name,getNextFileKey(), raF,f,binary);
			outCacheFile.setWritingMode(true);
			synchronized (files) {
				files.put(outCacheFile.key,outCacheFile);
			}
		}
		if (!string.endsWith("\n")) {
			string=string+"\n";
		}
		// Write the string in the file
		CacheEntry entry = outCacheFile.writeOnFile(string, outCacheFile.key);
		entries.put(entry);
	}
	
	/**
	 * Get and remove the next string from the cache.
	 * 
	 * @return The next string entry in cache.
	 *         <code>null</code> If the timeout happened
	 * @throws IOException In case of error reading from the file
	 * @throws InterruptedException When the call to <code>poll</code> is interrupted
	 */
	public String pop() throws IOException, InterruptedException {
		if (closed) {
			return null;
		}
		CacheEntry entry=null;
		entry = entries.get();
		if (entry==null) {
			// Timeout
			return null;
		}
		if (inCacheFile==null) {
			synchronized (files) {
				inCacheFile=files.get(entry.key);
				inCacheFile.setReadingMode(true);
			}
		} else if (inCacheFile.key!=entry.key) {
			inCacheFile.setReadingMode(false);
			synchronized (files) {
				files.remove(inCacheFile.key);
			}
			if (!filesToDelete.offer(inCacheFile)) {
				// Most unlikely to happen: the queue is full!
				releaseFile(inCacheFile);
			} 
			synchronized (files) {
				inCacheFile=files.get(entry.key);
				inCacheFile.setReadingMode(true);
			}
		}
		String ret= inCacheFile.readFromFile(entry);
		if (ret.endsWith("\n")) {
			return ret.substring(0,ret.length()-1);
		} else {
			return ret;
		}
	}
	
	/**
	 * Close the cache: delete all the entries and all the files the exit.
	 * <P>
	 * <B>Note</B>: this must be the last operation executed by the cache
	 * 
	 * @param sync <code>true</code> if must wait the termination of the threads before exiting
	 */
	public void close(boolean sync) {
		closed=true;
		interrupt();
		if (inCacheFile!=null) {
			inCacheFile.close();
		}
		if (outCacheFile!=null) {
			outCacheFile.close();
		}
		while (sync && isAlive()) {
			try {
				Thread.sleep(250);
			} catch (InterruptedException ie) {}
		}
		// Release all the files still in the queue
		if (!files.isEmpty()) {
			Set<Integer> keys = files.keySet();
			for (Integer key: keys) {
				CacheFile cf = files.get(key);
				releaseFile(cf);
			}
		}
	}
}
