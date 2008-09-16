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
import java.util.Random;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Objects from this class implement a FIFO cache of String objects. 
 * The strings are written on disk by using several files: a new file is created whenever
 * the dimension of the current file becomes greater then a fixed size.
 * For each entry in cache, a record is created and kept in a in-memory list. 
 * 
 * The cache is used by <code>{@link com.cosylab.logging.engine.ACS.ACSLogRetrieval}</code>,
 * but can be used wherever a set of strings needs to be stored because there is no assumption
 * about the content of the strings.
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
	 * The default max size for each file of the cache.
	 * The default value is used when the java property is not found and the 
	 * size is not given explicitly.
	 * 
	 * NFS could be limited to 2GB depending on the installed version
	 */
	public static long DEFAULT_SIZE = 1073741824; // 1Gb
	
	// The name of the property with the size of the file
	public static String MAXSIZE_PROPERTY_NAME = "jlog.enine.cache.maxFilesSize";
	
	/**
	 * The max length of each file of the cache
	 */
	private long maxSize;
	
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
	 * Build a cache.
	 * 
	 * The max size of each file of the cache is calculated in the following way:
	 * 1. if the java property is present, the size is taken from suc a property
	 * 2. the default size is used
	 */
	public EngineCache() {
		this(getDefaultMaxFileSize());
	}
	
	/**
	 * Build the cache with the passed maximum size for each file of the cache
	 * 
	 * @param size The max size of each file of the cache
	 */
	public EngineCache(long size) {
		super("EngineCache");
		if (size<=1024) {
			throw new IllegalArgumentException("The size can't be less then 1024");
		}
		maxSize=size;
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
	 * 
	 */
	private File getNewFile() {
		String name=null;
		File f=null;
		try {
			// Try to create the file in $ACSDATA/tmp
			String acsdata = System.getProperty("ACS.data");
			acsdata=acsdata+"/tmp/";
			File dir = new File(acsdata);
			f = File.createTempFile("jlogEngineCache",".tmp",dir);
			name=acsdata+f.getName();
		} catch (IOException ioe) {
			// Another error :-O
			String homeDir = System.getProperty("user.dir");
			do {
				// Try to create the file in the home directory
				int random = new Random().nextInt();
				name = homeDir + "/jlogEngineCache"+random+".jlog";
				f = new File(name);
			} while (f.exists());
		}
		if (f!=null) {
			f.deleteOnExit();
		}
		return f;
	}
	
	/**
	 * Close and delete a file.
	 * 
	 * @param itemToDel The item to delete
	 * @return true if the file is deleted
	 */
	private boolean releaseFile(CacheFile itemToDel) {
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
			return false;
		}
		return f.delete();
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
	public void push(String string) throws IOException {
		if (string==null || string.length()==0) {
			throw new IllegalArgumentException("The string can't be null nor empty");
		}
		if (closed) {
			return;
		}
		// Check if a new file must be created
		if (outCacheFile==null || outCacheFile.getFileLength()>=maxSize) {
			File f = getNewFile();
			if (f==null) {
				throw new IOException("Error creating a cache file");
			}
			if (outCacheFile!=null) {
				outCacheFile.setWritingMode(false);
			}
			String name = f.getAbsolutePath();
			RandomAccessFile raF = new RandomAccessFile(f,"rw");
			outCacheFile = new CacheFile(name,getNextFileKey(), raF,f);
			outCacheFile.setWritingMode(true);
			synchronized (files) {
				files.put(outCacheFile.key,outCacheFile);
			}
		}
		// Write the string in the file
		CacheEntry entry = outCacheFile.writeOnFile(string, outCacheFile.key);
		boolean inserted = false;
		while (!inserted) {
			try {
				entries.put(entry);
				inserted=true;
			} catch (InterruptedException ie) {	}
		}
	}
	
	/**
	 * Get and remove the next string from the cache.
	 * 
	 * @param timeout The timeout to wait getting the new entry
	 * @return The next string entry in cache.
	 *         <code>null</code> If the timeout happened
	 * @throws IOException In case of error reading from the file
	 * @throws InterruptedException When the call to <code>poll</code> is interrupted
	 */
	public String pop(int timeout) throws IOException, InterruptedException {
		if (timeout <=0 ) {
			throw new IllegalArgumentException("Invalid timeout: "+timeout);
		}
		if (closed) {
			return null;
		}
		CacheEntry entry=null;
		entry = entries.poll(timeout,TimeUnit.MILLISECONDS);
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
		return inCacheFile.readFromFile(entry);
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
		synchronized (files) {
			files.clear();
		}
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
	}

	/**
	 * Get the max size of the files out of the system properties or
	 * uses the default value if the property does not exist
	 */
	private static long getDefaultMaxFileSize() {
		Integer fileSizeFromProperty = Integer.getInteger(MAXSIZE_PROPERTY_NAME);
		if (fileSizeFromProperty != null) {
			return fileSizeFromProperty.longValue();
		}
		return DEFAULT_SIZE;
	}
}
