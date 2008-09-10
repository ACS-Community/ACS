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

import com.cosylab.logging.engine.cache.CacheFile.CacheFileType;

/**
 * Objects from this class implement a FIFO cache of String objects. 
 * The strings are written on disk by using several files: a new file is created whenever
 * the dimension of the current file becomes greater then a fixed size.
 * For each entry in the cache, an apposite record is created and kept in a
 * in-memory list. 
 * 
 * The cache is used by <code>{@link com.cosylab.logging.engine.ACS.ACSLogRetrieval}</code>,
 * but can be used wherever a set of strings needs to be stored because there is no assumption
 * about the content of the strings.
 * <P>
 * The logs are stored in a set of files and their ending position saved.
 * When all the logs in a file have been red, the file is deleted optimizing 
 * the usage disk space.  
 * <P>
 * The length of each file of cache can be specified by a parameter in the constructor
 * or by a java property. If both those values are not given, a default length is used.
 * <P> 
 * The vector <code>files</code> contains all the files used by the cache.
 * New items are added at the end of the file in the last record of <code>files</code>.
 * New items are read from the first item of <code>files</code>.
 * <P>
 * Each file that does not contain unread entries is deleted.
 * 
 * <P>
 * <code>entries</code> contains all the entries of in cache. 
 * 
 * @author acaproni
 *
 */
public class EngineCache {
	
	/**
	 * An entry of the cache.
	 * It contains the name of the file where the entry is stored together with the
	 * position of the entry.
	 * 
	 * Having the name of the file allows to open and close the file when needed.
	 * In a previous version there was a <code>RandomAccessFile</code> instead of the name
	 * but it ended up with an error because the number of open file was exceeding
	 * the maximum allowed.
	 * 
	 * 
	 * @author acaproni
	 *
	 */
	private class CacheEntry {
		/**
		 * The key of the file where the entry is stored
		 */
		public final Integer key;
		/**
		 * The starting position of the entry in the file
		 */
		public final long start;
		/**
		 * The ending position of the entry in the file
		 */
		public final long end;
		
		/**
		 * Constructor
		 * 
		 * @param key The key of the file where the entry is stored
		 * @param startPos The starting position of the entry in the file
		 * @param endPos The ending position of the entry in the file
		 */
		public CacheEntry(Integer key, long startPos, long endPos) {
			if (key==null) {
				throw new IllegalArgumentException("The file name can't be null nor empty");
			}
			if (startPos<0 || endPos<=0 || startPos>=endPos) {
				throw new IllegalArgumentException("Invalid start/end positions: "+startPos+", "+endPos);
			}
			this.key=key;
			start=startPos;
			end=endPos;
		}
	}
	
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
	 */
	private int fileKey = Integer.MIN_VALUE;
	
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
	 * 
	 * The items of the list are managed with a FIFO policy.
	 * This is particularly important because this order is used to know when a file
	 * is not used anymore and can be deleted.
	 * 
	 * @see {@link EngineCache.files}
	 */
	private LinkedBlockingQueue<CacheEntry> entries = new LinkedBlockingQueue<CacheEntry>();
	
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
	
	// true if the cache is closed
	private volatile boolean closed=false;
	
	/**
	 * Build a cache.
	 * 
	 * The max size of each file of the cache is calculated in the following way:
	 * 1. if the java property is present, the size is taken from suc a property
	 * 2. the default size is used
	 */
	public EngineCache() {
		maxSize=getDefaultMaxFileSize();
	}
	
	/**
	 * Build the cache with the passed maximum size for each file of the cache
	 * 
	 * @param size The max size of each file of the cache
	 */
	public EngineCache(long size) {
		if (size<=1024) {
			throw new IllegalArgumentException("The size can't be less then 1024");
		}
		maxSize=size;
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
	 * @param key The key of the file to close and delete
	 * @return true if the file is deleted
	 */
	private boolean releaseFile(Integer key) {
		if (key==null) {
			throw new IllegalArgumentException("Invalid key");
		}
		CacheFile item = files.remove(key);
		if (item==null) {
			throw new IllegalStateException("Got a null cache file for key "+key);
		}
		if (!item.key.equals(key)) {
			throw new IllegalStateException("Keys differ: key of the file to release="+key+", key from cache="+item.key);
		}
		File f=null;
		try {
			f = item.getFile();
		} catch (FileNotFoundException fnfe) {
			System.err.println("Error deleting "+item.fileName+" (key "+item.key+")");
			fnfe.printStackTrace(System.err);
			return false;
		}
		return f.delete();
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
			fileKey=Integer.MIN_VALUE;
		}
		Integer ret = Integer.valueOf(fileKey);
		// Check if the key is already used
		if (files.containsKey(fileKey)) {
			throw new IllegalStateException("No more room in cache");
		}
		return ret;
	}
	
	/**
	 * 
	 * @return The number of files used by the cache
	 */
	public int getActiveFilesSize() {
		return files.size();
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
				outCacheFile.releaseRaFile(CacheFileType.OUTPUT);
			}
			String name = f.getAbsolutePath();
			RandomAccessFile raF = new RandomAccessFile(f,"rw");
			outCacheFile = new CacheFile(name,getNextFileKey(), raF,f);
			files.put(outCacheFile.key,outCacheFile);
		}
		// Write the string in the file
		long startPos;
		long endPos;
		synchronized (outCacheFile) {
			startPos = outCacheFile.getFileLength();
			outCacheFile.getRaFile(CacheFileType.OUTPUT).seek(startPos);
			outCacheFile.getRaFile(CacheFileType.OUTPUT).writeBytes(string);
			endPos = outCacheFile.getFileLength();	
		}
		CacheEntry entry = new CacheEntry(outCacheFile.key,startPos,endPos);
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
		byte buffer[] = new byte[(int)(entry.end-entry.start)];
		if (inCacheFile==null) {
			inCacheFile=files.get(entry.key);
		} else if (inCacheFile.key!=entry.key) {
			inCacheFile.releaseRaFile(CacheFileType.INPUT);
			inCacheFile.close();
			releaseFile(inCacheFile.key);
			inCacheFile=files.get(entry.key);
		}
		synchronized (inCacheFile) {
			inCacheFile.getRaFile(CacheFileType.INPUT).seek(entry.start);
			inCacheFile.getRaFile(CacheFileType.INPUT).read(buffer);
		}
		return new String(buffer).trim();
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
		entries.clear();
		files.clear();
		if (inCacheFile!=null) {
			inCacheFile.close();
		}
		if (outCacheFile!=null) {
			outCacheFile.close();
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
