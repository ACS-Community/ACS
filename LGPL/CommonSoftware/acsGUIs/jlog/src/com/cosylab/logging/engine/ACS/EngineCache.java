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
package com.cosylab.logging.engine.ACS;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Random;
import java.util.Vector;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Objects from this class implement a FIFO cache of String objects. 
 * The strings are written on disk by using several files: a new file is created whenever
 * the dimension of the current file becomes greater then a fixed size.
 * For each entry in the cache, an apposite record is created and kept in a
 * in-memory list. 
 * 
 * The cache is used by <code>{@link com.cosylab.logging.engine.ACS.ACSLogRetrieval}</code>,
 * but can be used wherever a set of strings needs to be stored because there is no assunmption
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
	 * It contains the file where the entry is stored together with the
	 * position of the entry
	 * 
	 * @author acaproni
	 *
	 */
	private class CacheEntry {
		/**
		 * The file where the entry is stored
		 */
		public final RandomAccessFile file;
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
		 * @param outF The file where the entry is stored
		 * @param startPos The starting position of the entry in the file
		 * @param endPos The ending position of the entry in the file
		 */
		public CacheEntry(RandomAccessFile outF, long startPos, long endPos) {
			if (outF==null) {
				throw new IllegalArgumentException("The file can't be null");
			}
			if (startPos<0 || endPos<=0 || startPos>=endPos) {
				throw new IllegalArgumentException("Invalid start/end positions: "+startPos+", "+endPos);
			}
			file=outF;
			start=startPos;
			end=endPos;
		}
	}
	
	/**
	 * Each file used by the cache
	 * 
	 * @author acaproni
	 *
	 */
	private class CacheFile {
		public final File file;
		public final RandomAccessFile rndFile;
		
		public CacheFile(File theFile, RandomAccessFile rFile) {
			if (theFile==null) {
				throw new IllegalArgumentException("The File can't be null");
			}
			if (rFile==null) {
				throw new IllegalArgumentException("The RandomAccessFile can't be null");
			}
			file=theFile;
			rndFile=rFile;
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
	 * The file used to write the strings into.
	 * When the size of this file is greater then <code>maxSize</code> then a new file
	 * is created for output.
	 * <P>
	 * This file is the last entry of the vector <code>files</code> but it is stored here to
	 * enhance the writing time of a new string in the file.
	 * It can be <code>null</code> or equal to the last item in <code>files</code>.
	 */
	private volatile RandomAccessFile outFile=null;
	
	/**
	 * The file used to read the previous record.
	 * It is used to know when all the record in a file have been read:
	 * when the next read must be done in a different file.
	 * In this case <code>inFile</code> can be deleted.
	 */
	private volatile RandomAccessFile inFile=null;
	
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
	private Vector<CacheFile> files = new Vector<CacheFile>();
	
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
	private File getFile() {
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
	 * The file to delete is the first item in the <code>file</code> vector.
	 * 
	 * @param fileToRelease The file to close and delete
	 * @return true if the file is deleted
	 */
	private boolean releaseFile(RandomAccessFile fileToRelease) {
		if (fileToRelease==null) {
			throw new IllegalArgumentException("The file can't be null");
		}
		CacheFile item = files.remove(0);
		if (!item.rndFile.equals(fileToRelease)) {
			throw new IllegalStateException("Vector of files not consistent");
		}
		try {
			fileToRelease.close();
		} catch (IOException ioe) {
			System.err.println("Error closing file "+ioe.getMessage());
			ioe.printStackTrace(System.err);
			return false;
		}
		return item.file.delete();
	}
	
	/**
	 * Return the number of entries in cache.
	 * @return the number of entries in cache
	 */
	public int size() {
		return entries.size();
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
			throw new IllegalArgumentException("The string can't be null or empty");
		}
		if (closed) {
			return;
		}
		// Check if a new file must be created
		if (outFile==null || outFile.length()>=maxSize) {
			File f = getFile();
			if (f==null) {
				throw new IOException("Error creating a cache file");
			}
			RandomAccessFile raF = new RandomAccessFile(f,"rw");
			CacheFile cacheFile = new CacheFile(f,raF);
			files.add(cacheFile);
			outFile=raF;
		}
		// Write the string in the file
		long startPos;
		long endPos;
		synchronized (outFile) {
			startPos = outFile.length();
			outFile.seek(startPos);
			outFile.writeBytes(string);
			endPos = outFile.length();	
		}
		CacheEntry entry = new CacheEntry(outFile,startPos,endPos);
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
		if (inFile==null) {
			inFile=entry.file;
		} else if (inFile!=entry.file) {
			releaseFile(inFile);
			inFile=entry.file;
		}
		synchronized (inFile) {
			inFile.seek(entry.start);
			inFile.read(buffer);
		}
		return new String(buffer).trim();
	}
	
	/**
	 * Close the cache: delete all the entries and all the files the exit.
	 * 
	 * <B>Note</B>: this is the last operation executed in the cache
	 */
	public void close() {
		closed=true;
		entries.clear();
		files.clear();
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
