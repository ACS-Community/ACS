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

/**
 * Each file used by the cache.
 * <P>
 * The cache is composed by
 * <UL> 
 *  <LI>a {@link File} to know the length of the file on disc.
 *  <LI>a {@link RandomAccessFile} for reading and writing
 * </UL>
 * There are two booleans signaling if  the file is used for reading and writing.
 * In this way we know when the reads and writes are terminated close the file.
 *  
 * @author acaproni
 *
 */
public class CacheFile {
	
	/**
	 * The name of the file
	 */
	public final String fileName;
	
	/**
	 * The key identifying this file.
	 * <P>
	 * The key is stored only to perform run-time tests of correctness.
	 */
	public final Integer key;
	
	/**
	 * The <code>RandomAccessFile</code> of the file of this entry.
	 * <P>
	 * This is not <code>null</code> only when used for I/O.
	 */
	private RandomAccessFile raFile = null;
	
	/**
	 * The file used to build <code>raFile</code>.
	 * <P>
	 * It is not null as soon as raFile is not null
	 */
	private File file=null;
	
	/**
	 * Signal if the file is used for reading
	 */
	private boolean reading=false;
	
	/**
	 * Signal if the file is used for writing
	 */
	private boolean writing=false;
	
	/**
	 * Constructor 
	 * 
	 * @param fName The name of the file
	 * @param key The key of this entry
	 * @param rf The <code>RandomAccessFile</code> used for I/O
	 * @param f The <code>File</code> used to get the length
	 * 
	 * @see {@link CacheFile(String fName, Integer key)}
	 */
	public CacheFile(String fName, Integer key, RandomAccessFile rf, File f) {
		if (fName==null || fName.isEmpty()) {
			throw new IllegalArgumentException("The file name can't be null not empty");
		}
		if (key==null) {
			throw new IllegalArgumentException("Invalid null key");
		}
		if (rf==null) {
			throw new IllegalArgumentException("Invalid null random file");
		}
		fileName=fName;
		this.key=key;
		raFile=rf;
		file=f;
	}
	
	/**
	 * An helper methods that returns the <code>File</code>.
	 * <P>
	 * As soon as <code>raFile</code> is not <code>null</code>, <code>file</code> is not <code>null</code> too.
	 *  
	 * A new {@link File} is built if <code>file</code> is <code>null</code> 
	 * otherwise the method returns a reference to <code>file</code>.
	 * 
	 * @return The file
	 * @throws FileNotFoundException If the file does not exist
	 */
	public File getFile() throws FileNotFoundException {
		if (file!=null) {
			return file;
		}
		File f = new File(fileName);
		if (!f.exists()) {
			throw new FileNotFoundException("The cache file "+fileName+" does not exist");
		}
		if (!f.canRead() || !f.canWrite()) {
			throw new IllegalStateException("Impossible to read/write "+fileName);
		}
		return f;
	}
	
	/**
	 * An helper method that returns a <code>RandomAccessFile</code> by the file name 
	 * <code>fileName</code>.
	 * <P>
	 * The random access file is built from the <code>fileName</code>.
	 * 
	 * @return The file to read and/or write items
	 * @throws IOException In case of error creating the <code>File</code>.
	 * @throws FileNotFoundException If the file does not exist
	 */
	private void openFile() throws FileNotFoundException {
		raFile = new RandomAccessFile(getFile(),"rw");
	}
	
	/**
	 * Release all the resources (for instance it releases the random
	 * file).
	 */
	public void close() {
		if (raFile!=null) {
			try {
				raFile.close();
			} catch (Throwable t) {
				// Nothing to do here: print a message and go ahead.
				System.err.println("Error closing the file "+fileName+": "+t.getMessage());
			}
			raFile=null;
		}
		file=null;
	}
	
	/**
	 * Check if the file is used for reading or writing and 
	 * if not used, close the random file.
	 */
	private void checkRaFileUsage() {
		// Release raFile and file if both the file for input
		// and output are null (unused)
		if (!reading && !writing) {
			try {
				raFile.close();
			} catch (Throwable t) {
				// An error closing the file: do not stop the computation but cross the fingers!
				System.err.println("Error closing "+fileName+": "+t.getMessage());
			}
			raFile=null;
			file=null;
		}
	}
	
	/**
	 * Return the size of the file
	 * 
	 * @return the size of the file
	 */
	public long getFileLength() {
		if (file==null) {
			throw new IllegalStateException("The file is null");
		}
		return file.length();
	}
	
	/**
	 * Write the passed string in the file.
	 * 
	 * @param str The string to write in the file
	 * @return The ending position of the string in the file
	 */
	public synchronized CacheEntry writeOnFile(String str, Integer key) throws IOException {
		if (str==null || str.isEmpty()) {
			throw new IllegalArgumentException("Invalid string to write on file");
		}
		if (key==null || this.key!=key) {
			throw new IllegalArgumentException("Wrong key while writing");
		}
		if (raFile==null) {
			openFile();
		}
		writing=true;
		long startPos;
		long endPos;
		synchronized (raFile) {
			startPos = file.length();
			raFile.seek(startPos);
			raFile.writeBytes(str);
			endPos = file.length();	
		}
		return new CacheEntry(key,startPos,endPos);
	}
	
	/**
	 * Read a string from the file
	 * 
	 * @param entry The cache entry to saying how to read the entry
	 * 
	 * @return The string read from the file
	 */
	public synchronized String readFromFile(CacheEntry entry) throws IOException {
		if (entry==null) {
			throw new IllegalArgumentException("The CacheEntry can't be null");
		}
		if (entry.key!=key) {
			throw new IllegalArgumentException("Wrong key while reading");
		}
		if (raFile==null) {
			openFile();
		}
		reading=true;
		byte buffer[] = new byte[(int)(entry.end-entry.start)];
		raFile.seek(entry.start);
		raFile.read(buffer);
		return new String(buffer);
	}
	
	/**
	 * Set the reading mode of the file.
	 * 
	 * @param reading <code>true</code> if the file is used for reading
	 */
	public synchronized void setReadingMode(boolean reading) {
		this.reading=reading;
		checkRaFileUsage();
	}
	
	/**
	 * Set the writing mode of the file.
	 * 
	 * @param reading <code>true</code> if the file is used for writing
	 */
	public synchronized void setWritingMode(boolean writing) {
		this.writing=writing;
		checkRaFileUsage();
	}
}
