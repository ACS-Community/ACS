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
 * There are two more references to the same random file, used for reading and writing.
 * Having these 2 references allows us to know when the reads and writes are terminated 
 * in order to close the file.
 *  
 * @author acaproni
 *
 */
public class CacheFile {
	
	/**
	 * The type of file managed by the cache for I/O
	 *  
	 * @author acaproni
	 *
	 */
	public enum CacheFileType {
		INPUT,
		OUTPUT
	}
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
	 * Random file used for input.
	 * <P>
	 * It can be <code>null</code> or equal to <code>raFile</code>.
	 */
	private RandomAccessFile inRAFile = null;
	
	/**
	 * Random file used for outPut.
	 * <P>
	 * It can be <code>null</code> or equal to <code>raFile</code>.
	 */
	private RandomAccessFile outRAFile = null;
	
	/**
	 * Constructor
	 * 
	 * @param fName The name of the file
	 * @param key The key of this entry
	 */
	public CacheFile(String fName, Integer key) {
		if (fName==null || fName.isEmpty()) {
			throw new IllegalArgumentException("The file name can't be null not empty");
		}
		fileName=fName;
		this.key=key;
	}
	
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
		this(fName,key);
		if (rf==null) {
			throw new IllegalArgumentException("Invalid null random file");
		}
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
	 * Return the random file for reading or writing.
	 * <P>
	 * @param type The type of the file
	 * @return The random access file to get data from. 
	 */
	public RandomAccessFile getRaFile(CacheFileType type) throws FileNotFoundException {
		if (raFile==null) {
			openFile();
		}
		switch (type) {
		case INPUT: 
			inRAFile=raFile;
			break;
		case OUTPUT:
			outRAFile=raFile;
			break;
		}
		return raFile;
	}
	
	/**
	 * Release the file of the given type.
	 * 
	 * @param type The type of the file to release
	 */
	public void releaseRaFile(CacheFileType type) throws IOException {
		switch (type) {
		case INPUT:
			inRAFile=null;
			break;
		case OUTPUT:
			outRAFile=null;
			break;
		}
		// Release raFile and file if both the file for input
		// and output are null (unused)
		if (inRAFile==null && outRAFile==null) {
			raFile.close();
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
	
	public void writeOnFile(String str) {
		if (raFile==null) {
			throw new IllegalStateException("RandomAccessFile not initialized");
		}
		if (str==null || str.isEmpty()) {
			throw new IllegalArgumentException("Invalid string to write on file");
		}
		
	}
}
