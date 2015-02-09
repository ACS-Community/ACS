/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
/** 
 * @author  acaproni
 * @since  2014.6
 */
package alma.acs.util.stringqueue;

import java.io.File;
import java.io.IOException;

/**
 * The file handler invoked by the queue  
 * @author acaproni
 * @since 2014.6
 *
 */
public abstract class TimestampedStringQueueFileHandler {
	
	/**
	 * The default max size for each file of the cache.
	 * The default value is used when the java property is not found and the 
	 * size is not given explicitly.
	 * 
	 * NFS could be limited to 2GB depending on the installed version
	 */
	public static final long DEFAULT_SIZE = 1073741824; // 1Gb
	
	/**
	 * The name of the property with the size of the file
	 */
	public static final String MAXSIZE_PROPERTY_NAME = "acs.util.stringqueue.maxFilesSize";
	
	/**
	 * The default prefix for each file of the cache
	 */
	public static final String DEFAULT_PREFIX="StringsQueue"; 
	
	/**
	 * The max length of each file of the cache
	 */
	public final long maxFilesSize;
	
	/**
	 * The prefix to prepend to the name of each file of the cache
	 */
	public final String prefix;
	
	/**
	 * Constructor with the default max length and prefix of the files.
	 * <BR>
	 * If a property with name {@link #MAXSIZE_PROPERTY_NAME} exists then
	 * the max length is taken from the passed value other wise it is {@link #DEFAULT_SIZE}
	 * 
	 * @see StringQueueFileHandler
	 */
	public TimestampedStringQueueFileHandler() {
		this (Long.getLong(MAXSIZE_PROPERTY_NAME,DEFAULT_SIZE),DEFAULT_PREFIX);
	}
	
	/**
	 * Constructor with the default max length of the files.
	 * <BR>
	 * If a property with name {@link #MAXSIZE_PROPERTY_NAME} exists then
	 * the max length is taken from the passed value other wise it is {@link #DEFAULT_SIZE}
	 * 
	 * @param prefix The prefix of the name of the cache files
	 * @see StringQueueFileHandler
	 */
	public TimestampedStringQueueFileHandler(String prefix) {
		this (Long.getLong(MAXSIZE_PROPERTY_NAME,DEFAULT_SIZE),prefix);
	}
	/**
	 * Constructor with the prefix of the files.
	 * 
	 * @param maxLength The max length of each file of the cache
	 * @see StringQueueFileHandler
	 */
	public TimestampedStringQueueFileHandler(long maxLength) {
		this (maxLength,DEFAULT_PREFIX);
	}
	
	/**
	 * Constructor.
	 * 
	 * @param maxLength The max length of each file of the cache 
	 * @param prefix The prefix of the name of the cache files
	 * @see StringQueueFileHandler
	 */
	public TimestampedStringQueueFileHandler(long maxLength, String prefix) {
		if (prefix==null || prefix.isEmpty()) {
			throw new IllegalArgumentException("Invalid prefix");
		}
		if (maxLength<=1024) {
			throw new IllegalArgumentException("Max length must be greater the 1024");
		}
		this.prefix=prefix;
		this.maxFilesSize=maxLength;
	}
	
	/**
	 * Create and return a new file for reading and writing.
	 * <P>
	 * The created file will be used to store strings and accessed randomly
	 * by the engine.
	 * 
	 * <B>Note</B>: implementors of this method should ensure tha the file is
	 *              deleted for example setting deleteOnExit().
	 *               
	 * @return the newly created file
	 * @throws IOException In case of error creating the file
	 */
	public abstract File getNewFile() throws IOException;
	
	/**
	 * This method is invoked when all the strings in the passed file have been processed 
	 * by the queue i.e. queue is empty and the file can be deleted.
	 * <P>
	 * <B>Note</B>: implementors of this method shall ensure that the file
	 *              is deleted.
	 * <P>
	 * The dates are represented as strings in ISO format.
	 * 
	 * @param filePointer The file to be released
	 * @param minTime The min timestamp of the strings in cache
	 * 				  (<code>null</code> if the file is empty)
	 * @param maxTime The max timestamp of the strings in cache
	 * 				  (<code>null</code> if the file is empty)
	 */
	public abstract void fileProcessed(File filePointer, String minTime, String maxTime);
	
	/**
	 * Return the maximum size of each file of the cache.
	 * 
	 * @return The maximum size of each file of the cache
	 */
	public final long getMaxFileSize() {
		return this.maxFilesSize;
	}
	
	/**
	 * Return the prefix of each file of the cache.
	 * 
	 * @return The prefix of each file of the cache
	 */
	public final String getPrefix() {
		return this.prefix;
	}
	
}
