/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
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
import java.io.IOException;

/**
 * An interface to create and delete cache files.
 * <P>
 * CacheFile uses an implementation of this class whenever it needs to create
 * a new file or when all the logs in the file have been processed and the
 * file can be deleted.
 * <P>
 * The purpose of this interface is to customize ACS and ARCHIVE needs while getting
 * logs from the NC in order to improve performances avoiding file duplication.
 * <P>
 * For further information, read COMP-3982.
 * 
 * @author acaproni
 * @since ACS 9.0
 */
public interface ILogQueueFileHandler {
	
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
	public static final String MAXSIZE_PROPERTY_NAME = "jlog.enine.cache.maxFilesSize";

	/**
	 * Create and return a new file for reading and writing.
	 * <P>
	 * The created file will be used to store strings and accessed randomly
	 * by the engine.
	 * 
	 * @return the newly created file
	 * @throws IOException In case of error creating the file
	 */
	public File getNewFile() throws IOException;
	
	/**
	 * Notify that all the logs in the passed file have been processed 
	 * by the engine and the file can be deleted.
	 * <P>
	 * The dates are represented as strings in ISO format.
	 * 
	 * @param filePointer The file to be released
	 * @param minTime The min timestamp of the logs in cache
	 * 				  (<code>null</code> if the file is empty)
	 * @param maxTime The max timestamp of the logs in cache
	 * 				  (<code>null</code> if the file is empty)
	 */
	void fileProcessed(File filePointer, String minTime, String maxTime);
	
	/**
	 * Return the maximum size of each file of the cache.
	 * <P>
	 * The returned size, in bytes, must be greater then 1024.
	 * A default implementation of this method can be seen in {@link LogQueueFileHandlerImpl}.
	 * 
	 * @return The maximum size of each file of the cache
	 * 
	 * @see LogQueueFileHandlerImpl
	 */
	public long getMaxFileSize();
}
