/*    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package com.cosylab.logging.client.cache;

import java.io.IOException;
import java.util.Vector;
import com.cosylab.logging.engine.log.ILogEntry;

/**
*
* This class manages a set of LogBufferedFileCache objects allowing the storing of
* logs in different files, though from the user point of view the logs are 
* stored or retrieved from single data store.
* 
* This is due to the fact that files can grow up to a given size and hence setting 
* a limit on the number of logs that can be collected.
* 
* Logs are added to a file until it reaches a given size, then a new file is created.
* Logs have different length and therefore the number of logs in each File Buffered 
* Cache will be different (see example below).
* 
* Logs are accessible by the user through a keyword (logKey) which is implemented 
* as an increased running number.  On the other hand, within each File Buffered 
* Cache they have their own numbering system which starts from 0 to the maximum 
* number of logs.
* 
* Example :
*   
*     logKey    logCounter    Buffered       File
*     Range                  Cache Range     
*   --------------------------------------------------------
*    0  - 10       11         0 - 10        file1
*    11 - 25       15         0 - 14        file2
*    26 - 32        7         0 -  6        file3
*    33 - 50       18         0 - 18        file4
*
* When logs are deleted they are no longer accessible : when all logs stored in a 
* single file are marked as deleted the corresponding logCounter = 0 and as 
* consequence the file itself is removed.
* 
* Using the example let's assume we delete the logs with the following 
* keyword : 26,27,28,31,32, from now on these logs will not be accessible for the user.
* 
*     logKey    logCounter    Buffered       File
*     Range                  Cache Range     
*   --------------------------------------------------------
*    0  - 10       11         0 - 10        file1
*    11 - 25       15         0 - 14        file2
*    26 - 32        2         0 -  6        file3
*    33 - 50       18         0 - 18        file4
* 
*  If we delete also logs with keywords 29 and 30 the logCounter will be zero 
*  and file3 will be deleted.
*  
*  logKey Mapping
*  When retrieving the log we need to re-map the global log keyword into
*  the keyword used to add the log to the specific Buffered File Cache.
*  This is done by simply subtracting the minimum key value from the 
*  logKey.
*  
*  Example : Retrieve logKey 20.
*  logKey 20 is stored in the second Buffered File Cache and it is 
*  accessible with the local key = (20 - 11) =  9
*
*
* @author mcomin
*
*/
public class LogMultiFileCache {
	
	// This class defines a record of the file table.
	// The file table has one record for each generated log file.
	
	/**
	 * This class defines a data set providing information on a single 
	 * LogBufferedFileCache object. Each time a new file is created we
	 * also create an instance of this class.
	 * 
	 * LogFileTableRecord objects are organized in a vector to form the
	 * log file table used to retrieve the logs according to the user's
	 * defined keyword.
	 * 
	 * @author mcomin
	 */
	
	private class LogFileTableRecord {
		public LogFileTableRecord() throws LogCacheException {
			logCounter=0;
			minLogIdx=0;
			maxLogIdx=0;
			lbfc= new LogBufferedFileCache();
		}
		
		// Reference to an LogBufferedFileCache object
		LogBufferedFileCache lbfc;
		// Number of valid in the LogBufferedFileCache.
		int logCounter;
		// Minimum and maximum log keys stored in the LogBufferedFileCache
		// These two numbers are used to locate a given log by its keyword.
		int minLogIdx;
		int maxLogIdx;
	}
	
	// These two constants define the maximum size of the log file.
	public static final String FILEMAXSIZE_PROPERTY_NAME = "jlog.cache.fileMaxSize";
	public static final int DEFAULT_FILEMAXSIZE = 1000000;
	
	// Current size of the log file.  
	private long fileCurrSize;


	// This vecotor implement a table whose recods are specified by the
	// class LogFileTableRecord.

	private long fileMaxSize;

	private Vector<LogFileTableRecord> logFileTable= new Vector<LogFileTableRecord>();

	/**
	 * 
	 */
	private static int getDefaultMaxFileSize() {
		Integer fileSizeFromProperty = Integer.getInteger(FILEMAXSIZE_PROPERTY_NAME);
		if (fileSizeFromProperty != null) {
			return fileSizeFromProperty.intValue();
		}
		return DEFAULT_FILEMAXSIZE;
	}
	
	/**
	 * Creates a new record for the log file table : min/max log indexes are
	 * initialized according to the total number of received logs.
	 * 
	 * @params none
	 */
	private LogFileTableRecord createNewFileRecord() throws LogCacheException {

		// Check if file size exceeds fileMaxSize

		LogFileTableRecord lastFileRec = logFileTable.get(logFileTable.size() - 1);

		if (lastFileRec == null) {
			throw new LogCacheException(new IllegalStateException("Null file record"));
		}
				
		LogFileTableRecord newFileRec=new LogFileTableRecord();
		
		// Initialize counter and indexes according to the received number of logs
		newFileRec.minLogIdx = newFileRec.maxLogIdx = lastFileRec.maxLogIdx + 1;		
		
		// Add new record to vector
		logFileTable.add(newFileRec);
		
		return newFileRec;
	}
		
	/**
	 * Looks in the log file table in which file buffered cache the log has 
	 * been stored. It return the log file table record identifier.
	 * 
	 * @param pos  The log keyword
	 */
	private int searchFileLogTable(Integer logKey) throws LogCacheException {
				
		// Check for key validity
		if (logKey == null || logKey <= 0 ) {
			throw new IllegalArgumentException("Invalid or null log key "+logKey);
		}
		
		// Check if log file table empty
		
		if (logFileTable.isEmpty()) {
			throw new LogCacheException("Empty log file table");
		}
				
		int idx =0;
		int maxElem = logFileTable.size();
		
		while (idx < maxElem) {
			
			LogFileTableRecord fileRecord = logFileTable.get(idx);
			
			// log key belong to a set of logs that have already been deleted
			// and therefore they are no longer accessible.
			if (logKey < fileRecord.minLogIdx) {
				throw new IllegalArgumentException("log has been deleted"+logKey);
			}
			
			if (logKey >= fileRecord.minLogIdx && logKey <= fileRecord.maxLogIdx) {
				return idx;	
			}
			
			idx ++;
		}
		
		throw new IllegalArgumentException("log key not found"+logKey);
	}
	
	/**
	 * 
	 */
	public LogMultiFileCache(int fileSize) throws LogCacheException {
		if (fileSize<=0) {
			throw new IllegalArgumentException("Invalid size for logFile "+fileSize);
		}
		fileMaxSize = fileSize;
		logFileTable.add(new LogFileTableRecord());
	}
	
	/**
	 * 
	 */
	public LogMultiFileCache() throws LogCacheException {
		this(getDefaultMaxFileSize());
	}
		
	
	/**
	 * Retrieves a log by means of its keyword. 
	 * 
	 * @param pos The key of the log to retrieve
	 */
	
	public ILogEntry getLog(Integer logKey) throws LogCacheException {
						
		LogFileTableRecord fileRecord = logFileTable.get(searchFileLogTable(logKey));
			
		// Re-map global index into specific LogFileBufferedCache
		int newLogKey = logKey - fileRecord.minLogIdx;
		return fileRecord.lbfc.getLog(newLogKey);	
	}
	
	/**
	 * Logs are added using a keyword. 
	 * 
	 * 
	 * 
	 * @param pos The key of the log.
	 * 
	 */
	
	
	public synchronized int add(ILogEntry log) throws LogCacheException {
		
		if (log==null) {
			throw new LogCacheException("Trying to add a null log to the buffer");
		}
		
		// Check if file size exceeds fileMaxSize

		LogFileTableRecord fileRecord = logFileTable.get(logFileTable.size() - 1);

		if (fileRecord == null) {
			throw new LogCacheException("Null file record");
		}
		
		long fileSize;
		try {
			fileSize = fileRecord.lbfc.getFileSize();
		} catch (IOException ioe) {
			throw new LogCacheException(ioe);
		}

		if (fileSize >= fileMaxSize) {
			// Move logs from internal buffer to file
			fileRecord.lbfc.flushBuffer();

			// Replace old file record with new one.
			fileRecord= createNewFileRecord();
		} else {
			fileRecord.maxLogIdx ++;
		}
		
		fileRecord.logCounter ++;		
		return fileRecord.lbfc.add(log) + fileRecord.minLogIdx;
	}
	
	
	
	/**
	 * Delete a log with the given key. 
	 * 
	 * @param pos The key of the log to delete
	 */
	public synchronized void deleteLog(Integer logKey) throws LogCacheException {
		
		int recIdx = searchFileLogTable(logKey);
		LogFileTableRecord fileRecord = logFileTable.get(recIdx);
				
		// Delete log in the LogBufferedFile cache
		
		// Re-map global index into specific LogFileBufferedCache
		int newLogKey = logKey - fileRecord.minLogIdx;
		fileRecord.lbfc.deleteLog(newLogKey);

		// Update table record
		fileRecord.logCounter --;
		
		// Remove file and element from the vector only if this is not
		// the logFile we are using to add logs.
		
		if (fileRecord.logCounter == 0 && (recIdx < (logFileTable.size() - 1)) ) {		
			// Free resources.
			// @see LogBufferedFileCache.clear
			fileRecord.lbfc.clear(false,false);
			
			// Remove vector element
			logFileTable.remove(fileRecord);
		}
	}
}