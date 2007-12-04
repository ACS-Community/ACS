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

import java.util.Vector;
import com.cosylab.logging.engine.log.ILogEntry;
import com.sun.jdi.InvalidStackFrameException;

/**
*
* @author mcomin
*
*/
public class LogMultiFileCache {
	
	// This class defines a record of the file table.
	// The file table has one record for each generated log file.
	
	private class LogFileTableRecord {
		public LogFileTableRecord() throws LogCacheException {
			logCounter=0;
			minLogIdx=0;
			maxLogIdx=0;
			lbfc= new LogBufferedFileCache();
		}
		LogBufferedFileCache lbfc;
		int logCounter;
		int minLogIdx;
		int maxLogIdx;
	}
	
	// These two constants define the maximum size of the log file.
	public static final String FILEMAXSIZE_PROPERTY_NAME = "jlog.cache.fileMaxSize";
	public static final int DEFAULT_FILEMAXSIZE = 1000000;
	
	
	// Current size of the log file.  
	private int fileCurrSize;
	// Maximum size for the log file.
	private int fileMaxSize;
	// Minimum absolute log index,that is, the smaller index of a valid log.
//	private int minLogIndex;
	// Array of records each describing an exiting log file.
//	private LogFileTableRecord logFileRecord = new LogFileTableRecord(); 
	
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
	 * 
	 * @param pos The key of the log to delete
	 */
	private int searchFileLogTable(Integer logKey) throws LogCacheException {
				
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
				throw new IllegalArgumentException("log key not in range"+logKey);
			}
			
			if (logKey >= fileRecord.minLogIdx && logKey <= fileRecord.maxLogIdx) {
				return idx;	
			}
			
			idx ++;
		}
		
		throw new IllegalArgumentException("log key not in range"+logKey);
	}

	
	
	/**
	 * 
	 * @param pos The key of the log to delete
	 */
	public ILogEntry getLog(Integer logKey) throws LogCacheException {
						
		LogFileTableRecord fileRecord = logFileTable.get(searchFileLogTable(logKey));
			
		// Re-map global index into specific LogFileBufferedCache
		int newLogKey = logKey - fileRecord.minLogIdx;
		return fileRecord.lbfc.getLog(newLogKey);	
	}

	
	
//	Vector logFileTable = new Vector();
	
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
	 * 
	 */
	public synchronized int add(ILogEntry log) throws LogCacheException {
		
		if (log==null) {
			throw new LogCacheException("Error: trying to add a null log to the buffer");
		}
		
		// Check if file size exceeds fileMaxSize

		LogFileTableRecord fileRecord = logFileTable.get(logFileTable.size() - 1);

		if (fileRecord == null) {
			throw new LogCacheException("Null file record");
		}
		
		int size = fileRecord.lbfc.getFileSize();
		
		// If file Size exceeds maximum size :
		// - Flush buffer 
		// - Initialize next table record
		
		if (size >= fileMaxSize) {
			fileRecord.lbfc.flush();
			// Replace old file record with new one.
			fileRecord= createNewFileRecord();
		} else {
			fileRecord.maxLogIdx ++;
		}
		
		fileRecord.logCounter ++;		
		return fileRecord.lbfc.add(log) + fileRecord.minLogIdx;
	}
	
	
	
	/**
	 * Delete a log with the given key
	 * 
	 * @param pos The key of the log to delete
	 */
	public synchronized void deleteLog(Integer logKey) throws LogCacheException {
		
		LogFileTableRecord fileRecord = logFileTable.get(searchFileLogTable(logKey));
				
		// Delete log in the LogBufferedFile cache
		
		// Re-map global index into specific LogFileBufferedCache
		int newLogKey = logKey - fileRecord.minLogIdx;
		fileRecord.lbfc.deleteLog(newLogKey);

		// Update table record
		fileRecord.logCounter --;
		
		if (fileRecord.logCounter == 0) {		
			// Free resources.
			// @see LogBufferedFileCache.clear
			fileRecord.lbfc.clear(false,false);
			
			// Remove vector element
			logFileTable.remove(fileRecord);
		}
	}
}