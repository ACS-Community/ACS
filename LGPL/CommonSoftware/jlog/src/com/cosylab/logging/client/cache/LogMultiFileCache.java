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
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
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
public class LogMultiFileCache  implements ILogMap {
	
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
	public static final long DEFAULT_FILEMAXSIZE = 1000000;
	
	// Flag activating some info prints   
	private boolean debugTrace=false;

	// This vecotor implement a table whose recods are specified by the
	// class LogFileTableRecord.
	private long fileMaxSize;

	// The vector of objects describing the buffers on disk
	private Vector<LogFileTableRecord> logFileTable= new Vector<LogFileTableRecord>();
	
	// The number of logs in cache
	private volatile int logsInCache=0;

	/**
	 * Get the max size of the file out of the system properties or
	 * uses the default value if the property does not exist
	 */
	private static long getDefaultMaxFileSize() {
		Integer fileSizeFromProperty = Integer.getInteger(FILEMAXSIZE_PROPERTY_NAME);
		if (fileSizeFromProperty != null) {
			return fileSizeFromProperty.longValue();
		}
		return DEFAULT_FILEMAXSIZE;
	}
	
	/**
	 * Prints trace info if traceDebug flag is true
	 * 
	 */
	
	private void printDebugTrace(String traceInfo) {
		if (debugTrace) {
			System.out.println(traceInfo); 
		}
	}
	
	
	/**
	 * Prints trace info if traceDebug flag is true
	 * 
	 */
	
	private long getFileSize(LogFileTableRecord fileRecord ) throws LogCacheException {
		
		long fileSize;
		try {
			fileSize = fileRecord.lbfc.getFileSize();
		} catch (IOException ioe) {
			throw new LogCacheException(ioe);
		}
		return fileSize;
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
				
		printDebugTrace("LogMultiFileCache new file : min logId = "+newFileRec.minLogIdx);
		
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
		if (logKey == null || logKey < 0 ) {
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
	public LogMultiFileCache(long fileSize) throws LogCacheException {
		if (fileSize<=0) {
			throw new IllegalArgumentException("Invalid size for logFile "+fileSize);
		}
		fileMaxSize = fileSize;
		logFileTable.add(new LogFileTableRecord());
		
		printDebugTrace("LogMultiFileCache uses file of max size: "+fileMaxSize);
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
		
		long fileSize = getFileSize(fileRecord);
		
		if (fileSize >= fileMaxSize) {
						
			// Move logs from internal buffer to file
			fileRecord.lbfc.flushBuffer();
					
			printDebugTrace("LogMultiFileCache log file : fileSize "+fileSize+" logs = "+fileRecord.logCounter+
					" Range keys = "+fileRecord.minLogIdx+" "+fileRecord.maxLogIdx);

			// Replace old file record with new one.
			fileRecord= createNewFileRecord();
		} else {
			fileRecord.maxLogIdx ++;
		}
		
		fileRecord.logCounter ++;
		logsInCache++;
		
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
		fileRecord.logCounter--;
		logsInCache--;
		
		
		printDebugTrace("\n Delete log key = "+logKey+
				" newKey = " + newLogKey + "  Tot. Logs "+ logsInCache + " recId = " + recIdx +
				"  Range keys = "+fileRecord.minLogIdx+" "+fileRecord.maxLogIdx);
		

		// Remove file and element from the vector only if this is not
		// the logFile we are using to add logs.
		
		if (fileRecord.logCounter==0 && (recIdx < (logFileTable.size() - 1)) ) {		
			// Free resources.
			// @see LogBufferedFileCache.clear
			fileRecord.lbfc.clear(false,false);
			
			printDebugTrace("Delete log file : recId " + recIdx + " logs = "+fileRecord.logCounter + "Range keys = "
					+fileRecord.minLogIdx+" "+fileRecord.maxLogIdx);

			// Remove vector element
			logFileTable.remove(fileRecord);
		}
	}
	
	/**
	 * 
	 * @return The number of logs in the map
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public int getSize() {
		return logsInCache;
	}
	
	/**
	 * Return the key of the last valid log (FIFO)
	 * The key of the last log is the key of the last inserted log
	 * but it can change if such log has been deleted
	 * 
	 * @return The key of the last inserted log
	 *         null if the cache is empty
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public Integer getLastLog() {
		if (logsInCache==0) {
			return null;
		}
		LogFileTableRecord lastRecord = logFileTable.lastElement();
		Integer key= lastRecord.lbfc.getLastLog();
		if (key==null) {
			throw new IllegalStateException("Inconsistency between logsInCache and last LogBufferedFileCache");
		}
		return key;
	}
	
	/**
	 * Return the key of the first valid log (FIFO).
	 * The key of the first log is 0 but it can change if the log 0 has
	 * been deleted.
	 * 
	 * @return The key of the first log
	 *         null if the cache is empty
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public Integer getFirstLog() {
		if (logsInCache==0) {
			return null;
		}
		LogFileTableRecord firstRecord = logFileTable.get(0);
		Integer key= firstRecord.lbfc.getFirstLog();
		if (key==null) {
			throw new IllegalStateException("Inconsistency between logsInCache and last LogBufferedFileCache");
		}
		return key;
	}
	
	/**
	 * Delete a set of logs
	 * 
	 * @param keys The keys of logs to delete
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public void deleteLogs(Collection<Integer> keys) throws LogCacheException {
		if (keys==null || keys.size()==0) {
			return;
		}
		for (Integer key: keys) {
			deleteLog(key);
		}
	}
	
	/**
	 * Clear the Map i.e. remove all the logs and keys from the map
	 * 
	 * @throws LogCacheException
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public synchronized void clear() throws LogCacheException {
		for (LogFileTableRecord record: logFileTable) {
			record.lbfc.clear();
		}
		logFileTable.clear();
		logsInCache=0;
		// Add the first item to be ready to add logs
		logFileTable.add(new LogFileTableRecord());
	}
	
	/**
	 * Replace the log in the given position with the new one
	 
	 * @param position The position of the log to replace
	 * @param log The key (identifier) of the log
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public void replaceLog(Integer key, ILogEntry log) throws LogCacheException {
		int pos= searchFileLogTable(key);
		LogFileTableRecord tableRecord=logFileTable.get(pos);
		tableRecord.lbfc.replaceLog(key, log);
	}
	
	/**
	 * Return an Iterator to browse the logs in the map.
	 * The order the iterator returns the logs is that of the keys.
	 * 
	 * @return an Iterator over the elements in this map
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public Iterator<ILogEntry> iterator() {
		return new LogIterator(this);
	}
	

	/**
	 * The keys in the map
	 * 
	 * @return The key in the map
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public Set<Integer> keySet() {
		Set<Integer> ret = new HashSet<Integer>();
		for (LogFileTableRecord record: logFileTable) {
			ret.addAll(record.lbfc.keySet());
		}
		return ret;
	}
	
	/**
	 * Append at most n keys from the first valid logs to the collection.
	 * First here means first in the FIFO policy.
	 * 
	 * The number of added keys can be less then n if the cache doesn't
	 * contain enough logs.
	 * 
	 * @param n The desired number of keys of first logs
	 * @param keys The collection to add they keys to
	 * @return The number of keys effectively added
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public int getFirstLogs(int n, Collection<Integer> keys) {
		int ret=0;
		for (LogFileTableRecord record: logFileTable) {
			ret+=record.lbfc.getFirstLogs(n-ret, keys);
			if (ret==n) {
				break;
			}
		}
		return ret;
	}
	
	

	/**
	 * Return the current size of the log file
	 *
	 * @see 
	 */
	public long getLogFileSize() throws LogCacheException {
		LogFileTableRecord lastRecord = logFileTable.lastElement();
		
		if (lastRecord == null) {
			throw new LogCacheException("Null file record");
		}
		
		long fileSize;
		try {
			fileSize = lastRecord.lbfc.getFileSize();
		} catch (IOException ioe) {
			throw new LogCacheException(ioe);
		}
		
		return fileSize;
	}
	
	/**
	 * Return the current maximum size of the log file
	 *
	 * @see 
	 */
	public long getMaxFileSize() throws LogCacheException {
		return fileMaxSize;
	}
	
	/**
	 * Return the current maximum size of the log file
	 *
	 * @see 
	 */
	public void setDebugTrace(boolean flag) throws LogCacheException {
		debugTrace = flag;
		return;
	}
	
	/**
	 * Return the current maximum size of the log file
	 *
	 * @see 
	 */
	public void printFileTableInfo() throws LogCacheException {
		int idx;
		
		System.out.println("\nlogFileTable : total logs = "+ logsInCache);
		
		for (idx = 0; idx < logFileTable.size(); idx ++) {
			LogFileTableRecord fr = logFileTable.get(idx);
						
			long fileSize = getFileSize(fr);
			
			System.out.println("logFile"+idx+" log counter = "+ fr.logCounter + 
					"  Range Keys " + fr.minLogIdx + "  " + fr.maxLogIdx +
					" file size " + fileSize);
		}
		System.out.println("\n");
		return;
	}
	
	
}