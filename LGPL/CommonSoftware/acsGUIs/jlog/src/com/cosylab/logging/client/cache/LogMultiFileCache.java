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
	
	/**
	 * The name of the property defining the max size of each fileof the cache
	 */
	public static final String FILEMAXSIZE_PROPERTY_NAME = "jlog.cache.fileMaxSize";
	
	/**
	 * The default max size of each file of the cache
	 */
	public static final long DEFAULT_FILEMAXSIZE = 10000000;
	
	/**
	 * Flag activating some info prints   
	 */
	private boolean debugTrace=false;

	/**
	 * This vector implement a table whose records are specified by the
	 * class LogFileTableRecord.
	 */
	private long fileMaxSize;

	/**
	 * The vector of objects describing the buffers on disk
	 */
	private Vector<MultiFileTableRecord> logFileTable= new Vector<MultiFileTableRecord>();
	
	/**
	 * The number of logs in cache
	 */
	private volatile int logsInCache=0;
	
	/**
	 * The ID (i.e. the key) identifying each log
	 */
	private int ID=-1;

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
	 * Creates a new record for the log file table : min/max log indexes are
	 * initialized according to the total number of received logs.
	 * 
	 * The create <code>LogFileTableRecord</code> is added to the table of file records. 
	 * 
	 */
	private MultiFileTableRecord createNewFileRecord() throws LogCacheException {

		MultiFileTableRecord newFileRec=new MultiFileTableRecord();
		
		// Add new record to vector
		logFileTable.add(newFileRec);
		
		return newFileRec;
	}
		
	/**
	 * Looks in the log file table in which file buffered cache the log has 
	 * been stored. 
	 * 
	 * The search is made by checking the min and max key of the logs contained in each record
	 * against the key of the searched log.
	 * As a consequence the record returned by this method is the only one record that <B>can</B>
	 * contain this log but it could be that the log is not there (for example it has been deleted)
	 * 
	 * @param logKey  The key of the log to look for
	 * @return The record containing the log
	 * @throws LogCacheException If no record is found or an error happened
	 */
	private MultiFileTableRecord searchFileLogTable(Integer logKey) throws LogCacheException {
				
		// Check for key validity
		if (logKey == null || logKey < 0 ) {
			throw new IllegalArgumentException("Invalid or null log key "+logKey);
		}
		
		// Check if log file table empty
		
		if (logFileTable.isEmpty()) {
			throw new LogCacheException("Empty log file table");
		}
		
		for (MultiFileTableRecord record: logFileTable) {
			if (logKey>=record.getMinLogIdx() && logKey<=record.getMaxLogIdx()) {
				return record;
			}
		}
		throw new LogCacheException("Log record containing "+logKey+" not found");
	}
	
	/**
	 * Builds a LogMultiFileCache objects setting the maximum size of the
	 * cache files
	 * 
	 * @param fileSize The maximum size of cache files
	 */
	public LogMultiFileCache(long fileSize) throws LogCacheException {
		if (fileSize<=0) {
			throw new IllegalArgumentException("Invalid size for logFile "+fileSize);
		}
		fileMaxSize = fileSize;
		
		printDebugTrace("LogMultiFileCache uses file of max size: "+fileMaxSize);
	}
	
	/**
	 * Constructor
	 * 
	 * The maximum size of the files is read from the system properties.
	 * If the property is not found a default value is used.
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
						
		MultiFileTableRecord fileRecord = searchFileLogTable(logKey);
		return fileRecord.getLog(logKey);	
	}
	
	/**
	 * Add a log to the cache 
	 * 
	 * @param log The log to add to the cache
	 * 
	 */
	public synchronized int add(ILogEntry log) throws LogCacheException {
		if (log==null) {
			throw new LogCacheException("Trying to add a null log to the buffer");
		}
		
		// Check if file size exceeds fileMaxSize
		MultiFileTableRecord fileRecord;
		if (logFileTable.size()>0) {
			fileRecord=logFileTable.get(logFileTable.size() - 1);
		} else {
			fileRecord = createNewFileRecord();
		}
		
		long actualSize;
		try {
			actualSize = fileRecord.getFileSize();
		} catch (IOException e) {
			throw new LogCacheException("Error getting the size of the file",e);
		}
		
		if (actualSize >= fileMaxSize) {
						
			// Move logs from internal buffer to file
			fileRecord.flushBuffer();
					
			printDebugTrace("LogMultiFileCache log file : fileSize "+actualSize+" logs = "+fileRecord.getNumOfLogs()+
					" Range keys = "+fileRecord.getMinLogIdx()+" "+fileRecord.getMaxLogIdx());

			// Replace old file record with new one.
			fileRecord= createNewFileRecord();
		} 
		
		fileRecord.addLog(log, ++ID);
		logsInCache++;
		
		return ID;
	}
	
	/**
	 * Delete a log with the given key. 
	 * 
	 * @param pos The key of the log to delete
	 */
	public synchronized void deleteLog(Integer logKey) throws LogCacheException {
		if (logKey==null) {
			throw new IllegalArgumentException("The key can't be null");
		}
		
		MultiFileTableRecord fileRecord = searchFileLogTable(logKey);
		int idx = logFileTable.indexOf(fileRecord);
		if (idx==-1) {
			throw new IllegalStateException("The record is not in the vector");
		}
				
		// Delete log in the LogBufferedFile cache
		
		fileRecord.deleteLog(logKey);
		
		// Update table record
		logsInCache--;
		
		
		printDebugTrace("\n Delete log key = "+logKey+" from record "+idx+
				"  Tot. Logs "+ logsInCache + 
				"  Range keys = "+fileRecord.getMinLogIdx()+" "+fileRecord.getMaxLogIdx());
		

		// Remove file and element from the vector only if this is not
		// the logFile we are using to add logs.
		
		if (fileRecord.getNumOfLogs()==0) {		
			fileRecord.clear();
			
			printDebugTrace("Delete log file: recId " + idx + " num. of logs = "+fileRecord.getNumOfLogs()+ "Range keys = ["
					+fileRecord.getMinLogIdx()+", "+fileRecord.getMaxLogIdx()+"]");

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
		MultiFileTableRecord lastRecord = logFileTable.lastElement();
		Integer key = lastRecord.getLastLog();
		if (key==null) {
			throw new IllegalStateException("The cache is not empty but the last log does not exist?!?!");
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
	 * @throws In case of error getting the first log
	 */
	public Integer getFirstLog() {
		if (logsInCache==0) {
			return null;
		}
		MultiFileTableRecord firstRecord = logFileTable.get(0);
		Integer key = firstRecord.getFirstLog();
		if (key==null) {
			throw new IllegalStateException("The cache is not empty but the last log does not exist?!?!");
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
		for (MultiFileTableRecord record: logFileTable) {
			record.clear();
		}
		logFileTable.clear();
		logsInCache=0;
	}
	
	/**
	 * Replace the log in the given position with the new one
	 
	 * @param position The position of the log to replace
	 * @param log The key (identifier) of the log
	 * @see com.cosylab.logging.client.cache.ILogMap
	 */
	public void replaceLog(Integer key, ILogEntry log) throws LogCacheException {
		MultiFileTableRecord tableRecord=searchFileLogTable(key);
		tableRecord.replaceLog(key, log);
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
		HashSet<Integer> ret = new HashSet<Integer>();
		for (MultiFileTableRecord record: logFileTable) {
			ret.addAll(record.keySet());
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
		Vector<Integer>temp = new Vector<Integer>();
		for (MultiFileTableRecord record: logFileTable) {
			temp.clear();
			ret+=record.getFirstLogs(n-ret, temp);
			for (int t=0; t<temp.size(); t++) {
				temp.set(t, temp.get(t)+record.getMinLogIdx());
			}
			keys.addAll(temp);
			if (ret==n) {
				break;
			}
		}
		return ret;
	}

	/**
	 * Return the size of the last log file used by the cache
	 * (it is here for testing purposes)
	 *
	 * @see 
	 */
	public long getLogFileSize() throws LogCacheException {
		if (logFileTable.isEmpty()) {
			return 0;
		}
		MultiFileTableRecord lastRecord = logFileTable.lastElement();
		
		if (lastRecord == null) {
			throw new LogCacheException("Null file record");
		}
		
		long fileSize;
		try {
			fileSize = lastRecord.getFileSize();
		} catch (IOException ioe) {
			throw new LogCacheException(ioe);
		}
		
		return fileSize;
	}
	
	/**
	 * Return the disk space used by all the files of the cache.
	 * 
	 * @return The disk space used by all the files of the cache 
	 * @throws IOException In case of error getting the size of one of 
	 * 					   the files
	 */
	public synchronized long getFilesSize() throws IOException {
		long ret=0;
		for (MultiFileTableRecord mftb: logFileTable) {
			ret+=mftb.getFileSize();
		}
		return ret;
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
			MultiFileTableRecord fr = logFileTable.get(idx);
						
			long actualSize;
			try {
				actualSize = fr.getFileSize();
			} catch (IOException e) {
				throw new LogCacheException("Error getting the size of the file",e);
			}
			
			System.out.println("logFile "+idx+": log counter = "+ fr.getNumOfLogs()+ 
					"  Range Keys [" + fr.getMinLogIdx() + "  " + fr.getMaxLogIdx()+
					"] file size " + actualSize);
		}
		System.out.println("\n");
		return;
	}
	
	/**
	 * @return the number of the files used by the cache
	 */
	public int getNumberOfCacheFiles() {
		return logFileTable.size();
	}
	
}