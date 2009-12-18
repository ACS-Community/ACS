/*
 *    ALMA - Atacama Large Millimiter Array
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

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;
import java.util.TreeMap;
import java.util.Vector;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;
import com.cosylab.logging.engine.log.LogField;

/**
 * This class implements the cache in order to be able to manage
 * long log files.
 * It is implemented by a file of logs and a data structure to associate
 * the position in a file (LogCacheInfo) to each log.
 * Such data structure is realized by a TreeMap whith the key given by the (progressive,
 * unique) number of a log.
 * 
 * Actually it is not possible to remove a log from the file. 
 * To effectively delete logs from disk we need some kind of garbage collector that
 * removes the logs marked as delete from the file.
 * This garbage collector is needed to avoid the file on disk grows indefinitely.
 * 
 * Another potential problem is given by the integer identifier of the logs that 
 * grows indefinitely. A temporary solution could be that of using long instad of integers.
 * 
 * @author acaproni
 *
 */
public class LogFileCache implements ILogMap {
	
	/**
	 * An entry in the cache on disk.
	 * 
	 * We need this object in order to be able to delete log from the cache.
	 * 
	 * @author acaproni
	 *
	 */
	protected class LogCacheInfo {
		/**
		 * Starting position in the file
		 */ 
		public long start; 
		/**
		 * The length of the string in the file
		 */ 
		public int len;
		
		/**
		 * Create LogCacheInfo marked as not deleted
		 * 
		 * @param startPos The starting position in the file on disk
		 * @param length The length of the string the file on disk
		 */
		public LogCacheInfo(long startPos, int length) {
			if (startPos<0 || length<=0) {
				throw new IllegalArgumentException("Invalid positions for log [start="+startPos+", len="+len+"]");
			}
			start=startPos;
			len=length;
		}
		
		/**
		 * Empty constructor
		 */ 
		public LogCacheInfo() {
		}
	}
	
	/** 
	 * The name of the log file of the cache 
	 * This file is filled of logs when they arrive from the notification channel or by reading an input file 
	 * The file will be destroyed when the object is destroyed.
	 */
	private String logFileName;
	
	/**
	 * The file of logs is accessed in a random way 
	 * (the positions are stored in the index) 
	 */
	protected RandomAccessFile file=null;
	
	/**
	 *  Each log is identified by a unique integer used as key to get the log from the file 
	 *  (throw the index data structure) logID is incremented whenever a new log is added
	 *  
	 *  NOTE: in this implementation this value is incremented without taking 
	 *  		care of the logs deleted.
	 */
	protected int logID=0;
	
	/** 
	 * The index of the log is a SortedMap having the number identifying a log as key.  
	 * Each entry is a LogCacheInfo containing the starting and ending position of the log in the file
	 */
	protected TreeMap<Integer,LogCacheInfo> index = new TreeMap<Integer,LogCacheInfo>();
	
	/**
	 * The buffer to build the logs to write in cache
	 */
	private StringBuilder sb=new StringBuilder();
	
	/**
	 * The separator between the field of the logs written in cache
	 */
	private static final String SEPARATOR = new String (""+((char)0));
	
	/** 
	 * The logs replaced (for example the logs with some info added)  
	 * They are usually a few so we keep them in memory
	 */
	protected HashMap<Integer,ILogEntry> replacedLogs = new HashMap<Integer,ILogEntry>();
	
	/**
	 * 
	 * @return The number of logs in cache
	 */
	public synchronized int getSize() {
		int size;
		synchronized(index) {
			size=index.size();
		}
		return size;
	}
	
	/**
	 * Return the length of the file on disk
	 * 
	 * @return The size of the file cache
	 * 
	 * @throws IOException in case of I/O error
	 * @see java.io.RandomAccessFile
	 */
	public long getFileSize() throws IOException{
		return (file==null) ? 0 : file.length();
	}
	
	/**
	 * Init the file where the cache stores the logs
	 * If the file already exists it is truncated to 0 length
	 * this situation might happen whenever the cache is cleared 
	 * 
	 * @throws IOException
	 */
	protected void initCache() throws IOException {
		// The temporary file
		if (file==null) {
			file = new RandomAccessFile(getFile(),"rw");
		}
		file.setLength(0);
		// Clear the index
		synchronized(index) {
			index.clear();
		}
		// Clear the map of the replaced logs
		synchronized(replacedLogs) {
			replacedLogs.clear();
		}
	}
	
	/**
	 * Create the file for the cache trying in several places
	 * before giving up.
	 * 
	 * @ return The name of the file for the temporary log file 
	 * @throws IOException If an error happened while creating the new file
	 */
	private String getFile() throws IOException {
		String name=null;
		File f=null;
		// This does not work because the file is created into a 
		// $ACSDATA/tmp/ACS_INSTANCE.x that might be destroyed outside the control 
		// of jlog (for example acsStop).
		// I have commented out this line for the time being
		//name = FileHelper.getTempFileName(null,"jlog"+Math.abs(random)+".tmp");
		
		try {
			// Try to create the cache in $ACS_TMP
			String acsdata = System.getProperty("ACS.tmp");
			if (!acsdata.endsWith(File.separator)) {
				acsdata=acsdata+File.separator;
			}
			File dir = new File(acsdata);
			f = File.createTempFile("jlog",".tmp",dir);
		} catch (IOException ioe) {
			// Another error :-O
			String homeDir = System.getProperty("user.dir");
			File homeFileDir = new File(homeDir);
			if (homeFileDir.isDirectory() && homeFileDir.canWrite()) {
				do {
					// Try to create the file in the home diretory
					int random = new Random().nextInt();
					name = homeDir + File.separator+"jlog"+random+".jlog";
					f = new File(name);
				} while (f.exists());
			} else {
				// If for any reason the home folder was not writable then try 
				// to get a system temporary file
				f= File.createTempFile("jlog",".tmp");
				name=f.getAbsolutePath();
			}
		}
		if (f!=null) {
			logFileName=f.getAbsolutePath();
			f.deleteOnExit();
			f=null;
		} else {
			logFileName=null;
		}
		return logFileName;
	}
	
	/**
	 * Empty the cache. 
	 * 
	 * @throws IOException
	 */
	public synchronized void clear() throws LogCacheException {
		synchronized(index) {
			index.clear();
		}
		logID=0;
		if (file==null) {
			return;
		}
		
		try {
			file.close();
			if (logFileName!=null) {
				File f = new File(logFileName);
				f.delete(); 
			}
		} catch (IOException e) {
			throw new LogCacheException("Error clearing cache file",e);
		} finally {
			file=null;
			logFileName=null;
		}
	}
	
	/**
	 * Return the string representation (XML) of the log in position idx 
	 * The string is read from the temporary file
	 * 
	 * @param idx The position of the log; valid position are between 0 and size-1 
	 * @return A String representing the log in the given position 
	 */
	private String getLogAsString(int idx) {
		if (idx<0 || idx>=logID) {
			throw new IndexOutOfBoundsException("Index out of bounds: "+idx);
		}
		
		LogCacheInfo info=null;
		// Get the position of the log in the file
		synchronized(index) {
			info = index.get(idx);
		}
		// Allocate the string
		byte buffer[] = new byte[(int)(info.len)];
		
		try {
			// Move to the starting of the log and read the log
			//
			// This operation must be performed in mutual exclusion
			// because other thread can access the cache in the same moment
			// (one of them might be the load)
			synchronized(file) {
				file.seek(info.start);
				file.read(buffer);
			}
		} catch (IOException ioe) {
			System.err.println("Errore nella seek: "+ioe.getMessage());
			
		}
		
		return new String(buffer);
	}
	
	/**
	 * Return the log in the given position
	 *  
	 * @param pos The position of the log
	 * @return The LogEntryXML or null in case of error
	 */
	public synchronized ILogEntry getLog(Integer key) throws LogCacheException {
		// Some check about the key
		if (key<0 || key>=logID) {
			throw new LogCacheException("Key "+key+" out of range [0,"+logID+"[");
		}
		// Check if the log is in the index
		if (!index.containsKey(key)) {
			throw new LogCacheException("A log with the given key ("+key+") has not been found in the index");
		}
		// Check if the log is present in the list of the replaced logs
		synchronized(replacedLogs) {
			if (replacedLogs.containsKey(key)) {
				return replacedLogs.get(key);
			}
		}
		String logStr = getLogAsString(key); 
		try {
			return fromCacheString(logStr); 
		} catch (Exception e) {
			System.err.println("Exception "+e.getMessage());
			throw new LogCacheException("Exception parsing a log [logStr.len="+logStr.length()+", log={"+logStr+"}]",e);
		}
	}
	
	/**
	 * Append a log to the end of the cache
	 * 
	 * @param log The log to append in the cache
	 * @return The key in the cache of the added log
	 */
	public synchronized int add(ILogEntry log) throws LogCacheException {
		if (log==null) {
			throw new LogCacheException("Trying to add a null log!");
		}
		if (file==null) {
			try {
				initCache();
			} catch (IOException e) {
				throw new LogCacheException("Error initializing the cache",e);
			}
		}
		String cacheLogStr=toCacheString(log);
		long startingPos;
		synchronized(file) {
			try {
				startingPos=file.length();
				file.seek(file.length());
				file.writeBytes(cacheLogStr);
			} catch (IOException ioe) {
				throw new LogCacheException("Error adding a log",ioe);
			}
		}
		LogCacheInfo info = new LogCacheInfo(startingPos, cacheLogStr.length());
		synchronized(index) {
			index.put(logID,info);
		}
		return logID++;
	}

	/**
	 * Replace the log in the given position with the new one
	 * NOTE: the new log is kept in memory
	 
	 * @param position The position of the log to replace
	 * @param log The key (identifier) ot the log
	 */
	public void replaceLog(Integer key, ILogEntry log) throws LogCacheException {
		if (!index.containsKey(key)) {
			throw new LogCacheException("Trying to replace a log not in cache");
		}
		synchronized(replacedLogs) {
			replacedLogs.put(key,log);
		}
	}
	
	protected String toCacheString(ILogEntry log) {
		sb.delete(0,sb.length());
		for (LogField field: LogField.values()) {
			Object obj = log.getField(field);
			if (obj!=null) {
				if (obj instanceof Date) {
					sb.append(((Date)obj).getTime());
				} else if (obj instanceof LogTypeHelper ) {
					sb.append(((LogTypeHelper)obj).ordinal());
				} else {
					sb.append(obj.toString());
				}
			}
			sb.append((char)0);
		}
		if (log.hasDatas()) {
			Vector<ILogEntry.AdditionalData> datas = log.getAdditionalData();
			for (int t=0; t<datas.size(); t++) {
				ILogEntry.AdditionalData data = datas.get(t);
				sb.append(data.name);
				sb.append(SEPARATOR);
				sb.append(data.value);
				sb.append(SEPARATOR);
			}
		}
		return sb.toString();
	}
	
	private ILogEntry fromCacheString(String str) {
		String[] strs = str.split(SEPARATOR);
		Long millis = new Long(strs[0]);
		Integer entrytype = new Integer(strs[1]);
		String srcObject = null;
		if (strs.length>2) {
			srcObject=strs[2];
		}
		String fileNM = null;
		if (strs.length>3) {
			fileNM=strs[3];
		}
		Integer line = null;
		if (strs.length>4 && strs[4].length()!=0) {
			line =new Integer(strs[4]);
		}
		String routine = null;
		if (strs.length>5) {
			routine=strs[5];
		}
		String host = null;
		if (strs.length>6) {
			host=strs[6];
		}
		String process = null;
		if (strs.length>7) {
			process=strs[7];
		}
		String context = null;
		if (strs.length>8) {
			context=strs[8];
		}
		String thread = null;
		if (strs.length>9) {
			thread=strs[9];
		}
		String logid = null;
		if (strs.length>10) {
			logid=strs[10];
		}
		Integer priority = null;
		if (strs.length>11 && strs[11].length()>0) {
			priority=new Integer(strs[11]);
		}
		String uri = null;
		if (strs.length>12) {
			uri=strs[12];
		}
		String stackid = null;
		if (strs.length>13) {
			stackid=strs[13]; 
		}
		Integer stacklevel = null;
		if (strs.length>14 && strs[14].length()>0) {
			stacklevel =Integer.parseInt(strs[14]);
		}
		String logmessage = null;
		if (strs.length>15) {
			logmessage=strs[15];
		}
		String audience = null;
		if (strs.length>16) {
			audience=strs[16];
		}
		String array = null;
		if (strs.length>17) {
			array=strs[17];
		}
		String antenna = null;
		if (strs.length>18) {
			antenna=strs[18];
		}
        
        Vector<ILogEntry.AdditionalData> addDatas = null;
        if (strs.length>LogField.values().length) {
        	addDatas = new Vector<ILogEntry.AdditionalData>();
        	for (int t=LogField.values().length; t<strs.length; t+=2) {
        		addDatas.add(new AdditionalData(strs[t],strs[t+1]));
        	}
        }
        return new LogEntry(
        		millis,
        		entrytype,
        		fileNM,
        		line,
        		routine,
        		host,
        		process,
        		context,
        		thread,
        		logid,
        		priority,
        		uri,
        		stackid,
        		stacklevel,
        		logmessage,
        		srcObject,
                        audience,
                        array,
                        antenna,
        		addDatas);
	}
	
	/**
	 * Delete a log
	 * The log is marked as deleted and moved from the index to 
	 * the deleteLogIndex.
	 * The log still exists in the file but is not accessible.
	 * 
	 * @param pos The key of the log to remove
	 */
	public synchronized void deleteLog(Integer key) throws LogCacheException {
		if (key<0 || key>=logID) {
			throw new LogCacheException("Key "+key+" out of range [0,"+logID+"[");
		}
		if (!index.containsKey(key)) {
			throw new LogCacheException("The log "+key+" is not in cache [0,"+logID+"[");
		}
		// Remove the log from the index
		synchronized (index) {
			index.remove(key);
		}
		// Remove the log from the replacedLogs
		synchronized(replacedLogs) {
			replacedLogs.remove(key);
		}
	}
	
	/**
	 * Delete a set of logs
	 * 
	 * @param keys The keys of logs to delete
	 */
	public synchronized void deleteLogs(Collection<Integer> keys) throws LogCacheException {
		if (keys==null) {
			throw new IllegalArgumentException("Invalid null parameter");
		}
		for (Integer key: keys) {
			deleteLog(key);
		}
	}
	
	/**
	 * Return the key of the first valid log (FIFO).
	 * The key of the first log is 0 but it can change if the log 0 has
	 * been deleted.
	 * 
	 * @return The key of the first log
	 *         null if the cache is empty
	 */
	public Integer getFirstLog() {
		synchronized(index) {
			if (index.size()==0) {
				return null;
			} 
			return index.firstKey();
		}
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
	 */
	public int getFirstLogs(int n, Collection<Integer> keys) {
		if (n<=0 || keys==null) {
			throw new IllegalArgumentException("Invalid number of requested key or null collection");
		}
		int ret=0;
		Set<Integer> allTheKeys = index.keySet();
		Iterator<Integer> iter = allTheKeys.iterator();
		while (iter.hasNext() && ret<n) {
			keys.add(iter.next());
			ret++;
		}
		return ret;
	}
	
	/**
	 * Return the key of the last valid log (FIFO)
	 * The key of the last log is the key of the last inserted log
	 * but it can cheang if such log has been deleted
	 * 
	 * @return The key of the last inserted log
	 *         null if th cache is empty
	 */
	public Integer getLastLog() {
		synchronized(index) {
			if (index.size()==0) {
				return null;
			}
			return index.lastKey();
		}
	}
	
	/**
	 * Return a set with all the keys of the logs in cache
	 * 
	 * @return The keys of the logs in cache
	 */
	public Set<Integer> keySet() {
		return index.keySet();
	}
	
	/**
	 * Return an iterator over the logs in cache
	 */
	public Iterator<ILogEntry> iterator() {
		return new LogIterator(this);
	}
}
