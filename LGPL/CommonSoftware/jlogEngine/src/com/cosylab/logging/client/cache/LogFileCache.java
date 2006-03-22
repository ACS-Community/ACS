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

import java.util.Vector;
import java.util.Random;
import java.util.HashMap;

import java.io.File;
import java.io.RandomAccessFile;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.BufferedReader;

import javax.xml.parsers.ParserConfigurationException;

import com.cosylab.logging.engine.ACS.ACSLogParser;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.client.GroupedList;

/**
 * This class implements the cache in order to be able to manage
 * long log files.
 * It is implemented by a file of logs in XML format and an index of 
 * integers for each log entry in the file.
 * The cache is also used when receiving logs from the notification
 * channel.
 * The load and save are performed in parallel by the IOCacheHelper.
 *  
 * @author acaproni
 *
 */
public class LogFileCache {
	
	// The name of the log file of the cache
	// This file is filled of logs when they arrive from the notification
	// channel or by reading an input file
	// The file will be destroyed when the object is destroyed.
	private String logFileName;
	
	// The file of logs is accessed in a random way (the positions
	// are stored in the index)
	private RandomAccessFile file=null;
	
	// The index is an array of integer that stores the starting
	// position of each log entry in the logFile
	private Vector<Long> index = new Vector<Long>(256,16);
	
	// The parser
	private ACSLogParser parser; 
	
	// The logs replaced (for example the logs with some info added)
	// They are usually a few so we keep them in memory
	private HashMap<Integer,ILogEntry> replacedLogs = new HashMap<Integer,ILogEntry>();
	
	// Monitor if an async IO operation is in progress
	private boolean IOOperationInProgress =false;
	
	/**
	 * Build an empty cache
	 * 
	 * @param filters The user defined filters
	 * @param systemFilters The system filters
	 */
	public LogFileCache() throws LogCacheException {
		try {
			parser = new ACSLogParser();
		} catch (ParserConfigurationException pce) {
			throw new LogCacheException("Error instantiating the parser",pce);
		} 
		try {
			initCache();
		} catch (IOException ioe) {
			throw new LogCacheException("Error initializing the file",ioe);
		}
	}
	
	/**
	 * Add to the cache the logs in the given file
	 * NOTE: this method adds the logs to the cache (i.e. it does not
	 *       clear the cache and the index before loading)
	 *       
	 * @param fileName The XML file of logs to import
	 * @param filters The vector of the filters
	 * @param visibleLogs The visible logs
	 * @param showProgress If true a progress bar is shown
	 */
	public void loadLogs(
			String fileName, 
			FiltersVector filters, 
			GroupedList visibleLogs,
			boolean showProgress) throws LogCacheException {
		IOCacheHelper ioHelper = new IOCacheHelper(this);
		try {
			IOOperationInProgress=true;
			ioHelper.loadLogs(fileName,file,index,filters,visibleLogs,showProgress);
		} catch (FileNotFoundException fnfe) {
			IOOperationInProgress=false;
			throw new LogCacheException("File "+fileName+"not found",fnfe);
		} catch (Exception e) {
			IOOperationInProgress=false;
			throw new LogCacheException(e);
		} finally {
			ioHelper.done();
		}
	}
	
	/**
	 * Add to the cache the logs in the given reader
	 * NOTE: this method adds the logs to the cache (i.e. it does not
	 *       clear the cache and the index before loading)
	 *       
	 * @param reader The buffered reader to read the logs from
	 * @param filters The vector of the filters
	 * @param visibleLogs The visible logs
	 * @param showProgress If true a progress bar is shown
	 */
	public void loadLogs(
			BufferedReader reader, 
			FiltersVector filters, 
			GroupedList visibleLogs,
			boolean showProgress) throws LogCacheException {
		IOCacheHelper ioHelper = new IOCacheHelper(this);
		try {
			IOOperationInProgress=true;
			ioHelper.loadLogs(reader,file,index,filters,visibleLogs,showProgress);
		} catch (Exception e) {
			IOOperationInProgress=false;
			throw new LogCacheException(e);
		} finally {
			ioHelper.done();
		}
	}
	
	/**
	 * Save the logs in a file
	 * 
	 * @param fileName The name of the file
	 * @param showProgress If true a progress bar is shown
	 * @throws IOException
	 */
	public void saveLogs(String fileName, boolean showProgress) throws LogCacheException {
		IOCacheHelper ioHelper = new IOCacheHelper(this);
		try {
			IOOperationInProgress=true;
			ioHelper.saveLogs(fileName,this,showProgress);
		} catch (Exception e) {
			IOOperationInProgress=false;
			throw new LogCacheException(e);
		} finally {
			ioHelper.done();
		}
	}
	
	/**
	 * 
	 * @return The total number of logs in cache
	 */
	public int getSize() {
		int size;
		synchronized(index) {
			size=index.size();
		}
		return size;
	}
	
	/**
	 * Init the file where the cache stores the logs
	 * If the file already exists it is truncated to 0 length
	 * this situation might happen whenever the cache is cleared 
	 * 
	 * @throws IOException
	 */
	private void initCache() throws IOException {
		// The temporary file
		if (file==null) {
			file = new RandomAccessFile(getFile(),"rw");
		}
		file.setLength(0);
		// Clear the file to create the index
		synchronized(index) {
			index.clear();
		}
		// Clear the map of the replaced logs
		replacedLogs.clear();
	}
	
	/**
	 * Create the file for the cache trying in several places
	 * before giving up.
	 * 
	 * @ return The file for the temporary log file 
	 * 
	 */
	private File getFile() {
		String name=null;
		File f=null;
		// This does not work because the file is created into a 
		// $ACSDATA/tmp/ACS_INSTANCE.x that might be destroyed outside the control 
		// of jlog (for example acsStop).
		// I have commented out this line for the time being
		//name = FileHelper.getTempFileName(null,"jlog"+Math.abs(random)+".tmp");
		
		try {
			// Try to create the cache in $ACSDATA/tmp
			String acsdata = System.getProperty("ACS.data");
			acsdata=acsdata+"/tmp/";
			File dir = new File(acsdata);
			f = File.createTempFile("jlog",".tmp",dir);
			name=acsdata+f.getName();
		} catch (IOException ioe) {
			// Another error :-O
			do {
				// Try to create the file in the home diretory
				int random = new Random().nextInt();
				name = "~/jlog"+random+".jlog";
				f = new File(name);
			} while (f.exists());
		}
		if (f!=null) {
			logFileName=name;
			f.deleteOnExit();
		} else {
			logFileName=null;
		}
		return f;
	}
	
	
	/**
	 * Empty the cache reausing the same file
	 *
	 */
	public void clear() {
		try {
			clear(false,false);
		} catch (Exception e) {}
	}
	/**
	 * Empty the cache 
	 * 
	 * @param newFile If true the cache allocates a new file for storing the logs
	 * @param keepOldFile If true the old file for the cache is not deleted
	 * 
	 * @throws IOException
	 */
	public void clear(boolean newFile, boolean keepOldFile) throws LogCacheException {
		if (file==null) {
			synchronized(index) {
				index.clear();
			}
		} else {
			if (newFile) {
				try {
					file.close();
				} catch (IOException ioe) { }
				file = null;
				if (logFileName!=null) {
					if (!keepOldFile) {
						File f = new File(logFileName);
						f.delete();
					}
				}
			}
			try {
				initCache();
			} catch (IOException ioe) {
				throw new LogCacheException("Error initing the cahce",ioe);
			}
		}
	}
	
	/**
	 * Return the string representation (XML) of the log in position idx 
	 * The is string is read from the temporary file
	 * 
	 * @param idx The position of the log; valid position are between 0 and size-1 
	 * @return A String representing the log in the given position 
	 */
	private String getLogAsString(int idx) {
		//System.out.println("getLogAsString("+idx+")");
		if (idx<0 || idx>=getSize()) {
			throw new IndexOutOfBoundsException("Index out of bounds: "+idx);
		}
		
		// Get the position of the log in the file
		Long initPos=null;
		Long endPos=null;
		synchronized(index) {
			initPos= index.get(idx);
			endPos=null;
			if (idx==index.size()-1) {
				try {
					endPos=file.length();
				} catch (IOException ioe) {
					System.err.println("Error getting the length of the file");
				}
			} else {
				endPos = index.get(idx+1);
			}
		}
		
		//System.out.println("Getting log ["+initPos+","+endPos+"]");
		byte buffer[] = new byte[endPos.intValue()-initPos.intValue()];
		
		try {
			// Move to the starting of the log and read the log
			//
			// This operation must be performed in mutual exclusion
			// because other thread can access the cache in the same moment
			// (one of them might be the load)
			synchronized(file) {
				file.seek(initPos.longValue());
				file.read(buffer);
			}
		} catch (IOException ioe) {
			System.err.println("Errore nella seek: "+ioe.getMessage());
			
		}
		
		String tempStr = new String(buffer);
		//System.out.println("get Log returned: ["+tempStr+"]");
		return tempStr;
	}
	
	/**
	 * Return the log in the given position
	 *  
	 * @param pos The position of the log
	 * @return The LogEntryXML or null in case of error
	 */
	public ILogEntry getLog(int pos) {
		// Check if the log is present in the list of the replaced logs
		if (replacedLogs.containsKey(new Integer(pos))) {
			return replacedLogs.get(new Integer(pos));
		}
		String logStr = getLogAsString(pos).trim();
		try {
			
			return parser.parse(logStr);
		} catch (Exception e) {
			System.err.println("Exception "+e.getMessage());
			return null;
		}
	}
	
	/**
	 * Append a log to the end of the cache
	 * 
	 * @param log The log to append in the cache
	 * @return The position in the cache of the added log
	 */
	public int add(ILogEntry log) throws LogCacheException {
		String xml=log.toXMLString();
		long pos;
		synchronized(file) {
			try {
				pos=file.length();
				file.seek(file.length());
				file.writeBytes(xml);
			} catch (IOException ioe) {
				throw new LogCacheException("Error adding a log",ioe);
			}
		}
		synchronized(index) {
			index.add(pos);
		}
		return index.size()-1;
	}

	/**
	 * Replace the log in the given position with the new one
	 * NOTE: the new log is kept in memory
	 
	 * @param position The position of the log to replace
	 * @param log The new log
	 */
	public void replaceLog(int position, ILogEntry log) {
		replacedLogs.put(new Integer(position),log);
	}
	
	/**
	 * 
	 * @return true if an async load or save operation is in progress
	 */
	public boolean isPerformingIO() {
		return IOOperationInProgress;
	}
	
	/**
	 * The async thread for I/O uses this method to signal the cache
	 * that the operation is terminated
	 *
	 */
	public void IOOperationTerminated() {
		IOOperationInProgress=false;
	}
}
