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

import javax.xml.parsers.ParserConfigurationException;

import com.cosylab.logging.engine.ACS.ACSLogParser;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;

/**
 * This class implements the cache in order to be able to manage
 * long log files.
 * It is implemented by a file of logs in XML format and an index of 
 * integers for each log entry in the file.
 * The cache is also used when receiving logs from the notification
 * channel.
 * 
 * It uses a WriteBuffer to store the logs to write on disk. 
 * The purpose of this class is to write several logs at once reducing
 * the write operations on disk.
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
	
	/**
	 * The buffer of logs to write on disk
	 */
	private WriteBuffer wBuffer;
	
	/**
	 * The size of the buffer of logs to be written in the cache
	 */
	private final int WRITEBUFFERSIZE=8192;
	
	/** 
	 * Buffers the logs to write block of logs at once
	 * 
	 * The buffer conatins an HashMap of logs with their indexes as keys
	 * (the same as replaced logs)
	 * 
	 * @author acaproni
	 *
	 */
	private class WriteBuffer {
		/**
		 * The max number of logs in the buffer
		 */
		private int writeBufferSize;
		/**
		 * The map of buffered logs 
		 */
		private HashMap<Integer,ILogEntry> writeBuffer;
		
		/**
		 * The position of each XML in the buffer. These values will be added
		 * to the LogFileCache.index vector.
		 * 
		 * (it is zero base i.e. these numbers must be updated when they are flushed)
		 */
		private Vector<Long> bufferIndex;
		
		private StringBuffer charBuffer = new StringBuffer();
		
		/**
		 * The file of logs 
		 * @see LogFileCache.file
		 */
		private RandomAccessFile fileOfLogs;
		
		/**
		 * The vectors with the positions of the logs in cache
		 * @see LogFileCache.index
		 */
		private Vector<Long> indexes;
		
		/**
		 * Constructor 
		 * 
		 * @param sz The max num of logs in the buffer (the size of the buffer)
		 */
		public WriteBuffer(RandomAccessFile theFile, Vector<Long> theIndex, int sz) {
			if (theFile==null || theIndex==null || sz<=0) {
				throw new IllegalArgumentException("Illegal argument in constructor");
			}
			writeBufferSize=sz;
			fileOfLogs=theFile;
			indexes=theIndex;
			writeBuffer = new HashMap<Integer,ILogEntry>(writeBufferSize);
			bufferIndex = new Vector<Long>(writeBufferSize);
		}
		
		/**
		 * Add a log in the buffer
		 * If the buffer is full then it is flushed on disk
		 * @param log
		 */
		public synchronized int addLog(ILogEntry log) throws LogCacheException {
			// Add the log in the buffer
			bufferIndex.add((long)charBuffer.length());
			charBuffer.append(log.toXMLString());
			writeBuffer.put(bufferIndex.size()-1,log);
			if (writeBuffer.size()>=writeBufferSize) {
				flushBuffer();
			}
			return indexes.size()+bufferIndex.size()-1;
		}
		
		/**
		 * Flush the buffer on disk
		 *
		 */
		private void flushBuffer() throws LogCacheException {
			// Get all the logs
			String logsStr=charBuffer.toString();
			// The length of the file
			long pos;
			// Write the charBuffer on disk
			synchronized(fileOfLogs) {
				try {
					pos=fileOfLogs.length();
					fileOfLogs.seek(fileOfLogs.length());
					fileOfLogs.writeBytes(logsStr);
				} catch (IOException ioe) {
					throw new LogCacheException("Error flushing the buffer of logs",ioe);
				}
			}
			// Add the indexes in the vector 
			synchronized(index) {
				for (int t=0; t<bufferIndex.size(); t++) {
					index.add(pos+bufferIndex.get(t));
				}
			}
			// Clean up the buffer
			charBuffer.delete(0,charBuffer.length());
			bufferIndex.clear();
			writeBuffer.clear();
		}
		
		/**
		 * Return a log in the buffer
		 * 
		 * @param pos The position of the log
		 * @return The log
		 */
		public synchronized ILogEntry getLog(int pos)  throws LogCacheException {
			// Check if the log is in the buffer
			int lastLog = indexes.size()-1;
			if (pos<=lastLog) {
				throw  new LogCacheException("The log is not in the buffer!");
			}
			int posInBuffer = pos-lastLog-1;
			ILogEntry log = writeBuffer.get(posInBuffer);
			if (log==null) {
				throw new LogCacheException("The log is in the buffer but is null!");
			}
			return log;
		}
		
		/**
		 * Return the number of logs in buffer
		 * 
		 * @return the number of logs in buffer
		 */
		public synchronized int getSize() {
			return writeBuffer.size();
		}
		
		/**
		 * Clear the buffer maintaining the same file and cache
		 *
		 */
		public synchronized void clear() {
			bufferIndex.clear();
			writeBuffer.clear();
			charBuffer.delete(0,charBuffer.length());
		}
		
		/**
		 * Clear the cache setting a new file and vector index
		 * @param newFile The new cache file
		 * @param theIndex The new vector of positions
		 */
		public synchronized void clear(RandomAccessFile newFile, Vector<Long> newIndex) {
			fileOfLogs=newFile;
			indexes=newIndex;
			clear();
		}
	}
	
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
		wBuffer = new WriteBuffer(file,index,WRITEBUFFERSIZE);
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
		return size+wBuffer.getSize();
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
			String homeDir = System.getProperty("user.dir");
			do {
				// Try to create the file in the home diretory
				int random = new Random().nextInt();
				name = homeDir + "/jlog"+random+".jlog";
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
	public void clear() throws LogCacheException {
		try {
			clear(false,false);
		} catch (Exception e) {
			System.err.println("Exception caught while clearing "+e.getMessage());
			e.printStackTrace(System.err);
			throw new LogCacheException("Exception while clearing the cache",e);
		}
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
				throw new LogCacheException("Error initing the cache",ioe);
			}
		}
		wBuffer.clear(file,index);
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
	public ILogEntry getLog(int pos) throws LogCacheException {
		// Check if the log is present in the list of the replaced logs
		if (replacedLogs.containsKey(new Integer(pos))) {
			return replacedLogs.get(new Integer(pos));
		}
		if (pos>=index.size()) {
			return wBuffer.getLog(pos);
		}
		String logStr = getLogAsString(pos).trim();
		try {
			return new LogEntry(parser.parse(logStr));
		} catch (Exception e) {
			System.err.println("Exception "+e.getMessage());
			throw new LogCacheException("Exception parsing a log",e);
		}
	}
	
	/**
	 * Append a log to the end of the cache
	 * 
	 * @param log The log to append in the cache
	 * @return The position in the cache of the added log
	 */
	public int add(ILogEntry log) throws LogCacheException {
		return wBuffer.addLog(log);
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
	
}
