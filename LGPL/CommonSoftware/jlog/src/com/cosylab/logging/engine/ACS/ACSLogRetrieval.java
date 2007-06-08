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

/** 
 * @author  acaproni   
 * @version $Id: ACSLogRetrieval.java,v 1.19 2007/06/08 09:50:22 acaproni Exp $
 * @since    
 */

package com.cosylab.logging.engine.ACS;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Random;
import java.util.Vector;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import com.cosylab.logging.client.cache.CacheUtils;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;
import com.cosylab.logging.settings.ErrorLogDialog;

import  javax.xml.parsers.ParserConfigurationException;
/**
 * ACSLogRetireval stores the XML string (or a String in case of binary logs) 
 * representing logs on a file when the engine is not able to follow the flow 
 * of the incoming logs 
 * The strings are stored on disk and the logs published to
 * the listeners when there is enough CPU available.
 * 
 * @see ACSRemoteLogListener
 * @see ACSRemoteRawLogListener
 * @see ACSLogConnectionListener
 */
public class ACSLogRetrieval extends Thread {
	
	// If the number of logs in endingPositions queue if greater 
	// then this number, we ssume that there is a delay between the logs
	// received and those shown in the GUI
	// The thread will publish this situation to the listeners
	private static final int DELAY_NUMBER=1000;
	
	
	
	// The object to dispatch messages to the listeners
	private ACSListenersDispatcher listenersDispatcher = null;
	
	// The name of the temp file
	private String fileName;
	
	// The temporary random file
	private RandomAccessFile rOutF=null;
	
	// The position where a read starts
	private long readCursor=0;
	
	// If it is true, the thread will not publish logs 
	// to the listeners
	private boolean paused=false;
	
	// Signal the thread to terminate
	private volatile boolean terminateThread=false;
	
	// Remeber if the object is closed to avoid adding new logs
	private volatile boolean closed=false;
	
	/**
	 * The ending position of each log is stored in this queue
	 */
	private LinkedBlockingQueue<Long> endingPositions = new LinkedBlockingQueue<Long>();
	
	// The parser
	private ACSLogParser parser=null;
	
	// true if the binary format is in use, falso otherwise
	private boolean binaryFormat;
	
	// The simple date format used to write and read dates from a string
	private SimpleDateFormat dateFormat = new SimpleDateFormat(ILogEntry.TIME_FORMAT);
	
	/**
	 * Constructor
	 * 
	 * @param listenersDispatcher The object to send messages to the listeners
	 * @param binFormat true if the lags are binary, 
	 *                  false if XML format is used 
	 */
	public ACSLogRetrieval(
			ACSListenersDispatcher listenersDispatcher,
			boolean binFormat) {
		super("ACSLogRetrieval");
		this.listenersDispatcher=listenersDispatcher;
		this.binaryFormat=binFormat;
		initialize();
	}
	
	/**
	 * Init the file and the parser
	 *
	 */
	private void initialize() {
		try {
			rOutF = new RandomAccessFile(getFile(),"rw");
		} catch (FileNotFoundException fne) {
			rOutF=null;
		}
		if (!binaryFormat) {
			try {
				parser = new ACSLogParserDOM();
			} catch (ParserConfigurationException pce) {
				parser=null;
			}
		}
		if (rOutF!=null) {
			this.setPriority(Thread.MIN_PRIORITY);
			this.start();
		} else {
			System.err.println("Retrieval of discarded logs disabled");
		}
	}
	
	/**
	 * Add a log in the file
	 * 
	 * @param XMLLogStr The XML string of the new log to add
	 */
	public void addLog(String XMLLogStr) {
		if (rOutF==null || closed) {
			return;
		}
		long pos;
		synchronized (rOutF) {
			try {
				pos = rOutF.length();
				rOutF.seek(pos);
				rOutF.writeBytes(XMLLogStr);
				pos = rOutF.length();
			} catch (IOException ioe) {
				System.err.println("Log Discarded: "+XMLLogStr);
				System.err.println("Reason: "+ioe.getMessage());
				return;
			}
		}
		boolean inserted = false;
		while (!inserted) {
			try {
				endingPositions.put(pos);
				inserted=true;
			} catch (InterruptedException ie) {	}
		}
	}
	
	/**
	 * Attempts to create the file for the strings in several places
	 * before giving up.
	 * 
	 * @ return The file for the temporary log file 
	 * 
	 */
	private File getFile() {
		String name=null;
		File f=null;
		try {
			// Try to create the file in $ACSDATA/tmp
			String acsdata = System.getProperty("ACS.data");
			acsdata=acsdata+"/tmp/";
			File dir = new File(acsdata);
			f = File.createTempFile("jlogLongRetrieval",".tmp",dir);
			name=acsdata+f.getName();
		} catch (IOException ioe) {
			// Another error :-O
			String homeDir = System.getProperty("user.dir");
			do {
				// Try to create the file in the home diretory
				int random = new Random().nextInt();
				name = homeDir + "/jlogLongRetrieval"+random+".jlog";
				f = new File(name);
			} while (f.exists());
		}
		if (f!=null) {
			fileName=name;
			f.deleteOnExit();
		} else {
			fileName=null;
		}
		return f;
	}
	
	/**
	 * The thread to read and notify the logs read from the file to the listeners
	 */
	public void run() {
		// delay is used to remember if there is a delay between the logs received
		// and those shown in the GUI.
		// We assume that such delay exists if the number of logs in queue is 
		// bigger then DELAY_NUMBER
		boolean delay=false;
		while (!terminateThread) {
			//	Check if a delay has to be notifyed to the listeners
			if (endingPositions.size()>ACSLogRetrieval.DELAY_NUMBER) {
				if (!delay) {
					delay=true;
					listenersDispatcher.publishDiscarding();
				}
			} else if (delay) {
				delay=false;
				listenersDispatcher.publishConnected(true);
			}
			// Do not flush the logs if the application is paused
			if (paused) {
				try {
					Thread.sleep(250);
				} catch(InterruptedException e) {}
				continue;
			}
			// Get from the queue the final position of the next log to read
			Long endPos;
			try {
				endPos = endingPositions.poll(250,TimeUnit.MILLISECONDS);
			} catch (InterruptedException ie) {
				continue;
			}
			if (endPos==null) {
				// Timeout i.e. no logs received
				continue;
			} 
			byte buffer[] = new byte[endPos.intValue()-(int)readCursor];
			synchronized (rOutF) {
				try {
					rOutF.seek(readCursor);
					rOutF.read(buffer);
				} catch (IOException ioe) {
					System.out.println("Log lost "+ioe.getMessage());
					continue;
				} finally {
					readCursor=endPos;
				}
			}
			String tempStr = new String(buffer).trim();
			if (tempStr.length()>0) {
				ILogEntry log;
				if (!binaryFormat) {
					listenersDispatcher.publishRawLog(tempStr);
					try {
						log = parser.parse(tempStr);
						listenersDispatcher.publishLog(log);
					} catch (Exception e) {
						StringBuilder strB = new StringBuilder("\nException occurred while dispatching the XML log.\n");
						strB.append("This log has been lost: "+tempStr);
						ErrorLogDialog.getErrorLogDlg(true).appendText(strB.toString());
						listenersDispatcher.publishReport(strB.toString());
						System.err.println("error parsing a log "+e.getMessage());
						e.printStackTrace();
						continue;
					}
				} else {
					try {
						log=CacheUtils.fromCacheString(tempStr);
						String xmlStr=log.toXMLString();
						listenersDispatcher.publishRawLog(xmlStr);
						listenersDispatcher.publishLog(log);
					} catch (Exception e) {
						StringBuilder strB = new StringBuilder("\nException occurred while dispatching the XML log.\n");
						strB.append("This log has been lost: "+tempStr);
						ErrorLogDialog.getErrorLogDlg(true).appendText(strB.toString());
						listenersDispatcher.publishReport(strB.toString());
						System.err.println("error parsing a log "+e.getMessage());
						e.printStackTrace();
						continue;
					}
				}
			}
		}
	}
	
	/**
	 * Pause/unpause the thread that publishes logs
	 * 
	 * @param pause
	 */
	public void pause(boolean pause) {
		paused=pause;
	}
	
	/**
	 * Close the threads and free all the resources
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		closed=true;
		terminateThread=true;
		if (sync) {
			while (isAlive()) {
				try {
					Thread.sleep(250);
				} catch (InterruptedException ie) {
					continue;
				}
			}
		}
	}
	
	
	
}
