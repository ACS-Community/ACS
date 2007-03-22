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
 * @version $Id: ACSLogRetrieval.java,v 1.13 2007/03/22 10:33:42 acaproni Exp $
 * @since    
 */

package com.cosylab.logging.engine.ACS;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Random;
import java.util.concurrent.LinkedBlockingQueue;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.settings.ErrorLogDialog;

import  javax.xml.parsers.ParserConfigurationException;
/**
 * ACSLogRetireval stores the XML string representing logs on a file
 * when the engine is not able to follow the flow of the incoming logs
 * The strings are stored on disk and the logs published to
 * the listeners when there is enough CPU to do that.
 * For this reason, the thread in this method runs with a low priority.  
 */
public class ACSLogRetrieval extends Thread {
	
	// If the number of logs in endingPositions queue if greater 
	// then this number, we ssume that there is a delay between the logs
	// received and those shown in the GUI
	// The thread will publish this situation to the listeners
	private static final int DELAY_NUMBER=1000;
	
	// The engine
	private LCEngine engine;
	
	// The name of the temp file
	private String fileName;
	
	// The temporary random file
	private RandomAccessFile rOutF=null;
	
	// The position where a read starts
	private long readCursor=0;
	
	// If it is true, the thread will not publish logs 
	// to the listeners
	private boolean paused=false;
	
	/**
	 * The ending position of each log is stored in this queue
	 */
	private LinkedBlockingQueue<Long> endingPositions = new LinkedBlockingQueue<Long>();
	
	// The parser
	private ACSLogParser parser=null;
	
	/**
	 * Constructor
	 * 
	 * @param engine The engine
	 */
	public ACSLogRetrieval(LCEngine engine) {
		super("ACSLogRetrieval");
		this.engine=engine;
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
		try {
			parser = new ACSLogParserDOM();
		} catch (ParserConfigurationException pce) {
			parser=null;
		}
		if (rOutF!=null && parser!=null) {
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
		if (rOutF==null) {
			return;
		}
		long pos;
		synchronized (rOutF) {
			try {
				pos = rOutF.length();
				rOutF.seek(pos);
				rOutF.writeBytes(XMLLogStr);
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
		while (true) {
			//	Check if a delay has to be notifyed to the listeners
			if (endingPositions.size()>ACSLogRetrieval.DELAY_NUMBER) {
				if (!delay) {
					delay=true;
					engine.publishDiscarding();
				}
			} else if (delay) {
				delay=false;
				engine.publishConnected(true);
			}
			// Do not flush the logs if the application is paused
			if (paused) {
				try {
					Thread.sleep(250);
				} catch(InterruptedException e) {}
				continue;
			}
			// Get from the queue the final position of the next log to read
			long endPos;
			try {
				endPos = endingPositions.take();
			} catch (InterruptedException ie) {
				continue;
			}
			
			byte buffer[] = new byte[(int)endPos-(int)readCursor];
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
				if (engine.hasRawLogListeners()) {
					engine.publishRawLog(tempStr);
				}
				if (engine.hasLogListeners()) {
					ILogEntry log;
					try {
						log = parser.parse(tempStr);
					} catch (Throwable e) {
						StringBuilder strB = new StringBuilder("\nException occurred while dispatching the XML log.\n");
						strB.append("This log has been lost: "+tempStr);
						ErrorLogDialog.getErrorLogDlg(true).appendText(strB.toString());
						engine.publishReport(strB.toString());
						System.err.println("Error parsing a log "+e.getMessage());
						e.printStackTrace();
						continue;
					}
					try {
						engine.publishLog(log);
					} catch (Throwable t) {
						String msg = "Exception while publishing a log: "+t.getMessage();
						System.err.println(msg);
						t.printStackTrace(System.err);
						ErrorLogDialog.getErrorLogDlg(true).appendText(msg);
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
	
}
