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
 * @version $Id: ACSLogRetrieval.java,v 1.4 2006/06/06 23:57:11 sharring Exp $
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

/**
 * ACSLogRetireval stores the XML string representing logs on a file
 * when the engine is not able to follow the flow of the incoming logs
 * The strings are stored on disk and the logs published to
 * the listeners when there is enough CPU to do that.
 * For this reason, the thread in this method runs with a low priority.  
 */
public class ACSLogRetrieval extends Thread {
	
	// The engine
	private LCEngine engine;
	
	// The name of the temp file
	private String fileName;
	
	// The temporary random file
	private RandomAccessFile rOutF=null;
	
	// The position where a read starts
	private long readCursor=0;
	
	/**
	 * The ending position of each log is stored in this queue
	 */
	private LinkedBlockingQueue<Long> endingPositions = new LinkedBlockingQueue<Long>();
	
	// The parser
	ACSLogParser parser=null;
	
	/**
	 * Constructor
	 * 
	 * @param engine The engine
	 */
	public ACSLogRetrieval(LCEngine engine) {
		this.engine=engine;
		initialize();
	}
	
	private void initialize() {
		try {
			rOutF = new RandomAccessFile(getFile(),"rw");
		} catch (FileNotFoundException fne) {
			rOutF=null;
		}
		//try {
			parser = new ACSLogParserVTD();
		/*} catch (ParserConfigurationException pce) {
			parser=null;
		}*/
		if (rOutF!=null && parser!=null) {
			this.setPriority(Thread.MIN_PRIORITY);
			this.start();
		} else {
			System.out.println("Retrieval of discarded logs disabled");
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
	 * The thread to read and notify the logs from the file to the listeners
	 */
	public void run() {
		while (true) {
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
				ILogEntry log;
				try {
					log = parser.parse(tempStr);
				} catch (Exception e) {
					System.err.println("error parsing a log "+e.getMessage());
					e.printStackTrace();
					continue;
				}
				engine.publishLog(log);
			}
		}
	}
	
}
