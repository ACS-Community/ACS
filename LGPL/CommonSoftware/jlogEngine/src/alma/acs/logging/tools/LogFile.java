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
package alma.acs.logging.tools;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.Collection;
import java.util.concurrent.ArrayBlockingQueue;

import alma.acs.util.StopWatch;

import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.stats.ResourceChecker;

/**
 * This class read/write to/from a log file.
 * 
 * The load/save are performed asynchronously by a separate thread.
 * The callers are notified through events.
 * 
 * If an exception occurs during I/O, the operation terminates and the
 * exception is sent to the listeners (errorDetected). 
 * 
 * @author acaproni
 *
 */
public class LogFile extends Thread {
	/**
	 * This class implements add some functionalities
	 * to the standard StringBuffer like searching
	 * for the TAGs and closing TAGS.
	 * The purpose of this class is to support the loading of logs
	 * from a file.
	 * 
	 * @author acaproni
	 *
	 */
	public class LogStringBuffer {
		// The array that implements the buffer
		private StringBuffer buffer = new StringBuffer();
		
		// The types of the log
		// It is here so I don't need to get this reference very often
		private String[] logTypes=LogTypeHelper.getAllTypesDescriptions();
		
		/**
		 * The constructor
		 * 
		 * @param size The size of the rotating buffer
		 */
		public LogStringBuffer() {
			clear();
		}
		
		/**
		 * Clear the buffer
		 *
		 */
		public void clear() {
			if (buffer.length()>0) {
				buffer.delete(0,buffer.length());
			}
		}
		
		/**
		 * Append a char to the end of the array
		 * All the other chars are shifted back of one position, 
		 * and the first char is lost
		 * 
		 * @param ch The char to append
		 */
		public void append(char ch) {
			buffer.append(ch);
		}
		
		/**
		 * Remove any spurious in the buffer before a starting tag
		 *  
		 * @param tag The opening tag
		 */
		public void trim(String tag) {
			int pos=buffer.indexOf("<"+tag); 
			if (pos==0) {
				return;
			} else {
				buffer.delete(0,pos-1);
			}
		}
		
		/**
		 * @return a String representation of the buffer
		 */
		public String toString() {
			return buffer.toString();
		}
		
		/**
		 * @return The index of the type of the log found
		 *         as opening  TAG
		 *         -1: if the buffer contains no opening TAG
		 */
		private int getOpeningTagIndex() {
			for (int t=0; t<logTypes.length; t++) {
				if (buffer.indexOf("<"+logTypes[t])!=-1) {
					return t;
				}
			}
			return -1;
		}
		
		/**
		 * @return True is the buffer contains an opening tag
		 */
		public boolean hasOpeningTag() {
			return getOpeningTagIndex()!=-1;
		}
		
		/**
		 * Return the opening TAG in the buffer, if any
		 * 
		 * @return The opening TAG in the buffer (for example "Debug")
		 *         Return an empty string if there is no openin TAG in the buffer
		 */
		public String getOpeningTag() {
			int idx = getOpeningTagIndex();
			if (idx==-1) {
				return "";
			} else {
				return LogTypeHelper.getAllTypesDescriptions()[idx];
			}
		}
		
		/**
		 * Look for a closing TAG
		 * 
		 * NOTE: This method is not safe because does not take in account the CDATA
		 *       sections. This example fails:
		 *       <Info.....><![CDATA[... </Info>]]></Info>
		 *       The failure is because the closing tag is considered the first
		 *       occurrence of </Info> even if it apperas inside a CDATA section
		 *       The solution is that of traking the CDATA open/close  
		 * 
		 * @param tag The name of the closing tag (for example "Info")
		 * @return true if the buffer contains the closing TAG
		 */
		public boolean hasClosingTag(String tag) {
			String closingTag= "</"+tag+">";
			boolean ret = buffer.indexOf(closingTag)!=-1;
			if (tag.compareTo(LogTypeHelper.getLogTypeDescription(LogTypeHelper.ENTRYTYPE_TRACE))==0) {
				// Trace should terminate with "/>" but sometimes with </Trace>
				// even if I think that the latter is wrong
				ret = ret || buffer.indexOf("/>")!=-1;
			}			
			return ret;
		}
		
		/**
		 * Returns the index within this string of the first occurrence of the specified substring.
		 * 
		 * @param str Any string
		 * @return if the string argument occurs as a substring within this object, then the index 
		 *          of the first character of the first such substring is returned; 
		 *          if it does not occur as a substring, -1 is returned.
		 * 
		 * @see java.lang.StringBuffer
		 */
		public int indexOf(String str) {
			return buffer.indexOf(str);
		}
	}
	
	/**
	 * The asynchronous action to load save logs
	 * @author acaproni
	 *
	 */
	public class IOAction {
		
		// The name of the source file
		public String srcFileName;
		
		// The name of the destination file
		public String destFileName;
		
		// The type of the async operation
		public int type;
		
		// The identifier of this actiion
		public int UID;
		
		// The logs to save
		public Collection<ILogEntry> logs;
		
		// Type of async operations
		public static final int TERMINATE=0;
		public static final int LOAD=1;
		public static final int SAVE=2;
		
		/**
		 * Constructor for loading
		 * 
		 * @param src The name of the source file
		 * @param dest The name of the dest file
		 * @param type The type of the async operation
		 * @param uid The identifier of this action
		 */
		private IOAction(String src, int uid) {
			if (src==null || src.length()==0) {
				throw new IllegalArgumentException("Invalid source name");
			}
			srcFileName=src;
			destFileName=null;
			logs=null;
			this.type=LOAD;
			this.UID=uid;
		}
		
		/**
		 * Constructor for termination
		 * 
		 * @param uid The identifier of this action
		 */
		private IOAction(int uid) {
			this.UID=uid;
			this.type=TERMINATE;
			srcFileName=null;
			destFileName=null;
			logs=null;
		}
		
		/**
		 * Constructor for saving
		 * 
		 * @param dest The name of the destination
		 * @param logs The map of the logs to save
		 * @param uid The identifier of this action
		 */
		private IOAction(String dst, Collection<ILogEntry> logs, int uid) {
			if (dst==null || dst.length()==0) {
				throw new IllegalArgumentException("Invalid destination name");
			}
			if (logs==null || logs.size()==0) {
				throw new IllegalArgumentException("No logs to save");
			}
			srcFileName=null;
			destFileName=dst;
			type=SAVE;
			UID=uid;
			this.logs=logs;
		}
		
	}

	// The queue with the async ops
	private ArrayBlockingQueue<IOAction> asyncOps = new ArrayBlockingQueue<IOAction>(64);
	
    // The unique identifier for each async operation
	private static int actionID=0;
	
	// The listener to send the logs read to
	private ACSRemoteRawLogListener logListener;
	
	// The listener to notify when the async operation is finished
	private AsynchronousOperationListener asyncListener;
	
	// Buffer length for I/O
	private final int BUFFER_SIZE = 32768;
	
	/**
	 * Constructor
	 * 
	 * @param fileName The name of the file to read
	 * @param listener The listener to send the logs to
	 */
	public LogFile(ACSRemoteRawLogListener listener, AsynchronousOperationListener asyncListener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null RAW log listener");
		}
		if (asyncListener==null) {
			throw new IllegalArgumentException("Invalid null async operation listener");
		}
		this.logListener=listener;
		this.asyncListener=asyncListener;
		start();
	}
	
	
	/**
	 * Read the input file and send the logs to the listener
	 *
	 * @param action The action describing the requested load
	 */
	private void read(IOAction action) throws Exception {
		if (action.srcFileName==null || action.srcFileName.length()==0) {
			throw new IllegalArgumentException("Invalid source name");
		}
		if (action.type!=IOAction.LOAD) {
			throw new IllegalStateException("Trying to load but the action is not LOAD");
		}
		File inFile = new File(action.srcFileName);
		if (!inFile.canRead()) {
			throw new IllegalStateException("The file "+action.srcFileName+" is unreadable");
		}
		// The value for the progress information
		long start=0;
		long end = inFile.length();
		long bytesRead=0;
		
		asyncListener.operationProgress(start,end,bytesRead,action.UID);
		
		// The update is done every 3 secs
		long lastUpdate=0;
		
		BufferedReader inF = new BufferedReader(new FileReader(inFile),BUFFER_SIZE);
		
		// The last tag found
		String tag=null;
		
		// The "clever" buffer
		LogStringBuffer buffer = new LogStringBuffer();
		
		// Read one char per iteration
		int chRead;
		
		int logRecordsRead = 0;
		
		/**
		 * The size of the buffer
		 */
		final int size=16384;
		
		/** 
		 * The buffer of data read from the file
		 */
		char[] buf =new char[size];
		
		/**
		 * The cursor to scan the buffer (circular)
		 */
		int actualPos=-1;
		
		/**
		 * When it is 0, then we have to read another block from the file
		 */
		int bytesInBuffer=0;
		
		StopWatch stopWatch = new StopWatch();
	
		while (true) {
			// Read a block from the file if the buffer is empty
			if (bytesInBuffer==0) {
				bytesInBuffer = inF.read(buf,0,size);
			}
			if (bytesInBuffer<=0) { // EOF
				break;
			}
			bytesInBuffer--;
			actualPos=(actualPos+1)%size;
			chRead=buf[actualPos];
			
			bytesRead++;
			buffer.append((char)chRead);
			if (chRead == '>') {
				tag = buffer.getOpeningTag();
				if (tag.length()>0) {
					//System.out.println("TAG: "+tag+" (buffer ["+buffer.toString()+")");
					buffer.trim(tag);
				}
				if (buffer.hasClosingTag(tag)) {
					//System.out.println("\tClosing tag found (buffer ["+buffer.toString()+")");
					logListener.xmlEntryReceived(buffer.toString());
					buffer.clear();
					logRecordsRead++;
				}
			}
			if (System.currentTimeMillis()-lastUpdate>3000) {
				asyncListener.operationProgress(start,end,bytesRead,action.UID);
				lastUpdate=System.currentTimeMillis();
			}
		}
		asyncListener.operationProgress(start,end,bytesRead,action.UID);
		System.out.println("XML log record import finished with " + logRecordsRead + " records in " + 
					stopWatch.getLapTimeMillis()/1000 + " seconds.");
		System.out.println(ResourceChecker.getMemoryStatusMessage());
	}
	
	/**
	 * Write the logs into a file
	 * 
	 * @param action The action describing the requested save
	 */
	private void write(IOAction action) throws Exception {
		if (action.type!=IOAction.SAVE) {
			throw new IllegalStateException("Trying to save but the action is not SAVE");
		}
		if (action.destFileName==null || action.destFileName.length()==0) {
			throw new IllegalArgumentException("Invalid dest name");
		}
		if (action.logs==null || action.logs.size()==0) {
			throw new IllegalArgumentException("No logs to save");
		}
		FileWriter writer = new FileWriter(action.destFileName,false);
		BufferedWriter outF = new BufferedWriter(writer,BUFFER_SIZE);
		long start = 0;
		long end = action.logs.size();
		long current = 0;
		
		// The update is repeated every 3 secs
		long lastUpdate=0;
		
		outF.write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<Log>\n<Header Name=\"NameForXmlDocument\" Type=\"LOGFILE\" />\n");
		for (ILogEntry log: action.logs) {
			outF.write((log.toXMLString()+"\n").toCharArray());
			current++;
			if (System.currentTimeMillis()-lastUpdate>3000) {
				asyncListener.operationProgress(start,end,current,action.UID);
				lastUpdate=System.currentTimeMillis();
			}
		}
		asyncListener.operationProgress(start,end,current,action.UID);
		outF.write("</Log>");
		outF.flush();
		outF.close();
	}
	
	/**
	 * Create an async operation for loading
	 * 
	 * @param src The source file name
	 * 
	 * @return The IOAction for this operation
	 */
	public IOAction getLoadAction(String src) {
		return new IOAction(src, actionID++);
	}
	
	/**
	 * Create an action for saving
	 * 
	 * @param dest The name of the dest file
	 * @param logs The logs to save
	 * @return The IOAction for this operation 
	 */
	public IOAction getSaveAction(String dest, Collection<ILogEntry> logs) {
		return new IOAction(dest,logs,actionID++);
	}
	
	/**
	 * Create an action for teminating
	 * 
	 * @return The IOAction for this operation
	 */
	public IOAction getTerminationAction() {
		return new IOAction(actionID++);
	}
	
	/**
	 * Queue an async operation if there is enough room in the queue.
	 * 
	 * 
	 * @param a The action for the async operation
	 * @return true if the element has been added in the queue
	 */
	public boolean submitAsyncAction(IOAction a) {
		boolean ret=asyncOps.offer(a);
		return ret;
	}
	
	/**
	 * Thread to execute the actions
	 */
	public void run() {
		IOAction action;
		while (true) {
			try {
				action = asyncOps.take();
			} catch (InterruptedException e) {
				continue;
			}
			if (action == null) {
				continue;
			}
			asyncListener.operationStarted(action.UID);
			switch (action.type) {
				case IOAction.TERMINATE: {
					asyncListener.operationTerminated(action.UID);
					return;
				}
				case IOAction.LOAD: {
					try {
						read(action);
					} catch (Exception e) {
						asyncListener.errorDetected(e,action.UID);
					}
					break;
				}
				case IOAction.SAVE: {
					try {
						write(action);
					} catch (Exception e) {
						asyncListener.errorDetected(e,action.UID);
					}
					break;
				}
				default: {
					Exception e = new Exception("Invalid action requested ("+action.type+")");
					asyncListener.errorDetected(e,action.UID);
					break;
				}
			}
			asyncListener.operationTerminated(action.UID);
		}
	}
	
}
