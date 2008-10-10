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
package alma.acs.logging.io;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;

import javax.swing.JOptionPane;
import javax.swing.ProgressMonitor;

import alma.acs.logging.engine.io.IOHelper;
import alma.acs.logging.engine.io.IOPorgressListener;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.LCEngine;

/**
 * This class is intended to support the commons IO
 * operations aiming to keep the <code>LogTableDataModel</code> class shorter 
 * and more readable.
 * <P>
 * This class instantiate a new thread for each load/save.
 * To cleanly close, the <code>done()</code> must be called.
 * <P>
 * This class allows only one load/save at a time.
 * If an I/O is requested while another one is in progress, an exception is thrown.
 * <P>
 * This class is not thread safe!
 * 
 * @author acaproni
 *
 */
public class IOLogsHelper extends Thread  implements IOPorgressListener {
	
	/**
	 * The thread to load logs
	 * 
	 * @author acaproni
	 *
	 */
	final class LoadLogs extends IOThread {
		
		private final BufferedReader br;
		private final ACSRemoteLogListener logListener;
		private final ACSRemoteErrorListener errorListener;
		private final int range;
		
		/**
		 * Constructor
		 * 
		 * @param br The the file to read
		 * @param logListener The callback for each new log read from the IO
		 * @param errorListener The listener of errors
		 * @param progressRange The range of the progress bar (if <=0 the progress bar is undetermined)
		 */
		public LoadLogs(BufferedReader br,ACSRemoteLogListener logListener, ACSRemoteErrorListener errorListener, int progressRange) {
			this.br=br;
			this.logListener=logListener;
			this.errorListener=errorListener;
			this.range=progressRange;
		}
		
		/**
		 * @see Thread
		 */
		public void run() {
			// Set the progress range
			if (range<=0) {
				progressMonitor = new ProgressMonitor(loggingClient.getLogEntryTable(),"Loading logs...",null,0,Integer.MAX_VALUE);
			} else {
				progressMonitor= new ProgressMonitor(loggingClient.getLogEntryTable(),"Loading logs...",null,0,range);
			}
			
			// Apply discard level, filters and audience to the IOHelper
			LCEngine engine = loggingClient.getEngine();
			ioHelper.setDiscardLevel(engine.getDiscardLevel());
			ioHelper.setAudience(engine.getAudience());
			ioHelper.setFilters(engine.getFilters());
			
			// Load the logs
			logsRead=0;
			bytesRead=0;
			try {
				ioHelper.loadLogs(br, logListener, null, errorListener, IOLogsHelper.this);
			} catch (Exception ioe) {
				System.err.println("Exception loading the logs: "+ioe.getMessage());
				ioe.printStackTrace(System.err);
				JOptionPane.showMessageDialog(null, "Exception loading "+ioe.getMessage(),"Error loading",JOptionPane.ERROR_MESSAGE);
			}
			progressMonitor.close();
		}
	};
	
	/**
	 * The thread to save logs
	 * 
	 * @author acaproni
	 *
	 */
	final class SaveLogs extends IOThread {
		
		private final String fileName;
		private final boolean compress;
		private final int level;
		private final LogCache cache;
		
		/**
		 * Constructor
		 * 
		 * @param fileName The name of the file to save
		 * @param compress <code>true</code> if the file must be compressed (GZIP)
		 * @param level The level of compression (ignored if <code>compress</code> is <code>false</code>) 
		 * @param cache The cache that contains the logs
		 */
		public SaveLogs(
				String fileName,
				boolean compress,
				int level,
				LogCache cache) {
			this.fileName=fileName;
			this.compress=compress;
			this.level=level;
			this.cache=cache;
		}
		
		/**
		 * @see Thread
		 */
		public void run() {

			// Open the output file
			BufferedWriter outBW=null;
			try {
				outBW = ioHelper.getBufferedWriter(fileName, false, compress, level);
			} catch (IOException e) {
				System.err.println("Exception while saving logs: "+e.getMessage());
				e.printStackTrace(System.err);
				JOptionPane.showMessageDialog(null, "Exception saving "+e.getMessage(),"Error saving",JOptionPane.ERROR_MESSAGE);
				return;
			}
			progressMonitor= new ProgressMonitor(loggingClient.getLogEntryTable(),"Saving logs...",null,0,cache.getSize());
			
			logsWritten=0;
			try {
				ioHelper.writeHeader(outBW);
				ioHelper.saveLogs(outBW, cache.iterator(), IOLogsHelper.this);
				ioHelper.terminateSave(outBW, true);
			} catch (IOException e) {
				System.err.println("Exception while saving logs: "+e.getMessage());
				e.printStackTrace(System.err);
				JOptionPane.showMessageDialog(null, "Exception saving "+e.getMessage(),"Error saving",JOptionPane.ERROR_MESSAGE);
			}
			progressMonitor.close();
			progressMonitor=null;
		}
		
	}
	
	/**
	 * The <code>Thread</code> executing I/O
	 * @return
	 */
	private IOThread thread=null;
	
	/**
	 * The dialog
	 */
	private ProgressMonitor progressMonitor;
	
	/** 
	 * The IOHelper performing load and save
	 */
	private IOHelper ioHelper = new IOHelper();
	
	/** 
	 * The logging client
	 */
	private LoggingClient loggingClient=null;
	
	/**
	 * The bytes read during a load
	 */
	private long bytesRead;

	/**
	 * The number of logs read while loading
	 */
	private int logsRead;
	
	/**
	 * The number of logs read while saving
	 */
	private int logsWritten;
	
	/**
	 * Build an IOCacheHelper object
	 *
	 */
	public IOLogsHelper(LoggingClient client) {
		super();
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		loggingClient=client;
		// Try to speed up (less responsive but seems good enough)
		setPriority(Thread.MAX_PRIORITY);
		setName("IOLogsHelper");
		setDaemon(true);
		start();
	}
	
	/**
	 * Load the logs from the given file in the Cache appending their
	 * starting positions in the index vector.
	 * <P>
	 * The logs are appended at the end of the cache as well as the new
	 * positions are appended at the end of the index vector.
	 * The load is performed in a thread as it might be very slow
	 * <P>
	 * The filter, discard level and audience of the engine are applied while loading.
	 * This is done by applying to <code>ioHelper</code> the same constraints
	 * defined in the <code>LCEngine</code>.
	 * 
	 * @param br The the file to read
	 * @param logListener The callback for each new log read from the IO
	 * @param errorListener The listener of errors
	 * @param progressRange The range of the progress bar (if <=0 the progress bar is undetermined)
	 */
	public void loadLogs(BufferedReader br,ACSRemoteLogListener logListener, ACSRemoteErrorListener errorListener, int progressRange) {
		if (br==null || logListener==null|| errorListener==null) {
			throw new IllegalArgumentException("Null parameter received!");
		}
		if (thread!=null && thread.isAlive()) {
			throw new IllegalStateException("An I/O is already in progress");
		}
		
		thread = new LoadLogs( br, logListener, errorListener, progressRange);
		thread.start();
	}
	
	/**
	 * Return a string formatted for JOptionPane making a word wrap
	 * 
	 * @param error The error i.e. the exception
	 * @param msg The message to show
	 * @return A formatted string
	 */
	private String formatErrorMsg(String msg) {
		StringBuilder sb = new StringBuilder();
		int count = 0;
		for (int t=0; t<msg.length(); t++) {
			char c = msg.charAt(t);
			sb.append(c);
			if (c=='\n') {
				count=0;
				continue;
			}
			if (++count >= 80 && c==' ') {
				count=0;
				sb.append('\n');
			}
		}
		return sb.toString();
	}
	
	/**
	 * Save the logs in a file.
	 * 
	 * @param fileName The name of the file to save
	 * @param compress <code>true</code> if the file must be compressed (GZIP)
	 * @param level The level of compression (ignored if <code>compress</code> is <code>false</code>) 
	 * @param cache The cache that contains the logs
	 * @throws IOException
	 * 
	 * @see saveLogsFromThread
	 */
	public void saveLogs(
			String fileName,
			boolean compress,
			int level,
			LogCache cache) throws IOException {
		if (fileName==null || fileName.isEmpty()) {
			throw new IllegalArgumentException("Invalid file name");
		}
		if (cache==null) {
			throw new IllegalArgumentException("The cache can't be null");
		}
		// Check if the thread is alive
		if (thread!=null && thread.isAlive()) {
			throw new IllegalStateException("An I/O is already in progress");
		}
		
		thread = new SaveLogs(fileName,	compress, level, cache);
		thread.start();
	}
	
	/**
	 * Release the resource acquired by this object
	 * It terminates the thread so the object can be deleted by the JVM
	 * 
	 * NOTE: when the thread is terminate it is not possible to 
	 *       request asynchronous services 
	 *
	 */
	public void done() {
		thread.stopThread(false);
		thread=null;
	}
	
	/**
	 * Moves the progress bar of the progress monitor 
	 * 
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesRead(long)
	 */
	@Override
	public void bytesRead(long bytes) {
		bytesRead=bytes;
		if (progressMonitor!=null) {
			progressMonitor.setProgress((int)bytesRead);
		}
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesWritten(long)
	 */
	@Override
	public void bytesWritten(long bytes) {}

	/**
	 * Change the label of the progress monitor
	 * 
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsRead(int)
	 */
	@Override
	public void logsRead(int numOfLogs) {
		logsRead=numOfLogs;
		if (progressMonitor!=null && logsRead%100==0) {
			progressMonitor.setNote(""+numOfLogs+" logs read");
			if (progressMonitor.isCanceled()) {
				ioHelper.stopIO();
			}
		}
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsWritten(int)
	 */
	@Override
	public void logsWritten(int numOfLogs) {
		logsWritten=numOfLogs;
		if (progressMonitor!=null && logsWritten%100==0) {
			progressMonitor.setProgress(logsWritten);
			progressMonitor.setNote(""+logsWritten+" logs saved");
			if (progressMonitor.isCanceled()) {
				ioHelper.stopIO();
			}
		}
	}
	
	/**
	 * Check if a load/save is in progress
	 * 
	 * @return <code>true</code> if a load/save is in progress
	 */
	public boolean isPerformingIO() {
		if (thread==null) {
			return false;
		}
		return thread.isAlive();
	}
	
}

