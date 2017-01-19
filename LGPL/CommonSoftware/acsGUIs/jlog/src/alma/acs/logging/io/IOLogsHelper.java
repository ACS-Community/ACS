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
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.swing.JOptionPane;
import javax.swing.ProgressMonitor;
import javax.swing.SwingUtilities;

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
 * This class instantiate a new thread for each load/save operation.
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
public class IOLogsHelper implements IOPorgressListener {
	
	/**
	 * The thread to load logs
	 * 
	 * @author acaproni
	 *
	 */
	final class LoadLogs implements Callable<Void> {
		
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
		 * @see Callable
		 */
		public Void call() throws Exception {
			// Set the progress range
			if (range<=0) {
				progressMonitor = new ProgressMonitor(loggingClient.getContentPane(),"Loading logs...",null,0,Integer.MAX_VALUE);
			} else {
				progressMonitor= new ProgressMonitor(loggingClient.getContentPane(),"Loading logs...",null,0,range);
			}
			progressMonitor.setMillisToPopup(500);
			
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
			} catch (Throwable ioe) {
				System.err.println("Exception loading the logs: "+ioe.getMessage());
				ioe.printStackTrace(System.err);
				JOptionPane.showInternalMessageDialog(loggingClient.getContentPane(), "Exception loading "+ioe.getMessage(),"Error loading",JOptionPane.ERROR_MESSAGE);
			} finally {
				progressMonitor.close();
				progressMonitor=null;
				isPerformingIO=false;
			}
			return null;
		}
	};
	
	/**
	 * A class to download logs from a remote URL.
	 * The loading of logs from URL si done in 2 steps:
	 * <OL>
	 * 	<LI>Download the remote file in a temporary file
	 * 	<Li>Read the downloaded file
	 * </OL>
	 * This is to be able to download compressed files (gz) transparently.
	 * 
	 * @see UrlDownloader
	 * @author acaproni
	 * @since 2015.8
	 * 
	 */
	public class LoadUrl implements Callable<Void> {
		
		// The URL to download
		private final URL url;
		
		private final ACSRemoteLogListener logListener;
		private final ACSRemoteErrorListener errorListener;
		
		/**
		 * Constructor
		 * 
		 * @param url The URL to download 
		 * @param logListener  The callback for each new log read from the IO
		 * @param errorListener  The listener of errors
		 */
		public LoadUrl(URL url, ACSRemoteLogListener logListener, ACSRemoteErrorListener errorListener) {
			if (url==null) {
				throw new IllegalArgumentException("Can't download from a NULL URL!");
			}
			this.url=url;
			this.logListener=logListener;
			this.errorListener=errorListener;
		}
		
		public Void call() throws Exception {
			
			// Set the progress range
			SwingUtilities.invokeAndWait(new Runnable() {
				@Override
				public void run() {
					progressMonitor = new ProgressMonitor(loggingClient.getContentPane(),"Downloading from URL...",null,0,1);
					progressMonitor.setMillisToPopup(250);
				}
			});
			
			// Download the remote URL
			String fileToLoad;
			try {
				UrlDownloader urlDownloader = new UrlDownloader(url);
				fileToLoad=urlDownloader.download();	
			}  catch (Throwable t) {
				System.err.println("Exception downloading URL: "+t.getMessage());
				t.printStackTrace(System.err);
				JOptionPane.showInternalMessageDialog(loggingClient.getContentPane(), "Exception downloading URL "+t.getMessage(),"Error downloading",JOptionPane.ERROR_MESSAGE);
				isPerformingIO=false;
				return null;
			} finally {
				progressMonitor.close();
				progressMonitor=null;
			}
			
			System.out.println("File downloaded "+fileToLoad);
			
			// Open and read the file
			BufferedReader br = null;
			int len=0;
			try {
				br=getIoHelper().getBufferedReader(fileToLoad);
				File f = new File(fileToLoad);
				f.deleteOnExit();
				if (fileToLoad.toLowerCase().endsWith(".gz")) {
					len=0; 
				} else {
					len=(int)f.length();
				}
				loadLogs(br,logListener,errorListener,len);
			} catch (Throwable fnfe) {
				JOptionPane.showInternalMessageDialog(loggingClient.getContentPane(), fnfe.getMessage(), "Error opening "+fileToLoad, JOptionPane.ERROR_MESSAGE);
			}	 		
			return null;
		}
	}
	
	/**
	 * The thread to save logs
	 * 
	 * @author acaproni
	 *
	 */
	final class SaveLogs implements Callable<Void> {
		
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
		public Void call() throws Exception {

			// Open the output file
			BufferedWriter outBW=null;
			try {
				outBW = ioHelper.getBufferedWriter(fileName, false, compress, level);
			} catch (IOException e) {
				System.err.println("Exception while saving logs: "+e.getMessage());
				e.printStackTrace(System.err);
				JOptionPane.showInternalMessageDialog(loggingClient.getContentPane(), "Exception saving "+e.getMessage(),"Error saving",JOptionPane.ERROR_MESSAGE);
				isPerformingIO=false;
				return null;
			}
			progressMonitor= new ProgressMonitor(loggingClient.getContentPane(),"Saving logs...",null,0,cache.getSize());
			
			logsWritten=0;
			try {
				ioHelper.writeHeader(outBW);
				ioHelper.saveLogs(outBW, cache.iterator(), IOLogsHelper.this);
				ioHelper.terminateSave(outBW, true);
			} catch (Throwable e) {
				System.err.println("Exception while saving logs: "+e.getMessage());
				e.printStackTrace(System.err);
				JOptionPane.showInternalMessageDialog(loggingClient.getContentPane(), "Exception saving "+e.getMessage(),"Error saving",JOptionPane.ERROR_MESSAGE);
			} finally {
				progressMonitor.close();
				progressMonitor=null;
				isPerformingIO=false;
			}
			return null;
		}
		
	}
	
	/**
	 * We want one single thread to execute IO tasks.
	 */
	private final ExecutorService executor = Executors.newSingleThreadExecutor();
	
	/**
	 * The dialog
	 */
	private ProgressMonitor progressMonitor;
	
	/** 
	 * The IOHelper performing load and save
	 */
	private final IOHelper ioHelper;
	
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
	 * This variable is <code>true</code> when a I/O is in progress
	 * and false otherwise. It will be used by the logging window to enable/disable
	 * menu items.  
	 */
	private volatile boolean isPerformingIO=false;
	
	/**
	 * Build an IOCacheHelper object
	 *
	 * @throws Exception IN case of error building the {@link IOHelper}
	 */
	public IOLogsHelper(LoggingClient client) throws Exception {
		super();
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		loggingClient=client;
		ioHelper = new IOHelper();
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
		isPerformingIO=true;
		executor.submit(new LoadLogs( br, logListener, errorListener, progressRange));
	}
	
	/**
	 * Load logs from the passed url.
	 * 
	 * Loading logs from url is done in 2 steps:
	 * <OL>
	 * 	<LI>Download the file
	 * 	<LI>Load the file
	 * </OL>
	 * 
	 * Downloading the file and the reading it allows to download and read compressed (gz)
	 * files.
	 * 
	 * @param logListener The callback for each new log read from the IO
	 * @param errorListener The listener of errors
	 * @param url The url to read
	 */
	public void loadLogsFromUrl(URL url, ACSRemoteLogListener logListener, ACSRemoteErrorListener errorListener) {
		if (url==null || logListener==null|| errorListener==null) {
			throw new IllegalArgumentException("Null parameter received!");
		}
		isPerformingIO=true;
		executor.submit(new LoadUrl(url,logListener,errorListener));
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
		isPerformingIO=true;
		executor.submit(new SaveLogs(fileName,	compress, level, cache));
	}
	
	/**
	 * Release the resource acquired by this object
	 * It terminates the thread so the object can be deleted by the JVM
	 * 
	 * NOTE: when the thread is terminated it is not possible to 
	 *       request asynchronous services 
	 *       
	 * @param sync If it is <code>true</code> wait the termination of the threads before returning
	 */
	public void done(boolean sync) {
		executor.shutdownNow();
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
		return isPerformingIO;
	}

	public IOHelper getIoHelper() {
		return ioHelper;
	}
	
}

