/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
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
package alma.acs.logging.table;

import java.io.BufferedReader;
import java.io.File;
import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.swing.JOptionPane;

import alma.acs.logging.dialogs.LoadURLDlg;
import alma.acs.logging.io.IOLogsHelper;
import alma.acs.logging.io.LoadFileChooser;
import alma.acs.logging.io.SaveFileChooser;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.client.cache.LogCacheException;

/**
 * Extends the <code>LogEntryTableModelBase</code> adding I/O, deletion of logs
 * by time frame.
 * 
 * @see LogEntryTableModelBase
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class LogTableDataModel extends LogEntryTableModelBase {
    
	/**
	 * Contains references to the filters that are currently applied to logs.
	 * Actual filters are stored in filters.
	 * private final Vector appliedFilters = new Vector();
	 *
	 * Stores the current directory which is being accessed.	
	 */
	public File currentDir = null;
	
	private boolean isSuspended = true;
	
	/**
	 * An object to load and save logs
	 */
	private IOLogsHelper ioHelper=null;
	
	/**
	 * The time frame of the logs to keep in cache
	 * This limit is not for the buffer but for the timeframe of the whole cache
	 * A value of 0 means unlimited
	 */
	private volatile long timeFrame=0;
	
	/**
	 * Returns whether the saving/loading of the file has been cancelled or not that reflects on the
	 * status of the JToggleButton of the GUI. If canceled, then the button should be released. 
	 */
	public final boolean getSuspended() {
		return isSuspended;	
	}
	
	/**
	 * LogTableDataModel constructor.
	 */
	public LogTableDataModel(LoggingClient client) throws Exception {
		super(client);
	}
	
	/**
	 * Set the time frame of the logs in the cache
	 * The time frame if the amount of time we want to keep in the table 
	 * for example the last 2hr
	 * (the visible logs can be less)
	 * 
	 * @param timeframe The time frame in milliseconds
	 *                  0 means unlimited
	 */
	public void setTimeFrame(long timeframe) {
		if (timeframe<0) {
			throw new IllegalArgumentException("Impossible to set the time frame to "+timeframe);
		}
		timeFrame=timeframe;
		checkTimeFrame();
	}
	
	public void loadFromURL() {
		LoadURLDlg urlDlg = new LoadURLDlg("http://websqa.hq.eso.org/alma/snapshotRHE/ACS-Reports/TestCoverage-Linux/ACS/LGPL/CommonSoftware/jcont/test/tmp/all_logs.xml",loggingClient);
		urlDlg.setVisible(true);
		URL url = urlDlg.getURL();
		if (url==null) {
			// The user pressed Cancel
			return;
		}
		System.out.println("URL: "+url.toString());
		
		java.io.BufferedReader in = null;
		try {
			in = new java.io.BufferedReader(new java.io.InputStreamReader(url.openStream()), 16384);
		} catch (Throwable t) {
				t.printStackTrace();
				JOptionPane.showMessageDialog(null, "Exception opening "
						+ t.getMessage(), "Error opening " + url.toString(),
						JOptionPane.ERROR_MESSAGE);
				return;
		}

		try {
			isSuspended = true;
			clearAll();

			getIOHelper().loadLogs(in, loggingClient, loggingClient, 0);
		} catch (Throwable t) {
			t.printStackTrace();
			JOptionPane.showMessageDialog(null, "Exception reading "
					+ t.getMessage(), "Error reading " + url.toString(),
					JOptionPane.ERROR_MESSAGE);
		} finally {
			isSuspended = false;
		}
	}
	
	
	/**
	 * Loads logs from a file. If the name of the file is null, a dialog to
	 * choose the file is shown.
	 * 
	 * @param filename
	 *            The name of the file to load
	 */
	public void loadFromFile(String fileName) {
		if (fileName==null) {
			String[] filter = new String[] {
				".gz",
				".xml"
			};
			LoadFileChooser fc = new LoadFileChooser(currentDir,"Load",filter,loggingClient);
			File f = fc.getSelectedFile();
			if (f!=null) {
				 // Assigns to filename the name of the selected file			
				fileName = f.getAbsolutePath();
				
				// Assigns the current directory which 
				// the file chooser has accessed for getting the file
				currentDir = fc.getCurrentDirectory(); // if curDir is declared as File
			} else {
				// Load aborted
				return;
			}
			// Disconnect the engine and clear the table
			fc.execute();
		}
		
		BufferedReader br=null;
		int len=0;
		try {
			br=getIOHelper().getIoHelper().getBufferedReader(fileName);
			File f = new File(fileName);
			if (fileName.toLowerCase().endsWith(".gz")) {
				len=0; 
			} else {
				len=(int)f.length();
			}
			isSuspended = true;
			getIOHelper().loadLogs(br,loggingClient,loggingClient,len);
		} catch (Throwable fnfe) {
			JOptionPane.showInternalMessageDialog(loggingClient.getLogEntryTable(), fnfe.getMessage(), "Error opening "+fileName, JOptionPane.ERROR_MESSAGE);
			fnfe.printStackTrace();
		} 
	}

	/**
	 * Saves input logs into a file.
	 */
	public void saveFile() {
		
		SaveFileChooser fc = new SaveFileChooser("Save",currentDir);
		if (fc.getSelectedFile()!=null) {
			try {
				String filename = fc.getSelectedFile().getAbsolutePath();
					
				currentDir = fc.getCurrentDirectory();
				
				if (fc.mustBeCompressed() && !filename.toLowerCase().endsWith(".gz")) {
					filename=filename+".gz";
				} else if (!fc.mustBeCompressed() && !filename.toLowerCase().endsWith(".xml")) {
					filename=filename+".xml";
				}
				// Checks whether the selected file exists
				File fileToSave = new File(filename);
				if (fileToSave.exists()) {
					int act = JOptionPane.showConfirmDialog(null, filename + " already exists.  Overwrite?");
					
					// Checks whether a file exists
					while (act == JOptionPane.NO_OPTION) {
						fc = new SaveFileChooser("Save",currentDir);
						act = JOptionPane.showConfirmDialog(null, filename + " already exists.  Overwrite?");
					}
		
					// Canceled saving action
					if (act == JOptionPane.CANCEL_OPTION) {
						filename = null;
						isSuspended = false;
						return;
					}
				}
				isSuspended	= true;		
				saveFile(filename,fc.mustBeCompressed(),fc.getCompressionLevel());
			} catch (Exception e) {
				System.out.println("Exception "+e.getMessage());
				e.printStackTrace(System.err);
			};
			
		} else {
			isSuspended = false;
		}
	}
	
	/**
	 * Save the logs in a file
	 * 
	 * @param fileName The name of the file to save logs into
	 * @param compress <code>true</code> if the file must be compressed (GZIP)
	 * @param level The level of compression (ignored if <code>compress</code> is <code>false</code>) 
	 */
	private void saveFile(String fileName, boolean compress, int level) {
		try {
			getIOHelper().saveLogs(fileName,compress,level,allLogs);
	 	} catch (Exception e) {
	 		JOptionPane.showMessageDialog(null, "Exception saving the file: "+e.getMessage(),"Error saving "+fileName,JOptionPane.ERROR_MESSAGE);
	 	};
	}
	
	/**
	 * Return true if an async load/save is in progress
	 * @return
	 */
	public boolean IOInProgress() {
		if (ioHelper==null) {
			return false;
		}
		return ioHelper.isPerformingIO();
	}
	
	/**
	 * A getter method that created the helper only when needed
	 * 
	 * @return The IOLogsHelper object
	 * @throws Exception In case of error building the helper
	 */
	private IOLogsHelper getIOHelper() throws Exception {
		if (ioHelper==null) {
			ioHelper = new IOLogsHelper(loggingClient);
		}
		return ioHelper;
	}
	
	/**
	 * Check if the time frame of the logs in cache exceeds the limit and
	 * if it is the case, deletes the oldest logs
	 */
	private void checkTimeFrame() {
		if (timeFrame==0) {
			return;
		}
		Collection<Integer> logsToDelete = allLogs.getLogExceedingTimeFrame(timeFrame);
		Collections.sort((List<Integer>)logsToDelete);
		Collections.reverse((List<Integer>)logsToDelete);
		
		if (logsToDelete!=null && logsToDelete.size()>0) {
			for (Integer logNumber: logsToDelete) {
				//deleteLog(logNumber);
			}
		}
	}
	
	/**
	 * Closes all the threads and frees the resources
	 * This is the last method to call before closing the application
	 * @param sync If it is <code>true</code> wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		if (ioHelper!=null) {
			ioHelper.done(sync);
			ioHelper=null;
		}
		super.close(sync);
	}
	
	/**
	 * Start the thread to remove oldest logs 
	 */
	public void start() {
		super.start();
	}
	
	public int getFieldSortNumber() {
		 return 0;
	 }

	 public boolean sortedAscending() {
		 return true;
	 }
	 
	 public void setSortComparator(int index, boolean ascending) {
	 }

}
