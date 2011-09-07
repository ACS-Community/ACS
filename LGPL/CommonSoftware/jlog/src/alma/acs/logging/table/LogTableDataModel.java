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

import java.net.URL;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import java.io.BufferedReader;
import java.io.File;
import javax.swing.JOptionPane;

import alma.acs.logging.dialogs.LoadURLDlg;
import alma.acs.logging.engine.io.IOHelper;
import alma.acs.logging.io.LoadFileChooser;
import alma.acs.logging.io.IOLogsHelper;
import alma.acs.logging.io.SaveFileChooser;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.client.cache.LogCacheException;

/**
 * Extends the <code>LogEntryTableModelBase</code> adding I/O, deletion of logs
 * and so on.
 * 
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
	 * The max number of logs in cache 
	 * This limit is not for the buffer but for the size of the whole cache
	 * A value of 0 means unlimited 
	 */
	private int maxLog=0;
	
	/**
	 * The time frame of the logs to keep in cache
	 * This limit is not for the buffer but for the timeframe of the whole cache
	 * A value of 0 means unlimited
	 */
	private long timeFrame=0;
	
	/**
	 * The time interval between 2 removal of logs from the table
	 */
	private final int DELETION_INTERVAL_TIME=30*1000;
	
	/**
	 * The thread removing logs from the cache.
	 * <P>
	 * During a deletion of logs, logs are immediately removed by the table 
	 * (i.e. from the <code>rows</code> vector) but they are removed from the cache
	 * by this thread. 
	 */
	private Thread deleterThread=null;
	
	/**
	 * The time to check for deletion.
	 * <P>
	 * The deletion is triggered by the calling of <code>updateTableEntries()</code>
	 * by {@link LogEntryTableModelBase.TableUpdater}.
	 * 
	 */
	private long nextDeletionCheckTime=System.currentTimeMillis()+DELETION_INTERVAL_TIME;
	
	/**
	 * Returns whether the saving/loading of the file has been cancelled or not that reflects on the
	 * status of the JToggleButton of the GUI. If canceled, then the button should be released. 
	 */
	public final boolean getSuspended() {
		return isSuspended;	
	}
	
	/**
	 * LCLogTableDataModel constructor comment. Gets updated logs.
	 */
	public LogTableDataModel(LoggingClient client) throws Exception {
		super(client);
	}
	
	/**
	 * Set the max number of logs to keep in cache
	 * This is the max number of logs stored in cache
	 * (the visible logs can be less)
	 * 
	 * @param max The max number of logs
	 *            0 means unlimited
	 */
	public void setMaxLog(int max) {
		if (max<0) {
			throw new IllegalArgumentException("Impossible to set the max log to "+max);
		}
		maxLog=max;
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

			isSuspended = true;
			clearAll();

			getIOHelper().loadLogs(in, loggingClient, loggingClient, 0);
		} catch (Exception ex) {
			ex.printStackTrace();
			JOptionPane.showMessageDialog(null, "Exception reading "
					+ ex.getMessage(), "Error reading " + url.toString(),
					JOptionPane.ERROR_MESSAGE);
		}
		isSuspended = false;
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
				try {
					 // Assigns to filename the name of the selected file			
					fileName = f.getAbsolutePath();
					
					// Assigns the current directory which 
					// the file chooser has accessed for getting the file
					currentDir = fc.getCurrentDirectory(); // if curDir is declared as File
					isSuspended	= true;
				} catch (Exception e) {
					System.err.println("Exception while loading: "+e.getMessage());
					e.printStackTrace(System.err);
				}
			} else {
				// Load aborted
				isSuspended = false;
				return;
			}
			// Disconnect the engine and clear the table
			fc.execute();
		}
		
		BufferedReader br=null;
		int len=0;
		try {
			br=new IOHelper().getBufferedReader(fileName);
			File f = new File(fileName);
			if (fileName.toLowerCase().endsWith(".gz")) {
				len=0; 
			} else {
				len=(int)f.length();
			}
		} catch (Exception fnfe) {
			JOptionPane.showInternalMessageDialog(loggingClient.getLogEntryTable(), fnfe.getMessage(), "Error opening "+fileName, JOptionPane.ERROR_MESSAGE);
			fnfe.printStackTrace();
			return;
		}
		getIOHelper().loadLogs(br,loggingClient,loggingClient,len);
		
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
	 */
	private IOLogsHelper getIOHelper() {
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
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		if (ioHelper!=null) {
			ioHelper.done();
			ioHelper=null;
		}
		super.close(sync);
	}

	 
	 public int getFieldSortNumber() {
		 return 0;
	 }

	 public boolean sortedAscending() {
		 return true;
	 }
	 
	 public void setSortComparator(int index, boolean ascending) {
	 }
	 
	 /**
	 * Delete the first <code>logsToDelete</code> logs from the data structures
	 * of the model.
	 * 
	 * @param The keys to remove from the cache
	 */
	private void deleteLogs(Integer[] keys) {
		if (keys==null || keys.length==0) {
			throw new IllegalArgumentException("Nothing to delete!");
		}
		for (Integer key: keys) {
			if (key!=null) {
				try {
					allLogs.deleteLog(key);
				} catch (LogCacheException e) {
					System.err.println("Exception deleting key="+key+": "+e.getMessage());
					e.printStackTrace();
				}
			}
		}
	}
	
	/**
	 * Update the table entries before refreshing the table
	 * 
	 * @return <code>true</code> If the model has been changed and the table needs to be refreshed
	 */
	@Override
	protected synchronized void updateTableEntries() {
		super.updateTableEntries();
		if (System.currentTimeMillis()>nextDeletionCheckTime) {
			deleteLogsFromTable();
		}
	}
	
	
	private void deleteLogsFromTable() {
		// Do not delete logs if the application is paused 
		if (loggingClient.isPaused() || maxLog<=0) {
			nextDeletionCheckTime=System.currentTimeMillis()+DELETION_INTERVAL_TIME;
			return;
		}	
		
		// Is there another delete in progress?
		// If yes then skip this iteration
		if (deleterThread!=null && deleterThread.isAlive()) {
			nextDeletionCheckTime=System.currentTimeMillis()+1000;
			return;
		}
		
		Integer[] keys=null;
		int removed=0;
		synchronized (rowsToAdd) {
			synchronized (rows) {
				removed= rows.size()-maxLog;
				if (removed>0) {
					keys = new Integer[removed];
					for (int t=0; t<removed; t++) {
						keys[t]=rows.get(rows.size()-t-1);
					}
					rows.removeLastEntries(removed);
					
				} 
			}
		}
		
		class DeleteFromCache implements Runnable {
			private final Integer[] keys;
			public DeleteFromCache(Integer[] k) {
				keys=k;
			}
			public void run() {
				deleteLogs(keys);
			}
		}
		
		
		nextDeletionCheckTime=System.currentTimeMillis()+DELETION_INTERVAL_TIME;
		if (removed<=0) {
			return;
		}
		System.out.println("deleteLogsFromTable.fire");
		fireTableDataChanged();
		
		deleterThread = new Thread(new DeleteFromCache(keys),"DeleteFromCache");
		deleterThread.setDaemon(true);
		deleterThread.start();
		keys=null;	
		
	}

}
