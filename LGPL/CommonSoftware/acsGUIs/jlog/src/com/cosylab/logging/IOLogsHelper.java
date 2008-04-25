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
package com.cosylab.logging;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedList;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;

import alma.acs.logging.engine.io.IOHelper;
import alma.acs.logging.engine.io.IOPorgressListener;

import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.LCEngine;

/**
 * This class is intended to support the commons IO
 * operations aiming to keep the LogTableDataModel class shorter 
 * and more readable.
 * This class instantiate a thread so to let the JVM able
 * to destroy objects of this class the thread must be terminated with
 * the close.
 * 
 * NOTE: The cache on file could be accessed in parallel
 *       by other thread reading logs (for example to refresh
 *       the logs already in the visible list)
 * 
 * @author acaproni
 *
 */
public class IOLogsHelper extends Thread  implements IOPorgressListener {
	
	// Monitor if an async IO operation is in progress
	private volatile boolean IOOperationInProgress =false;
	
	/**
	 * The dialog to show the progress of time consuming 
	 * operations (load and save for example)
	 * 
	 * Throw this dialog is also possible to abort the operation
	 * by pressing a button. The dialog does not communicate the abort
	 * request but just store it into a variable that the long operation
	 * should poll from time to time 
	 * 
	 * @author acaproni
	 */
	private class ProgressDialog extends JDialog implements ActionListener {
		
		/**
		 * The minimum interval of time between 2 updates of the panel 
		 */
		private static final int UPDATE_INTERVAL = 1000; 

		// The label with the status
		private JLabel statusLbl = new JLabel();
		// The progress bar
		private JProgressBar progressBar=new JProgressBar(JProgressBar.HORIZONTAL);
		
		// The checkbox to perform a fast IO
		// It is done by hiding the table of logs
		private JCheckBox fastCB = null;
		
		// The button to stop IO
		private JButton abortBtn;
		
		/**
		 * The last time the label has been updated
		 */
		private long lastUpdateTime=0;
		
		/**
		 * The label shown in the GUI reporting a textual description
		 * of the progress of the I/O
		 */
		private String label="";
		
		/**
		 * The thread to update the label and the progress bar into
		 * the swing thread
		 */
		private Thread updaterThread=null;
		
		/**
		 * The actual position of the progress bar
		 */
		private int progressBarPosition;
		
		/**
		 * Build a progress dialog with a progress bar
		 * 
		 * @param title The tile of the dialog
		 * @param min The min range for the progress bar
		 * @param max The max range for the progress bar
		 */
		public ProgressDialog(String title, int min, int max) {
			super();
			this.setTitle(title);
			progressBar.setMinimum(min);
			progressBar.setMaximum(max);
			progressBar.setValue(min);
			progressBar.setIndeterminate(false);
			progressBarPosition=min;
			initGUI();
			setVisible(true);
		}
		
		/**
		 * Build a progress dialog without an indeterminate progress bar
		 * 
		 * @param title The tile of the dialog
		 */
		public ProgressDialog(String title) {
			super();
			progressBar.setIndeterminate(true);
			setTitle(title);
			initGUI();
		}
		
		/** 
		 * Init the GUI
		 */
		private void initGUI() {
			addWindowListener(new WindowAdapter() {
				/**
				 * @see java.awt.event.WindowListener
				 */
				public void windowClosed(WindowEvent e) {
					ioHelper.stopIO();
				}
				
				/**
				 * @see java.awt.event.WindowListener
				 */
				public void windowClosing(WindowEvent e) {
					ioHelper.stopIO();
				}
			});
			
			BorderLayout borderLayout =new BorderLayout();
			borderLayout.setVgap(5);
			borderLayout.setHgap(5);
			setLayout(borderLayout);
			
			
			// Create the progress bar in the North
			add(progressBar,BorderLayout.NORTH);
			progressBar.setString("");
			progressBar.setStringPainted(true);
			
			// Create the labels into the label panel
			add(new JLabel("Status:"),BorderLayout.WEST);
			add(statusLbl,BorderLayout.CENTER);
			
			// Bottom panel contains the checkbox to hide/show the logs
			// and the abort button
			JPanel bottomPanel = new JPanel(new BorderLayout());
			boolean checked=!loggingClient.getLogEntryTable().isVisible();
			fastCB = new JCheckBox("Fast IO (hide logs)",checked);
			fastCB.addActionListener(this);
			bottomPanel.add(fastCB,BorderLayout.NORTH);
			
			// Add the abort button
			JPanel buttonPanel = new JPanel(new FlowLayout());
			abortBtn = new JButton("Abort");
			abortBtn.addActionListener(this);
			buttonPanel.add(abortBtn);
			bottomPanel.add(buttonPanel,BorderLayout.SOUTH);
			
			add(bottomPanel,BorderLayout.SOUTH);

			setVisible(true);
		}
		
		/**
		 * Replace the status label with the new one
		 * It packs the window to ensure that the string is visible
		 * 
		 * @param status The new status string
		 */
		public void updateStatus(String status) {
			label=status;
			updateGUI();
		}
		
		/**
		 * Update the status bar and the label.
		 * <P>
		 * The updated id done only if the <code>UPDATE_INTERVAL</code> time has elapsed
		 * since the previous update.
		 */
		private void updateGUI() {
			if (System.currentTimeMillis()-lastUpdateTime<UPDATE_INTERVAL) {
				return;
			}
			lastUpdateTime=System.currentTimeMillis();
			if (updaterThread==null) {
				updaterThread = new Thread(new Runnable() {
					public void run() {
						// Update the status bar
						if (!progressBar.isIndeterminate()) {
							int percent = (progressBar.getMaximum()-progressBar.getMinimum())/100;
							if (percent!=0) {
								progressBar.setString(""+progressBarPosition/percent+"%");
							}
							progressBar.setStringPainted(true);
							progressBar.setValue(progressBarPosition);
						}
						// Update the label
						int oldLength = statusLbl.getText().length();
						statusLbl.setText(label);
						if (label.length()>oldLength) {
							pack();
						}
					}
				});
			}
			SwingUtilities.invokeLater(updaterThread);
		}
		
		/**
		 * Update the position of the status bar
		 * 
		 * @param current The new position of the status bar
		 */
		public void updateProgressBar(int current) {
			progressBarPosition=current;
			updateGUI();
		}
		
		/**
		 * @see java.awt.event.ActionListener
		 */
		public void actionPerformed(ActionEvent evt) {
			if (evt.getSource()==fastCB) {
				loggingClient.getLogEntryTable().setVisible(
						!fastCB.isSelected());
			} else if (evt.getSource()==abortBtn) {
				ioHelper.stopIO();
			}
		}
		
		/**
		 * Override <code>setVisible()</code> to move the statistic window
		 * over the logging client and in front of other windows
		 */
		@Override
		public void setVisible(boolean visible) {
			super.setVisible(visible);
			// Move the statistic win on top of jlog
			if (visible && isShowing()) {
				Point loggingPos = IOLogsHelper.this.loggingClient.getLocationOnScreen();
				setLocation(loggingPos);
				pack();
				toFront();
			}
		}
		
	}
	
	/**
	 * An action for operations to be performed asynchronously
	 * 
	 * There is one specific constructor for each possible type
	 * of action
	 * 
	 * @author acaproni
	 *
	 */
	private class IOAction {
		
		public final static int LOAD_ACTION=0;
		public final static int SAVE_ACTION=1;
		public final static int TERMINATE_THREAD_ACTION=2;
		
		// The type of the action to perform
		public final int type;
		
		// The buffered reader to read the logs
		public final BufferedReader inFile;
		
		// The buffered writer to write the logs to save into
		public final BufferedWriter outFile;
		
		// The cache with all the logs
		public final LogCache logsCache; 
		
		// The range for the progress bar
		public final int progressRange;
		
		/**
		 * The listener i.e. the callback to push the loaded logs in 
		 */
		public final ACSRemoteLogListener logListener;
		
		/**
		 * The listener for errors parsing logs
		 * 
		 */
		public final ACSRemoteErrorListener errorListener;
		
		/**
		 * The constructor with all arguments.
		 * 
		 * Each overloaded public constructor calls this method with the right 
		 * parameters 
		 * 
		 * @param type The type of the action
		 * @param inFile The file to read logs from
		 * @param outF The file to write logs into
		 * @param listener The listener for each read log
		 * @param errorListener The listener for errors reading logs
		 * @param theCache The cache
		 * @param range The range of the progress bar (<0 means undtereminate)
		 */
		private IOAction(
				int type,
				BufferedReader inFile, 
				BufferedWriter outF,
				ACSRemoteLogListener listener, 
				ACSRemoteErrorListener errorListener, 
				LogCache theCache,
				int range) {
			this.type=type;
			this.logsCache=theCache;
			this.inFile=inFile;
			this.logListener=listener;
			this.errorListener=errorListener;
			this.outFile=outF;
			this.progressRange=range;
		}
		
		/**
		 * Build an action for asynchronous load operations
		 * 
		 * @param inFile The BuffredReader to read logs from
		 * @param theEngine The logging client engine
		 * @param theCache The cache of logs (used to shown values in the 
		 *                 progress dialog)
		 * @param range The range of the progress bar (<0 means undtereminate)
		 */
		public IOAction(BufferedReader inFile, ACSRemoteLogListener listener, ACSRemoteErrorListener errorListener, LogCache theCache, int range) {
			this(IOAction.LOAD_ACTION,inFile,(BufferedWriter)null,listener,errorListener,theCache, range);
		}
		
		/**
		 * Build an action for asynchronous save operations
		 * 
		 * @param outF The buffered file to save the logs into
		 */
		public IOAction(BufferedWriter outF, LogCache theCache) {
			this(IOAction.SAVE_ACTION,(BufferedReader)null,outF,(ACSRemoteLogListener)null,(ACSRemoteErrorListener)null,theCache,theCache.getSize());
		}
		
		/** 
		 * This constructor builds an action to terminate the thread
		 * The class will not be able to perform further requests
		 * and it will throw an IllegalStateException
		 *
		 */
		public IOAction() {
			this(IOAction.TERMINATE_THREAD_ACTION,(BufferedReader)null,(BufferedWriter)null,(ACSRemoteLogListener)null,(ACSRemoteErrorListener)null,(LogCache)null,0);
		}
	}
	
	// A queue of actions to perform in a separate thread
	private LinkedList<IOAction> actions = new LinkedList<IOAction>();
	
	// The dialog
	private ProgressDialog progressDialog;
	
	// The IOHelper performing load and save
	private IOHelper ioHelper = new IOHelper();
	
	// The logging client
	private LoggingClient loggingClient=null;
	
	/**
	 * The bytes read during a load
	 */
	private long bytesRead;
	
	/**
	 * The bytes read during a save
	 */
	private long bytesWritten;
	
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
	 * @param cache The cache of logs
	 * @param progressRange The range of the progress bar (if <=0 the progress bar is undeterminated)
	 */
	private void loadLogsFromThread(BufferedReader br,ACSRemoteLogListener logListener, ACSRemoteErrorListener errorListener, LogCache cache, int progressRange) {
		if (br==null || logListener==null|| errorListener==null) {
			throw new IllegalArgumentException("Null parameter received!");
		}
		
		IOOperationInProgress=true;
		
		// Hide the table with the logs
		// It reduces the access to the cache to redraw the logs 
		// speeding up the loading
		loggingClient.getLogEntryTable().setVisible(false);
		
		// Set the progress range
		if (progressRange<=0) {
			progressDialog = new ProgressDialog("Loading logs...");
		} else {
			progressDialog = new ProgressDialog("Loading logs...",0,progressRange);
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
			ioHelper.loadLogs(br, logListener, errorListener, this);
		} catch (IOException ioe) {
			System.err.println("Exception loading the logs: "+ioe.getMessage());
			ioe.printStackTrace(System.err);
			JOptionPane.showMessageDialog(null, "Exception loading "+ioe.getMessage(),"Error loading",JOptionPane.ERROR_MESSAGE);
		}
		if (progressDialog!=null) {
			// Close the dialog
			progressDialog.setVisible(false);
			progressDialog.dispose();
			progressDialog=null;
		}
		IOOperationTerminated();
	}
	
	/**
	 * Load the logs from a buffered reader.
	 * <P>
	 * The filter, discard level and audience of the engine are applied while loading
	 * 
	 * @param reader The buffered reader containing the logs
	 * @param listener The listener to add logs in
	 * @param cache The cache To show info in the dialog (can be null)
	 * @param showProgress If true a progress bar is shown
	 * @param progressRange An integer to make the progress bar showing the real 
	 *                      state of the operation (determinate)
	 *                      If it is <=0 then the progress bar is indeterminate
	 * 
	 * @see loadLogsFromThread
	 */
	public void loadLogs (
			BufferedReader reader,
			ACSRemoteLogListener listener,
			ACSRemoteErrorListener errorListener,
			LogCache cache,
			int progressRange) {
		// Check if the thread is alive
		if (!this.isAlive()) {
			throw new IllegalStateException("Unable to execute asynchronous operation");
		}
		// Check the params
		if (listener==null || reader==null || errorListener==null) {
			throw new IllegalArgumentException("Listseners and Reader can't be null");
		}
		
		
		
		// Add an action in the queue
		IOAction action = new IOAction(reader,listener,errorListener,cache,progressRange);
		synchronized(actions) {
			actions.add(action);
		}
		
		synchronized(this) {
			// Wake up the thread
				notifyAll();
		}
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
	 * @param cache The cache that contains the logs
	 * @param showProgress If true a progress bar is shown
	 * @throws IOException
	 * 
	 * @see saveLogsFromThread
	 */
	public void saveLogs(
			String fileName, 
			LogCache cache,
			boolean showProgress) throws IOException {
		// Check if the thread is alive
		if (!this.isAlive()) {
			throw new IllegalStateException("Unable to execute asynchronous operation");
		}
		IOOperationInProgress=true;
		// Open the output file
		FileWriter fw = new FileWriter(fileName,false);
		BufferedWriter outBW = new BufferedWriter(fw);
		
		IOAction action = new IOAction(outBW,cache);
		
		synchronized(actions) {
			actions.add(action);
		}
		
		synchronized(this) {
			// Wake up the thread
			notifyAll();
		}
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
		// Stop every load/save operation
		ioHelper.stopIO();
		// Check if the thread is alive
		IOAction terminateAction = new IOAction();
		synchronized(actions) {
			actions.add(terminateAction);
		}
		synchronized(this) {
			// Wake up the thread
			notifyAll();
		}
	}
	
	/**
	 * The thread for asynchronous operation.
	 * It takes an action out of the actions queue and perform the requested
	 * task until there no more actions in the list
	 */
	public void run() {
		IOAction action;
		while (true) {
			if (!actions.isEmpty()) {
				synchronized(actions) {
					action = actions.removeFirst();
				}
				switch (action.type) {
					case IOAction.LOAD_ACTION: {
						loadLogsFromThread(action.inFile,action.logListener,action.errorListener,action.logsCache,action.progressRange);
						break;
					}
					case IOAction.TERMINATE_THREAD_ACTION: {
						// Terminate the thread
						return;
					}
					case IOAction.SAVE_ACTION: {
						saveLogsFromThread(action.outFile,action.logsCache);
						break;
					}
				}
			} else {
				try {
	                synchronized (this) {
		                wait();
	                }
	            } catch (InterruptedException e) { }
			}
		}
	}
	
	/** 
	 * Save all the logs in the cache in the file
	 * 
	 * @param outBW The buffered writer where the logs have to be stored
	 * @param logs The cache with all the logs
	 */
	private void saveLogsFromThread(BufferedWriter outBW, LogCache cache) {
		if (outBW==null || cache==null) {
			throw new IllegalArgumentException("BufferedWriter and LogCache can't be null");
		}
		
		progressDialog = new ProgressDialog("Saving logs...",0,cache.getSize());
		
		logsWritten=0;
		bytesWritten=0;
		try {
			ioHelper.writeHeader(outBW);
			ioHelper.saveLogs(outBW, cache.iterator(), this);
			ioHelper.terminateSave(outBW, true);
		} catch (IOException e) {
			System.err.println("Exception while saving logs: "+e.getMessage());
			e.printStackTrace(System.err);
			JOptionPane.showMessageDialog(null, "Exception saving "+e.getMessage(),"Error saving",JOptionPane.ERROR_MESSAGE);
		}
			
		if (progressDialog!=null) {
			// Close the dialog
			progressDialog.setVisible(false);
			progressDialog.dispose();
			progressDialog=null;
		}
		IOOperationTerminated();
	}
	
	/*
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
		loggingClient.getLogEntryTable().setVisible(true);
		IOOperationInProgress=false;
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesRead(long)
	 */
	@Override
	public void bytesRead(long bytes) {
		bytesRead=bytes;
		if (progressDialog!=null) {
			progressDialog.updateProgressBar((int)bytesRead);
		}
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesWritten(long)
	 */
	@Override
	public void bytesWritten(long bytes) {
		bytesWritten=bytes;
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsRead(int)
	 */
	@Override
	public void logsRead(int numOfLogs) {
		logsRead=numOfLogs;
		if (progressDialog!=null) {
			progressDialog.updateStatus("bytes "+bytesRead+", logs: "+logsRead);
		}
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsWritten(int)
	 */
	@Override
	public void logsWritten(int numOfLogs) {
		logsWritten=numOfLogs;
		if (progressDialog!=null) {
			progressDialog.updateProgressBar(logsWritten);
			progressDialog.updateStatus("bytes "+bytesWritten+", logs "+logsWritten);
		}
	}
	
}

