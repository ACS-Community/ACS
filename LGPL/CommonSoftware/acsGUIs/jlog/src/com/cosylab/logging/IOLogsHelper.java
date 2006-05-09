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

import java.util.LinkedList;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

import com.cosylab.logging.client.cache.LogCache;
import com.cosylab.logging.client.cache.LogCacheException;

import com.cosylab.logging.engine.ACS.ACSLogParser;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.stats.ResourceChecker;
import com.cosylab.logging.LoggingClient;

import alma.acs.util.StopWatch;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.BorderLayout;
import java.awt.Toolkit;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JProgressBar;
import javax.swing.JOptionPane;
import javax.swing.JCheckBox;

import java.awt.event.ActionListener;
import java.awt.event.WindowListener;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;


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
public class IOLogsHelper extends Thread  {
	
	// Monitor if an async IO operation is in progress
	private boolean IOOperationInProgress =false;
	private ACSLogParser parser;
	
	/**
	 * The dialog to show the progress of time consuming 
	 * operations (load and save for example)
	 * 
	 * Throw this dialog is also possible to abort the operation
	 * by pressing a button. The dialog does not comunicate the abort
	 * request but just store it into a variable that the long operation
	 * should poll from time to time 
	 * 
	 * @author acaproni
	 */
	private class ProgressDialog extends JDialog implements ActionListener, WindowListener {
		// The label with the status
		private JLabel statusLbl = new JLabel();
		// The progress bar
		private JProgressBar progressBar=new JProgressBar(JProgressBar.HORIZONTAL);
		
		// True if the user requested to abort the IO
		private boolean abortRequested=false;
		
		// The checkbox to perform a fast IO
		// It is done by hiding the table of logs
		private JCheckBox fastCB = null;
		
		// The button to stop IO
		private JButton abortBtn;
		
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
			addWindowListener(this);
			
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
			boolean checked=!LoggingClient.getInstance().getLogEntryTable().isVisible();
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
			
			pack();
			toFront();
			
			Toolkit tk = Toolkit.getDefaultToolkit();
			Dimension screenDim = tk.getScreenSize();
			setLocation(screenDim.width/2-this.getWidth()/2,screenDim.height/2-this.getHeight()/2);
			
			setVisible(true);
		}
		
		/**
		 * Replace the status lable with the new one
		 * It packs the window to ensure that the string is visible
		 * 
		 * @param status The new status string
		 */
		public void updateStatus(String status) {
			int oldLength = statusLbl.getText().length();
			statusLbl.setText(status);
			if (status.length()>oldLength) {
				pack();
			}
		}
		
		/**
		 * Update the position of the status bar
		 * 
		 * @param current The newposition of the status bar
		 */
		public void updateProgressBar(int current) {
			if (!progressBar.isIndeterminate()) {
				int percent = (progressBar.getMaximum()-progressBar.getMinimum())/100;
				if (percent!=0) {
					progressBar.setString(""+current/percent+"%");
				}
				progressBar.setStringPainted(true);
				progressBar.setValue(current);
			}
		}
		
		/**
		 * 
		 * @return true if the user request to abort
		 */
		public boolean wantsToAbort() {
			return abortRequested;
		}
		
		/**
		 * @see java.awt.event.ActionListener
		 */
		public void actionPerformed(ActionEvent evt) {
			if (evt.getSource()==fastCB) {
				LoggingClient.getInstance().getLogEntryTable().setVisible(
						!fastCB.isSelected());
			} else if (evt.getSource()==abortBtn) {
				abortRequested=true;
			}
		}
		
		/**
		 * @see java.awt.event.WindowListener
		 */
		public void windowClosed(WindowEvent e) {
			abortRequested=true;
		}
		
		/**
		 * @see java.awt.event.WindowListener
		 */
		public void windowClosing(WindowEvent e) {
			abortRequested=true;
		}
		
		/**
		 * @see java.awt.event.WindowListener
		 */
		public void windowActivated(WindowEvent e) {}
		
		/**
		 * @see java.awt.event.WindowListener
		 */
		public void windowDeactivated(WindowEvent e) {}
		
		/**
		 * @see java.awt.event.WindowListener
		 */
		public void windowDeiconified(WindowEvent e) {}
		
		/**
		 * @see java.awt.event.WindowListener
		 */
		public void windowIconified(WindowEvent e) {}
		
		/**
		 * @see java.awt.event.WindowListener
		 */
		public void windowOpened(WindowEvent e) {}
		
	}
	
	/**
	 * An action for operations to be perfomed asynchronously
	 * 
	 * There is one specific conbstructor for each possible type
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
		private int type;
		
		// The buffered reader to read the logs
		private BufferedReader inFile;
		
		// The buffered writer to write the logs to save into
		private BufferedWriter outFile;
		
		// The cache with all the logs
		private LogCache logsCache; 
		
		/**
		 * The listener i.e. the callback to push the loaded logs in 
		 */
		private ACSRemoteLogListener logListener;
		
		/**
		 * Build an action for asynchronous load operations
		 * 
		 * @param inFile The BuffredReader to read logs from
		 * @param theEngine The logging client engine
		 * @param theCache The cache of logs (used to shown values in the 
		 *                 progress dialog)
		 */
		public IOAction(BufferedReader inFile, ACSRemoteLogListener listener, LogCache theCache) {
			this.logsCache=theCache;
			this.inFile=inFile;
			this.logListener=listener;
			this.type=IOAction.LOAD_ACTION;
		}
		
		/**
		 * Build an action for asynchronous save operations
		 * 
		 * @param outF The buffered file to save the logs into
		 */
		public IOAction(BufferedWriter outF, LogCache theCache) {
			this.type=IOAction.SAVE_ACTION;
			this.logsCache = theCache;
			this.outFile=outF;
		}
		
		/** 
		 * This constructor builds an action to terminate the thred
		 * The class will not be able to perform further requests
		 * and it will throw an IllegalStateException
		 *
		 */
		public IOAction() {
			this.type=IOAction.TERMINATE_THREAD_ACTION;
		}
		
		/**
		 * Getter method 
		 * @return The input file
		 */
		public BufferedReader getInputFile() {
			return inFile;
		}
		
		/**
		 * Getter method 
		 * @return The output file
		 */
		public BufferedWriter getOutputFile() {
			return outFile;
		}
		
		/**
		 * Getter method
		 *  
		 * @return The cache of logs
		 */
		public LogCache getLogsCache() {
			return logsCache;
		}
		
		/**
		 * Getter method
		 *  
		 * @return The listener to push loaded logs in
		 */
		public ACSRemoteLogListener getLogListener() {
			return logListener;
		}
		
		/**
		 * Getter method 
		 * @return The type of the action requested
		 */
		public int getActionType() {
			return type;
		}
	}
	
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
	private class LogStringBuffer {
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
	 * Build an IOCacheHelper object
	 *
	 */
	public IOLogsHelper() {
		super();
		// Try to speed up (less responsive but seems good enough)
		this.setPriority(Thread.MAX_PRIORITY);
		start();
	}
	
	// A queue of actions to perform in a separate thread
	private LinkedList<IOAction> actions = new LinkedList<IOAction>();
	
	private ProgressDialog progressDialog;
	
	/**
	 * Load the logs from the given file in the Cache appending their
	 * starting position in the index vector.
	 * The logs are appended at the end of the cache as well as the new
	 * positions are appended at the end of the index vector.
	 * The load is performed in a thread as it might be very slow
	 * 
	 * @param br The the file to read
	 * @param logListener The callback for each new log read from the IO
	 * @param cache The cache of logs
	 */
	private void loadLogs(BufferedReader br,ACSRemoteLogListener logListener, LogCache cache) {
		if (br==null || logListener==null) {
			throw new IllegalArgumentException("Null parameter received!");
		}
		
		// The last tag found
		String tag=null;
		
		// The "clever" buffer
		LogStringBuffer buffer = new LogStringBuffer();
		
		// Read one char per iteration
		int chRead;
		
		// Count the bytes read for updating the progressbar
		int bytesRead=0;
		
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
		
		try {
			StopWatch stopWatch = new StopWatch();
		
			while (true) {
				// Read ablock from the file if the buffer is empty
				if (bytesInBuffer==0) {
					bytesInBuffer = br.read(buf,0,size);
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
						injectLog(buffer.toString(),logListener);
						buffer.clear();
						logRecordsRead++;
//							if (logRecordsRead % 100 == 0) {
//								System.out.println("Number of log records read: " + logRecordsRead);
//							}
						if (progressDialog!=null) {
							if (cache!=null) {
								progressDialog.updateStatus(""+cache.getSize()+" logs loaded");
							} 
							progressDialog.updateProgressBar(bytesRead);
							// Check if the user pressed the abort button
							if (progressDialog.wantsToAbort()) {
								break;
							}
						}
					}
				}
			}
			System.out.println("XML log record import finished with " + logRecordsRead + " records in " + 
						stopWatch.getLapTimeMillis()/1000 + " seconds.");
			System.out.println(ResourceChecker.getMemoryStatusMessage());
		} catch (IOException ioe) {
			System.err.println("Exception loading the logs: "+ioe.getMessage());
			ioe.printStackTrace(System.err);
			JOptionPane.showMessageDialog(null, "Exception loading "+ioe.getMessage(),"Error loading",JOptionPane.ERROR_MESSAGE);
			return ;
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
	 * Load the logs from a buffered reader
	 * 
	 * @param reader The buffered reader containing the logs
	 * @param listener The listener to add logs in
	 * @param cache The cache To show info in the dialog (can be null)
	 * @param showProgress If true a progress bar is shown
	 * @param progressRange An integer to make the progress bar showing the real 
	 *                      state of the operation (determinate)
	 *                      If it is <=0 then the progress bar is indeterminate
	 * 
	 * @see loadLogs
	 */
	public void loadLogs (
			BufferedReader reader,
			ACSRemoteLogListener listener,
			LogCache cache,
			boolean showProgress,
			int progressRange) {
		// Check if the thread is alive
		if (!this.isAlive()) {
			throw new IllegalStateException("Unable to execute asynchronous operation");
		}
		// Check the params
		if (listener==null || reader==null) {
			throw new IllegalArgumentException("Engine and Reader can't be null");
		}
		
		IOOperationInProgress=true;
		
		// Hide the table with the logs
		// It reduces the access to the cache to redraw the logs 
		// speeding up the loading
		LoggingClient.getInstance().getLogEntryTable().setVisible(false);
		
		// No progress bar because we don't know the length of this kind of file
		if (showProgress) {
			if (progressRange<=0) {
				progressDialog = new ProgressDialog("Loading logs...");
			} else {
				progressDialog = new ProgressDialog("Loading logs...",0,progressRange);
			}
		} else {
			progressDialog=null;
		}
		
		// Add an action in the queue
		IOAction action = new IOAction(reader,listener,cache);
		synchronized(actions) {
			actions.add(action);
		}
		
		synchronized(this) {
			// Wake up the thread
				notifyAll();
		}
	}
	
	/**
	 * Inject the log into the engine 
	 * 
	 * @param logStr The string representation of the log
	 * @param logListener The listener i.e. the callback for each new log to add
	 */
	private void injectLog(String logStr, ACSRemoteLogListener logListener) {
		ILogEntry log=null;
		try {
			if (parser == null) {
				parser = new ACSLogParser();
			}
			//System.out.println("Parsing "+logStr);
			log=new LogEntry(parser.parse(logStr));
		} catch (Exception e) {
			System.err.println("Exception parsing a log: "+e.getMessage());
			System.out.println("Log Str: ["+logStr+"]");
			JOptionPane.showMessageDialog(null, formatErrorMsg(e.getMessage(),logStr),"Error parsing a log!",JOptionPane.ERROR_MESSAGE);
			return;
		}
		logListener.logEntryReceived(log);
	}
	
	/**
	 * Return a string formatted for JOptionPane making a word wrap
	 * 
	 * @param error The error i.e. the exception
	 * @param msg The message to show
	 * @return A formatted string
	 */
	private String formatErrorMsg(String error, String msg) {
		StringBuffer sb = new StringBuffer("Exception: "+error+"\nParsing the Log:");
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
		
		if (showProgress) {
			progressDialog = new ProgressDialog("Saving logs...",0,cache.getSize());
		} else {
			progressDialog=null;
		}
		
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
		// Check if the thread is alive
		if (!this.isAlive()) {
			IOAction terminateAction = new IOAction();
			synchronized(actions) {
				actions.add(terminateAction);
			}
			synchronized(this) {
				// Wake up the thread
					notifyAll();
			}
		}
	}
	
	/**
	 * The thread for asynchrnous operation.
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
				switch (action.getActionType()) {
					case IOAction.LOAD_ACTION: {
						loadLogs(action.getInputFile(),action.getLogListener(),action.getLogsCache());
						break;
					}
					case IOAction.TERMINATE_THREAD_ACTION: {
						// Terminate the thrread
						return;
					}
					case IOAction.SAVE_ACTION: {
						saveLogs(action.getOutputFile(),action.getLogsCache());
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
	 * @param logs The cahce with all the logs
	 */
	private void saveLogs(BufferedWriter outBW, LogCache cache) {
		if (outBW==null || cache==null) {
			throw new IllegalArgumentException("BufferedWriter and LogCache can't be null");
		}
		try {
			outBW.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<Log>\n<Header Name=\"NameForXmlDocument\" Type=\"LOGFILE\" />\n");
		} catch (Exception e) {
			// I ignore this exception because it is not very important
			// to have the header in the file (however another exception 
			// of the same type will most likely happen in the for loop below!)
			System.err.println("Warning exception ignored "+e.getMessage());
			e.printStackTrace(System.err);
		}
		for (int count =0; count<cache.getSize(); count++) {
			try {
				ILogEntry log = cache.getLog(count);
				outBW.write((log.toXMLString()+"\n").toCharArray());
			} catch (IOException ioe) {
				System.err.println("Exception while saving logs: "+ioe.getMessage());
				ioe.printStackTrace(System.err);
				JOptionPane.showMessageDialog(null, "Exception saving "+ioe.getMessage(),"Error saving",JOptionPane.ERROR_MESSAGE);
				break;
			} catch (LogCacheException lce) {
				System.err.println("Exception getting a log from the cache: "+lce.getMessage());
				lce.printStackTrace(System.err);
				JOptionPane.showMessageDialog(null, "Exception saving "+lce.getMessage(),"Error saving",JOptionPane.ERROR_MESSAGE);
				break;
			}
			if (progressDialog!=null) {
				progressDialog.updateStatus(""+count+" logs saved");
				progressDialog.updateProgressBar(count);
				// Check if the user pressed the abort button
				if (progressDialog.wantsToAbort()) {
					break;
				}
			}
		}
		try {
			outBW.write("</Log>");
			outBW.flush();
			outBW.close();
		} catch (Exception e) { 
			// Again.. not important to have this in the file}
			System.err.println("Warning exception ignored "+e.getMessage());
			e.printStackTrace(System.err);
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
		LoggingClient.getInstance().getLogEntryTable().setVisible(true);
		IOOperationInProgress=false;
	}
	
}
