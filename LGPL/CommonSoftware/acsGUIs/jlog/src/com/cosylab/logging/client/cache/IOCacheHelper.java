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
package com.cosylab.logging.client.cache;

import java.util.Vector;
import java.util.LinkedList;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.RandomAccessFile;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.File;

import com.cosylab.logging.LogTypeHelper;
import com.cosylab.logging.client.GroupedList;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.ACS.ACSLogParser;
import com.cosylab.logging.engine.LogEntry;
import com.cosylab.logging.LoggingClient;

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

import java.awt.event.ActionListener;
import java.awt.event.WindowListener;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;


/**
 * This class is intended to support the cache with commons IO
 * operations aiming to keep the cache class shorter and more
 * readable.
 * This class instantiate a thread so to let the JVM able
 * to destroy objects of this class the thread must be terminated:
 * the close method terminates the thread.
 * 
 * NOTE: The cache on file could be accessed in parallel
 *       by other thread reading logs (for example to refresh
 *       the logs already in the visible list)
 * 
 * @author acaproni
 *
 */
public class IOCacheHelper extends Thread  {
	// A reference to the LogFileCache this object belogs to
	LogFileCache owner;
	
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
		
		private boolean abortRequested=false;
		
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
		 * 
		 * @param progressBar If true a progress bar is added
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
			
			// Add the abort button
			JPanel buttonPanel = new JPanel(new FlowLayout());
			JButton abortBtn = new JButton("Abort");
			abortBtn.addActionListener(this);
			buttonPanel.add(abortBtn);
			add(buttonPanel,BorderLayout.SOUTH);
			
			pack();
			toFront();
			
			Toolkit tk = Toolkit.getDefaultToolkit();
			Dimension screenDim = tk.getScreenSize();
			setLocation(screenDim.width/2-this.getWidth()/2,screenDim.height/2-this.getHeight()/2);
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
			// There is only one possible action
			abortRequested=true;
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
	private class LogFileCacheAction {
		
		public final static int LOAD_ACTION=0;
		public final static int SAVE_ACTION=1;
		public final static int TERMINATE_THREAD_ACTION=2;
		
		// The type of the action to perform
		private int type;
		
		// The buffered reader to read the logs
		private BufferedReader inFile;
		
		// The vector of the filters (knowing the filters
		// it is possible to add immediately each log in the list
		// of the visible logs 
		private FiltersVector filters;
		
		// The list of the visible logs
		private GroupedList visibleLogs;
		
		// The cache on disk
		private RandomAccessFile fileCache;
		
		// The index of positions of logs in the cache
		private Vector<Long> index;
		
		// The buffered writer to writ the logs to save into
		private BufferedWriter outFile;
		
		// The cache with all the logs
		private LogFileCache logsCache; 
		
		/**
		 * Build an action for asynchronous load operations
		 * 
		 * @param type The type of action to perform
		 * @param inFile The BuffredReader to read logs from
		 * @param cache The cache of logs on disk
		 * @param index The vector of position of logs in the cache
		 * @param filters The active filters
		 * @param visibleLogs The vector of the visible logs
		 */
		public LogFileCacheAction(BufferedReader inFile, RandomAccessFile cache, Vector<Long> index, FiltersVector filters,GroupedList visibleLogs) {
			this.inFile=inFile;
			this.fileCache=cache;
			this.index=index;
			this.filters=filters;
			this.visibleLogs=visibleLogs;
			this.type=LOAD_ACTION;
		}
		
		/**
		 * Build an action for asynchronous save operations
		 * 
		 * @param outF The buffered file to save the logs into
		 */
		public LogFileCacheAction(BufferedWriter outF, LogFileCache logs) {
			this.type=SAVE_ACTION;
			this.logsCache=logs;
			this.outFile=outF;
		}
		
		/** 
		 * This constructor builds an action to terminate the thred
		 * The class will not be able to perform further requests
		 * and it will throw an IllegalStateException
		 *
		 */
		public LogFileCacheAction() {
			this.type=LogFileCacheAction.TERMINATE_THREAD_ACTION;
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
		 * @return The cache on disk
		 */
		public RandomAccessFile getFileCache() {
			return fileCache;
		}
		
		/**
		 * Getter method
		 *  
		 * @return The cache of logs
		 */
		public LogFileCache getLogsCache() {
			return logsCache;
		}
		
		/**
		 * Getter method 
		 * @return The index
		 */
		public Vector<Long>getIndex() {
			return index;
		}
		
		/**
		 * Getter method 
		 * @return The active filters
		 */
		public FiltersVector getFilters() {
			return filters;
		}
		
		/**
		 * Getter method 
		 * @return The list of the visible logs
		 */
		public GroupedList getVisibleLogs() {
			return visibleLogs;
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
	public IOCacheHelper(LogFileCache cache) {
		super();
		owner=cache;
		// Try to speed up (less responsive but seems good enough)
		this.setPriority(Thread.MAX_PRIORITY);
		start();
	}
	
	// A queue of actions to perform in a separate thread
	LinkedList<LogFileCacheAction> actions = new LinkedList<LogFileCacheAction>();
	
	ProgressDialog progressDialog;
	
	/**
	 * Load the logs from the given file in the Cache appending their
	 * starting position in the index vector.
	 * The logs are appended at the end of the cache as weel as the new
	 * positions are appended at the end of the index vector.
	 * The load is performed in the thread as it might be very slow
	 * 
	 * @param br The the file to read
	 * @param cache The file cache where the logs are appended
	 * @param index The index of the positions of the logs in the cache
	 * @param filters The filters to check before inserting the log in the list
	 *                of the visible logs
	 * @param visibleLogs The list of the visible logs
	 */
	private void loadLogsFromDisk(BufferedReader br, 
			RandomAccessFile cache, 
			Vector<Long> index,
			FiltersVector filters, 
			GroupedList visibleLogs) {
		
		if (
				br==null || 
				cache==null || 
				index==null || 
				filters==null || 
				visibleLogs==null) {
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
		
		boolean lookForOpenTag =true;
		
		try {
		
			while ((chRead=br.read())!=-1) {
				bytesRead++;
				buffer.append((char)chRead);
				if (lookForOpenTag) {
					tag = buffer.getOpeningTag();
					if (tag.length()>0) {
						//System.out.println("TAG: "+tag+" (buffer ["+buffer.toString()+")");
						lookForOpenTag=false;
						buffer.trim(tag);
					}
				} else {
					if (buffer.hasClosingTag(tag)) {
						//System.out.println("\tClosing tag found (buffer ["+buffer.toString()+")");
						lookForOpenTag=true;
						synchronized(index) {
							index.add(cache.length());
						}
						synchronized(cache) {
							cache.seek(cache.length());
							cache.writeBytes(buffer.toString());
						}
						addToVisibleLog(buffer.toString(),filters,visibleLogs,index.size()-1);
						buffer.clear();
						if (progressDialog!=null) {
							progressDialog.updateStatus(""+index.size()+" logs loaded");
							progressDialog.updateProgressBar(bytesRead);
							// Check if the user pressed the abort button
							if (progressDialog.wantsToAbort()) {
								break;
							}
						}
					}
				}
			}
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
		owner.IOOperationTerminated();
	}
	
	/**
	 * Load the logs from the given file in the Cache appending their
	 * starting position in the index vector.
	 * The logs are appended at the end of the cache as weel as the new
	 * positions are appended at the end of the index vector.
	 * The load is performed in the thread as it might be very slow
	 * 
	 * @param fileName The name of the file to read
	 * @param cache The file cache where the logs are appended
	 * @param index The index of the positions of the logs in the cache
	 * @param filters The filters to check before inserting the log in the list
	 *                of the visible logs
	 * @param visibleLogs The list of the visible logs
	 * @param showProgress If true a progress bar is shown
	 * @throws FileNotFoundException 
	 */
	public void loadLogs (
			String fileName, 
			RandomAccessFile cache, 
			Vector<Long> index, 
			FiltersVector filters, 
			GroupedList visibleLogs,
			boolean showProgress) throws FileNotFoundException  {
		// Check if the thread is alive
		if (!this.isAlive()) {
			throw new IllegalStateException("Unable to execute asynchronous operation");
		}
		// Open the file for reading
		BufferedReader br=null;
		br = new BufferedReader(new FileReader(fileName));
		
		File f = new File(fileName);
		if (showProgress) {
			progressDialog = new ProgressDialog("Loading logs...",0,(int)f.length());
		} else {
			progressDialog=null;
		}
		
		// Add an action in the que
		LogFileCacheAction action = new LogFileCacheAction(br,cache,index,filters,visibleLogs);
		synchronized(actions) {
			actions.add(action);
		}
		
		synchronized(this) {
			// Wake up the thread
			notifyAll();
		}
	}
	
	/**
	 * Load the logs from a buffered reader
	 * 
	 * @param reader The buffered reader
	 * @param cache The file cache where the logs are appended
	 * @param index The index of the positions of the logs in the cache
	 * @param filters The filters to check before inserting the log in the list
	 *                of the visible logs
	 * @param visibleLogs The list of the visible logs
	 * @param showProgress If true a progress bar is shown
	 * 
	 * @see loadLogs
	 */
	public void loadLogs (
			BufferedReader reader, 
			RandomAccessFile cache, 
			Vector<Long> index, 
			FiltersVector filters, 
			GroupedList visibleLogs,
			boolean showProgress) {
		// Check if the thread is alive
		if (!this.isAlive()) {
			throw new IllegalStateException("Unable to execute asynchronous operation");
		}
		// No progress bar because we don't know the length of this kind of file
		if (showProgress) {
			progressDialog = new ProgressDialog("Loading logs...");
		} else {
			progressDialog=null;
		}
		
		// Add an action in the que
		LogFileCacheAction action = new LogFileCacheAction(reader,cache,index,filters,visibleLogs);
		synchronized(actions) {
			actions.add(action);
		}
		
		synchronized(this) {
			// Wake up the thread
				notifyAll();
		}
	}
	
	/**
	 * Add the log to the list of the visible logs 
	 * 
	 * @param logStr The string representation of the log
	 * @param filters The filter to check the log against
	 * @param index The position of the log in the cache
	 * @param visibles The list of the visible logs
	 */
	private void addToVisibleLog(
			String logStr, 
			FiltersVector filters, 
			GroupedList visibles, 
			int index) {
		LogEntry log=null;
		try {
			ACSLogParser parser = new ACSLogParser();
			//System.out.println("Parsing "+logStr);
			log=parser.parse(logStr);
		} catch (Exception e) {
			System.err.println("Exception parsing a log: "+e.getMessage());
			System.out.println("Log Str: ["+logStr+"]");
			JOptionPane.showMessageDialog(null, formatErrorMsg(e.getMessage(),logStr),"Error parsing a log!",JOptionPane.ERROR_MESSAGE);
			return;
		}
		int discardLevel = LoggingClient.getInstance().getDiscardLevel();
		if (discardLevel!=0) {
			int logLevel = (Integer)log.getField(LogEntry.FIELD_ENTRYTYPE);
			if (logLevel<discardLevel) {
				return;
			}
		}
		if (filters.applyFilters(log)) {
			visibles.add(index);
		}
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
			LogFileCache cache,
			boolean showProgress) throws IOException {
		// Check if the thread is alive
		if (!this.isAlive()) {
			throw new IllegalStateException("Unable to execute asynchronous operation");
		}
		// Open the output file
		FileWriter fw = new FileWriter(fileName,false);
		BufferedWriter outBW = new BufferedWriter(fw);
		
		LogFileCacheAction action = new LogFileCacheAction(outBW, cache);
		
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
			LogFileCacheAction terminateAction = new LogFileCacheAction();
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
		LogFileCacheAction action;
		while (true) {
			if (!actions.isEmpty()) {
				synchronized(actions) {
					action = actions.removeFirst();
				}
				switch (action.getActionType()) {
					case LogFileCacheAction.LOAD_ACTION: {
						loadLogsFromDisk(
							action.getInputFile(),
							action.getFileCache(),
							action.getIndex(),
							action.getFilters(),
							action.getVisibleLogs());
					}
					case LogFileCacheAction.TERMINATE_THREAD_ACTION: {
						// Terminate the thrread
						return;
					}
					case LogFileCacheAction.SAVE_ACTION: {
						saveLogsOnDisk(action.getOutputFile(),action.getLogsCache());
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
	private void saveLogsOnDisk(BufferedWriter outBW, LogFileCache logs) {
		if (outBW==null || logs==null) {
			throw new IllegalArgumentException("Null parameter received!");
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
		for (int count =0; count<logs.getSize(); count++) {
			LogEntry log = logs.getLog(count);
			try {
				outBW.write((log.toXMLString()+"\n").toCharArray());
			} catch (IOException ioe) {
				System.err.println("Exception while saving logs: "+ioe.getMessage());
				ioe.printStackTrace(System.err);
				JOptionPane.showMessageDialog(null, "Exception saving "+ioe.getMessage(),"Error saving",JOptionPane.ERROR_MESSAGE);
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
		owner.IOOperationTerminated();
	}
	
}
