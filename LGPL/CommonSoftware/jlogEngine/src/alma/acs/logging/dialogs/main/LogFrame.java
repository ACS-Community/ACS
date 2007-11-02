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
package alma.acs.logging.dialogs.main;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.WindowListener;
import java.io.File;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import com.cosylab.logging.LoggingClient;

/**
 * The window with all the  controls of the main GUI:
 * 
 * @author acaproni
 *
 */
public class LogFrame extends JFrame implements WindowListener { 
	
	private volatile boolean closing =false;
	
	private LoggingClient loggingClient;
	
	/**
	 * Constructor
	 * Creates the main window and setup the panel with the controls.
	 * 
	 * @param filterFile A file of filters to load
	 *                   It can be null if there are no filters to load
	 * @param logFile A file of logs to load
	 *                It can be null if there are no logs to load
	 */
	public LogFrame(File filterFile, String logFileName) {
		super();
		setName("LogFrame");
		
		initialize();
		// Move the window to the center of the screen 
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension windowSize = getSize();
        setLocation(
        		Math.max(0,(screenSize.width -windowSize.width)/2), 
        		Math.max(0,(screenSize.height-windowSize.height)/2));
        pack();
		setVisible(true);
		
		//	Load the filters (if any)
		if (filterFile!=null) {
			loggingClient.getLCModel1().getFilters().loadFilters(filterFile,true,null);
		}
		// If there is no file name in the command line then connect
		// the client to the CORBA notification channel
		if (logFileName==null) {
			loggingClient.connect();
		} else {
			loggingClient.getLCModel1().loadFromFile(logFileName);
		}
		
		// Get events from the main window
		this.addWindowListener(this);
		
	}
	
	/**
	 * Initialize the content of the frame
	 *
	 */
	private void initialize() {
		setTitle("LoggingClient");
		addWindowListener(this);
        
        loggingClient = new LoggingClient(this); // build the LoggingClient
        if (loggingClient==null) {
        	throw new NullPointerException("The logging client is null");
        }
        this.setRootPane(loggingClient);
        // Enable the exit menu
        loggingClient.hideExitMenu(false);
	}
	
	/**
	 * Starts the application.
	 * @param args an array of command-line arguments
	 */
	public static void main(java.lang.String[] args)
	{
		// First check if there are parameter in the command line
		
		// If it is null then the user specified a file name in the
		// command line
		String initLogFileName = null;
		// If it is null then the user specified a filter file name in the
		// command line
		String initFilterFileName = null;
		
		if (args.length>3) {
			// Wrong number of params
			printUsage("cmd line too long");
			System.exit(-1);
		} else if (args.length>0) {
			// Retrieve the params
			for (int t=0; t<args.length; t++) {
				if (args[t].compareTo("-f")==0 || args[t].compareTo("--filter")==0) {
					t++;
					if (t<args.length) {
						initFilterFileName=args[t];
					} else if (initFilterFileName!=null) {
						// A filter file was already defined
						printUsage("Two filter file names in cmd line");
						System.exit(-1);
					} else {
						// -f was the last param in the cmd
						printUsage("No filter file name after "+args[t-1]);
						System.exit(-1);
					}
				} else {
					if (initLogFileName==null) {
						initLogFileName=args[t];
					} else {
						// A log file was already found!
						printUsage("Two log file names in cmd line");
						System.exit(-1);
					}
				}
			}
		}
		
		File logFile = null;
		if (initLogFileName!=null) {
			// Check if the file in the cmd line is readable
			logFile = new File(initLogFileName);
			if (!logFile.canRead()) {
				initLogFileName=null;
				System.err.println(initLogFileName+" is unreadable!");
				System.exit(-1);
			}
		}
		
		File filterFile = null;
		if (initFilterFileName!=null) {
			filterFile = new File(initFilterFileName);
			if (!filterFile.canRead()) {
				System.err.println(initFilterFileName+" is unreadable!");
				System.exit(-1);
			}
		}
		
		try
		{
			// Create the frame
			class FrameLauncher extends Thread {
				File f;
				String name;
				public FrameLauncher(File fltFile, String initFileName) {
					f=fltFile;
					name=initFileName;
				}
				public void run() {
					new LogFrame(f,name);
				}
			}
			SwingUtilities.invokeLater(new FrameLauncher(filterFile,initLogFileName));
		}
		catch (Throwable exception)
		{
			System.err.println("Exception occurred in main() of LoggingFrame");
			exception.printStackTrace(System.out);
		}
	}
	
	/**
	 * Print the standard usage message if the parameters in the command
	 * line are wrong.
	 *
	 * @param errorMsg An optional error message to print
	 */
	private static void printUsage(String errorMsg) {
		if (errorMsg!=null) {
			System.out.println("Wrong parameters: "+errorMsg);
		}
		System.out.println("USAGE:");
		System.out.println("jlog [logFileName] [(-f|--filter) filterFileName]\n");
	}
	
	/**
	 * @see WindowListener
	 */
	public void windowActivated(java.awt.event.WindowEvent e) {}
	
	/**
	 * @see WindowListener
	 */
	public void windowOpened(java.awt.event.WindowEvent e) {}
	
	/**
	 * @see WindowListener
	 */
	public void windowClosed(java.awt.event.WindowEvent e) {
	    	 loggingClient=null;
	}
	
	/**
	 * @see WindowListener
	 */
	public void windowDeactivated(java.awt.event.WindowEvent e) {}
	
	/**
	 * @see WindowListener
	 */
	public void windowDeiconified(java.awt.event.WindowEvent e) {}
	
	/**
	 * @see WindowListener
	 */
	public void windowIconified(java.awt.event.WindowEvent e) {}

	/**
	 * @see WindowListener
	 */
	public void windowClosing(java.awt.event.WindowEvent e)	{
		if (closing) {
			return;
		}
		closing=true;
		loggingClient.close(true);
		setVisible(false);
		dispose();
	}
}
