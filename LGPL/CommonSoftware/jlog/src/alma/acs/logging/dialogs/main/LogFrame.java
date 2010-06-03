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
import java.util.logging.Logger;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import alma.acs.logging.ClientLogManager;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.ACS.ACSLogConnectionListener;
import com.cosylab.logging.engine.audience.Audience.AudienceInfo;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * The window with all the  controls of the main GUI:
 * 
 * @author acaproni
 *
 */
public class LogFrame extends JFrame implements WindowListener, ACSLogConnectionListener {
	
	/**
	 * A boolean to signal that the application is closing
	 */
	private volatile boolean closing =false;
	
	/**
	 * The GUI showed into the frame
	 */
	private LoggingClient loggingClient;
	
	/**
	 * The logger
	 */
	private Logger logger;
	
	/**
	 * The Shutdown hook
	 */
	private ShutdownHook shutdownHook;
	
	/**
	 * Shown in the title bar while working online
	 */
	private static final String online = "LoggingClient - Online";
	
	/**
	 * Shown in the title bar while working offline
	 */
	private static final String offline = "LoggingClient - Offline";
	
	/**
	 * Shown in the title bar while trying to connect to the logging NC
	 */
	private static final String connecting = "LoggingClient - Connecting...";
	
	/**
	 * Constructor: creates the main window and setup the panel with the controls.
	 * 
	 * @param filterFile A file of filters to load
	 *                   It can be <code>null</code> if there are no filters to load
	 * @param engineFilterFile A file of filters to set in the engine
	 * 					It can be <code>null</code> if there are no filters to load in the engine
	 * @param logFile A file of logs to load
	 *                It can be <code>null</code> if there are no logs to load
	 * @param discardLevel The discard level to set in the engine; 
	 *                     If <code>null</code> the level in the engine is not set and the default is used
	 * @param doNotConnect If <code>true</code> do not try to connect to ACS (i.e. start offline)
	 * @param unlimited If <code>true</code> the number of logs in memory is unlimited, 
	 *                  otherwise the default is used
	 * @param audienceInfo The audience.
	 * 					It can be <code>null</code> if there is no audience to set at startup
	 */
	public LogFrame(
			File filterFile,
			File engineFilterFile, 
			String logFileName, 
			LogTypeHelper discardLevel, 
			boolean doNotConnect, 
			boolean unlimited,
			AudienceInfo audienceInfo) {
		super();
		setName(offline);
		
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("Logging client GUI",true);
		initShutdownHook(); 
		
		initialize(discardLevel,unlimited,audienceInfo);
		// Move the window to the center of the screen 
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension windowSize = getSize();
        setLocation(
        		Math.max(0,(screenSize.width -windowSize.width)/2), 
        		Math.max(0,(screenSize.height-windowSize.height)/2));
        pack();
		setVisible(true);
		
		//	Load the filters in the table (if any)
		if (filterFile!=null) {
			FiltersVector filters = new FiltersVector();
			try {
				filters.loadFilters(filterFile, true, null);
				loggingClient.getLogEntryTable().setFilters(filters, false);
			} catch (Throwable t) {
				JOptionPane.showMessageDialog(null, "Error: "+t.getMessage(), "Error loading filters", JOptionPane.ERROR_MESSAGE);
			}
		}
		
		// Set the filters in the engine
		if (engineFilterFile!=null) {
			FiltersVector filters = new FiltersVector();
			try {
				filters.loadFilters(engineFilterFile, true, null);
				loggingClient.getEngine().setFilters(filters, false);
			} catch (Throwable t) {
				JOptionPane.showMessageDialog(null, "Error: "+t.getMessage(), "Error loading engine filters", JOptionPane.ERROR_MESSAGE);
			}
		}
		
		// If there is no file name in the command line then connect
		// the client to the CORBA notification channel
		if (logFileName==null) {
			if (!doNotConnect) {
				loggingClient.connect();
			}
		} else {
			loggingClient.getLCModel1().loadFromFile(logFileName);
		}
		
		// Get events from the main window
		this.addWindowListener(this);
		
	}
	
	/**
	 * Initialize the content of the frame
	 *
	 * @param discardLevel The discard level
	 * @param unlimited If <code>true</code> the number of logs in memory is unlimited, 
	 *                  otherwise the default is used
	 * @param aInfo The audience
	 */
	private void initialize(
			LogTypeHelper discardLevel, 
			boolean unlimited,
			AudienceInfo aInfo) {
		setTitle("LoggingClient");
		addWindowListener(this);
		
		// Set the icon
		ImageIcon image = new ImageIcon(LogFrame.class.getResource("/alma.png"));
		setIconImage(image.getImage());
        
		// build the LoggingClient
        loggingClient = new LoggingClient(
        		this, 
        		LoggingClient.DEFAULT_LOGLEVEL, 
        		discardLevel, 
        		unlimited,
        		aInfo); 
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
		// Parse the command line
		CommandLineParser parser=null;
		try {
			parser = new CommandLineParser(args);
		} catch (Throwable t) {
			CommandLineParser.printUsage(t.getMessage());
			System.exit(-1);
		}
		if (parser.getHelp()) {
			CommandLineParser.printUsage(null);
			return;
		}
		/** 
		 * If it is not <code>null</code> then the user specified a file name in the
		 * command line
		 */
		String initLogFileName = parser.getFileToLoad();
		
		/** 
		 * If it is not <code>null</code> then the user specified a filter file name in the
		 * command line
		 */
		String initFilterFileName = parser.getFilterFileName();
		
		/** 
		 * If it is not <code>null</code> then the user specified an engine  filter 
		 * file name in the command line
		 */
		String initEngineFilterFileName = parser.getEngineFilterFileName();
		
		/**
		 *  <code>true</code> if the user do not want the logging client tries to connect to ACS 
		 *  at startup
		 */
		boolean doNotConnect=parser.isDoNotConnect();
		
		/**
		 * <code>true</code> if the user does not want to limit the number of logs to  keep in memory
		 */
		boolean unlimited=parser.isUnlimited();
		
		/**
		 * The audience set in the command line.
		 * <P>
		 * <code>null</code> if the user did not set the audience in the
		 * command line. 
		 */
		AudienceInfo audienceInfo = parser.getAudience();
		
		/**
		 * The initial discard level.
		 * If it not set in the command line, the logging client starts
		 * with the default discard level
		 */
		LogTypeHelper initialDiscardLevel=parser.getDiscardLevel();
		
		File logFile = null;
		if (initLogFileName!=null) {
			// Check if the file in the cmd line is readable
			logFile = new File(initLogFileName);
			if (!logFile.exists()) {
				System.err.println("log file "+initLogFileName+" does not exist!");
				initLogFileName=null;
				System.exit(-1);
			}
			if (!logFile.canRead()) {
				System.err.println("log file "+initLogFileName+" is unreadable!");
				initLogFileName=null;
				System.exit(-1);
			}
		}
		
		File filterFile = null;
		if (initFilterFileName!=null) {
			filterFile = new File(initFilterFileName);
			if (!filterFile.canRead()) {
				System.err.println("Filter file "+initFilterFileName+" is unreadable!");
				System.exit(-1);
			}
		}
		
		File engineFilterFile = null;
		if (initEngineFilterFileName!=null) {
			engineFilterFile = new File(initEngineFilterFileName);
			if (!engineFilterFile.canRead()) {
				System.err.println("Filter file "+initFilterFileName+" is unreadable!");
				System.exit(-1);
			}
		}
		
		try
		{
			// Create the frame
			class FrameLauncher extends Thread {
				private final File f;
				private final File ef;
				private final String name;
				private final boolean offline;
				private final LogTypeHelper discard;
				private final boolean noLimit;
				private final AudienceInfo aInfo;
				public FrameLauncher(
						File fltFile, 
						File engfltFile, 
						String initFileName, 
						LogTypeHelper initDiscard, 
						boolean noACS, 
						boolean unlimit,
						AudienceInfo info) {
					f=fltFile;
					ef=engfltFile;
					name=initFileName;
					discard=initDiscard;
					offline=noACS;
					noLimit=unlimit;
					aInfo=info;
				}
				public void run() {
					new LogFrame(f,ef,name,discard,offline,noLimit,aInfo);
				}
			}
			SwingUtilities.invokeLater(new FrameLauncher(
					filterFile,
					engineFilterFile,
					initLogFileName,
					initialDiscardLevel,
					doNotConnect,unlimited,audienceInfo));
		} catch (Throwable exception) {
			System.err.println("Exception occurred in main() of LoggingFrame");
			exception.printStackTrace(System.err);
		}
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
		closeApplication();
	}
	
	/**
	 * Close the application
	 */
	public void closeApplication() {
		setVisible(false);
		closing=true;
		try {
			if (loggingClient!=null) {
				// loggingClient can be null if an exception happens in the constructor
				loggingClient.stop();
			}
		} catch (Throwable t) {
			System.err.println("Exception caught while closing the logging client: "+t.getMessage());
			t.printStackTrace(System.err);
		}
		
		dispose();
	}
	
	/**
	 * Init the shutdown hook that intercept CTRL+C events
	 * and cleanly terminates the application
	 */
	private void initShutdownHook() {
		shutdownHook = new ShutdownHook(logger,"Logging client GUI", this);
		Runtime.getRuntime().addShutdownHook(shutdownHook);
	}
	
	/**
	 * Set the title of the frame online/offline
	 * 
	 * @param mode <code>true</code> if running online
	 */
	public void voidSetWorkingMode(boolean mode) {
		if (mode) {
			setTitle(online);
		} else {
			setTitle(offline);
		}
	}

	@Override
	public void acsLogConnConnecting() {
		setTitle(connecting);
	}

	@Override
	public void acsLogConnDisconnected() {
		setTitle(offline);
	}

	@Override
	public void acsLogConnEstablished() {
		setTitle(online);
	}

	@Override
	public void acsLogConnLost() {
		setTitle(offline);
	}

	@Override
	public void acsLogConnSuspended() {}

	@Override
	public void acsLogsDelay() {}

	@Override
	public void reportStatus(String status) {}
}
