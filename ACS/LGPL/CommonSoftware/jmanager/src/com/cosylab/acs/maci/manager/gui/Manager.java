/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.manager.gui;

import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import javax.swing.JFrame;
import javax.swing.JProgressBar;
import javax.swing.Timer;

import alma.acs.logging.formatters.ConsoleLogFormatter;

import com.cosylab.acs.maci.manager.ManagerShutdown;
import com.cosylab.acs.maci.manager.app.ManagerEngine;

/**
 * Manager activation application and GUI implementation.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class Manager extends JFrame implements ManagerShutdown 
{
	/**
	 * Serial version UID.
	 */
	private static final long serialVersionUID = -1120227545786082722L;

	/**
	 * Manager engine.
	 */
	private ManagerEngine engine;

	/**
	 * Shutdown status.
	 */
	private boolean shuttingDown = false;

	/**
	 * GUI update interval in ms.
	 */
	public final static int GUI_UPDATE_INTERVAL = 250;		// 1/4th of second

	/**
	 * Progress bar indicating number of requests.
	 */
	private JProgressBar progressBar;

	/**
	 * Timer updating the GUI.
	 */
	private Timer guiUpdaterTimer;

	/**
	 * Default progress bar foreground color.
	 */
	private Color defaultProgressBarColor;

	/**
	 * Constructor for Manager.
	 */
	public Manager()
	{
		initialize();
	}

	/**
	 * Get manager engine.
	 */
	public ManagerEngine getManagerEngine()
	{
		if (engine == null)
			engine = new ManagerEngine(this);
		return engine;
	}

	/**
	 * Initialize.
	 */
	protected void initialize()
	{
		getManagerEngine().initialize();

		initializeGUI();

		LogRecord record = new LogRecord(Level.INFO, "AcsManagerStatusMessage_ManagerStarted Manager Application initialized.");
		record.setLoggerName(getManagerEngine().getLogger().getName());
		String formattedString = new ConsoleLogFormatter().format(record);
		System.out.println(formattedString);
		//getManagerEngine().getLogger().log(Level.OFF, "AcsManagerStatusMessage_ManagerStarted Manager Application initialized.");
	}

	/**
	 * Initialize GUI.
	 */
	public void initializeGUI()
	{
		setSize(320, 200);
		setLayout(new GridBagLayout());
		setTitle("ACS Manager");
		
		add(getRequestsProgressBar(),
			new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(40, 40, 40, 40), 0, 0));
		
		guiUpdaterTimer = new Timer(GUI_UPDATE_INTERVAL, new GUIUpdater());
		guiUpdaterTimer.start();
		
		setLocationByPlatform(true);
		setVisible(true);
		toFront();
		
		addWindowListener(new WindowListener() {

			/* (non-Javadoc)
			 * @see java.awt.event.WindowListener#windowActivated(java.awt.event.WindowEvent)
			 */
			public void windowActivated(WindowEvent event) {
			}

			/* (non-Javadoc)
			 * @see java.awt.event.WindowListener#windowClosed(java.awt.event.WindowEvent)
			 */
			public void windowClosed(WindowEvent event) {
			}

			/* (non-Javadoc)
			 * @see java.awt.event.WindowListener#windowClosing(java.awt.event.WindowEvent)
			 */
			public void windowClosing(WindowEvent event) {
				destroy();
			}

			/* (non-Javadoc)
			 * @see java.awt.event.WindowListener#windowDeactivated(java.awt.event.WindowEvent)
			 */
			public void windowDeactivated(WindowEvent event) {
			}

			/* (non-Javadoc)
			 * @see java.awt.event.WindowListener#windowDeiconified(java.awt.event.WindowEvent)
			 */
			public void windowDeiconified(WindowEvent event) {
			}

			/* (non-Javadoc)
			 * @see java.awt.event.WindowListener#windowIconified(java.awt.event.WindowEvent)
			 */
			public void windowIconified(WindowEvent event) {
			}

			/* (non-Javadoc)
			 * @see java.awt.event.WindowListener#windowOpened(java.awt.event.WindowEvent)
			 */
			public void windowOpened(WindowEvent event) {
			}
			
		});
	}

	/**
	 * @see com.cosylab.acs.maci.manager.ManagerShutdown#isShutdownInProgress()
	 */
	public boolean isShutdownInProgress()
	{
		return shuttingDown;
	}

	/**
	 * @see com.cosylab.acs.maci.manager.ManagerShutdown#shutdown()
	 */
	public void shutdown(boolean sigInt)
	{
		// delegate destroy
		if (!shuttingDown)
			destroy();
	}

	/**
	 * Destroyed.
	 */
	public void destroy()
	{
		shuttingDown = true;
		if (guiUpdaterTimer != null)
			guiUpdaterTimer.stop();
		dispose();
		getManagerEngine().destroy();
	}

 	/*****************************************************************************/
	/*************************** [ GUI components ] ******************************/
	/*****************************************************************************/

	/**
	 * GUI updater
	 */
	class GUIUpdater implements ActionListener
	{
		/**
		 * "Requests pending" string.
		 */
		private static final String requestsPending = " request(s) pending";
		
		public void actionPerformed(ActionEvent e) 
		{
			ManagerEngine engine = getManagerEngine();
			JProgressBar progressBar = getRequestsProgressBar();
			
			// update number of requests
			int requests = engine.getNumberOfPendingRequests();
			if (requests != progressBar.getValue())
			{
				// colorize it
				if (requests < progressBar.getMaximum()/2)
					progressBar.setForeground(defaultProgressBarColor);
				else if (requests < progressBar.getMaximum())
					progressBar.setForeground(Color.yellow);
				else
					progressBar.setForeground(Color.red);
					
				progressBar.setValue(requests);
				progressBar.setString(String.valueOf(requests)+requestsPending);
			}
		}
	}

	/**
	 * Returns progress bar indicating number of components.
	 */
	public JProgressBar getRequestsProgressBar()
	{
		if (progressBar == null)
		{
			// create progress bar
			progressBar = new JProgressBar(0, 8);
			defaultProgressBarColor = progressBar.getForeground();
			progressBar.setStringPainted(true);
			progressBar.setString("0 requests pending");
		}
		return progressBar;
		
	}

 	/*****************************************************************************/
	/***************************** [ Main entry ] ********************************/
	/*****************************************************************************/

	/**
	 * Application main entry point.
	 * @param args
	 */
	public static void main(String[] args)
	{
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-n") || args[i].equals("-nr")) {
				// do not read saved state of the Manager
				System.getProperties().put("Manager.recovery", "false");
			}
		}

		new Manager();
	}

}
