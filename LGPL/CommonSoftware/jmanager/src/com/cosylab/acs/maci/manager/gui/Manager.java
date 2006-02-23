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
import javax.swing.JApplet;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JProgressBar;
import javax.swing.Timer;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.core.defaults.MessageLogEntry;
import abeans.framework.FrameworkLayer;
import abeans.pluggable.acs.logging.LoggingLevel;

import com.cosylab.abeans.AbeansEngine;
import com.cosylab.abeans.AbeansLaunchable;
import com.cosylab.abeans.AbeansLauncher;
import com.cosylab.abeans.plugins.AbeansExceptionPanelPlugIn;
import com.cosylab.abeans.plugins.AbeansStandardActionsPlugIn;
import com.cosylab.abeans.plugins.AbeansSystemMenuPlugIn;
import com.cosylab.abeans.plugins.AboutPlugIn;
import com.cosylab.abeans.plugins.LoggingPlugIn;
import com.cosylab.abeans.plugins.ReportAreaPlugIn;
import com.cosylab.abeans.plugins.TreeBrowserPlugIn;
import com.cosylab.acs.maci.manager.ManagerShutdown;
import com.cosylab.acs.maci.manager.app.ManagerEngine;
import com.cosylab.gui.framework.Desktop;
import com.cosylab.gui.framework.Launcher;
import com.cosylab.gui.framework.LauncherEnvironment;
import com.cosylab.gui.plugins.CosyStandardActionsPlugIn;

/**
 * Manager activation application and GUI implementation.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class Manager extends AbeansLaunchable implements Identifiable, ManagerShutdown
{

	/**
	 * Identifier.
	 */
	private Identifier id;

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
		super();
	}

	/**
	 * Constructor for Manager.
	 * @param launcher
	 * @param env
	 * @param owner
	 */
	public Manager(Launcher launcher, LauncherEnvironment env, JFrame owner)
	{
		super(launcher, env, owner);
	}

	/**
	 * Constructor for Manager.
	 * @param launcher
	 * @param env
	 * @param desk
	 * @param owner
	 */
	public Manager(
		Launcher launcher,
		LauncherEnvironment env,
		Desktop desk,
		JInternalFrame owner)
	{
		super(launcher, env, desk, owner);
	}

	/**
	 * Constructor for Manager.
	 * @param launcher
	 * @param env
	 * @param owner
	 */
	public Manager(Launcher launcher, LauncherEnvironment env, JApplet owner)
	{
		super(launcher, env, owner);
	}

	/**
	 * @see com.cosylab.abeans.AbeansLaunchable#getAbeansEngine()
	 */
	public AbeansEngine getAbeansEngine()
	{
		if (engine == null)
			engine = new ManagerEngine(this);
		return engine;
	}

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null)
			id = new IdentifierSupport("Manager", "Manager", "Manager", "Manager", Identifier.APPLICATION);
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return true;
	}

	/**
	 * @see com.cosylab.gui.core.CosyPanel#userAllInitializationsDone()
	 */
	protected void userAllInitializationsDone()
	{
		super.userAllInitializationsDone();
		if (isDebug())
			new MessageLogEntry(this, "userAllInitializationsDone", "Manager GUI Application initialized.", LoggingLevel.INFO).dispatch();

		// start updater
		if (guiUpdaterTimer != null)
			guiUpdaterTimer.start();
	}

	/**
	 * @see com.cosylab.gui.core.CosyPanel#userInitializeGUI()
	 */
	public void userInitializeGUI()
	{
		setSize(320, 200);
		setLayout(new GridBagLayout());
		
		add(getRequestsProgressBar(),
			new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH,
					new Insets(40, 40, 40, 40), 0, 0));
		
		guiUpdaterTimer = new Timer(GUI_UPDATE_INTERVAL, new GUIUpdater());
	}

	/**
	 * @see com.cosylab.gui.core.CosyPanel#userInitializePlugIns()
	 */
	public void userInitializePlugIns()
	{
		try
		{
			installPlugIn(AboutPlugIn.class);

			installPlugIn(ReportAreaPlugIn.class);
			installPlugIn(AbeansExceptionPanelPlugIn.class);
			installPlugIn(LoggingPlugIn.class);
			installPlugIn(TreeBrowserPlugIn.class);

			installPlugIn(AbeansSystemMenuPlugIn.class);
			installPlugIn(AbeansStandardActionsPlugIn.class);
			installPlugIn(CosyStandardActionsPlugIn.class);
		} catch (Exception ex)
		{
			new MessageLogEntry(this, "Failed to initialize plugins.", ex, LoggingLevel.ERROR).dispatch();
		}
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
	 * @see com.cosylab.gui.core.CosyPanel#internalDestroy()
	 * This method is called within synchronized block.
	 */
	public void internalDestroy()
	{
		shuttingDown = true;
		guiUpdaterTimer.stop();
		super.internalDestroy();
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
			ManagerEngine engine = (ManagerEngine)getAbeansEngine();
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

		// disable Abeans shutdown hook
		System.setProperty(FrameworkLayer.PROPERTY_DISABLE_SHUTDOWN_HOOK, "true");

		AbeansLauncher.main( new String[] { Manager.class.getName() } );
	}

}
