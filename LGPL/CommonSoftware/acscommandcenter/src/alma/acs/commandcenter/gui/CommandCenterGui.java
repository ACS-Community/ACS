/*
 * Created on 25.10.2003
 * 
 * To change the template for this generated file go to Window - Preferences - Java - Code
 * Generation - Code and Comments
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.help.DefaultHelpBroker;
import javax.help.HelpSet;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;
import javax.swing.SpringLayout;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.filechooser.FileFilter;

import alma.acs.commandcenter.CommandCenter;
import alma.acs.commandcenter.app.CommandCenterLogic;
import alma.acs.commandcenter.engine.ExecuteTools;
import alma.acs.commandcenter.engine.Executor;
import alma.acs.commandcenter.engine.NativeCommand;
import alma.acs.commandcenter.engine.ToolManager;
import alma.acs.commandcenter.gui.thirdparty.SpringUtilities;
import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.commandcenter.util.VariableString.UnresolvableException;
import alma.entity.xmlbinding.acscommandcenterproject.ContainerT;
import alma.entity.xmlbinding.acscommandcenterproject.types.ModeType;
import alma.entity.xmlbinding.acscommandcentertools.Tool;

/**
 */
public class CommandCenterGui {

	// =========================================================================
	protected static Color COLOR_ActiveButton = Color.gray;

	protected static Color COLOR_PassiveButton = new JButton().getBackground(); // standard
	// button
	// color
	protected static Color COLOR_LogoBackground_A = Color.black;
	protected static Color COLOR_LogoBackground_B = new Color(0, 110, 160);
	// protected static Color COLOR_LogoBackground_A = new Color(1,36,100);
	// protected static Color COLOR_LogoBackground_B = new Color(0, 102, 153);


	protected static Color COLOR_LogoForeground = new Color(255, 255, 255);

	// =========================================================================
	// =========================================================================
	public CommandCenterLogic controller;
	protected Logger log;

	public JFrame frame;

	protected TabPanel frontPanel;
	protected FeedbackTabs feedbackTabs;
	protected DeploymentTree deployTree;
	protected NativeCommand.Listener taskListenerObjectExplorer;
	protected NativeCommand.Listener taskListenerAdminClient;
	protected NativeCommand.Listener taskListenerJlogClient;
	protected NativeCommand.Listener taskListenerCdbBrowser;
	protected NativeCommand.Listener taskListenerDynClient;
	protected NativeCommand.Listener taskListenerInterfaceRepBrowser;
	protected NativeCommand.Listener taskListenerNameServiceBrowser;
	protected JMenu toolsMenu;
	protected File currentProjectFile;

	protected BasicDialog managerLocationDialog1;
	protected ManagerLocationPanel.ForTools pnlManagerLocationForTools;
	
	protected BasicDialog managerLocationDialog2;
	protected ManagerLocationPanel.ForContainers pnlManagerLocationForContainers;
	
	protected JSplitPane split1;
	protected JSplitPane split2;
	

	public CommandCenterGui(CommandCenterLogic controller) {
		this.controller = controller;
      log = MiscUtils.getPackageLogger(this);
	}


	public void prepare () {

		boolean setLookAndFeel = false;
		if (setLookAndFeel) {
			String lafName = UIManager.getSystemLookAndFeelClassName();
			try {
				UIManager.setLookAndFeel(lafName);
				/* SwingUtilities.updateComponentTreeUI(SwingUtilities.getRoot(gui.frame));
				 * SwingUtilities.updateComponentTreeUI(SwingUtilities.getRoot(gui.dlgOptions));
				 */
			} catch (Exception exc) {
				log.info("Couldn't set look and feel " + lafName + " due to " + exc);
			}
		}

		try {
			frame = new JFrame(""); // title added later in doFrameTitle()
			frame.addWindowListener(new WindowAdapter() {

				public void windowClosing (WindowEvent evt) {
					showExitDialog();
					controller.stop();
				}
			});

			frontPanel = new TabPanel(this);
			writeModelToFrontPanel();

			// Splitter between tree and the rest
			split1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
			split1.setOneTouchExpandable(true);
			JPanel p2 = new JPanel(new BorderLayout());
			p2.setBorder(new EmptyBorder(10, 10, 10, 10));
			p2.add(frontPanel, BorderLayout.NORTH);
			split1.setLeftComponent(p2);

			// Deployment Tree
			JPanel deploymentInfoPanel = new JPanel(new BorderLayout());
			deploymentInfoPanel.setBorder(new CompoundBorder(new EmptyBorder(5, 7, 5, 7), new TitledBorder(LineBorder
					.createBlackLineBorder(), " Deployment Info ")));

			deployTree = new DeploymentTree(controller.deploymentTreeControllerImpl);
			JPanel addToDeployTree = new AddToDeployTree(this, deployTree);

			deploymentInfoPanel.add(addToDeployTree, BorderLayout.NORTH);
			deploymentInfoPanel.add(new JScrollPane(deployTree), BorderLayout.CENTER);
			split1.setRightComponent(deploymentInfoPanel);

			// Feedback Area
			feedbackTabs = new FeedbackTabs(this, FeedbackTabs.BOTTOM);

			// Logo Panel
			JPanel logoPanel = new LogoPanel(COLOR_LogoBackground_A, COLOR_LogoBackground_B);
			logoPanel.setLayout(new BorderLayout());

			JLabel alma = new JLabel(new ImageIcon(controller.findResource("alma.jpg")));
			logoPanel.add(alma, BorderLayout.WEST);

			JLabel text = new JLabel("Acs Command Center");
			text.setForeground(COLOR_LogoForeground);
			text.setHorizontalTextPosition(SwingConstants.CENTER);
			text.setFont(text.getFont().deriveFont((float) (text.getFont().getSize() * 2.5)));
			text.setBorder(new EmptyBorder(5, 30, 5, 30));
			logoPanel.add(text, BorderLayout.CENTER);

			JLabel version = new JLabel(controller.version());
			version.setForeground(COLOR_LogoForeground);
			version.setBorder(new EmptyBorder(0, 0, 0, 4));
			JPanel pnl2 = new JPanel(new BorderLayout());
			pnl2.setOpaque(false);
			pnl2.add(version, BorderLayout.SOUTH);
			logoPanel.add(pnl2, BorderLayout.EAST);

			// Menu
			JMenuBar menuBar = new JMenuBar();
			JMenu fileMenu = new JMenu("Project");
			fileMenu.setMnemonic(KeyEvent.VK_P);
			{
				JMenu newMenu = new JMenu("New");	
				newMenu.add(new ActionNewProject("Project"));
				fileMenu.add(newMenu);
			}
			
			fileMenu.add(new ActionOpenProject("Open..."));
			fileMenu.add(new ActionSaveProject("Save"));
			fileMenu.add(new ActionSaveAsProject("Save As..."));
			fileMenu.addSeparator();
			fileMenu.add(new ActionExit("Exit"));
			menuBar.add(fileMenu);
			toolsMenu = new JMenu("Tools");
			toolsMenu.setMnemonic(KeyEvent.VK_T);
			toolsMenu.add(new ActionConfigureTools("Configure Tools..."));
			toolsMenu.addSeparator();
			menuBar.add(toolsMenu);
			// ---
			JMenu extrasMenu = new JMenu("Expert");
			extrasMenu.setMnemonic(KeyEvent.VK_E);
			{
				JMenu sshMode = new JMenu("SSH Mode");
				sshMode.add(new ActionSetSshMode("Platform-independent", false, false));
				sshMode.add(new ActionSetSshMode("Native ssh", true, false));
				sshMode.add(new ActionSetSshMode("Native ssh, kill ssh on quit", true, true));
				extrasMenu.add(sshMode);
				extrasMenu.add(new JSeparator());

				JMenu extraTools = new JMenu("Tools");
				extraTools.add(new ActionShowExtraTools("View All..."));
				extraTools.add(new ActionInstallExtraTools("Replace All..."));
				extrasMenu.add(extraTools);

				JMenu builtinTools = new JMenu("Built-in Tools");
				builtinTools.add(new ActionShowBuiltinTools("View Latest Replacement..."));
				builtinTools.add(new ActionLoadBuiltinTools("Replace Some..."));
				extrasMenu.add(builtinTools);
			}
			extrasMenu.add(new ActionEditCommands("Edit Java Args..."));
			extrasMenu.add(new ActionEditPexpects("Edit Java Expr..."));
			extrasMenu.add(new JSeparator());
			extrasMenu.add(new ActionShowVariables("Variables..."));
			
			menuBar.add(extrasMenu);
			// ---
			JMenuItem item;
			JMenu helpMenu = new JMenu("Help");
			helpMenu.setMnemonic(KeyEvent.VK_H);
			item = helpMenu.add(new ActionShowHelp("Online Help"));
			item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0));
			menuBar.add(Box.createHorizontalGlue());
			menuBar.add(helpMenu);
			// ---
			JPanel h = new JPanel(new SpringLayout());
			h.add(logoPanel);
			h.add(menuBar);
			SpringUtilities.makeCompactGrid(h, 0, 1);
			frame.getContentPane().add(h, BorderLayout.NORTH);

			// ---
			pnlManagerLocationForTools = new ManagerLocationPanel.ForTools();
			managerLocationDialog1 = new BasicDialog(this, "Specify Manager and Services for Tools", "Set", pnlManagerLocationForTools);

			// ---
			pnlManagerLocationForContainers = new ManagerLocationPanel.ForContainers();
			managerLocationDialog2 = new BasicDialog(this, "Specify Manager and Services for Containers", "Set", pnlManagerLocationForContainers);

			// ---

			
			// ---
			dlgContainerLocation = new EditContainerSettingsDialog(this, "Choose where to run this Container",
					"Choose where to run this container:                                      ", "Set");

			split2 = new JSplitPane(JSplitPane.VERTICAL_SPLIT, split1, feedbackTabs);
			split2.setOneTouchExpandable(true);
			frame.getContentPane().add(split2, BorderLayout.CENTER);

		/* frame.getContentPane().add(feedbackArea, BorderLayout.SOUTH); */
			doFrameTitle();
			frame.pack();
			if (controller.startupOptions.geometry != null) {
				frame.setBounds(controller.startupOptions.geometry);
			}
			
			if (controller.startupOptions.manager != null) {
				deployTree.addManager(controller.startupOptions.manager);
				//	fix/enforce locations of the dividers
				split1.setDividerLocation(10);
				split2.setDividerLocation(split2.getMaximumDividerLocation());
			} else {
				// fix/enforce locations of the dividers
			/* split2.setDividerLocation(0.5D); */
				split2.validate();
				split1.setDividerLocation((int) (frame.getWidth() - deploymentInfoPanel.getPreferredSize().width * 1.1));
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void go () {
		frame.setVisible(true);
	}

	public void stop () {
		frame.setVisible(false);
		frame.dispose();
		exitDialog.setVisible(false);
		exitDialog.dispose();
	}


	/*
	 * event handler, invoked by TabPanel
	 */
	protected void managerStarted () {

		if (deployTree == null)
			return;

		String host = controller.model.deriveMgrHostfromCommonSettings();
		String port = controller.model.deriveMgrPortfromCommonSettings();

		try {
			deployTree.addManager(host, port);

		} catch (Exception exc) {
			log.warning("Couldn't add manager (" + host + "," + port + ") to deployment view due to " + exc);
			log.log(Level.FINER, "Couldn't add manager (" + host + "," + port + ") to deployment view", exc);
		}
	}

	/*
	 * event handler, invoked by TabPanel
	 */
	protected void managerStopped () {

		if (deployTree == null)
			return;

		String host = controller.model.deriveMgrHostfromCommonSettings();
		String port = controller.model.deriveMgrPortfromCommonSettings();

		try {
			boolean ok = deployTree.removeManager(host, port, true);
			if (!ok) {
				log.info("Couldn't remove manager from deployment view: no such manager known: " + host + "," + port);
			}

		} catch (Exception exc) {
			log.warning("Tried to remove manager (" + host + "," + port + ") from deployment view due to " + exc);
			log.log(Level.FINER, "Tried to remove (" + host + "," + port + ") manager from deployment view", exc);
		}
	}

	

	public void setCurrentProjectFile (File f) {
		this.currentProjectFile = f;
	}

	// event handler
	public void currentProjectChanged () {
		doFrameTitle();
		writeModelToFrontPanel();
	}

	protected void doFrameTitle () {
		String app = "Acs Command Center";
		String proj = (currentProjectFile == null) ? "(project)" : currentProjectFile.getName();
		frame.setTitle(proj + " - " + app);
	}

	JDialog exitDialog = new JDialog();

	protected void showExitDialog () {
		frame.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		// PENDING(msc): exitDialog is a nice idea, but not properly working yet
		// exitDialog.getContentPane().removeAll();
		// exitDialog.getContentPane().add(new JLabel("\n Exiting. Please wait. \n"));
		// exitDialog.setLocationRelativeTo(frame);
		// exitDialog.pack();
		// exitDialog.setVisible(true);
	}

	protected File showOpenDialog () {
		File ret = null;
		JFileChooser chooser = new JFileChooser();
		int returnVal = chooser.showOpenDialog(frame);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			ret = chooser.getSelectedFile();
		return ret;
	}

	protected File showSaveDialog () {
		File ret = null;
		JFileChooser chooser = new JFileChooser();
		int returnVal = chooser.showSaveDialog(frame);
		if (returnVal == JFileChooser.APPROVE_OPTION)
			ret = chooser.getSelectedFile();
		return ret;
	}

	public void showUnresolvableVariableErrorDialog(String var) {
		showErrorDialog(
				"Variable has been used but is undefined: "+var,
				"Go to the Variables Editor now and define variable '"+var+"'.\n\n" +
				"Tip: Define a Java system property '"+var+"' before running\n" +
				"Acs Command Center. E.g., on the command line you'd do\n" +
				"export JAVA_OPTIONS=\"$JAVA_OPTIONS -D"+var+"=YourValue\"");
	}
	
	protected void showErrorDialog (String summary, Throwable detail) {
		StringWriter w = new StringWriter(1024);
		detail.printStackTrace(new PrintWriter(w, true));
		showErrorDialog(summary, w.toString());
	}

	protected void showErrorDialog (String summary, String detail) {
		JTextArea s = new JTextArea(summary);
		s.setOpaque(false);
		s.setEditable(false);
		s.setLineWrap(true);
		
		JTextArea a = new JTextArea(6, 40);
		Box c = Box.createVerticalBox();
		c.add(s);
		c.add(Box.createVerticalStrut(10));
		c.add(new JScrollPane(a));
		a.setText(detail);
		a.setCaretPosition(0);
		JOptionPane.showMessageDialog(frame, c, summary, JOptionPane.ERROR_MESSAGE);
	}

	protected void showMessageDialog (String message, boolean failure) {
		int type = (failure) ? JOptionPane.ERROR_MESSAGE : JOptionPane.INFORMATION_MESSAGE;
		String title = (failure) ? "Error" : "Information";

		JOptionPane.showMessageDialog(frame, message, title, type);
	}

	
	// ============== Variables Management ===============

	protected BasicDialog dlgEditVariables;
	protected EditVariablesPanel pnlEditVariables;

	protected void showVariablesEditor() {

		if (dlgEditVariables == null) {
			pnlEditVariables = new EditVariablesPanel(this);
			dlgEditVariables = new BasicDialog(this, "Project Variables", "Set", pnlEditVariables);
			//dlgEditVariables.pack();
		}
		
		Map[] m = controller.giveVariableMapsForGui();
		pnlEditVariables.preShow(m[0], m[1], m[2]);
		
		dlgEditVariables.bringUp(); // dialog is modal
		
		if (dlgEditVariables.okChosen) {
			pnlEditVariables.afterOk();
		}

	}
	
	
	// ======= CDB downloading ================

	protected File showCdbChooser () {
		if (cdbChooserDialog == null) {
			cdbChooserDialog = new JDialog(frame) {

				public void paint (Graphics g) {
					// PENDING(msc): remove this hack for this swing-related concurrency problem
					// there's a problem with setPage() when the dialog is invisible,
					// but i don't have time to investigate and find a swing-bug-workaround now
					try {
						super.paint(g);
					} catch (ArrayIndexOutOfBoundsException e) {}
				}
			};
			cdbChooser = new CdbChooser();
			cdbChooserDialog.getContentPane().add(cdbChooser);
			cdbChooserDialog.setSize(400, 500);
			cdbChooserDialog.setLocationRelativeTo(frame);
			cdbChooserDialog.setModal(true);
		}

		cdbChooser.reset();
		// cdbChooserDialog.setModal(false);

		// cdbChooserDialog.setVisible(true);
		new Thread() {

			public void run () {
				try {
					while (!cdbChooser.isVisible()) {
						try {
							Thread.sleep(10);
						} catch (InterruptedException exc1) {}
					}
					cdbChooser.setPage("http://www.eso.org/~mschilli/acc/cdb");
				} catch (IOException exc) {
					log.warning(exc.toString());
					showErrorDialog("Could not load the Cdb page", exc.toString());
				}
			}
		}.start();

		// cdbChooserDialog.setVisible(false);
		// cdbChooserDialog.setModal(true);
		cdbChooserDialog.setVisible(true);
		// cdbChooserDialog.dispose();
		return cdbChooser.getResult();
	}

	protected JDialog cdbChooserDialog;
	protected CdbChooser cdbChooser;

	protected class CdbChooser extends JEditorPane implements HyperlinkListener {

		protected CdbChooser() {
			super("text/html", "<html><body> please wait ... </body></html>");
			this.setEditable(false);
			this.setContentType("text/html");
			this.addHyperlinkListener(this);
			this.result = null;
		}

		// the return values
		protected File result = null;
		protected URL theURL = null;

		protected void reset () {
			result = null;
			theURL = null;
		}

		protected File getResult () {
			return result;
		}

		protected String getCdbRoot () {
			try {
				File diskParent = result.getParentFile();
				String nameFromURL = theURL.getFile(); // "~mschilli/acc/cdb/cdb2.jar"
				String name = nameFromURL.substring(nameFromURL.lastIndexOf('/') + 1, nameFromURL.length() - 4);
				return new File(diskParent, name).getAbsolutePath();

			} catch (Exception exc) {
				exc.printStackTrace();
				return null;
			}
		}

		public void hyperlinkUpdate (HyperlinkEvent evt) {
			// only clicking on a link is interesting for us
			if (evt.getEventType() != HyperlinkEvent.EventType.ACTIVATED)
				return;
			respond(evt.getURL());

		}

		protected JFileChooser chooser = new JFileChooser();

		public void respond (final URL url) {
			try {

				File suggestion = new File(chooser.getCurrentDirectory(), url.getFile());
				chooser.setSelectedFile(suggestion);
				int returnVal = chooser.showSaveDialog(frame);

				if (returnVal != JFileChooser.APPROVE_OPTION) {
					// user cancelled
					return;
				}

				new Thread() {

					public void run () {
						try {

							File target = chooser.getSelectedFile();
							controller.download(url, target);
							result = target;
							theURL = url;

						} catch (Exception exc) {
							showErrorDialog("Failed to download Cdb", exc.toString());
						} finally {
							// PENDING(msc): would be prettier to add an extra event handler
							cdbChooserDialog.setVisible(false);
						}

					}
				}.start();

			} catch (Exception exc) {
				showErrorDialog("A problem occurred", exc.toString());
			}
		}

	}



	// ========== writing Model to Gui and vice versa
	// ===========================================


	protected void writeModelToManagerLocationForTools () {

		// stored as runtime value
		pnlManagerLocationForTools.defaultHostF.setText(controller.model.deriveMgrHostfromCommonSettings());
		pnlManagerLocationForTools.defaultPortF.setText(controller.model.deriveMgrPortfromCommonSettings());
		pnlManagerLocationForTools.defaultIntRepF.setText(controller.model.deriveIRfromCommonSettings());
		pnlManagerLocationForTools.defaultNameServiceF.setText(controller.model.deriveNSfromCommonSettings());

		// stored in model
		pnlManagerLocationForTools.customHostF.setText(String.valueOf(controller.project.getToolAgainstManagerHost()));
		pnlManagerLocationForTools.customPortF.setText(String.valueOf(controller.project.getToolAgainstManagerPort()));
		pnlManagerLocationForTools.customIntRepF.setText(String.valueOf(controller.project.getToolAgainstInterfaceRepository()));
		pnlManagerLocationForTools.customNameServiceF.setText(String.valueOf(controller.project.getToolAgainstNameService()));

		pnlManagerLocationForTools.btnCustom.setSelected(controller.project.getToolRunAgainstDedicatedSettings());
		pnlManagerLocationForTools.btnLatest.setSelected(!controller.project.getToolRunAgainstDedicatedSettings());
	}

	protected void writeManagerLocationForToolsToModel () {
		// store in model
		controller.project.setToolAgainstManagerHost(pnlManagerLocationForTools.customHostF.getText().trim());
		controller.project.setToolAgainstManagerPort(pnlManagerLocationForTools.customPortF.getText().trim());
		controller.project.setToolAgainstInterfaceRepository(pnlManagerLocationForTools.customIntRepF.getText().trim());
		controller.project.setToolAgainstNameService(pnlManagerLocationForTools.customNameServiceF.getText().trim());

		// the flag will be used in model.getToolsAgainstManagerHost() etc.
		controller.project.setToolRunAgainstDedicatedSettings(pnlManagerLocationForTools.btnCustom.isSelected());
	}

	protected void showManagerLocationForContainersDialog () {

		writeModelToManagerLocationForContainers();

		// dialog is modal, so this returns only after ok-click
		managerLocationDialog2.bringUp();

		// if dialog cancelled or closed, we're done
		boolean ok = managerLocationDialog2.okChosen;
		if (ok) {
			writeManagerLocationForContainersToModel();
		}

	}


	protected void writeModelToManagerLocationForContainers () {

		// stored as runtime value
		pnlManagerLocationForContainers.defaultHostF.setText(controller.model.deriveMgrHostfromCommonSettings());
		pnlManagerLocationForContainers.defaultPortF.setText(controller.model.deriveMgrPortfromCommonSettings());
		pnlManagerLocationForContainers.defaultCdbF.setText(controller.model.deriveCDBfromCommonSettings());
		pnlManagerLocationForContainers.defaultIntRepF.setText(controller.model.deriveIRfromCommonSettings());

		// stored in model
		pnlManagerLocationForContainers.customHostF.setText(String.valueOf(controller.project.getContainers().getAgainstManagerHost()));
		pnlManagerLocationForContainers.customPortF.setText(String.valueOf(controller.project.getContainers().getAgainstManagerPort()));
		pnlManagerLocationForContainers.customCdbF.setText(String.valueOf(controller.project.getContainers().getAgainstCDB()));
		pnlManagerLocationForContainers.customIntRepF.setText(String.valueOf(controller.project.getContainers().getAgainstInterfaceRepository()));
		
		pnlManagerLocationForContainers.btnCustom.setSelected(controller.project.getContainers().getRunAgainstDedicatedSettings());
		pnlManagerLocationForContainers.btnLatest.setSelected(!controller.project.getContainers().getRunAgainstDedicatedSettings());
	}

	protected void writeManagerLocationForContainersToModel () {
		// store in model
		controller.project.getContainers().setAgainstManagerHost(pnlManagerLocationForContainers.customHostF.getText().trim());
		controller.project.getContainers().setAgainstManagerPort(pnlManagerLocationForContainers.customPortF.getText().trim());
		controller.project.getContainers().setAgainstCDB(pnlManagerLocationForContainers.customCdbF.getText().trim());
		controller.project.getContainers().setAgainstInterfaceRepository(pnlManagerLocationForContainers.customIntRepF.getText().trim());

		// the flag will be used in this.getContainersAgainstManagerHost() etc.
		controller.project.getContainers().setRunAgainstDedicatedSettings(pnlManagerLocationForContainers.btnCustom.isSelected());
	}

	protected EditContainerSettingsDialog dlgContainerLocation;

	protected void showContainerLocationDialog () {

		// update data in dialog
		ContainerT cont = writeModelToContainerLocationDialog();

		// show dialog
		correctDialogLocation(dlgContainerLocation);
		dlgContainerLocation.bringUp();

		// if dialog cancelled or closed, we're done
		if (!dlgContainerLocation.okChosen)
			return;

		// read out data from dialog
		writeContainerLocationDialogToModel(cont);
	}


	protected ContainerT writeModelToContainerLocationDialog () {
		int selectedContainer = controller.project.getContainers().getSelect();
		ContainerT cont = controller.project.getContainers().getContainer(selectedContainer);

		dlgContainerLocation.defaultScriptBaseF.setText(controller.project.getScriptBase());
		dlgContainerLocation.defaultHostF.setText(controller.project.getRemoteHost());
		dlgContainerLocation.defaultAccountF.setText(controller.project.getRemoteAccount());
		dlgContainerLocation.defaultPasswordF.setText(controller.project.getRemotePassword());

		dlgContainerLocation.customScriptBaseF.setText(cont.getScriptBase());
		dlgContainerLocation.customHostF.setText(cont.getRemoteHost());
		dlgContainerLocation.customAccountF.setText(cont.getRemoteAccount());
		dlgContainerLocation.customPasswordF.setText(cont.getRemotePassword());

		dlgContainerLocation.btnCustom.setSelected(cont.getUseDedicatedSettings());
		dlgContainerLocation.btnGlobal.setSelected(!cont.getUseDedicatedSettings());

		return cont;
	}

	protected void writeContainerLocationDialogToModel (ContainerT cont) {
		cont.setScriptBase(dlgContainerLocation.customScriptBaseF.getText().trim());
		cont.setRemoteHost(dlgContainerLocation.customHostF.getText().trim());
		cont.setRemoteAccount(dlgContainerLocation.customAccountF.getText().trim());
		cont.setRemotePassword(dlgContainerLocation.customPasswordF.getText().trim());

		cont.setUseDedicatedSettings(!dlgContainerLocation.btnGlobal.isSelected());
	}

	/**
	 * Reads out the gui elements
	 */
	protected void writeFrontPanelToModel () {

		// msc (2004-10-28): according to a request by gianluca:
		// these special-settings are now derived from the
		// common-settings, triggered by a FocusListener
		// msc (2005-04-12): the special-settings will now be derived on-the-fly
		// on each read-access to them, but the base settings for their computation
		// still need to be set instantly, triggered by the FocusListener
		controller.project.setScriptBase(frontPanel.acsinstanceF.getText().trim());
		controller.project.setServicesLocalJavaRoot(frontPanel.cdbrootF.getText().trim());
		controller.project.setRemoteHost(frontPanel.hostF.getText().trim());
		controller.project.setRemoteAccount(frontPanel.accountF.getText().trim());
		controller.project.setRemotePassword(frontPanel.passwordF.getText().trim());

		// "mode" checkboxes
		if (frontPanel.chkLocalScript.isSelected()) {
			controller.project.setMode(ModeType.LOCAL);
		} else
		if (frontPanel.chkRemoteScript.isSelected()) {
			controller.project.setMode(ModeType.REMOTE);
		} else
		if (frontPanel.chkLocalJava.isSelected()) {
			controller.project.setMode(ModeType.JAVA);
		}

		// read container definitions and see which
		// of the container-RadioButtons is checked
		for (int i = 0; i < frontPanel.containerLines.size(); i++) {
			TabPanel.ContainerLine c = (TabPanel.ContainerLine) frontPanel.containerLines.elementAt(i);
			ContainerT cont = controller.project.getContainers().getContainer(i);

			if (cont == null) {
				log.warning("number of containers in model and gui is out-of-sync");
				break;
			}

			if (c.selectB.isSelected()) {
				controller.project.getContainers().setSelect(i);
			}

			cont.setName(c.nameF.getText().trim());
			cont.setType(String.valueOf(c.typeF.getSelectedItem()).trim());
		}

	}

	protected void writeModelToFrontPanel () {
		// fields
		frontPanel.acsinstanceF.setText(controller.project.getScriptBase());
		frontPanel.cdbrootF.setText(controller.project.getServicesLocalJavaRoot());
		frontPanel.hostF.setText(controller.project.getRemoteHost());
		frontPanel.accountF.setText(controller.project.getRemoteAccount());

		// "mode" checkboxes
		ModeType mode = controller.project.getMode();
		if (mode.equals(ModeType.LOCAL)) {
			frontPanel.chkLocalScript.setSelected(true);
		} else 
		if (mode.equals(ModeType.REMOTE)) {
			frontPanel.chkRemoteScript.setSelected(true);
		} else
		if (mode.equals(ModeType.JAVA)) {
			frontPanel.chkLocalJava.setSelected(true);
		}
		
		// update container definitions + set container-RadioButton
		while (frontPanel.containerLines.size() > 0)
			frontPanel.lessContainerLines();
		for (int i = 0; i < controller.project.getContainers().getContainerCount(); i++) {
			ContainerT cont = controller.project.getContainers().getContainer(i);
			frontPanel.addContainerLine(cont.getName(), cont.getType(), (i == controller.project.getContainers().getSelect()));
		}
	}


	// ==========================================================================================



	// makes a dialog appear above the main-frame on first show-up
	public void correctDialogLocation (JDialog d) {
		Point p = d.getLocation();
		if (p.x == 0 && p.y == 0) {
			d.setLocationRelativeTo(d.getParent());
		}
	}

	
	// ======= Html Browsing ========
	
	JDialog dialog = null;
	JEditorPane editor = null;

	public void showUrlContent (URL url, String title) {

		if (dialog == null) {
			dialog = new JDialog(frame);
			dialog.setSize(600, 400);
			editor = new JEditorPane();
			editor.setEditable(false);
			JScrollPane scroll = new JScrollPane(editor);
			dialog.getContentPane().add(scroll);
			dialog.setLocationRelativeTo(frame);
		}

		try {
			editor.setPage(url);
			dialog.setTitle(title + "  -  " + url);
			dialog.setVisible(true);
		} catch (Exception exc) {
			showMessageDialog("Cannot show the resource: " + exc, true);
		}
	}

	// ====== Help =======
	
	protected DefaultHelpBroker helpBroker;
	
	protected void showHelpBrowser() {

		if (helpBroker == null) {
		
			HelpSet helpSet = controller.getHelpSet();
			if (helpSet == null) {
				showMessageDialog("Online Help could not be loaded", true);
				return;
			}
			
			helpBroker = (DefaultHelpBroker)helpSet.createHelpBroker();
		}
		
		helpBroker.setCurrentID("intro");
		helpBroker.setDisplayed(true);
	}

	// ====== Edit Java Commands/Wait-Strings ======
	
	protected JDialog editCommandsDialog;

	protected EditCommandsPanel editCommandsPanel;
	
	protected JDialog editPexpectsDialog;

	protected EditPexpectsPanel editPexpectsPanel;

	
	// ====== ToolMenu Tools =======
	
	
	/** for book-keeping, to allow later removal of actions from the tools menu */
	protected Vector<JMenuItem> addedToolMenuItems = new Vector<JMenuItem>();

	/**
	 * Adds an entry to the tools menu, on activation an input dialog is shown and
	 * evaluated. Finally the specified listener is triggered.
	 */
	public void addExtraTool (Tool tool, final HashMap result, final ExecuteTools.ToolStarter ts) {

		// --- create an input dialog
		
		final ToolInputPanel tp = new ToolInputPanel(this, tool);
		
		// --- create an action that shows the input panel (if needed) and runs the tool
		 
		// make up the pretty names to show in the gui 
		final String cap = tool.getCaption();
		// menu text: append three dots if user has to give input
		String menuCaption = (tp.counter == 0)? cap : cap + "...";
		// tab text: shorten if too long 
		final String tabCaption = (cap.length() < 15)? cap : cap.substring(0, 8) + "..." +cap.substring(cap.length()-4);
		
		Action a = new AbstractAction(menuCaption) {

			public void actionPerformed (ActionEvent evt) {

				// only show if panel contains more than zero lines
				if (tp.counter > 0) {
					boolean ok = tp.showPanel();
				
					if (!ok) { 
						return; // user cancelled the dialog
					}
					
					// read results from dialog
					Map<String, String> map = tp.evaluate();
					result.putAll(map);
				}

				NativeCommand.Listener listener = giveSimpleListener(tabCaption);
				
				try {
					ts.start(listener);
				
				} catch (UnresolvableException exc) {
					String var = exc.getVariableName();
					controller.handleUnresolvableVariable(var);
					log.log(Level.INFO, "Could not start tool '"+cap+"': Variable '"+var+"' is undefined");
					
				} catch (Throwable t) {
					log.warning("Could not start tool '"+cap+"', check definition file " + "(default file: "
							+ ToolManager.getDefaultExtraToolsName() + "); reason was: " + t);
					t.printStackTrace(System.err);
				}
			}
		};
		
		// append the action to the tools menu
		JMenuItem mi = toolsMenu.add(a);
		addedToolMenuItems.add(mi);
	}

	public void removeAllExtraTools () {
		for (int i = 0; i < addedToolMenuItems.size(); i++) {
			toolsMenu.remove((JMenuItem) addedToolMenuItems.get(i));
		}
		addedToolMenuItems.clear();
	}


	// ================== Actions ==============================================

	
	
	/** Runs the response to the action in its own thread */
	protected abstract class ActionBaseClass extends AbstractAction {

		protected ActionBaseClass(String name) {
			super(name);
		}

		protected ActionBaseClass(String name, Icon icon) {
			super(name, icon);
		}

		public void actionPerformed (ActionEvent evt) {
			// TODO(msc): re-use threads
			new Thread(new Runnable() {

				public void run () {
					try {
						actionPerformed();
						
					} catch (UnresolvableException exc) {
						String name = "'"+getValue(Action.NAME)+"'";
						String var = exc.getVariableName();
						controller.handleUnresolvableVariable(var);
						log.log(Level.INFO, "Error while performing "+name+": Variable '"+var+"' is undefined");
						
					} catch (Throwable t) {
						String name = "'"+getValue(Action.NAME)+"'";
						showErrorDialog("Encountered an error while performing "+name, t);
						log.log(Level.INFO, "Error while performing "+name, t);
					}
				};
			}).start();
		}

		protected abstract void actionPerformed () throws Throwable;
	}

	protected class ActionNewProject extends ActionBaseClass {

		public ActionNewProject(String name) {
			super(name);
		}

		public void actionPerformed () {
			controller.project = controller.createProject();
			currentProjectFile = null;
			currentProjectChanged();
		}
	}

	
	protected class ActionShowHelp extends ActionBaseClass {

		public ActionShowHelp(String name) {
			super(name);
		}

		public void actionPerformed () {
			showHelpBrowser();
		}
	}

	protected class ActionOpenProject extends ActionBaseClass {

		public ActionOpenProject(String name) {
			super(name);
		}

		public void actionPerformed () throws Exception {
			File f = showOpenDialog();
			if (f == null)
				return;

			controller.loadProject(f);
			
		}
	}

	protected class ActionSaveProject extends ActionBaseClass {

		public ActionSaveProject(String name) {
			super(name);
		}

		public void actionPerformed () throws Exception {
			// as i learn from the OT, focus-listening is
			// not 100% reliable, so we should probably enforce
			// the following before we go to write to disk.
			/* writeFrontPanelToModel(); */
			
			if (currentProjectFile != null) {
				controller.writeProject(controller.project, currentProjectFile);
			} else {
				File f = showSaveDialog();
				if (f == null)
					return;
				controller.writeProject(controller.project, f);
				currentProjectFile = f;
				currentProjectChanged();
			}
		}
	}

	protected class ActionSaveAsProject extends ActionBaseClass {

		public ActionSaveAsProject(String name) {
			super(name);
		}

		public void actionPerformed () throws Exception {
			File f = showSaveDialog();
			if (f == null)
				return;
			controller.writeProject(controller.project, f);
			// do not change currentProjectFile setting here as most apps would
			// (i hate when they do that)
		}
	}

	protected class ActionExit extends ActionBaseClass {

		public ActionExit(String name) {
			super(name);
		}

		public void actionPerformed () {
			controller.stop();
		}
	}

	protected class ActionConfigureTools extends ActionBaseClass {

		public ActionConfigureTools(String name) {
			super(name);
		}

		public void actionPerformed () {
			writeModelToManagerLocationForTools();

			managerLocationDialog1.bringUp(); // dialog is modal

			boolean ok = managerLocationDialog1.okChosen;
			if (ok) {
				writeManagerLocationForToolsToModel();
			}
		}
	}

	protected class ActionEditCommands extends ActionBaseClass {

		public ActionEditCommands(String name) {
			super(name);
		}

		public void actionPerformed () {
			if (editCommandsDialog == null) {
				editCommandsPanel = new EditCommandsPanel(controller);
				editCommandsDialog = new JDialog(frame, "Edit Java Args", false);
				editCommandsDialog.getContentPane().add(editCommandsPanel);
				editCommandsDialog.pack();
			}
			correctDialogLocation(editCommandsDialog);
			editCommandsDialog.setVisible(true);
		}
	}

	protected class ActionEditPexpects extends ActionBaseClass {

		public ActionEditPexpects(String name) {
			super(name);
		}

		public void actionPerformed () {
			if (editPexpectsDialog == null) {
				editPexpectsPanel = new EditPexpectsPanel(controller);
				editPexpectsDialog = new JDialog(frame, "Edit Java Expr", false);
				editPexpectsDialog.getContentPane().add(editPexpectsPanel);
				editPexpectsDialog.pack();
			}
			correctDialogLocation(editPexpectsDialog);
			editPexpectsDialog.setVisible(true);
		}
	}

	protected class ActionInstallExtraTools extends ActionBaseClass {

		public ActionInstallExtraTools(String name) {
			super(name);
		}

		public void actionPerformed () {
			// show file dialog
			File f = null;
			JFileChooser c = new JFileChooser();
			c.setFileFilter(new FileFilter() {

				public boolean accept (File f) {
					return (f.isDirectory() || f.getName().endsWith(".xml") || f.getName().endsWith(".XML"));
				}

				public String getDescription () {
					return "CommandCenter Tools (*.xml)";
				}
			});
			// check if user cancelled, otherwise get file
			int returnVal = c.showOpenDialog(frame);
			if (returnVal == JFileChooser.APPROVE_OPTION)
				f = c.getSelectedFile();
			if (f == null)
				return;

			// read file
			try {
				// FileInputStream is = new FileInputStream(f);
				controller.removeExtraTools();
				controller.installExtraTools(f.toURL());
			} catch (Exception exc) {
				showMessageDialog("The tools could not be installed. Check console for error output. ", true);
				log.info("could not install tools: " + exc);
				return;
			}

			showMessageDialog("The tools have been installed successfully. ", false);
		}
	}

	protected class ActionSetSshMode extends ActionBaseClass {
		
		private boolean useNativeSSH;
		private boolean killNativeSSH;

		public ActionSetSshMode (String name, boolean useNativeSSH, boolean killNativeSSH) {
			super(name);
			this.useNativeSSH = useNativeSSH;
			this.killNativeSSH = killNativeSSH;
		}
		
		public void actionPerformed () {
			System.setProperty(Executor.SYSPROP_USE_NATIVE_SSH, Boolean.toString(this.useNativeSSH));
			System.setProperty(Executor.SYSPROP_KILL_NATIVE_SSH, Boolean.toString(this.killNativeSSH));
		}
	}
	
	protected class ActionShowExtraTools extends ActionBaseClass {

		public ActionShowExtraTools(String name) {
			super(name);
		}

		public void actionPerformed () {
			showUrlContent(controller.currentExtraToolsUrl, "Tools");
		}
	}

	protected class ActionShowBuiltinTools extends ActionBaseClass {

		public ActionShowBuiltinTools(String name) {
			super(name);
		}

		public void actionPerformed () {
			showUrlContent(controller.latestBuiltinToolsUrl, "Built-in Tools");
		}
	}

	protected class ActionLoadBuiltinTools extends ActionBaseClass {

		public ActionLoadBuiltinTools(String name) {
			super(name);
		}

		public void actionPerformed () {
			// show file dialog
			File f = null;
			JFileChooser c = new JFileChooser();
			c.setFileFilter(new FileFilter() {

				public boolean accept (File f) {
					return (f.isDirectory() || f.getName().endsWith(".xml") || f.getName().endsWith(".XML"));
				}

				public String getDescription () {
					return "CommandCenter Built-in Tools (*.xml)";
				}
			});
			// check if user cancelled, otherwise get file
			int returnVal = c.showOpenDialog(frame);
			if (returnVal == JFileChooser.APPROVE_OPTION)
				f = c.getSelectedFile();
			if (f == null)
				return;

			// read file
			try {
				URL url = f.toURL();
				controller.loadBuiltinTools(url);
			} catch (Exception exc) {
				JOptionPane.showMessageDialog(frame, "The built-in tools could not be loaded. Check console for error output. ");
				log.info("could not load built-in tools: " + exc);
				return;
			}

			JOptionPane.showMessageDialog(frame, "The built-in tools have been loaded. ");
		}
	}

	protected class ActionShowVariables extends ActionBaseClass {

		public ActionShowVariables(String name) {
			super(name);
		}

		public void actionPerformed () {
			showVariablesEditor();
		}
	}

	
	/**
	 * The simple listener just writes a task's output to a feedback area, nothing more and
	 * nothing less. this is sufficient when running tools like the admin client.
	 */
	protected NativeCommand.Listener giveSimpleListener (String taskName) {
		NativeCommand.Listener ret = feedbackTabs.viewTab(taskName);
		return ret;
	}

	/**
	 * The complex listener is like the simple plus: it scans the output for automatic
	 * acs_instance assignments. this is needed when running services or a manager.
	 */
	protected NativeCommand.Listener giveComplexListener (String taskName) {
		NativeCommand.Listener first = giveSimpleListener(taskName);
		NativeCommand.Listener second = new CheckAcsInstanceTaskListener();
		return new CompoundTaskListener(first, second);
	}

	/**
	 * An instance of this is passed to some of the launch methods.
	 */
	protected class CompoundTaskListener implements NativeCommand.Listener {

		NativeCommand.Listener a, b;

		protected CompoundTaskListener(NativeCommand.Listener a, NativeCommand.Listener b) {
			this.a = a;
			this.b = b;
		}

		public void statusChanged (NativeCommand task, String oldStatus) {
			a.statusChanged(task, oldStatus);
			b.statusChanged(task, oldStatus);
		}

		public void stdoutWritten (NativeCommand task, String additionalOutput) {
			a.stdoutWritten(task, additionalOutput);
			b.stdoutWritten(task, additionalOutput);
		}

		public void stderrWritten (NativeCommand task, String additionalOutput) {
			a.stderrWritten(task, additionalOutput);
			b.stderrWritten(task, additionalOutput);
		}
	}


	/** Writes all output to the feedback area */
	public class WriteToFeedbackAreaTaskListener implements NativeCommand.Listener {

		FeedbackArea feedbackArea;

		public WriteToFeedbackAreaTaskListener(FeedbackArea f) {
			this.feedbackArea = f;
		}

		public void statusChanged (NativeCommand task, String oldStatus) {}

		public void stdoutWritten (NativeCommand task, String additionalOutput) {
			if (feedbackArea == null) {
				return;
			}
			feedbackArea.append(additionalOutput);
		}

		public void stderrWritten (NativeCommand task, String additionalOutput) {
			if (feedbackArea == null) {
				return;
			}
			feedbackArea.append(additionalOutput);
		}
	}

	// msc (2005-10-24): the automatic instance choosing has been removed from the
	// acs startup scripts, so we check for the corresponding error message instead.
	protected static final Pattern p = Pattern.compile(".*using the same ACS_INSTANCE (.+). This is not possible.*");

	/**
	 * Checks the output for whether the desired acs instance is already taken.
	 */
	protected class CheckAcsInstanceTaskListener implements NativeCommand.Listener {

		public void statusChanged (NativeCommand task, String oldStatus) {}

		public void stderrWritten (NativeCommand task, String additionalOutput) {}

		public void stdoutWritten (NativeCommand task, String additionalOutput) {

			// (Apr 8, 2004) msc:
			// the pattern we use does not work in some cases where line
			// terminators (\r or \n) are contained in the additionalOutput text.
			// this is frequently the case with the output from our ssh-sessions.
			// thus we pre-process the text a bit..
			additionalOutput = additionalOutput.replace('\r', ' ').replace('\n', ' ');

			final Matcher m = p.matcher(additionalOutput);
			if (m.matches()) {
				final String desiredAcsInstance = frontPanel.acsinstanceF.getText().trim();
				new Thread(new Runnable() {
					public void run () {
						String message = "The Acs Instance you requested ( " + desiredAcsInstance + " ) is already" +
								" in use by you or somebody else." +
								"\nPlease choose a different Acs Instance in the Common Settings.";
						JOptionPane.showMessageDialog(frame, message, "Acs Instance in use", JOptionPane.INFORMATION_MESSAGE);
					}
				}).start();
			}
		}
	}

	// ==========================================================================================

	protected Icons icons = new Icons();

	protected class Icons {

		protected Icon startIcon = null;

		protected Icon getStartIcon () {
			if (startIcon == null) {
				startIcon = new ImageIcon(controller.findResource("start.gif"));
			}
			return startIcon;
		}

		protected Icon stopIcon = null;

		protected Icon getStopIcon () {
			if (stopIcon == null) {
				stopIcon = new ImageIcon(controller.findResource("stop.gif"));
			}
			return stopIcon;
		}

		protected Icon stopIconRed = null;

		protected Icon getStopIconRed () {
			if (stopIconRed == null) {
				stopIconRed = new ImageIcon(controller.findResource("stop_red.gif"));
			}
			return stopIconRed;
		}

		protected Icon plusIcon = null;

		protected Icon getPlusIcon () {
			if (plusIcon == null) {
				plusIcon = new ImageIcon(controller.findResource("plus.gif"));
			}
			return plusIcon;
		}

		protected Icon minusIcon = null;

		protected Icon getMinusIcon () {
			if (minusIcon == null) {
				minusIcon = new ImageIcon(controller.findResource("minus.gif"));
			}
			return minusIcon;
		}

		protected Icon okIcon = null;

		protected Icon getOkIcon () {
			if (okIcon == null) {
				okIcon = new ImageIcon(controller.findResource("ok-8x8.gif"));
			}
			return okIcon;
		}

		protected Icon errIcon = null;

		protected Icon getErrIcon () {
			if (errIcon == null) {
				errIcon = new ImageIcon(controller.findResource("err-8x8.gif"));
			}

			return errIcon;
		}
		
		protected Icon configIcon = null;

		protected Icon getConfigIcon () {
			if (configIcon == null) {
				configIcon = new ImageIcon(controller.findResource("config.gif"));
			}

			return configIcon;
		}

		protected Icon upIcon = null;

		protected Icon getUpIcon () {
			if (upIcon == null) {
				upIcon = new ImageIcon(controller.findResource("up.gif"));
			}

			return upIcon;
		}

		protected Icon downIcon = null;

		protected Icon getDownIcon () {
			if (downIcon == null) {
				downIcon = new ImageIcon(controller.findResource("down.gif"));
			}

			return downIcon;
		}
		
	}

}