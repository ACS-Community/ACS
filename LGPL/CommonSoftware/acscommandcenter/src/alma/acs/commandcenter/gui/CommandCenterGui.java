/*
 * Created on 25.10.2003
 * 
 * To change the template for this generated file go to Window - Preferences - Java - Code
 * Generation - Code and Comments
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyVetoException;
import java.io.File;
import java.net.URL;
import java.util.HashMap;
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
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.SpringLayout;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;
import javax.swing.plaf.basic.BasicInternalFrameUI;

import alma.acs.commandcenter.app.CommandCenterLogic;
import alma.acs.commandcenter.engine.ExecuteTools;
import alma.acs.commandcenter.engine.NativeCommand;
import alma.acs.commandcenter.gui.TabPanel.ContainerLine;
import alma.acs.commandcenter.gui.thirdparty.SpringUtilities;
import alma.acs.commandcenter.util.MiscUtils;
import alma.acs.commandcenter.util.VariableString.UnresolvableException;
import alma.acs.util.AcsLocations;
import alma.entity.xmlbinding.acscommandcenterproject.ContainerT;
import alma.entity.xmlbinding.acscommandcenterproject.types.ModeType;
import alma.entity.xmlbinding.acscommandcentertools.Tool;

/**
 * The main Gui class of the Acs Command Center application.
 *
 * @author M.Schilling, ESO
 */
public class CommandCenterGui {

	// =========================================================================
	protected static Color COLOR_ActiveButton = Color.gray;
	protected static Color COLOR_PassiveButton = new JButton().getBackground();
	protected static Color COLOR_LogoBackground_A = Color.black;
	protected static Color COLOR_LogoBackground_B = new Color(0, 110, 160);
	protected static Color COLOR_LogoForeground = new Color(255, 255, 255);

	// =========================================================================
	// =========================================================================
	
	
	public CommandCenterLogic controller;
	protected Logger log;
	public JFrame frame;
	public JDesktopPane desktop;
	protected TabPanel frontPanel;
	protected FeedbackTabs feedbackTabs;
	public DeploymentTree deployTree;
	protected JPanel deploymentInfoPanel;
	protected NativeCommand.Listener taskListenerObjectExplorer;
	protected NativeCommand.Listener taskListenerAdminClient;
	protected NativeCommand.Listener taskListenerJlogClient;
	protected NativeCommand.Listener taskListenerCdbBrowser;
	protected NativeCommand.Listener taskListenerDynClient;
	protected NativeCommand.Listener taskListenerInterfaceRepBrowser;
	protected NativeCommand.Listener taskListenerNameServiceBrowser;
	protected JMenuBar menuBar;
	protected JMenu toolsMenu;
	protected File currentProjectFile;

	protected BasicDialog managerLocationDialog1;
	protected ManagerLocationPanel.ForTools pnlManagerLocationForTools;
	
	protected BasicDialog managerLocationDialog2;
	protected ManagerLocationPanel.ForContainers pnlManagerLocationForContainers;
	
	protected JSplitPane splitLeftRight; // split controls from tree 
	protected JSplitPane splitTopBottom; // split upper-part from log-area 
	

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
			} catch (Exception exc) {
				log.fine("Couldn't set look and feel " + lafName + " due to " + exc);
			}
		}

		frame = new JFrame(""); // title added later in doFrameTitle()
		frame.addWindowListener(new WindowAdapter() {

			@Override
			public void windowClosing (WindowEvent evt) {
				controller.stop();
			}
		});

		
		dlgContainerSettings = new EditContainerSettingsDialog(this);

		frontPanel = new TabPanel(this);
		writeModelToFrontPanel();

		// Splitter between tree and the rest
		splitLeftRight = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		splitLeftRight.setOneTouchExpandable(true);
		JPanel p2 = new JPanel(new BorderLayout());
		p2.setBorder(new EmptyBorder(10, 10, 10, 10));
		p2.add(frontPanel, BorderLayout.NORTH);
		splitLeftRight.setLeftComponent(p2);

		// Deployment Tree
		deploymentInfoPanel = new JPanel(new BorderLayout());
		deploymentInfoPanel.setBorder(new CompoundBorder(new EmptyBorder(5, 7, 5, 7), new TitledBorder(LineBorder
				.createBlackLineBorder(), " Deployment Info ")));

		deployTree = new DeploymentTree(controller.deploymentTreeControllerImpl);
		JPanel addToDeployTree = new AddToDeployTree(this, deployTree);

		deploymentInfoPanel.add(addToDeployTree, BorderLayout.NORTH);
		deploymentInfoPanel.add(new JScrollPane(deployTree), BorderLayout.CENTER);
		splitLeftRight.setRightComponent(deploymentInfoPanel);

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

//		JLabel version = new JLabel(controller.version());
//		version.setForeground(COLOR_LogoForeground);
//		version.setBorder(new EmptyBorder(0, 0, 0, 4));
//		JPanel pnl2 = new JPanel(new BorderLayout());
//		pnl2.setOpaque(false);
//		pnl2.add(version, BorderLayout.SOUTH);
//		logoPanel.add(pnl2, BorderLayout.EAST);

		menuBar = new JMenuBar();
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
//				JMenu sshMode = new JMenu("SSH Library");
//				sshMode.add(new ActionSetSshMode("Platform-independent", false, false));
//				sshMode.add(new ActionSetSshMode("Natively installed ssh", true, true));
//				extrasMenu.add(sshMode);
//				extrasMenu.add(new JSeparator());

			JMenu extraTools = new JMenu("Tools Menu");
			extraTools.add(new ActionShowExtraTools("View..."));
			extraTools.add(new ActionInstallExtraTools("Replace..."));
			extrasMenu.add(extraTools);

			JMenu builtinTools = new JMenu("Acs Scripts");
			builtinTools.add(new ActionShowBuiltinTools("View..."));
			builtinTools.add(new ActionLoadBuiltinTools("Replace..."));
			extrasMenu.add(builtinTools);
		}
		extrasMenu.add(new JSeparator());
		extrasMenu.add(new ActionShowVariables("Variables..."));
		
		menuBar.add(extrasMenu);
		
		// ---
		
		JMenuItem item;
		JMenu helpMenu = new JMenu("Help");
		helpMenu.setMnemonic(KeyEvent.VK_H);
		item = helpMenu.add(new ActionShowHelp("Online Help"));
		item.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0));
		item = helpMenu.add(new ActionShowAbout("About"));
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

		splitTopBottom = new JSplitPane(JSplitPane.VERTICAL_SPLIT, splitLeftRight, feedbackTabs);
		splitTopBottom.setOneTouchExpandable(true);


		// --- 
		// 2009-04: Introducing a desktop layout so i can make the
		// progress dialog a lightweight window on top the front panel

		AccInternalFrame bigInternalFrame = new AccInternalFrame();
		bigInternalFrame.add(splitTopBottom);
		desktop = new JDesktopPane();
		bigInternalFrame.setVisible(true);
		desktop.add(bigInternalFrame);
		frame.getContentPane().add(desktop, BorderLayout.CENTER);
		try {
			bigInternalFrame.setSelected(true);
			bigInternalFrame.setMaximum(true);
		} catch (PropertyVetoException exc) {}

		// for mysterious swing reasons, the desktop has a preferred size
		// of (1,1) instead of picking up the preferred size of its child
		// component, so i'm doing this manually here.
		desktop.setPreferredSize(bigInternalFrame.getPreferredSize());

		doFrameTitle();

	}


	public void go (boolean admincMode) {

		if (admincMode) {
			//	in adminclient mode, we run with a stripped GUI  
			menuBar.setVisible(false);
			splitLeftRight.getLeftComponent().setVisible(false);
			splitTopBottom.getBottomComponent().setVisible(false);
		} else {
			splitTopBottom.validate();
			splitLeftRight.setDividerLocation((int) (frame.getWidth() - deploymentInfoPanel.getPreferredSize().width * 1.1));
		}
		
		frame.pack();
		if (controller.startupOptions.geometry != null) {
			frame.setBounds(controller.startupOptions.geometry);
		}
		
		frame.setVisible(true);
	}

	/** Disposes the frame. */
	public void stop () {
		frame.setVisible(false);
		frame.dispose();
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
			deployTree.shieldedAddManager(AcsLocations.convertToManagerLocation(host, port));

		} catch (Exception exc) {
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
			String managerLoc = AcsLocations.convertToManagerLocation(host, port);
			boolean ok = deployTree.removeManager(managerLoc, true);
			if (!ok) {
				log.finer("Couldn't remove manager from deployment view: no such manager known: " + host + "," + port);
			}

		} catch (Exception exc) {
			log.log(Level.FINER, "Tried to remove (" + host + "," + port + ") manager from deployment view, failed", exc);
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
		ErrorBox.showErrorDialog(frame,
				"Variable has been used but is undefined: "+var,
				"Go to the Variables Editor now and define variable '"+var+"'.\n\n" +
				"Tip: Define a Java system property '"+var+"' before running\n" +
				"Acs Command Center. E.g., on the command line you'd do\n" +
				"export JAVA_OPTIONS=\"$JAVA_OPTIONS -D"+var+"=YourValue\"");
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
		
		Map<String, Object>[] m = controller.giveVariableMapsForGui();
		pnlEditVariables.preShow(m[0], m[1], m[2]);
		
		dlgEditVariables.bringUp(); // dialog is modal
		
		if (dlgEditVariables.okChosen) {
			pnlEditVariables.afterOk();
		}

	}
	
	


	// writing Model to Gui and vice versa
	// =====================================================


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

	protected EditContainerSettingsDialog dlgContainerSettings;

	protected void showContainerSettingsDialog () {

		// update data in dialog
		int contIndex = controller.project.getContainers().getSelect();
		ContainerT cont = controller.project.getContainers().getContainer(contIndex);
		writeModelToContainerSettingsDialog(cont);

		// show dialog
		dlgContainerSettings.bringUp();

		// if dialog cancelled or closed, we're done
		if (!dlgContainerSettings.okChosen)
			return;

		// read out data from dialog
		writeContainerSettingsDialogToModel(cont);

		// the dialog and the frontpanel share some data, so we should update
		// the frontpanel now. we could update it as a a whole but that seems
		// overly expensive, so we just update the affected containerline.
		ContainerLine contline = frontPanel.containerLines.get(contIndex);
		// for host, we have an additional complication: we want to show what
		// host is going to be used. this may be the same as field "remoteHost"
		// or it may be the host from the Common Settings. for the latter case
		// i consider it nicer to not display any host name.
		String host = (cont.getUseDedicatedSettings())? cont.getRemoteHost() : null;
		contline.populate(cont.getName(), cont.getType(), host, true);
	}


	protected void writeModelToContainerSettingsDialog (ContainerT cont) {

		String modif = MiscUtils.join(cont.getTypeModifier());
		dlgContainerSettings.modifF.setText(modif);
		dlgContainerSettings.heapF.setText(cont.getHeapSizeMB());

		dlgContainerSettings.defaultHostF.setText(controller.project.getRemoteHost());
		dlgContainerSettings.defaultAccountF.setText(controller.project.getRemoteAccount());
		dlgContainerSettings.defaultPasswordF.setText(controller.project.getRemotePassword());

		dlgContainerSettings.hostF.setText(cont.getRemoteHost());
		dlgContainerSettings.accountF.setText(cont.getRemoteAccount());
		dlgContainerSettings.passwordF.setText(cont.getRemotePassword());

		dlgContainerSettings.btnCustom.setSelected(cont.getUseDedicatedSettings());
		dlgContainerSettings.btnGlobal.setSelected(!cont.getUseDedicatedSettings());
	}

	
	protected void writeContainerSettingsDialogToModel (ContainerT cont) {

		cont.setTypeModifier(MiscUtils.split(dlgContainerSettings.modifF.getText().trim()));
		cont.setHeapSizeMB(dlgContainerSettings.heapF.getText().trim());

		// msc 2010-10: avoid cross-talk between instances:
		// acs instance of a container is no longer freely choosable.
		cont.setScriptBase(controller.project.getScriptBase());
		cont.setRemoteHost(dlgContainerSettings.hostF.getText().trim());
		cont.setRemoteAccount(dlgContainerSettings.accountF.getText().trim());
		cont.setRemotePassword(dlgContainerSettings.passwordF.getText().trim());

		cont.setUseDedicatedSettings(!dlgContainerSettings.btnGlobal.isSelected());
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

			if (frontPanel.chkRemoteDaemons.isSelected()) // Acs8.0.1 : store remote-variant in project
				controller.project.setMode(ModeType.REMOTE_DAEMON);
			else
			if (frontPanel.chkRemoteNative.isSelected())
				controller.project.setMode(ModeType.REMOTE_NATIVE);
			else
			controller.project.setMode(ModeType.REMOTE);
		} 

		// read container definitions and see which
		// of the container-RadioButtons is checked
		for (int i = 0; i < frontPanel.containerLines.size(); i++) {
			TabPanel.ContainerLine c = (TabPanel.ContainerLine) frontPanel.containerLines.elementAt(i);
			ContainerT cont = controller.project.getContainers().getContainer(i);

			if (cont == null) {
				log.fine("PROBLEM: number of containers in model and gui is out-of-sync");
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
			frontPanel.chkRemoteBuiltin.setSelected(true);
			frontPanel.chkRemoteDaemons.setSelected(false);
			frontPanel.chkRemoteNative.setSelected(false);
		} else
		if (mode.equals(ModeType.REMOTE_DAEMON)) { // Acs8.0.1 : store remote-variant in project
			frontPanel.chkRemoteScript.setSelected(true);
			frontPanel.chkRemoteBuiltin.setSelected(false);
			frontPanel.chkRemoteDaemons.setSelected(true);
			frontPanel.chkRemoteNative.setSelected(false);
		} else
		if (mode.equals(ModeType.REMOTE_NATIVE)) { // Acs8.0.1 : store remote-variant in project
			frontPanel.chkRemoteScript.setSelected(true);
			frontPanel.chkRemoteBuiltin.setSelected(false);
			frontPanel.chkRemoteDaemons.setSelected(false);
			frontPanel.chkRemoteNative.setSelected(true);
		} else
		if (mode.equals(ModeType.JAVA)) {
			frontPanel.chkLocalScript.setSelected(true); // Acs8.0: remove local java
		}

		// remove all existing, and re-add from project
		// ---------------------------------------------
		while (frontPanel.containerLines.size() > 0)
			frontPanel.lessContainerLines();
		
		for (int i = 0; i < controller.project.getContainers().getContainerCount(); i++) {
			ContainerT cont = controller.project.getContainers().getContainer(i);
			ContainerLine contline = frontPanel.addEmptyContainerLine();
			
			String host = (cont.getUseDedicatedSettings())? cont.getRemoteHost() : null;
			contline.populate(cont.getName(), cont.getType(), host, (i == controller.project.getContainers().getSelect()));
		}
		
		frontPanel.disenabler.actionPerformed(null);
	}


	// ==========================================================================================



	// makes a dialog appear above the main-frame on first show-up
	public void correctDialogLocation (JDialog d) {
		Point p = d.getLocation();
		if (p.x == 0 && p.y == 0) {
			d.setLocationRelativeTo(d.getParent());
		}
	}


	protected Border createTitledBorder (String title) {
		Border ret = new CompoundBorder(new EmptyBorder(5, 7, 5, 7), new TitledBorder(LineBorder.createBlackLineBorder(), title));
		return ret;
	}

	
	// Html Browsing
	// ===========================================================================
	
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
			ErrorBox.showMessageDialog(frame, "Cannot show the resource: " + exc, true);
		}
	}

	// Help
	// ===========================================================================
	
	protected DefaultHelpBroker helpBroker;
	
	protected void showHelpBrowser() {

		if (helpBroker == null) {
		
			HelpSet helpSet = controller.getHelpSet();
			if (helpSet == null) {
				ErrorBox.showMessageDialog(frame, "Online Help could not be loaded", true);
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


	
	// Tools
	// ===========================================================================
	
	/** for book-keeping, to allow later removal of actions from the tools menu */
	protected Vector<JMenuItem> addedToolMenuItems = new Vector<JMenuItem>();

	/**
	 * Adds an entry to the tools menu, on activation an input dialog is shown and
	 * evaluated. Finally the specified listener is triggered.
	 */
	public void addExtraTool (Tool tool, final HashMap<String, Object> result, final ExecuteTools.ToolStarter ts) {

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

				NativeCommand.Listener listener = giveOutputListener(tabCaption);
				
				try {
					ts.start(listener);
				
				} catch (UnresolvableException exc) {
					String var = exc.getVariableName();
					controller.handleUnresolvableVariable(var);
					log.log(Level.INFO, "Couldn't start tool '"+cap+"': Variable '"+var+"' is undefined");
					
				} catch (Throwable t) {
					log.info("Couldn't start tool '"+cap+"', check definition file; reason was: " + t);
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


	
	
	// ################################################################
	// Actions
	// ################################################################


	/**
	 * Performs the work to be done within the event-dispatcher thread
	 */
	protected abstract class SwingAction extends AbstractAction {

		protected SwingAction(String name) {
			super(name);
		}

		final public void actionPerformed (ActionEvent e) {
			try {
				actionPerformed();

			} catch (Exception exc) {
				String name = "'"+getValue(Action.NAME)+"'";
				ErrorBox.showErrorDialog(frame, "Encountered an error while performing "+name, exc);
				log.log(Level.INFO, "Error while performing "+name, exc);
			}
		}

		protected abstract void actionPerformed () throws Exception;
	}

	
	/** Runs the response to the action in its own thread */
	protected abstract class BackgroundAction extends AbstractAction {

		protected BackgroundAction(String name) {
			super(name);
		}

		protected BackgroundAction(String name, Icon icon) {
			super(name, icon);
		}

		public void actionPerformed (ActionEvent evt) {
			controller.runBackground(new Runnable() {

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
						ErrorBox.showErrorDialog(frame, "Encountered an error while performing "+name, t);
						log.log(Level.INFO, "Error while performing "+name, t);
					}
				};
			});
		}

		protected abstract void actionPerformed () throws Throwable;
	}

	protected class ActionNewProject extends SwingAction {

		public ActionNewProject(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () {
			controller.project = controller.createProject();
			currentProjectFile = null;
			currentProjectChanged();
		}
	}

	protected class ActionShowAbout extends SwingAction {

		protected ActionShowAbout(String name) {
			super(name);
		}

		@Override
		protected void actionPerformed () throws Exception {
			String title = "About: Acs Command Center";
			String msg = "This Acs Command Center creates projects of version: "+controller.projectCreatorId();
			JOptionPane.showInternalMessageDialog(desktop, msg, title, JOptionPane.PLAIN_MESSAGE);
		}
	}

	protected class ActionShowHelp extends SwingAction {

		public ActionShowHelp(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () {
			showHelpBrowser();
		}
	}

	protected class ActionOpenProject extends BackgroundAction {

		public ActionOpenProject(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () throws Exception {
			File f = showOpenDialog();
			if (f == null)
				return;

			controller.loadProject(f);
			
		}
	}

	protected class ActionSaveProject extends BackgroundAction {

		public ActionSaveProject(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () throws Exception {
			// as i learn from the OT, focus-listening is
			// not 100% reliable, so it might be wise to enforce
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

	protected class ActionSaveAsProject extends BackgroundAction {

		public ActionSaveAsProject(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () throws Exception {
			File f = showSaveDialog();
			if (f == null)
				return;
			controller.writeProject(controller.project, f);
			// do not change currentProjectFile setting here as most apps would
			// (i hate when they do that)
		}
	}

	protected class ActionExit extends BackgroundAction {

		public ActionExit(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () {
			controller.stop();
		}
	}

	protected class ActionConfigureTools extends SwingAction {

		public ActionConfigureTools(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () {
			writeModelToManagerLocationForTools();

			managerLocationDialog1.bringUp(); // dialog is modal

			boolean ok = managerLocationDialog1.okChosen;
			if (ok) {
				writeManagerLocationForToolsToModel();
			}
		}
	}

	protected class ActionEditCommands extends BackgroundAction {

		public ActionEditCommands(String name) {
			super(name);
		}

		@Override
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

	protected class ActionEditPexpects extends BackgroundAction {

		public ActionEditPexpects(String name) {
			super(name);
		}

		@Override
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

	protected class ActionInstallExtraTools extends BackgroundAction {

		public ActionInstallExtraTools(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () {
			// show file dialog
			File f = null;
			JFileChooser c = new JFileChooser();
			c.setFileFilter(new FileFilter() {

				@Override
				public boolean accept (File f) {
					return (f.isDirectory() || f.getName().endsWith(".xml") || f.getName().endsWith(".XML"));
				}

				@Override
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
				controller.installExtraTools(f.toURI().toURL());
			} catch (Exception exc) {
				ErrorBox.showMessageDialog(frame, "The tools could not be installed. Check console for error output. ", true);
				log.fine("could not install tools: " + exc);
				return;
			}

			ErrorBox.showMessageDialog(frame, "The tools have been installed successfully. ", false);
		}
	}

	protected class ActionShowExtraTools extends BackgroundAction {

		public ActionShowExtraTools(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () {
			showUrlContent(controller.currentExtraToolsUrl, "Tools");
		}
	}

	protected class ActionShowBuiltinTools extends BackgroundAction {

		public ActionShowBuiltinTools(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () {
			showUrlContent(controller.latestBuiltinToolsUrl, "Built-in Tools");
		}
	}

	protected class ActionLoadBuiltinTools extends BackgroundAction {

		public ActionLoadBuiltinTools(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () {
			// show file dialog
			File f = null;
			JFileChooser c = new JFileChooser();
			c.setFileFilter(new FileFilter() {

				@Override
				public boolean accept (File f) {
					return (f.isDirectory() || f.getName().endsWith(".xml") || f.getName().endsWith(".XML"));
				}

				@Override
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
				URL url = f.toURI().toURL();
				controller.loadBuiltinTools(url);
			} catch (Exception exc) {
				JOptionPane.showMessageDialog(frame, "The built-in tools could not be loaded. Check console for error output. ");
				log.fine("could not load built-in tools: " + exc);
				return;
			}

			JOptionPane.showMessageDialog(frame, "The built-in tools have been loaded. ");
		}
	}

	protected class ActionShowVariables extends BackgroundAction {

		public ActionShowVariables(String name) {
			super(name);
		}

		@Override
		public void actionPerformed () {
			showVariablesEditor();
		}
	}

	
	// ################################################################
	// Listening to Processes
	// ################################################################

	
	/**
	 * The outputlistener just writes a task's output to a feedback area, nothing more and
	 * nothing less. this is sufficient when running tools like the admin client.
	 */
	protected NativeCommand.Listener giveOutputListener (String taskName) {
		NativeCommand.Listener ret = feedbackTabs.viewTab(taskName);
		return ret;
	}

	/**
	 * The outputlistener2 is like the standard one, plus it scans the output for automatic
	 * acs_instance assignments. this is needed when running services or a manager.
	 */
	protected NativeCommand.Listener giveOutputListener2 (String taskName) {
		NativeCommand.Listener first = giveOutputListener(taskName);
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
	protected static final Pattern p = Pattern.compile(".*the ACS_INSTANCE, (.+), is currently in use.*");

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
				controller.runBackground(new Runnable() {
					public void run () {
						String message = "The Acs Instance you requested ( " + desiredAcsInstance + " ) is already" +
								" in use (by you or somebody else)." +
								"\nChoose a different Acs Instance in the Common Settings.";
						JOptionPane.showMessageDialog(frame, message, "Acs Instance in use", JOptionPane.INFORMATION_MESSAGE);
					}
				});
			}
		}
	}

	
	// Icons
	// ===========================================================================
		
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


	// =========================================================================

	/**
	 * An internal frame without decorations.
	 */
	protected static class AccInternalFrame extends JInternalFrame {
		AccInternalFrame() {
			super (null);
			setUI(new AccInternalFrameUI(this));
			setBorder(new EmptyBorder(0,0,0,0));
		}
	}

	/**
	 * Helper class 
	 */
	protected static class AccInternalFrameUI extends BasicInternalFrameUI {
		AccInternalFrameUI (JInternalFrame frame) {
			super (frame);
		}
		@Override
		protected JComponent createNorthPane(JInternalFrame frame) {
			return null;
		}
	}

	
	
}