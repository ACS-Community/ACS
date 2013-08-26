/*
 * Created on Oct 21, 2003 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.beans.PropertyVetoException;
import java.lang.reflect.InvocationTargetException;
import java.util.Vector;
import java.util.WeakHashMap;

import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPasswordField;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.SpringLayout;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;

import alma.acs.commandcenter.engine.Executor;
import alma.acs.commandcenter.engine.RunModel;
import alma.acs.commandcenter.gui.CommandCenterGui.BackgroundAction;
import alma.acs.commandcenter.gui.thirdparty.SpringUtilities;
import alma.acs.commandcenter.trace.DefaultChecklistPanel;
import alma.acs.commandcenter.trace.Flow;
import alma.acs.commandcenter.trace.FlowListener;
import alma.entity.xmlbinding.acscommandcenterproject.ContainerT;
import alma.entity.xmlbinding.acscommandcenterproject.ContainersT;
import alma.entity.xmlbinding.acscommandcenterproject.types.ModeType;

/**
 * @author mschilli
 */
public class TabPanel extends JPanel {


	CommandCenterGui master;
	DefaultChecklistPanel remoteFlowPanel;
	DefaultChecklistPanel localJavaFlowPanel;
	DefaultChecklistPanel localScriptFlowPanel;
	DefaultChecklistPanel singleStepFlowPanel;
	DefaultChecklistPanel remoteServicesDaemonFlowPanel;
	DefaultChecklistPanel remoteContainerDaemonFlowPanel;

	BackgroundAction actStartAcs;
	BackgroundAction actStopAcs;
	BackgroundAction actKillAcs;
	BackgroundAction actStartServices;
	BackgroundAction actStopServices;
	BackgroundAction actStartManager;
	BackgroundAction actStopManager;
	BackgroundAction actShowAdvanced;
	BackgroundAction actStartContainer;
	BackgroundAction actStopContainer;
	BackgroundAction actConfigureContainer;
	BackgroundAction actStartAllContainers;
	BackgroundAction actStopAllContainers;

	FlowDialog flowDialog;
	FixSizeScrollPane scp;
	JPanel controlsPanel;
	JPanel buttonPanel;
	JPanel panelAdvanced;
	JPanel buttonPanelAdvanced;
	JToggleButton btnShowAdvanced;
	JButton btnStartServices;
	JButton btnStopServices;
	JButton btnStartManager;
	JButton btnStopManager;
	JButton btnKillAcs;
	JButton btnStopAcs;
	JButton btnStartAcs;
	JButton btnStartAllContainers;
	JButton btnStopAllContainers;
	JButton btnMoreContainers;
	JButton btnLessContainers;
	JButton btnContainersAgainstManager;
	JButton btnMoveContainerUp;
	JButton btnMoveContainerDown;

	MyRadioButton chkLocalScript = new MyRadioButton("Localhost (single-machine project)");
	MyRadioButton chkRemoteScript = new MyRadioButton("Remote (distributed project)");
	MyRadioButton chkRemoteBuiltin = new MyRadioButton("Use built-in ssh");
	MyRadioButton chkRemoteDaemons = new MyRadioButton("Use Acs Daemons");
	MyRadioButton chkRemoteNative = new MyRadioButton("Use native ssh");

	JPanel containerPanel;
	JPanel containerLinePanel;
	ButtonGroup buttonGroup1 = new ButtonGroup(); // the radiobuttons for the mode 
	ButtonGroup buttonGroup3 = new ButtonGroup(); // the connection types for remote mode
	ButtonGroup buttonGroup2 = new ButtonGroup(); // for radiobuttons inside containerpanel

	JLabel acsinstanceL, hostL, accountL, passwordL, cdbrootL, lblF, lblG;
	JTextField acsinstanceF = new JTextField(5);
	JTextField hostF = new JTextField(20);
	JTextField accountF = new JTextField(10);
	JTextField passwordF = new JPasswordField(10);
	JTextField cdbrootF = new JTextField(20);

	FocusListener focusListener = new MyFocusListener();


	protected TabPanel(CommandCenterGui master) {
		this.master = master;

		flowDialog = new FlowDialog();

		Icon okIcon = master.icons.getOkIcon();
		Icon errIcon = master.icons.getErrIcon();

		remoteFlowPanel = new DefaultChecklistPanel(Executor.remoteFlow, null, null, null, okIcon, null, errIcon);
		localJavaFlowPanel = new DefaultChecklistPanel(Executor.localInProcFlow, null, null, null, okIcon, null, errIcon);
		localScriptFlowPanel = new DefaultChecklistPanel(Executor.localOutProcFlow, null, null, null, okIcon, null, errIcon);
		singleStepFlowPanel = new DefaultChecklistPanel(Executor.singleStepFlow, null, null, null, okIcon, null, errIcon);
		remoteServicesDaemonFlowPanel = new DefaultChecklistPanel(Executor.remoteServicesDaemonFlow, null, null, null, okIcon, null, errIcon);
		remoteContainerDaemonFlowPanel = new DefaultChecklistPanel(Executor.remoteContainerDaemonFlow, null, null, null, okIcon, null, errIcon);
		init();
	}
	
	protected void init () {

		buttonGroup1.add(chkLocalScript);
		buttonGroup1.add(chkRemoteScript);

		chkLocalScript.setToolTipText("Run all of Acs on a single machine (localhost)");
		chkRemoteScript.setToolTipText("Run a distributed Acs on multiple hosts");

		chkLocalScript.setName("chk_Local");
		chkRemoteScript.setName("chk_Remote");
		
		chkLocalScript.setMnemonic(KeyEvent.VK_L);
		chkRemoteScript.setMnemonic(KeyEvent.VK_R);
		
		btnMoreContainers = new JButton(new ActionMoreContainers());
		btnLessContainers = new JButton(new ActionLessContainers());
		btnContainersAgainstManager = new JButton(new ActionConfigureAllContainers());
		btnMoveContainerUp = new JButton(new ActionMoveContainerUp());
		btnMoveContainerDown = new JButton(new ActionMoveContainerDown());
		
		Insets margin = new Insets(1, 0, 1, 0);
		btnMoreContainers.setMargin(margin);
		btnLessContainers.setMargin(margin);
		btnContainersAgainstManager.setMargin(margin);
		btnMoveContainerUp.setMargin(margin);
		btnMoveContainerDown.setMargin(margin);
		
		this.setLayout(new BorderLayout());

		// -------------------------------------------------------------------
		// general settings
		// -------------------------------------------------------------------

		JPanel generalTab = new JPanel(new BorderLayout());
		JPanel n = new JPanel(new SpringLayout());

		n.add(acsinstanceL = new JLabel("Acs Instance"));
		n.add(acsinstanceF);
		acsinstanceL.setLabelFor(acsinstanceF);
		acsinstanceF.addFocusListener(focusListener);
		acsinstanceF.setToolTipText("The desired Acs instance between 0 and 9)");
		acsinstanceL.setDisplayedMnemonic(KeyEvent.VK_I);

		n.add(cdbrootL = new JLabel("Cdb Root Dir"));
		n.add(cdbrootF);
		cdbrootL.setLabelFor(cdbrootF);
		cdbrootF.addFocusListener(focusListener);
		cdbrootL.setToolTipText("The Cdb describing all components");

		SpringUtilities.makeCompactGrid(n, 2, 0);
		generalTab.add(n);

		acsinstanceF.setName("txt_AcsInstance");
		cdbrootF.setName("txt_CdbRoot");
		
		
		// -------------------------------------------------------------------
		// local script settings
		// -------------------------------------------------------------------

		JPanel localScriptTab = new JPanel(new BorderLayout());
		JPanel h = new JPanel(new BorderLayout());
		h.add(chkLocalScript, BorderLayout.NORTH);

		chkLocalScript.addFocusListener(focusListener);
		localScriptTab.add(h);


		// -------------------------------------------------------------------
		// remote settings
		// -------------------------------------------------------------------

		JPanel remoteTab = new JPanel(new BorderLayout());
		JPanel j = new JPanel(new BorderLayout());
		j.add(chkRemoteScript, BorderLayout.NORTH);

		JPanel k = new JPanel(new GridBagLayout());
		
		buttonGroup3.add(chkRemoteBuiltin);
		buttonGroup3.add(chkRemoteNative);
		buttonGroup3.add(chkRemoteDaemons);

		chkRemoteBuiltin.setToolTipText("Run Acs using built-in ssh client");
		k.add(chkRemoteBuiltin, gridbagpos(0,0).width(2).gapy(1));

		chkRemoteNative.setToolTipText("Run Acs using the local ssh program");
		k.add(chkRemoteNative, gridbagpos(0,2).width(2).gapy(1));
		
		chkRemoteDaemons.setToolTipText("Run Acs using Daemons");
		k.add(chkRemoteDaemons, gridbagpos(1,0).width(2).gapy(1));

		k.add(hostL = new JLabel("Host"), gridbagpos(2,0));
		k.add(hostF, gridbagpos(2,1).width(3));
		chkRemoteScript.addFocusListener(focusListener);
		chkRemoteBuiltin.addFocusListener(focusListener);
		chkRemoteNative.addFocusListener(focusListener);
		chkRemoteDaemons.addFocusListener(focusListener);
		hostF.addFocusListener(focusListener);
		
		k.add(accountL = new JLabel("User"), gridbagpos(3,0));
		k.add(accountF, gridbagpos(3,1));
		accountL.setLabelFor(accountF);
		accountF.addFocusListener(focusListener);

		k.add(passwordL = new JLabel("Pwd"), gridbagpos(3,2));
		k.add(passwordF, gridbagpos(3,3).weightx(0.2));
		passwordL.setLabelFor(passwordF);
		passwordF.addFocusListener(focusListener);

		k.setBorder(new EmptyBorder(0,30,0,5));
		j.add(k, BorderLayout.CENTER);
		
		remoteTab.add(j);

		
		hostF.setName("txt_RemoteHost");
		accountF.setName("txt_RemoteUser");
		passwordF.setName("txt_RemotePassword");
		
		// -------------------------------------------------------------------
		// buttons / actions
		// -------------------------------------------------------------------
		
		actStartContainer = new ActionStartContainer();
		actStopContainer = new ActionStopContainer();
		actConfigureContainer = new ActionConfigureContainer();

		btnStartAcs = new JButton(actStartAcs = new ActionStartAcs());
		btnStopAcs = new JButton(actStopAcs = new ActionStopAcs());
		btnKillAcs = new JButton(actKillAcs = new ActionKillAcs());
		btnStartServices = new JButton(actStartServices = new ActionStartServices());
		btnStopServices = new JButton(actStopServices = new ActionStopServices());
		btnStartManager = new JButton(actStartManager = new ActionStartManager());
		btnStopManager = new JButton(actStopManager = new ActionStopManager());
		btnShowAdvanced = new MyCheckBox(actShowAdvanced = new ActionShowAdvanced());
		btnStartAllContainers = new JButton(actStartAllContainers = new ActionStartAllContainers());
		btnStopAllContainers = new JButton(actStopAllContainers = new ActionStopAllContainers());
		btnStartServices.setToolTipText("Start Services with the specified Common Settings");
		btnStopServices.setToolTipText("Stop the specified Services");
		btnStartManager.setToolTipText("Start a Manager with the specified Common Settings");
		btnStopManager.setToolTipText("Stop the specified Manager");
		btnStopAcs.setToolTipText("Stop Services and Manager AND Containers");
		btnKillAcs.setToolTipText("Terminate Everything related to Acs");
		btnStartAcs.setToolTipText("Start Services and Manager with the specified Common Settings");
		btnShowAdvanced.setToolTipText("Enable/Disable Advanced Controls");

		btnStartServices.setName("btn_Start_Services");
		btnStopServices.setName("btn_Stop_Services");
		btnStartManager.setName("btn_Start_Manager");
		btnStopManager.setName("btn_Stop_Manager");
		btnStopAcs.setName("btn_Stop_Acs");
		btnKillAcs.setName("btn_Kill_Acs");
		btnStartAcs.setName("btn_Start_Acs");
		btnShowAdvanced.setName("btn_Show_Advanced");
		
		btnStartAcs.setMnemonic(KeyEvent.VK_A);
		btnStopAcs.setMnemonic(KeyEvent.VK_S);
		btnKillAcs.setMnemonic(KeyEvent.VK_K);
		btnShowAdvanced.setMnemonic(KeyEvent.VK_V);

		margin = new Insets(1, 1, 1, 1);
		btnStartServices.setMargin(margin);
		btnStopServices.setMargin(margin);
		btnStartManager.setMargin(margin);
		btnStopManager.setMargin(margin);
		btnShowAdvanced.setMargin(margin);

		margin = new Insets(1, 3, 1, 3);
		btnStartAllContainers.setMargin(margin);
		btnStopAllContainers.setMargin(margin);


		// -------------------------------------------------------------------
		// control section
		// -------------------------------------------------------------------

		controlsPanel = new JPanel();
		controlsPanel.setBorder(master.createTitledBorder(" Acs Suite "));

		buttonPanel = new JPanel(new SpringLayout());

		buttonPanel.add(btnStartAcs);
		flowDialog.disenable(btnStartAcs);
		
		buttonPanel.add(btnStopAcs);
		flowDialog.disenable(btnStopAcs);

		buttonPanel.add(Box.createVerticalStrut(1));
		buttonPanel.add(new FixSizeSeparator(SwingConstants.HORIZONTAL, new Dimension(10, 3)));

		buttonPanel.add(btnKillAcs);
		flowDialog.disenable(btnKillAcs);

		buttonPanel.add(btnShowAdvanced);
		flowDialog.disenable(btnShowAdvanced);
		
		SpringUtilities.makeCompactGrid(buttonPanel, 0, 1);

		// -------------------------------------------------------------------
		// advanced section
		// -------------------------------------------------------------------

		panelAdvanced = new JPanel(new BorderLayout());
		buttonPanelAdvanced = new JPanel(new SpringLayout());

		buttonPanelAdvanced.add(new JLabel("Srv"));
		buttonPanelAdvanced.add(btnStartServices);
		flowDialog.disenable(btnStartServices);
		buttonPanelAdvanced.add(btnStopServices);
		flowDialog.disenable(btnStopServices);

		buttonPanelAdvanced.add(new JLabel("Mgr"));
		buttonPanelAdvanced.add(btnStartManager);
		flowDialog.disenable(btnStartManager);
		buttonPanelAdvanced.add(btnStopManager);
		flowDialog.disenable(btnStopManager);

		SpringUtilities.makeCompactGrid(buttonPanelAdvanced, 2, 0);
		panelAdvanced.add(buttonPanelAdvanced, BorderLayout.CENTER);

		controlsPanel.setLayout(new BoxLayout(controlsPanel, BoxLayout.Y_AXIS));
		controlsPanel.add(buttonPanel);
		controlsPanel.add(panelAdvanced);

		makeButtonPair(btnStartAcs, btnStopAcs);
		makeButtonPair(btnStartServices, btnStopServices);
		makeButtonPair(btnStartManager, btnStopManager);


		// -------------------------------------------------------------------
		// container section
		// -------------------------------------------------------------------

		containerPanel = new JPanel(new BorderLayout());
		containerPanel.setBorder(master.createTitledBorder(" Containers "));

		JPanel a = new JPanel(new GridLayout(1, 0));
		a.add(new JLabel("Name", JLabel.CENTER));
		a.add(new JLabel("Type", JLabel.CENTER));
		a.add(new JLabel("Remote Host", JLabel.CENTER));
		a.add(new JLabel());
		containerPanel.add(a, BorderLayout.NORTH);

		containerLinePanel = new JPanel();
		JPanel q = new JPanel(new SpringLayout());
		JPanel scpRoot = new JPanel(new BorderLayout());
		containerLinePanel.setLayout(new GridLayout(0, 1));
		scpRoot.add(containerLinePanel, BorderLayout.NORTH);
		q.add(scpRoot);
		SpringUtilities.makeCompactGrid(q, 1, 1);
		scp = new FixSizeScrollPane(q, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

		containerPanel.add(scp, BorderLayout.CENTER);

		// freeze the size of the scrollpane to n containerlines
		ContainerLine filler = new ContainerLine();
		scp.validate();
		int scrollbarWidth = 25; // can't find out this value before scrollpane is visible on screen
		int nLines = 5; // number of lines
		scp.freezeSize(filler.getPreferredSize().width + scrollbarWidth,
				         (filler.getPreferredSize().height + 2) * nLines + 2); // <-- don't ask...


		btnMoreContainers.setToolTipText("More Containers");
		btnLessContainers.setToolTipText("Less Containers (removes Last from List)");
		btnContainersAgainstManager.setToolTipText("Choose Manager and Services to Run Containers Against");
		btnMoveContainerUp.setToolTipText("Move Selected Container Up in List");
		btnMoveContainerDown.setToolTipText("Move Selected Container Down in List");
		btnStartAllContainers.setToolTipText("Start all Containers");
		btnStopAllContainers.setToolTipText("Stop all Containers");

		btnMoreContainers.setName("btn_More_Containers");
		btnLessContainers.setName("btn_Less_Containers");
		btnContainersAgainstManager.setName("btn_Containers_Against_Manager");
		btnMoveContainerUp.setName("btn_Move_Container_Up");
		btnMoveContainerDown.setName("btn_Move_Container_Down");
		btnStartAllContainers.setName("btn_Start_All_Containers");
		btnStopAllContainers.setName("btn_Stop_All_Containers");
		
		btnMoreContainers.setMnemonic(KeyEvent.VK_DOWN);
		btnLessContainers.setMnemonic(KeyEvent.VK_UP);
		btnContainersAgainstManager.setMnemonic(KeyEvent.VK_C);

		makeButtonPair(btnStartAllContainers, btnStopAllContainers);
		flowDialog.disenable(btnLessContainers);
		flowDialog.disenable(btnStartAllContainers);
		flowDialog.disenable(btnStopAllContainers);
		flowDialog.disenable(btnMoveContainerUp);
		flowDialog.disenable(btnMoveContainerDown);
		flowDialog.disenable(btnContainersAgainstManager);

		
		JPanel southwest = new JPanel();
		southwest.setLayout(new SpringLayout());
		southwest.add(btnMoreContainers);
		southwest.add(btnLessContainers);
		southwest.add(btnMoveContainerUp);
		southwest.add(btnMoveContainerDown);
		southwest.add(new FixSizeSeparator(SwingConstants.VERTICAL, new Dimension(4, 10)));
		southwest.add(btnContainersAgainstManager);
		SpringUtilities.makeCompactGrid(southwest, 1, 0);

		JPanel southeast = new JPanel(new SpringLayout());
		southeast.add(btnStartAllContainers);
		southeast.add(btnStopAllContainers);
		SpringUtilities.makeCompactGrid(southeast, 1, 0);
		
		
		JPanel south = new JPanel();
		south.setLayout(new BoxLayout(south, BoxLayout.X_AXIS));
		south.add(southwest);
		south.add(Box.createHorizontalGlue());
		south.add(southeast);
		
		containerPanel.add(south, BorderLayout.SOUTH);

		// -------------------------------------------------------------------
		// assemble / layout
		// -------------------------------------------------------------------

		Box commonSettingsPanel = new Box(BoxLayout.Y_AXIS);
		commonSettingsPanel.add(generalTab);
		commonSettingsPanel.add(Box.createVerticalStrut(10));
		commonSettingsPanel.add(localScriptTab);
		commonSettingsPanel.add(Box.createVerticalStrut(10));
		commonSettingsPanel.add(remoteTab);
		commonSettingsPanel.add(Box.createVerticalStrut(10));
		commonSettingsPanel.setBorder(master.createTitledBorder(" Common Settings "));
		JPanel tabbedPanelLeft = new JPanel(new BorderLayout());
		tabbedPanelLeft.add(commonSettingsPanel, BorderLayout.NORTH);		

		Box tabbedPane1Right = new Box(BoxLayout.Y_AXIS);
		tabbedPane1Right.add(controlsPanel);
		tabbedPane1Right.add(Box.createVerticalStrut(10));
		tabbedPane1Right.add(containerPanel);
		
		Box tabbedPane1 = Box.createHorizontalBox();
		tabbedPane1.add(tabbedPanelLeft);
		tabbedPane1.add(tabbedPane1Right);

		this.setLayout(new GridBagLayout());
		this.add(commonSettingsPanel, gridbagpos(0, 0).weightx(0.2).align(GridBag.NORTHEAST));
		this.add(controlsPanel, gridbagpos(0, 1).align(GridBag.NORTHEAST));
		this.add(containerPanel, gridbagpos(1, 0).width(2).align(GridBag.NORTHEAST));

		
		// -------------------------------------------------------------------
		// select some checkboxes, etc.
		// -------------------------------------------------------------------

		disenabler = new Disenabler();
		chkRemoteBuiltin.setSelected(true);
		chkLocalScript.setSelected(true);
		btnShowAdvanced.setSelected(false);
	}


	/** 
	 * Enables/Disables widgets according to the selected radiobuttons/checkboxes.
	 */ 
	protected Disenabler disenabler;

	protected class Disenabler implements ActionListener {

		Disenabler() {
			chkRemoteScript.addActionListener(this);
			chkLocalScript.addActionListener(this);
			chkRemoteBuiltin.addActionListener(this);
			chkRemoteNative.addActionListener(this);
			chkRemoteDaemons.addActionListener(this);
			actionPerformed(null);
		}

		public void actionPerformed (ActionEvent e) {
			/*System.out.println(e);*/

			boolean local = chkLocalScript.isSelected();
			boolean remote = chkRemoteScript.isSelected();
			boolean remoteBuiltin = remote && chkRemoteBuiltin.isSelected();
			boolean remoteNative = remote && chkRemoteNative.isSelected();
			boolean remoteDaemon = remote && chkRemoteDaemons.isSelected();

			chkRemoteBuiltin.setEnabled(remote);
			chkRemoteDaemons.setEnabled(remote);
			chkRemoteNative.setEnabled(remote);
			hostL.setEnabled(remote);
			hostF.setEnabled(remote);

			accountL.setEnabled(remoteBuiltin || remoteNative);
			accountF.setEnabled(remoteBuiltin || remoteNative);

			passwordL.setEnabled(remoteBuiltin);
			passwordF.setEnabled(remoteBuiltin);

			cdbrootF.setEnabled(local || remoteBuiltin || remoteNative);

			if (master.dlgContainerSettings != null) {
				master.dlgContainerSettings.btnCustom.setEnabled(remote);
				master.dlgContainerSettings.hostL.setEnabled(remote);
				master.dlgContainerSettings.hostF.setEnabled(remote);
				master.dlgContainerSettings.accountL.setEnabled(remoteBuiltin || remoteNative);
				master.dlgContainerSettings.accountF.setEnabled(remoteBuiltin || remoteNative);
				master.dlgContainerSettings.passwordL.setEnabled(remoteBuiltin);
				master.dlgContainerSettings.passwordF.setEnabled(remoteBuiltin);
				master.dlgContainerSettings.modifL.setEnabled(remoteDaemon);
				master.dlgContainerSettings.modifF.setEnabled(remoteDaemon);
				master.dlgContainerSettings.heapL.setEnabled(remoteDaemon);
				master.dlgContainerSettings.heapF.setEnabled(remoteDaemon);
			}

			for (ContainerLine contline : containerLines) {
				contline.btnConfigure.setEnabled(remote);
			}
			
		}
	}


	/**
	 * The progress dialog for long-running actions.
	 */
	protected class FlowDialog extends JInternalFrame implements FlowListener {

		DefaultChecklistPanel currentFlowUI; // the flow-ui currently held by this dialog
		JButton flowDialogOk;

		protected FlowDialog() {
			this.getContentPane().setLayout(new BorderLayout());
			this.setBorder(BorderFactory.createCompoundBorder(
				BorderFactory.createRaisedBevelBorder(),
				BorderFactory.createLoweredBevelBorder()
			));
			this.setFrameIcon(master.icons.getConfigIcon());

			flowDialogOk = new JButton("Close");
			flowDialogOk.addActionListener(new ActionListener() {
				public void actionPerformed (ActionEvent evt) {
					close();
				}
			});
			this.getRootPane().setDefaultButton(flowDialogOk); // TODO setDefaultButton doesn't work

			flowDialogOk.setName("btn_Close_FlowDialog");
		}

		/**
		 * Bring up the dialog (execution in swing thread guaranteed).
		 * @param title
		 * @param content
		 */
		protected void prepareShow (final String title, final DefaultChecklistPanel content) throws InterruptedException, InvocationTargetException {

			Runnable r = new Runnable(){
				public void run () {

					// prepare show
					// --------------

					setTitle(title+" - please wait");
//					JLabel label = new JLabel(title+" - Progress");
//					label.setBorder(BorderFactory.createEmptyBorder(3,3,3,3));
//					label.setBackground(UIManager.getColor(key));
					getContentPane().removeAll();
//					getContentPane().add(label, BorderLayout.NORTH);
					getContentPane().add(content, BorderLayout.CENTER);
					getContentPane().add(flowDialogOk, BorderLayout.SOUTH);

					validate();
					pack();
					
					setLayer(JLayeredPane.MODAL_LAYER);
		
					// listen on the flow in order to close when flow completes.
					// for undoing this in close(), we must store the flow.
					currentFlowUI = content;
					currentFlowUI.getFlow().addListener(FlowDialog.this);

					// bring up
					// ----------
		
					// disable widgets
					for (JComponent c : disenable.keySet())
						c.setEnabled(false);


					setVisible(true);
					master.desktop.add(FlowDialog.this);

					// center location
					Dimension dialogSize = getPreferredSize();
					Dimension desktopSize = master.desktop.getSize();
					setBounds(
							(desktopSize.width - dialogSize.width) / 2,
							(desktopSize.height - dialogSize.height) / 2,
							dialogSize.width,
							dialogSize.height
					);

					// Note: We do not call InternalFrame.setSelected(true)
					// here because this would steal the focus from other
					// applications which can be annoying for the user.

					master.desktop.validate();
				}};

				if (SwingUtilities.isEventDispatchThread())
					r.run();
				else
					SwingUtilities.invokeAndWait(r);
		}


		/**
		 * Close the dialog (execution in swing thread guaranteed).
		 */
		public void close() {

			Runnable r = new Runnable() {
				public void run () {

					setVisible(false);
					master.desktop.remove(FlowDialog.this);
					master.desktop.validate();
					if (isSelected())
						try {
							setSelected(false);
						} catch (PropertyVetoException pve) {}

					// undo the listener registration done above
					if (currentFlowUI != null)
						currentFlowUI.getFlow().removeListener(FlowDialog.this);
					
					// enable widgets
					for (JComponent c : disenable.keySet())
						c.setEnabled(true);
				}
			};

			if (SwingUtilities.isEventDispatchThread())
				r.run();
			else
				SwingUtilities.invokeLater(r);
		}


		// Disabling/Enabling widgets according to visibility of this dialog
		// ------------------------------------------
		
		WeakHashMap<JComponent, Object> disenable = new WeakHashMap<JComponent, Object>();

		/*
		 * msc 2009-05: sometimes the progress dialog gets "lost": it is suddenly gone and
		 * the controls remain disabled. this is a quick hack to help me out.
		 */
		protected void disenable (JComponent c) {
			/*
			 * disenable.put(c, "");
			 */
			}
		

		// FlowListener implementation
		// ------------------------------------------
		// This dialog must be a FlowListener,
		// so it can close when the flow is complete

		public void completion (Flow f) {
			try {
				// just to have a chance to see the final "ok" on the screen
				Thread.sleep(800);
			} catch (InterruptedException exc) {}
			close();
		}
		public void reset (Flow f, Object info) {}
		public void trying (Flow f, String s) {}
		public void success (Flow f, String s) {}
		public void failure (Flow f, String s, Object o) {}

	}

	
	// Container Section
	// =========================================================================

	protected Vector<ContainerLine> containerLines = new Vector<ContainerLine>();

	protected ContainerLine addEmptyContainerLine() {
		/* 
		 *  When modifying this, remember to
		 *  change lessContainerLines() as well
		 */

		ContainerLine containerLine = new ContainerLine();
		containerLinePanel.add(containerLine);
		containerLines.add(containerLine);

		int index = containerLines.size()-1;
		setMnemonicForContainerLine(index);

		// (2007-01-19) set a name for this containerline object
		containerLine.setName("pnl_ContainerLine#"+index);
		containerLine.btnStart.setName("btn_Start_Container#"+index);
		containerLine.btnStop.setName("btn_Stop_Container#"+index);
		containerLine.btnConfigure.setName("btn_Edit_Container#"+index);
		containerLine.nameF.setName("txt_ContainerName#"+index);
		containerLine.typeF.setName("txt_ContainerType#"+index);
		containerLine.hostF.setName("txt_ContainerHost#"+index);

		// add model-autowrite
		containerLine.nameF.addFocusListener(focusListener);
		containerLine.typeF.addFocusListener(focusListener);
		containerLine.hostF.addFocusListener(focusListener);

		// force scrollbar to recalculate its appearance
		containerPanel.validate();
		scp.revalidate();
		
		return containerLine;
	}

	protected void setMnemonicForContainerLine(int index) {
		if (0 <= index && index < 10) {
			ContainerLine containerLine = (ContainerLine)containerLines.get(index);
			// use index of container as Mnemomic but beautify by adding 1
			index = (index == 9)? 0 : index + 1;
			containerLine.lblMnemomic.setText(String.valueOf(index));
			containerLine.lblMnemomic.setDisplayedMnemonic(KeyEvent.VK_0 + index);
			containerLine.lblMnemomic.setLabelFor(containerLine.nameF);
		}
	}

	protected void moveContainerLine (int index, boolean upwards) {
		boolean err = (upwards)? index == 0 : index >= containerLines.size()-1;
		if (!err) {
			int newIndex = (upwards)? index -1 : index + 1; 
			ContainerLine containerLine = (ContainerLine)containerLines.remove(index);
			containerLines.add(newIndex, containerLine);
			containerLinePanel.remove(containerLine);
			containerLinePanel.add(containerLine, newIndex);

			// re-shuffle all the Mnemonics to 
			// match the order of the containers
			for (int i=0; i < containerLines.size(); i++) {
				setMnemonicForContainerLine(i);
			}
			
			containerPanel.validate();
		}
	}

	/** reverses all effects of addContainerLine() */
	protected void lessContainerLines () {

		if (containerLines.size() == 0)
			return;

		ContainerLine containerLine = (ContainerLine) containerLines.get(containerLines.size() - 1);

		// if removed one is selected, make its predecessor selected
		if (containerLine.selectB.isSelected() && containerLines.size() > 1)
			((ContainerLine) containerLines.get(containerLines.indexOf(containerLine) - 1)).selectB.setSelected(true);

		containerLine.nameF.removeFocusListener(focusListener);
		containerLine.typeF.removeFocusListener(focusListener);
		containerLine.hostF.removeFocusListener(focusListener);

		containerLines.remove(containerLine);
		containerLinePanel.remove(containerLine);
		containerPanel.validate();
	}

	
	
	// Other stuff
	// =========================================================================


	protected void setAdvancedVisible (boolean b) {
		// msc (2008-10): advanced panel is now really invisible when disabled
		buttonPanelAdvanced.setVisible(b);
		TabPanel.this.validate();
	}

	protected void managerStarted () {
		master.managerStarted();
	}

	protected void managerStopped () {
		master.managerStopped();
	}

	protected boolean confirmKillAcs () {
		int res = JOptionPane.showConfirmDialog(master.frame, "This is recommended in severe cases only\n"
				+ "and may kill the Acs Command Center as well.\n" + "Are you sure?", "Warning", JOptionPane.YES_NO_OPTION);
		return (res == JOptionPane.YES_OPTION);
	}

	protected boolean confirmStartJavaOnlyAcs () {
		String msg = "Please note the following restrictions:\n" + "\n" + "* Only one Manager can be started\n"
				+ "* That Manager may be stopped but cannot be restarted afterwards\n" + "\n"
				+ "* Only one Container can be started\n" + "* That Container may be stopped but cannot be restarted afterwards\n"
				+ "\n" + "If you need more, launch another instance of Acs Command Center.\n" + "Do you want to continue?\n";
		int res = JOptionPane.showConfirmDialog(master.frame, msg, "Restrictions of Java-only Acs", JOptionPane.YES_NO_OPTION);
		return (res == JOptionPane.YES_OPTION);
	}


	protected int getMode () {
		ModeType mode = master.controller.project.getMode();
		return mode.getType();
	}


	/**
	 * When the status or content of a Gui component has been modified programmatically,
	 * this method must be called to ensure the new contents are written through to the
	 * model.
	 * 
	 * @param trg a gui component that was programmatically updated
	 */
	public void validateAfterProgrammaticUpdate (JComponent trg) {

		FocusEvent evt = new FocusEvent(trg, FocusEvent.FOCUS_LOST);
		FocusListener[] lis = trg.getFocusListeners();

		for (int i = 0; i < lis.length; i++) {
			lis[i].focusLost(evt);
		}

	}

	
	protected class MyFocusListener extends FocusAdapter {

		@Override
		public void focusLost (FocusEvent evt) {
			master.writeFrontPanelToModel();
		}
	}


	
	// ################################################################
	// Widgets
	// ################################################################



	protected Vector<ButtonPair> buttonPairs = new Vector<ButtonPair>();

	protected void makeButtonPair (JButton on, JButton off) {
		// keep reference somewhere to prevent garbage collection
		buttonPairs.add(new ButtonPair(on, off));
	}

	protected class ButtonPair implements ActionListener {

		protected JButton on, off;

		protected ButtonPair(JButton on, JButton off) {
			this.on = on;
			on.addActionListener(this);

			this.off = off;
			off.addActionListener(this);
		}

		public void actionPerformed (ActionEvent e) {
			if (e.getSource() == on)
				respond(on, off);
			else if (e.getSource() == off)
				respond(off, on);
		}

		protected void respond (JButton active, JButton passive) {
			// would be neater to save the original color, but too little time for fancy things now
			active.setBackground(CommandCenterGui.COLOR_ActiveButton);
			passive.setBackground(CommandCenterGui.COLOR_PassiveButton);
		}
	}

	
	// GridBagLayout is such a pain
	protected GridBag gridbagpos (int y, int x) {
		GridBag ret = new GridBag();
		ret.pos(y, x);
		ret.gapx(2).gapy(2);
		ret.fill(GridBag.BOTH);
		ret.align(GridBag.WEST);
		return ret;
	}
	/* the fields are explained at 
	 * http://java.sun.com/docs/books/tutorial/uiswing/layout/gridbag.html */
	protected class GridBag extends GridBagConstraints {
		GridBag pos (int y, int x) {gridy = y; gridx = x; return this;}
		GridBag gapy (int n) {insets.top = insets.bottom = n;  return this;}
		GridBag gapx (int n) {insets.left = insets.right = n;  return this;}
		GridBag width (int n) {gridwidth = n; return this;}
		GridBag weightx (double n) {weightx = n; return this;}
		GridBag fill (int n) {fill = n; return this;}
		GridBag align (int n) {anchor = n; return this;}
	}


	
	protected class FixSizeScrollPane extends JScrollPane {

		Dimension d;

		public FixSizeScrollPane(Component q, int vsbPolicy, int hsbPolicy) {
			super(q, vsbPolicy, hsbPolicy);
		}

		public void freezeSize (int width, int height) {
			int barWidth = getVerticalScrollBar().getWidth();  // note: these values will be 0 if the
			int barHeight = getVerticalScrollBar().getWidth(); // scrollpane is not visible on screen
			Insets ins = getInsets();
			d = new Dimension(ins.left + width + barWidth + ins.right, ins.top + height + barHeight + ins.bottom);
		}

		@Override
		public Dimension getMaximumSize () {
			return (d != null) ? d : super.getMaximumSize();
		}

		@Override
		public Dimension getMinimumSize () {
			return (d != null) ? d : super.getMinimumSize();
		}

		@Override
		public Dimension getPreferredSize () {
			return (d != null) ? d : super.getPreferredSize();
		}
	}

	protected class FixSizeSeparator extends JSeparator {
		Dimension d;

		public FixSizeSeparator(int orientation, Dimension d) {
			super(orientation);
			this.d = d;
		}

		@Override
		public Dimension getMaximumSize () {
			return d;
		}

		@Override
		public Dimension getMinimumSize () {
			return d;
		}

		@Override
		public Dimension getPreferredSize () {
			return d;
		}
		
	}
	
	
	protected class MyRadioButton extends JRadioButton {

		MyRadioButton(String text) {
			super(text);
		}

		// needed since doClick() doesn't trigger itemStateChanged
		@Override
		public void setSelected (boolean b) {
			ActionEvent e = new ActionEvent(this, 0, "");
			super.fireActionPerformed(e);
			super.setSelected(b);
		}

//		// while setting up the gui we want to avoid some events
//		protected void setInitiallyTo (boolean b) {
//			super.setSelected(b);
//		}
		
	}
	
	
	protected class MyCheckBox extends JCheckBox {

		MyCheckBox(Action a) {
			super(a);
		}

		@Override
		public void setSelected (boolean b) {
			ActionEvent e = new ActionEvent(this, 0, "");
			super.fireActionPerformed(e);
			super.setSelected(b);
		}

	}
	

	protected class ContainerLine extends JPanel implements ActionListener {

		JRadioButton selectB;
		JLabel lblMnemomic;
		Button btnStart, btnStop, btnConfigure;
		JTextField nameF;
		JComboBox typeF;
		JTextField hostF;

		/** 
		 * The constructor does not fill data into this containerline:
		 * populate(...) does that.
		 */
		ContainerLine() {
			this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));

			this.add(lblMnemomic = new JLabel("x"));
			this.add(Box.createHorizontalStrut(5));

			JPanel b = new JPanel(new GridLayout(0, 3, 5, 5));
			b.add(nameF = new JTextField(10));
			b.add(typeF = new JComboBox(new String[]{"java", "cpp", "py"}));
			b.add(hostF = new JTextField(10));
			this.add(b);
			this.add(Box.createHorizontalStrut(5));

			hostF.setToolTipText("Remote Host (if different from Manager host)");
			hostF.setEditable(false);

			// "selectB" is now invisible, but it will be internally PRESSED 
			// when the user clicks any other buttons belonging to this container
			selectB = new JRadioButton();
			selectB.addFocusListener(focusListener);
			buttonGroup2.add(selectB);
			/* this.add(selectB); */

			// support for selecting a container line 
			// by placing the cursor in the name field
			nameF.addFocusListener(new FocusAdapter() {
				@Override public void focusGained (FocusEvent e) {
					selectB.setSelected(true);
					validateAfterProgrammaticUpdate(selectB);
				}
			});
			
			this.add(btnConfigure = new Button(master.icons.getConfigIcon(), this));
			btnConfigure.setToolTipText("Edit Remote Settings");
			
			this.add(btnStart = new Button(master.icons.getStartIcon(), this));
			btnStart.setToolTipText("Start this Container");
			flowDialog.disenable(btnStart);

			this.add(btnStop = new Button(master.icons.getStopIcon(), this));
			btnStop.setToolTipText("Stop this Container");
			flowDialog.disenable(btnStop);

			makeButtonPair(btnStart, btnStop);

		}
		
		void populate (String name, String type, String host, boolean selected) {
			nameF.setText(name);
			typeF.setSelectedItem(type);
			hostF.setText(host);
			selectB.setSelected(selected);
		}

		public void actionPerformed (ActionEvent e) {
			selectB.setSelected(true);
			validateAfterProgrammaticUpdate(selectB);

			if (e.getSource() == btnStart) {
				if (sanityCheck())
					actStartContainer.actionPerformed(null);
			}

			if (e.getSource() == btnStop) {
				if (sanityCheck())
					actStopContainer.actionPerformed(null);
			}

			if (e.getSource() == btnConfigure) {
				actConfigureContainer.actionPerformed(null);
			}
		}

		boolean sanityCheck () {
			if (nameF.getText().trim().equals("")) {
				JOptionPane.showMessageDialog(master.frame, "The specified Name is invalid", "Invalid Container Settings",
						JOptionPane.ERROR_MESSAGE);
				return false;
			}
			return true;
		}
		
		class Button extends JButton {
			Button(Icon icon, ContainerLine cl) {
				super(icon);
				this.setMargin(new Insets(1, 0, 1, 0));
				this.addActionListener(cl);
			}
		}

	}


	// ################################################################
	// Actions
	// ################################################################


	protected class ActionStartAcs extends BackgroundAction {

		protected ActionStartAcs() {
			master.super("Start", master.icons.getStartIcon());
		}

		// confirm whether necessary to have all the thread-creation etc. by the super-class
		@Override
		public void actionPerformed (ActionEvent evt) {

			// in java-only mode, show restrictions-dialog
			if (getMode() == ModeType.JAVA_TYPE) {
				if (!confirmStartJavaOnlyAcs())
					return;
			}

			super.actionPerformed(evt);
		}

		@Override
		protected void actionPerformed () throws Throwable {

			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					flowDialog.prepareShow("Starting Acs", localScriptFlowPanel);
					master.controller.executeAcs.startLocalScript(master.giveOutputListener2("Acs"));
					managerStarted();
					break;

				case ModeType.REMOTE_TYPE :
					flowDialog.prepareShow("Starting Acs", remoteFlowPanel);
					master.controller.executeAcs.startRemote(false, master.giveOutputListener2("Acs"));
					managerStarted();
					break;

				case ModeType.REMOTE_NATIVE_TYPE :
					flowDialog.prepareShow("Starting Acs", localScriptFlowPanel);
					master.controller.executeAcs.startRemote(true, master.giveOutputListener2("Acs"));
					managerStarted();
					break;
					
				case ModeType.REMOTE_DAEMON_TYPE :
					flowDialog.prepareShow("Starting Acs", remoteServicesDaemonFlowPanel);
					master.controller.executeAcs.startRemoteDemonic(master.giveOutputListener("Acs"));
					managerStarted();
					break;

				case ModeType.JAVA_TYPE :
					// --- trigger two steps (code is a duplicate of the single actions)
					flowDialog.prepareShow("Starting Cdb", localJavaFlowPanel);
					master.controller.executeServices.startLocalJava(master.giveOutputListener2("Acs"));

					flowDialog.prepareShow("Starting Manager", localJavaFlowPanel);
					master.controller.executeManager.startLocalJava(master.giveOutputListener2("Acs"));

					managerStarted();
					break;
			}

		}
	}

	protected class ActionStopAcs extends BackgroundAction {

		protected ActionStopAcs() {
			master.super("Stop", master.icons.getStopIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					flowDialog.prepareShow("Stopping Acs", localScriptFlowPanel);
					master.controller.executeAcs.stopLocalScript(master.giveOutputListener2("Acs"));
					managerStopped();
					break;
				case ModeType.REMOTE_TYPE :
					flowDialog.prepareShow("Stopping Acs", remoteFlowPanel);
					master.controller.executeAcs.stopRemote(false, master.giveOutputListener2("Acs"));
					managerStopped();
					break;
				case ModeType.REMOTE_NATIVE_TYPE :
					flowDialog.prepareShow("Stopping Acs", localScriptFlowPanel);
					master.controller.executeAcs.stopRemote(true, master.giveOutputListener2("Acs"));
					managerStopped();
					break;
				case ModeType.REMOTE_DAEMON_TYPE :
					flowDialog.prepareShow("Stopping Acs", remoteServicesDaemonFlowPanel);
					master.controller.executeAcs.stopRemoteDemonic(master.giveOutputListener("Acs"));
					managerStopped();
					break;
				case ModeType.JAVA_TYPE :
					// --- trigger two steps (code is a duplicate of the single actions)
					flowDialog.prepareShow("Stopping Manager", localJavaFlowPanel);
					master.controller.executeManager.stopLocalJava();

					flowDialog.prepareShow("Stopping Cdb", singleStepFlowPanel);
					master.controller.executeServices.stopLocalJava();

					managerStopped();
					break;
			}
		}
	}

	protected class ActionStartServices extends BackgroundAction {

		protected ActionStartServices() {
			master.super("", master.icons.getStartIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					flowDialog.prepareShow("Starting Services", localScriptFlowPanel);
					master.controller.executeServices.startLocalScript(master.giveOutputListener2("Services"));
					break;

				case ModeType.REMOTE_DAEMON_TYPE :
					// fall through, daemon mode not supported

				case ModeType.REMOTE_TYPE :
					flowDialog.prepareShow("Starting Services", remoteFlowPanel);
					master.controller.executeServices.startRemote(false, master.giveOutputListener2("Services"));
					break;
					
				case ModeType.REMOTE_NATIVE_TYPE :
					flowDialog.prepareShow("Starting Services", localScriptFlowPanel);
					master.controller.executeServices.startRemote(true, master.giveOutputListener2("Services"));
					break;

				case ModeType.JAVA_TYPE :
					flowDialog.prepareShow("Starting Cdb", localJavaFlowPanel);
					master.controller.executeServices.startLocalJava(master.giveOutputListener2("Services"));
					break;
			}
		}
	}

	protected class ActionStopServices extends BackgroundAction {

		protected ActionStopServices() {
			master.super("", master.icons.getStopIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					flowDialog.prepareShow("Stopping Services", localScriptFlowPanel);
					master.controller.executeServices.stopLocalScript(master.giveOutputListener2("Services"));
					break;

				case ModeType.REMOTE_DAEMON_TYPE :
					// fall through, daemon mode not supported

				case ModeType.REMOTE_TYPE :
					flowDialog.prepareShow("Stopping Services", remoteFlowPanel);
					master.controller.executeServices.stopRemote(false, master.giveOutputListener2("Services"));
					break;

				case ModeType.REMOTE_NATIVE_TYPE :
					flowDialog.prepareShow("Stopping Services", localScriptFlowPanel);
					master.controller.executeServices.stopRemote(true, master.giveOutputListener2("Services"));
					break;

				case ModeType.JAVA_TYPE :
					flowDialog.prepareShow("Stopping Cdb", singleStepFlowPanel);
					master.controller.executeServices.stopLocalJava();
					break;

			}
		}
	}

	protected class ActionStartManager extends BackgroundAction {

		protected ActionStartManager() {
			master.super("", master.icons.getStartIcon());
		}

		// confirm whether necessary to have all the thread-creation etc. by the super-class
		@Override
		public void actionPerformed (ActionEvent evt) {

			// in java-only mode, show restrictions-dialog
			if (getMode() == ModeType.JAVA_TYPE) {
				if (!confirmStartJavaOnlyAcs())
					return;
			}

			super.actionPerformed(evt);
		}

		@Override
		protected void actionPerformed () throws Throwable {

			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					flowDialog.prepareShow("Starting Manager", localScriptFlowPanel);
					master.controller.executeManager.startLocalScript(master.giveOutputListener2("Manager"));
					managerStarted();
					break;

				case ModeType.REMOTE_DAEMON_TYPE :
					// fall through, daemon mode not supported

				case ModeType.REMOTE_TYPE :
					flowDialog.prepareShow("Starting Manager", remoteFlowPanel);
					master.controller.executeManager.startRemote(false, master.giveOutputListener2("Manager"));
					managerStarted();
					break;

				case ModeType.REMOTE_NATIVE_TYPE :
					flowDialog.prepareShow("Starting Manager", localScriptFlowPanel);
					master.controller.executeManager.startRemote(true, master.giveOutputListener2("Manager"));
					managerStarted();
					break;

				case ModeType.JAVA_TYPE :
					flowDialog.prepareShow("Starting Manager", localJavaFlowPanel);
					master.controller.executeManager.startLocalJava(master.giveOutputListener2("Manager"));
					managerStarted();
					break;
			}
		}
	}

	protected class ActionStopManager extends BackgroundAction {

		protected ActionStopManager() {
			master.super("", master.icons.getStopIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					flowDialog.prepareShow("Stopping Manager", localScriptFlowPanel);
					master.controller.executeManager.stopLocalScript(master.giveOutputListener2("Manager"));
					managerStopped();
					break;

				case ModeType.REMOTE_DAEMON_TYPE :
					// fall through, daemon mode not supported

				case ModeType.REMOTE_TYPE :
					flowDialog.prepareShow("Stopping Manager", remoteFlowPanel);
					master.controller.executeManager.stopRemote(false, master.giveOutputListener2("Manager"));
					managerStopped();
					break;

				case ModeType.REMOTE_NATIVE_TYPE :
					flowDialog.prepareShow("Stopping Manager", localScriptFlowPanel);
					master.controller.executeManager.stopRemote(true, master.giveOutputListener2("Manager"));
					managerStopped();
					break;

				case ModeType.JAVA_TYPE :
					flowDialog.prepareShow("Stopping Manager", localJavaFlowPanel);
					master.controller.executeManager.stopLocalJava();
					managerStopped();
					break;

			}
		}
	}

	protected class ActionStartContainer extends BackgroundAction {

		protected ActionStartContainer() {
			master.super("Start Container");
		}

		@Override
		protected void actionPerformed () throws Throwable {
			int contNumber = master.controller.project.getContainers().getSelect();
			String contName = master.controller.project.getContainers().getContainer(contNumber).getName();

			// msc (2005-11-23): ExecuteContainer API has changed to allow concurrent starts of containers
			RunModel runmodel =  master.controller.model.createViewOnContainer(contNumber);

			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					flowDialog.prepareShow("Starting "+contName, localScriptFlowPanel);
					master.controller.executeContainer.startLocalScript(runmodel, master.giveOutputListener2(contName));
					break;
					
				case ModeType.REMOTE_TYPE :
					flowDialog.prepareShow("Starting "+contName, remoteFlowPanel);
					master.controller.executeContainer.startRemote(runmodel, false, master.giveOutputListener2(contName));
					break;

				case ModeType.REMOTE_NATIVE_TYPE :
					flowDialog.prepareShow("Starting "+contName, localScriptFlowPanel);
					master.controller.executeContainer.startRemote(runmodel, true, master.giveOutputListener2(contName));
					break;

				case ModeType.REMOTE_DAEMON_TYPE :
					flowDialog.prepareShow("Starting "+contName, remoteContainerDaemonFlowPanel);
					master.controller.executeContainer.startRemoteDemonic(runmodel, master.giveOutputListener(contName));
					break;

				case ModeType.JAVA_TYPE :
					flowDialog.prepareShow("Starting "+contName, localJavaFlowPanel);
					master.controller.executeContainer.startLocalJava(runmodel, master.giveOutputListener2(contName));
					break;
			}
		}
	}

	protected class ActionStopContainer extends BackgroundAction {

		protected ActionStopContainer() {
			master.super("Stop Container");
		}

		@Override
		protected void actionPerformed () throws Throwable {
			int contNumber = master.controller.project.getContainers().getSelect();
			String contName = master.controller.project.getContainers().getContainer(contNumber).getName();

			// msc (2005-11-23): ExecuteContainer API has changed to allow concurrent starts of containers
			RunModel runmodel =  master.controller.model.createViewOnContainer(contNumber);

			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					flowDialog.prepareShow("Stopping "+contName, localScriptFlowPanel);
					master.controller.executeContainer.stopLocalScript(runmodel, master.giveOutputListener2(contName));
					break;
				case ModeType.REMOTE_TYPE :
					flowDialog.prepareShow("Stopping "+contName, remoteFlowPanel);
					master.controller.executeContainer.stopRemote(runmodel, false, master.giveOutputListener2(contName));
					break;
				case ModeType.REMOTE_NATIVE_TYPE :
					flowDialog.prepareShow("Stopping "+contName, localScriptFlowPanel);
					master.controller.executeContainer.stopRemote(runmodel, true, master.giveOutputListener2(contName));
					break;
				case ModeType.REMOTE_DAEMON_TYPE :
					flowDialog.prepareShow("Stopping "+contName, remoteContainerDaemonFlowPanel);
					master.controller.executeContainer.stopRemoteDemonic(runmodel, master.giveOutputListener(contName));
					break;
				case ModeType.JAVA_TYPE :
					flowDialog.prepareShow("Stopping "+contName, localJavaFlowPanel);
					master.controller.executeContainer.stopLocalJava(runmodel);
					break;
			}
		}
	}


	protected class ActionStartAllContainers extends BackgroundAction {

		protected ActionStartAllContainers() {
			master.super("", master.icons.getStartIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			ContainersT conts = master.controller.project.getContainers();
			for (int contNumber = 0; contNumber < conts.getContainerCount(); contNumber++) {
				conts.setSelect(contNumber);
				actStartContainer.actionPerformed();
			}
		}
	}


	protected class ActionStopAllContainers extends BackgroundAction {

		protected ActionStopAllContainers() {
			master.super("", master.icons.getStopIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			ContainersT conts = master.controller.project.getContainers();
			for (int contNumber = conts.getContainerCount() - 1; contNumber >= 0; contNumber--) {
				conts.setSelect(contNumber);
				actStopContainer.actionPerformed();
			}
		}
	}


	protected class ActionKillAcs extends BackgroundAction {

		protected ActionKillAcs() {
			master.super("Kill", master.icons.getStopIconRed());
		}

		// confirm whether necessary to have all the thread-creation etc. by the super-class
		@Override
		public void actionPerformed (ActionEvent evt) {

			if (getMode() == ModeType.JAVA_TYPE) {
				JOptionPane.showMessageDialog(master.frame, "Kill not available in Java-only Acs.\n"
						+ "Terminate AcsCommandCenter instead.");
				return;
			}

			if (confirmKillAcs())
				super.actionPerformed(evt);
		}

		@Override
		protected void actionPerformed () throws Throwable {
			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					flowDialog.prepareShow("Killing Acs", localScriptFlowPanel);
					master.controller.executeAcs.killLocalScript(master.giveOutputListener2("Acs"));
					break;
				case ModeType.REMOTE_DAEMON_TYPE :
					// fall through, daemon mode not supported
				case ModeType.REMOTE_TYPE :
					flowDialog.prepareShow("Killing Acs", remoteFlowPanel);
					master.controller.executeAcs.killRemote(false, master.giveOutputListener2("Acs"));
					break;
				case ModeType.REMOTE_NATIVE_TYPE :
					flowDialog.prepareShow("Killing Acs", localScriptFlowPanel);
					master.controller.executeAcs.killRemote(true, master.giveOutputListener2("Acs"));
					break;
				case ModeType.JAVA_TYPE :
					// Should we exit the vm (that would guarantee all delegates die), or what?
					break;
			}
		}
	}

	protected class ActionConfigureAllContainers extends BackgroundAction {

		protected ActionConfigureAllContainers() {
			master.super("", master.icons.getConfigIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			master.showManagerLocationForContainersDialog();
		}
	}

	protected class ActionMoreContainers extends BackgroundAction {

		protected ActionMoreContainers() {
			master.super("", master.icons.getPlusIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			addEmptyContainerLine();
			master.controller.moreContainers();
			disenabler.actionPerformed(null);
		}
	}

	protected class ActionLessContainers extends BackgroundAction {

		protected ActionLessContainers() {
			master.super("", master.icons.getMinusIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			master.controller.lessContainers();
			lessContainerLines();
		}
	}

	protected class ActionConfigureContainer extends BackgroundAction {

		protected ActionConfigureContainer() {
			master.super("Configure Container");
		}

		@Override
		protected void actionPerformed () throws Throwable {
			master.showContainerSettingsDialog();
		}
	}

	protected class ActionMoveContainerUp extends BackgroundAction {

		protected ActionMoveContainerUp() {
			master.super("", master.icons.getUpIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			int contNumber = master.controller.project.getContainers().getSelect();
			if (contNumber > 0) {
				ContainerT cont = master.controller.removeContainer(contNumber);
				master.controller.insertContainer(cont, contNumber-1);
				moveContainerLine(contNumber, true);
				master.controller.project.getContainers().setSelect(contNumber-1);
			}
		}
	}

	protected class ActionMoveContainerDown extends BackgroundAction {

		protected ActionMoveContainerDown() {
			master.super("", master.icons.getDownIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			int contNumber = master.controller.project.getContainers().getSelect();
			int lastIndex = master.controller.project.getContainers().getContainerCount()-1;
			if (contNumber < lastIndex) {
				ContainerT cont = master.controller.removeContainer(contNumber);
				master.controller.insertContainer(cont, contNumber+1);
				moveContainerLine(contNumber, false);
				master.controller.project.getContainers().setSelect(contNumber+1);
			}
		}
	}
	
	
	protected class ActionShowAdvanced extends BackgroundAction {

		protected ActionShowAdvanced() {
			master.super("advanced", null);
		}

		@Override
		protected void actionPerformed () throws Throwable {
			boolean b = btnShowAdvanced.isSelected();
			setAdvancedVisible(b);
		}
	}

}
