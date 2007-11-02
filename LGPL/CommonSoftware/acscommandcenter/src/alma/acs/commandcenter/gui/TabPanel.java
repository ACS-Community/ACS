/*
 * Created on Oct 21, 2003 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.Enumeration;
import java.util.Vector;

import javax.swing.AbstractButton;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
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
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.border.TitledBorder;

import alma.acs.commandcenter.engine.Executor;
import alma.acs.commandcenter.engine.RunModel;
import alma.acs.commandcenter.gui.CommandCenterGui.ActionBaseClass;
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

	protected CommandCenterGui master;
	protected JPanel remoteTab;
	protected DefaultChecklistPanel remoteFlowPanel;
	protected DefaultChecklistPanel localJavaFlowPanel;
	protected DefaultChecklistPanel localScriptFlowPanel;
	protected DefaultChecklistPanel singleStepFlowPanel;
	protected DefaultChecklistPanel remoteServicesDaemonFlowPanel;
	protected DefaultChecklistPanel remoteContainerDaemonFlowPanel;


	protected TabPanel(CommandCenterGui master) {
		super();
		this.master = master;

		flowDialog = new FlowDialog(master.frame, "Progress");

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

	ActionBaseClass actStartAcs;
	ActionBaseClass actStopAcs;
	ActionBaseClass actKillAcs;
	ActionBaseClass actStartServices;
	ActionBaseClass actStopServices;
	ActionBaseClass actStartManager;
	ActionBaseClass actStopManager;
	ActionBaseClass actShowAdvanced;
	ActionBaseClass actStartContainer;
	ActionBaseClass actStopContainer;
	ActionBaseClass actStartAllContainers;
	ActionBaseClass actStopAllContainers;
	ActionBaseClass actConfigureContainer;


	FlowDialog flowDialog;
	FixSizeScrollPane scp;
	JPanel controlsPanel;
	JPanel buttonPanel;
	JPanel panelAdvanced;
	JPanel buttonPanelAdvanced;
	JToggleButton btnShowAdvanced;
	TwoStageEnableButton btnStartServices;
	TwoStageEnableButton btnStopServices;
	TwoStageEnableButton btnStartManager;
	TwoStageEnableButton btnStopManager;
	JButton btnKillAcs;
	JButton btnStopAcs;
	JButton btnStartAcs;

	JButton btnStartContainer;
	JButton btnStopContainer;
	JButton btnConfigureContainer;
	JButton btnStartAllContainers;
	JButton btnStopAllContainers;
	JButton btnMoreContainers;
	JButton btnLessContainers;
	JButton btnContainersAgainstManager;
	JButton btnMoveContainerUp;
	JButton btnMoveContainerDown;

	JButton btnShowCdbChooser;

	MyRadioButton chkLocalScript = new MyRadioButton("Local");
	MyRadioButton chkRemoteScript = new MyRadioButton("Remote");
	MyRadioButton chkLocalJava = new MyRadioButton("Java-only Acs");
	JCheckBox chkUseDaemons;

	ButtonGroup buttonGroup1 = new ButtonGroup();
	{
		buttonGroup1.add(chkLocalScript);
		buttonGroup1.add(chkRemoteScript);
		buttonGroup1.add(chkLocalJava);

		chkLocalScript.setToolTipText("Run Acs scripts like acsStartManager etc. on your local machine");
		chkRemoteScript.setToolTipText("Connect to a host via ssh and run the Acs scripts there");
		chkLocalJava.setToolTipText("Run a pure Java Acs on your local machine");

		chkLocalScript.setName("chk_Local");
		chkRemoteScript.setName("chk_Remote");
		chkLocalJava.setName("chk_Java");
		
		chkLocalScript.setMnemonic(KeyEvent.VK_L);
		chkRemoteScript.setMnemonic(KeyEvent.VK_R);
		chkLocalJava.setMnemonic(KeyEvent.VK_J);

		String osName = System.getProperty("os.name");
		boolean win32 = osName.length() >= 3 && osName.substring(0, 3).equalsIgnoreCase("win");
		chkLocalJava.setEnabled(true);
	}

	JPanel containerPanel;
	JPanel containerLinePanel;
	ButtonGroup buttonGroup2 = new ButtonGroup(); // for radiobuttons inside containerpanel

	JLabel lblA, lblB, lblC, lblD, lblE, lblF, lblG;

	JTextField acsinstanceF = new JTextField(5);

	JTextField hostF = new JTextField(20);
	JTextField accountF = new JTextField(20);
	JTextField passwordF = new JPasswordField(20);

	JTextField cdbrootF = new JTextField(20);

	FocusListener focusListener = new MyFocusListener();

	protected void init () {

		btnMoreContainers = new JButton(new ActionMoreContainers());
		btnLessContainers = new JButton(new ActionLessContainers());
		btnContainersAgainstManager = new JButton(new ActionConfigureAllContainers());
		btnMoveContainerUp = new JButton(new ActionMoveContainerUp());
		btnMoveContainerDown = new JButton(new ActionMoveContainerDown());
		btnShowCdbChooser = new JButton(new ActionShowCdbChooser());

		Insets margin = new Insets(1, 0, 1, 0);
		btnMoreContainers.setMargin(margin);
		btnLessContainers.setMargin(margin);
		btnContainersAgainstManager.setMargin(margin);
		btnMoveContainerUp.setMargin(margin);
		btnMoveContainerDown.setMargin(margin);
		btnShowCdbChooser.setMargin(margin);

		
		this.setLayout(new BorderLayout());

		Box tabbedPane1LeftContent = new Box(BoxLayout.Y_AXIS);

		Box tabbedPane1Right = new Box(BoxLayout.Y_AXIS);

		// -------------------------------------------------------------------
		// general settings
		// -------------------------------------------------------------------

		JPanel generalTab = new JPanel(new BorderLayout());

		JPanel o = new JPanel(new BorderLayout());

		JPanel n = new JPanel(new SpringLayout());
		lblA = new JLabel("Acs Instance");
		n.add(lblA);
		n.add(acsinstanceF);
		n.add(new JPanel());
		
		flowDialog.disenable(acsinstanceF);
		acsinstanceF.addFocusListener(focusListener);
		
		lblA.setLabelFor(acsinstanceF);
		lblA.setDisplayedMnemonic(KeyEvent.VK_I);
		SpringUtilities.makeCompactGrid(n, 1, 0);
		o.add(n, BorderLayout.NORTH);
		o.add(new JLabel(" ")); // empty placeholder

		generalTab.add(o);

		tabbedPane1LeftContent.add(generalTab);
		tabbedPane1LeftContent.add(Box.createVerticalStrut(1));

		acsinstanceF.setName("txt_AcsInstance");
		
		
		// -------------------------------------------------------------------
		// local script settings
		// -------------------------------------------------------------------


		JPanel localScriptTab = new JPanel(new BorderLayout());

		JPanel h = new JPanel(new BorderLayout());
		h.add(chkLocalScript, BorderLayout.NORTH);
		h.add(new JLabel(" "), BorderLayout.SOUTH); // empty placeholder
		chkLocalScript.addFocusListener(focusListener);
		localScriptTab.add(h);

		tabbedPane1LeftContent.add(localScriptTab);
		tabbedPane1LeftContent.add(Box.createVerticalStrut(1));

		// -------------------------------------------------------------------
		// local java settings
		// -------------------------------------------------------------------

		JPanel localJavaTab = new JPanel(new BorderLayout());

		JPanel g = new JPanel(new BorderLayout());
		g.add(chkLocalJava, BorderLayout.NORTH);

		JPanel m = new JPanel(new SpringLayout());
		lblE = new JLabel("Cdb Root");
		lblE.setToolTipText("The Cdb Location  (Additional schemas searched for in " + System.getProperty("ACS.cdbpath") + ")");
		m.add(lblE);
		m.add(cdbrootF);
		/*m.add(btnShowCdbChooser);*/ // cdb-chooser is experimental
		cdbrootF.addFocusListener(focusListener);
		chkLocalJava.addFocusListener(focusListener);
		chkLocalJava.disenable(lblE);
		chkLocalJava.disenable(cdbrootF);
		chkLocalJava.disenable(btnShowCdbChooser);
		SpringUtilities.makeCompactGrid(m, 1, 0, 30, 3, 6, 2);
		g.add(m, BorderLayout.SOUTH);

		localJavaTab.add(g);

		tabbedPane1LeftContent.add(localJavaTab);
		tabbedPane1LeftContent.add(Box.createVerticalStrut(1));

		cdbrootF.setName("txt_CdbRoot");
		btnShowCdbChooser.setName("btn_Show_Cdb_Chooser");
		
		// -------------------------------------------------------------------
		// remote settings
		// -------------------------------------------------------------------


		remoteTab = new JPanel(new BorderLayout());

		JPanel j = new JPanel(new BorderLayout());
		j.add(chkRemoteScript, BorderLayout.NORTH);

		JPanel k = new JPanel(new SpringLayout());
		k.add(lblB = new JLabel("Host"));
		k.add(hostF);
		chkRemoteScript.addFocusListener(focusListener);
		chkRemoteScript.disenable(lblB);
		chkRemoteScript.disenable(hostF);
		hostF.addFocusListener(focusListener);
		k.add(lblC = new JLabel("Username"));
		k.add(accountF);
		lblC.setLabelFor(accountF);
		chkRemoteScript.disenable(lblC);
		chkRemoteScript.disenable(accountF);
		accountF.addFocusListener(focusListener);
		k.add(lblD = new JLabel("Password"));
		k.add(passwordF);
		lblD.setLabelFor(passwordF);
		chkRemoteScript.disenable(lblD);
		chkRemoteScript.disenable(passwordF);
		passwordF.addFocusListener(focusListener);
		SpringUtilities.makeCompactGrid(k, 3, 2, 30, 3, 6, 2);
		j.add(k, BorderLayout.CENTER);

		chkUseDaemons = new JCheckBox(new ActionUseDaemons());
		chkUseDaemons.setToolTipText("Use Acs Daemons instead of ssh");
		chkUseDaemons.setBorder(new EmptyBorder(6,30,3,0)); // align with textfields
		chkRemoteScript.disenable(chkUseDaemons);
		j.add(chkUseDaemons, BorderLayout.SOUTH);
		
		
		remoteTab.add(j);

		tabbedPane1LeftContent.add(remoteTab);
		tabbedPane1LeftContent.add(Box.createVerticalGlue());

		
		hostF.setName("txt_RemoteHost");
		accountF.setName("txt_RemoteUser");
		passwordF.setName("txt_RemotePassword");
		
		// -------------------------------------------------------------------
		// buttons / actions
		// -------------------------------------------------------------------


		btnStartAcs = new JButton(actStartAcs = new ActionStartAcs());
		btnStopAcs = new JButton(actStopAcs = new ActionStopAcs());
		btnKillAcs = new JButton(actKillAcs = new ActionKillAcs());
		btnStartServices = new TwoStageEnableButton(actStartServices = new ActionStartServices());
		btnStopServices = new TwoStageEnableButton(actStopServices = new ActionStopServices());
		btnStartManager = new TwoStageEnableButton(actStartManager = new ActionStartManager());
		btnStopManager = new TwoStageEnableButton(actStopManager = new ActionStopManager());
		btnShowAdvanced = new JCheckBox(actShowAdvanced = new ActionShowAdvanced());
		btnStartContainer = new JButton(actStartContainer = new ActionStartContainer());
		btnStopContainer = new JButton(actStopContainer = new ActionStopContainer());
		btnConfigureContainer = new JButton(actConfigureContainer = new ActionConfigureContainer());
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

		// chkLocalScript.disenable(btnKillAcs);
		// chkRemoteScript.disenable(btnKillAcs);

		// -------------------------------------------------------------------
		// control section
		// -------------------------------------------------------------------

		controlsPanel = new JPanel(new SpringLayout());
		controlsPanel.setBorder(createTitledBorder(" Controls "));

		buttonPanel = new JPanel(new SpringLayout());

		JLabel label1 = new JLabel("Acs Suite");
		buttonPanel.add(label1);

		buttonPanel.add(btnStartAcs);
		flowDialog.disenable(btnStartAcs);

		buttonPanel.add(btnStopAcs);
		flowDialog.disenable(btnStopAcs);

		buttonPanel.add(btnKillAcs);
		flowDialog.disenable(btnKillAcs);

		SpringUtilities.makeCompactGrid(buttonPanel, 1, 4);

		// -------------------------------------------------------------------
		// advanced section
		// -------------------------------------------------------------------

		panelAdvanced = new JPanel(new BorderLayout());

		flowDialog.disenable(btnShowAdvanced);

		JPanel p = new JPanel(new BorderLayout());
		p.add(btnShowAdvanced, BorderLayout.NORTH);
		p.setBorder(new EmptyBorder(0, 0, 0, 0));

		panelAdvanced.add(p, BorderLayout.WEST);

		buttonPanelAdvanced = new JPanel(new SpringLayout());
		buttonPanelAdvanced.setBorder(new CompoundBorder(new EmptyBorder(8, 4, 4, 4), LineBorder.createGrayLineBorder()));

		JLabel label2 = new JLabel("Services");
		buttonPanelAdvanced.add(label2);

		buttonPanelAdvanced.add(btnStartServices);
		flowDialog.disenable(btnStartServices);

		buttonPanelAdvanced.add(btnStopServices);
		flowDialog.disenable(btnStopServices);

		buttonPanelAdvanced.add(new JPanel());

		JLabel label3 = new JLabel("Manager");
		buttonPanelAdvanced.add(label3);

		buttonPanelAdvanced.add(btnStartManager);
		flowDialog.disenable(btnStartManager);

		buttonPanelAdvanced.add(btnStopManager);
		flowDialog.disenable(btnStopManager);

		SpringUtilities.makeCompactGrid(buttonPanelAdvanced, 1, 7);

		panelAdvanced.add(buttonPanelAdvanced, BorderLayout.CENTER);

		/*
		 * These buttons are now invisible - BUT CLICKED programmatically by the
		 * per-container buttons!
		 * 
		 * controlsPanel.add(btnStartContainer); controlsPanel.add(btnStopContainer);
		 * controlsPanel.add(btnConfigureContainer);
		 */

		controlsPanel.add(buttonPanel);
		controlsPanel.add(panelAdvanced);
		SpringUtilities.makeCompactGrid(controlsPanel, 2, 1);

		makeButtonPair(btnStartAcs, btnStopAcs);
		makeButtonPair(btnStartServices, btnStopServices);
		makeButtonPair(btnStartManager, btnStopManager);
		makeButtonPair(btnStartContainer, btnStopContainer);

		tabbedPane1Right.add(controlsPanel);
		tabbedPane1Right.add(Box.createVerticalStrut(10));


		// -------------------------------------------------------------------
		// container section
		// -------------------------------------------------------------------

		containerPanel = new JPanel(new BorderLayout());
		containerPanel.setBorder(createTitledBorder(" Containers "));

		JPanel a = new JPanel(new GridLayout(0, 6, 5, 5));
		a.add(new JLabel());
		a.add(new JLabel("Name"));
		a.add(new JLabel());
		a.add(new JLabel("Type"));
		a.add(new JLabel());
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

		// freeze the size of the scrollpane to 3 containerlines
		ContainerLine filler = new ContainerLine();
		scp.validate();
		int scrollbarWidth = 25; // can't find out this value before scrollpane is visible on screen
		scp.freezeSize(filler.getPreferredSize().width + scrollbarWidth,
				         filler.getPreferredSize().height * 3 + 4 * 2 + 2); // <-- don't ask...


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

		
		JPanel southwest = new JPanel();
		//southwest.setLayout(new BoxLayout(southwest, BoxLayout.X_AXIS));
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
		tabbedPane1Right.add(containerPanel);

		// -------------------------------------------------------------------
		// assemble / layout
		// -------------------------------------------------------------------

		JPanel tabbedPane1Left = new JPanel(new BorderLayout());
		tabbedPane1Left.add(tabbedPane1LeftContent, BorderLayout.NORTH);
		tabbedPane1Left.setBorder(createTitledBorder(" Common Settings "));

		JPanel tabbedPane1 = new JPanel(new BorderLayout());
		tabbedPane1.add(tabbedPane1Left, BorderLayout.CENTER);
		tabbedPane1.add(tabbedPane1Right, BorderLayout.EAST);
		this.add(tabbedPane1, BorderLayout.NORTH);

		// -------------------------------------------------------------------
		// select some checkboxes, etc.
		// -------------------------------------------------------------------

		// unselect all radiobuttons, then select wanted one
		// weird but needed to activate the buttons' disenable event-handlers
		for (Enumeration en = buttonGroup1.getElements(); en.hasMoreElements();)
			((AbstractButton) en.nextElement()).setSelected(false);
		chkLocalScript.setSelected(true);

		// we want the "advanced" section to be off, initially
		btnShowAdvanced.setSelected(false);
		setAdvancedVisible(false);
	}

	protected Border createTitledBorder (String title) {
		Border ret = new CompoundBorder(new EmptyBorder(5, 7, 5, 7), new TitledBorder(LineBorder.createBlackLineBorder(), title));
		return ret;
	}

	protected JPanel createLabeledTextField (JLabel label, JTextField textfield) {
		JPanel ret = new JPanel(new SpringLayout());
		ret.add(label);
		ret.add(textfield);
		label.setLabelFor(textfield);
		SpringUtilities.makeCompactGrid(ret, 1, 2, 0, 0, 6, 0);
		return ret;
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
	
	protected class FlowDialog extends JDialog implements FlowListener {

		DefaultChecklistPanel currentFlowUI; // the flow-ui currently held by this dialog
		JButton flowDialogOk;

		protected FlowDialog(JFrame parent, String title) {
			super(parent, title);
			this.getContentPane().setLayout(new BorderLayout());
			this.setModal(false);
			flowDialogOk = new JButton("Close");
			flowDialogOk.addActionListener(new ActionListener() {

				public void actionPerformed (ActionEvent evt) {
					close();
				}
			});
			this.getRootPane().setDefaultButton(flowDialogOk); // TODO setDefaultButton doesn't work
			
			this.addWindowListener(new WindowAdapter(){
				@Override
				public void windowClosing (WindowEvent e) {
					cleanUp();
				}
			});
			
			
			flowDialogOk.setName("btn_Close_FlowDialog");
		}

		protected void prepareShow (String title, JComponent content) {
			this.setTitle(title);
			this.getContentPane().removeAll();
			this.getContentPane().add(content, BorderLayout.CENTER);
			this.getContentPane().add(flowDialogOk, BorderLayout.SOUTH);
			this.validate();
			this.pack();

			// the dialog must listen on the flow to be able to close when the flow has completed.
			// to be able to undo this in close(), the flow must be stored.
			// this dialog can hold any JComponent, thus we need to distinguish whether that jcomponent is a flow-UI
			if (content instanceof DefaultChecklistPanel) {
				currentFlowUI = (DefaultChecklistPanel) content;
				currentFlowUI.getFlow().addListener(this);
			} else {
				currentFlowUI = null;
			}
		}

		public void bringUp () {
			// disable buttons
			for (int i = 0; i < comps.size(); i++)
				((JComponent) comps.elementAt(i)).setEnabled(false);

			// super.setLocationRelativeTo(getParent()); // PENDING: try out if this works as well
			setLocation((getParent().getWidth() - getWidth()) / 2, (getParent().getHeight() - getHeight()) / 2);
			super.setVisible(true);
		}

		
		public void close() {
			super.setVisible(false);
			cleanUp();
		}
		
		protected void cleanUp() {

			// undoes the listener registration that may have been done in prepareShow()
			if (currentFlowUI != null)
				currentFlowUI.getFlow().removeListener(this);

			// enable buttons
			for (int i = 0; i < comps.size(); i++)
				((JComponent) comps.elementAt(i)).setEnabled(true);
		}

		Vector<JComponent> comps = new Vector<JComponent>();

		// use this to add swing components that will be automatically
		// enabled/disabled according to the visibility of this dialog
		protected void disenable (JComponent c) {
			comps.add(c);
		}

		protected void undisenable (JComponent c) {
			comps.remove(c);
		}

		// ---- FlowListener implementation ----

		// This dialog must be a FlowListener,
		// so it can close when the flow is complete

		public void completion (Flow f) {

			// just to have a chance to see the final "ok" on the screen
			try {
				Thread.sleep(800);
			} catch (InterruptedException exc) {}

			close();
		}

		public void reset (Flow f) {}

		public void trying (Flow f, String s) {}

		public void success (Flow f, String s) {}

		public void failure (Flow f, String s, Object o) {}
		// -------------------------------------
	}

	protected JPanel createLine (JLabel caption, JTextField input, String tooltip) {
		JPanel ret = new JPanel();
		ret.setLayout(new BoxLayout(ret, BoxLayout.X_AXIS));
		ret.setBorder(new EmptyBorder(0, 800, 0, 10));
		// input.setColumns(25);
		caption.setMinimumSize(new Dimension(30, caption.getPreferredSize().height));
		caption.setMaximumSize(new Dimension(30, caption.getPreferredSize().height));
		caption.setLabelFor(input);
		caption.setToolTipText(tooltip);

		ret.add(caption);
		ret.add(Box.createHorizontalStrut(5));
		ret.add(Box.createHorizontalGlue());
		ret.add(input);
		return ret;
	}

	protected Vector<ContainerLine> containerLines = new Vector<ContainerLine>();

	/**
	 * Adds a container element
	 * 
	 * @return the newly created ContainerLine
	 */
	protected void addContainerLine (String name, String type, boolean selected) {
		/*
		 *  When modifying this, remember to
		 *  change lessContainerLines() as well
		 */
		ContainerLine containerLine = new ContainerLine();
		containerLine.nameF.setText(name);
		containerLine.typeF.setSelectedItem(type);
		containerLine.selectB.setSelected(selected);

		containerLinePanel.add(containerLine);
		containerLines.add(containerLine);

		int index = containerLines.size()-1;
		setMnemonicForContainerLine(index);

		// (2007-01-19) set a name for this containerline object
		containerLine.setName("pnl_ContainerLine#"+index);
		containerLine.btnStart.setName("btn_Start_Container#"+index);
		containerLine.btnStop.setName("btn_Stop_Container#"+index);
		containerLine.btnData.setName("btn_Edit_Container#"+index);
		containerLine.nameF.setName("txt_ContainerName#"+index);
		containerLine.typeF.setName("txt_ContainerType#"+index);

		// add model-autowrite
		containerLine.nameF.addFocusListener(focusListener);
		containerLine.typeF.addFocusListener(focusListener);

		// when pure-java is selected, the container type is fixed, namely: it
		// will be java container
		chkLocalScript.disenable(containerLine.typeF);
		chkRemoteScript.disenable(containerLine.typeF);

		// TODO(msc) same problem as in constructor: simulate a user click on
		// radiobutton to invoke disenable listener

		// force scrollbar to recalculate its appearance
		containerPanel.validate();
		scp.revalidate();
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

		chkLocalScript.undisenable(containerLine.typeF);
		chkRemoteScript.undisenable(containerLine.typeF);

		containerLines.remove(containerLine);
		containerLinePanel.remove(containerLine);
		containerPanel.validate();
	}

	protected void setProgressPanelContent (String title, JComponent content) {
		flowDialog.prepareShow(title+" - Progress", content);
	}

	protected void setAdvancedVisible (boolean b) {
		for (int i = 0; i < buttonPanelAdvanced.getComponentCount(); i++) {
			Component c = buttonPanelAdvanced.getComponent(i);
			if (c instanceof TwoStageEnableButton)
				((TwoStageEnableButton) c).setEnabled2(b);
			else
				c.setEnabled(b);
		}
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

	protected Vector<ButtonPair> buttonPairs = new Vector<ButtonPair>();

	protected void makeButtonPair (JButton on, JButton off) {
		// keep reference somewhere to prevent garbage collection
		buttonPairs.add(new ButtonPair(on, off));
	}


	protected int getMode () {
		ModeType mode = master.controller.project.getMode();
		return mode.getType();
	}

	// =====================================================
	// ===================== API ===========================
	// =====================================================


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


	//
	// ====================================================================
	// ========================= Inner Types ==============================
	// ====================================================================
	//

	/**
	 * This button provides a method <code>setEnabled2()</code> the setting of which
	 * overrides the setting of the standard inherited method <code>setEnabled()</code>.
	 */
	protected class TwoStageEnableButton extends JButton {

		public TwoStageEnableButton(String text) {
			super(text);
		}

		public TwoStageEnableButton(Action a) {
			super(a);
		}

		public TwoStageEnableButton(Icon icon) {
			super(icon);
		}

		/**
		 * After setting this to <code>true</code>, this button behaves as normal. After
		 * setting this to <code>false</code>, the button is disabled and can't be
		 * enabled through the standard <code>setEnabled()</code>. It can only be
		 * re-enabled through another call to this method.
		 */
		public void setEnabled2 (boolean b) {
			super.setEnabled(b);
			this.invariablyDisabled = !b;
		}

		protected boolean invariablyDisabled;

		/**
		 * This implementation ignores invokations if <code>setEnabled2()</code> has been
		 * set to <code>false</code>.
		 */
		@Override
		public void setEnabled (boolean b) {
			if (this.invariablyDisabled)
				return; // the other method must be used to revoke the disabled-status
			super.setEnabled(b);
		}

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

	protected class MyRadioButton extends JRadioButton implements ItemListener {

		Vector<JComponent> comps = new Vector<JComponent>();

		MyRadioButton(String text) {
			super(text);
			addItemListener(this);
		}

		// use this to add swing components that will be automatically
		// enabled/disabled according to the selection status of this radiobutton
		protected void disenable (JComponent c) {
			comps.add(c);
		}

		protected void undisenable (JComponent c) {
			comps.remove(c);
		}

		public void itemStateChanged (ItemEvent e) {
			respondToIsSelected(e.getStateChange() == ItemEvent.SELECTED);
		}

		// needed since doClick() doesn't trigger the itemStateChange event handler
		@Override
		public void setSelected (boolean b) {
			respondToIsSelected(b);
			super.setSelected(b);
		}

		protected void respondToIsSelected (boolean b) {
			for (int i = 0; i < comps.size(); i++)
				((JComponent) comps.elementAt(i)).setEnabled(b);
		}

	}

	protected class ContainerLine extends JPanel implements ActionListener {

		JRadioButton selectB = new JRadioButton();
		JLabel lblMnemomic = new JLabel("x");
		Button btnStart, btnStop, btnData;
		JTextField nameF = new JTextField(10);
		JComboBox typeF = new JComboBox(new String[]{"java", "cpp", "py"});

		ContainerLine() {
			super();
			this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
			this.add(lblMnemomic);
			this.add(Box.createHorizontalStrut(5));

			JPanel b = new JPanel(new GridLayout(0, 2, 5, 5));
			b.add(nameF);
			b.add(typeF);
			this.add(b);
			this.add(Box.createHorizontalStrut(5));

			selectB.addFocusListener(focusListener);
			buttonGroup2.add(selectB);
			// This is now invisible, note that it will  
			// internally be SELECTED BY PRESSING either
			// one of the 2 BUTTONS
			/* 
			 * this.add(selectB); 
			 */

			// support for selecting a container line 
			// by placing the cursor in the name field
			nameF.addFocusListener(new FocusAdapter() {
				@Override
				public void focusGained (FocusEvent e) {
					selectB.setSelected(true);
					validateAfterProgrammaticUpdate(selectB);
				}
			});
			
			this.add(btnStart = new Button(master.icons.getStartIcon(), this));
			btnStart.setToolTipText("Start this Container");
			flowDialog.disenable(btnStart);

			this.add(btnStop = new Button(master.icons.getStopIcon(), this));
			btnStop.setToolTipText("Stop this Container");
			flowDialog.disenable(btnStop);

			makeButtonPair(btnStart, btnStop);

			this.add(btnData = new Button(master.icons.getConfigIcon(), this));
			btnData.setToolTipText("Choose where to run this container");
			
		}

		public void actionPerformed (ActionEvent e) {
			selectB.setSelected(true);
			validateAfterProgrammaticUpdate(selectB);

			if (e.getSource() == btnStart) {
				boolean ok = doSanityCheck();
				if (!ok)
					return;
				btnStartContainer.doClick();
			}

			if (e.getSource() == btnStop) {
				boolean ok = doSanityCheck();
				if (!ok)
					return;
				btnStopContainer.doClick();
			}

			if (e.getSource() == btnData) {
				btnConfigureContainer.doClick();
			}
		}

		boolean doSanityCheck () {
			// --- little sanity check for a popular mistake
			if (nameF.getText().trim().equals("")) {
				JOptionPane.showMessageDialog(master.frame, "The specified container name is invalid", "Invalid Container Settings",
						JOptionPane.ERROR_MESSAGE);
				return false;
			}
			return true;
		}

		class Button extends JButton {

			Button(Icon icon, ContainerLine cl) {
				super(icon);
				init(cl);
			}

			Button(String text, ContainerLine cl) {
				super(text);
				init(cl);
			}

			void init (ContainerLine cl) {
				this.setMargin(new Insets(1, 0, 1, 0));
				this.addActionListener(cl);
			}

		}

	}


	//
	// ========================= Actions =========================
	//

	/**
	 * Invokes super which runs the response in its own thread. Then makes the progress
	 * panel for the action-response visible.
	 */
	protected abstract class ActionBaseClass2 extends CommandCenterGui.ActionBaseClass {

		protected ActionBaseClass2(String name) {
			master.super(name);
		}

		protected ActionBaseClass2(String name, Icon icon) {
			master.super(name, icon);
		}

		@Override
		public void actionPerformed (final ActionEvent evt) {
			setProgressPanelContent("Progress", new JLabel(" processing ... "));

			super.actionPerformed(evt); // super-implementation will invoke actionPerformed()

			// show progress
			flowDialog.bringUp();
		}
	}



	protected class ActionStartAcs extends ActionBaseClass2 {

		protected ActionStartAcs() {
			super("Start", master.icons.getStartIcon());
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
					setProgressPanelContent("Starting Acs", localScriptFlowPanel);
					master.controller.executeAcs.startLocalScript(master.giveComplexListener("Acs"));
					managerStarted();
					break;

				case ModeType.REMOTE_TYPE :
					if (Boolean.getBoolean("AcsCommandCenter.useServicesDaemon")) {
						setProgressPanelContent("Starting Acs", remoteServicesDaemonFlowPanel);
						master.controller.executeAcs.startRemoteDemonic(master.giveSimpleListener("Acs"));
						managerStarted();
					} else {
						setProgressPanelContent("Starting Acs", remoteFlowPanel);
						master.controller.executeAcs.startRemote(master.giveComplexListener("Acs"));
						managerStarted();
					}
					break;

				case ModeType.JAVA_TYPE :
					// --- trigger two steps (code is a duplicate of the single actions)
					setProgressPanelContent("Starting Cdb", localJavaFlowPanel);
					flowDialog.bringUp();
					master.controller.executeServices.startLocalJava(master.giveComplexListener("Acs"));

					setProgressPanelContent("Starting Manager", localJavaFlowPanel);
					flowDialog.bringUp();
					master.controller.executeManager.startLocalJava(master.giveComplexListener("Acs"));

					managerStarted();
					break;
			}

		}
	}

	protected class ActionStopAcs extends ActionBaseClass2 {

		protected ActionStopAcs() {
			super("Stop", master.icons.getStopIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					setProgressPanelContent("Stopping Acs", localScriptFlowPanel);
					master.controller.executeAcs.stopLocalScript(master.giveComplexListener("Acs"));
					managerStopped();
					break;
				case ModeType.REMOTE_TYPE :
					if (Boolean.getBoolean("AcsCommandCenter.useServicesDaemon")) {
						setProgressPanelContent("Stopping Acs", remoteServicesDaemonFlowPanel);
						master.controller.executeAcs.stopRemoteDemonic(master.giveSimpleListener("Acs"));
						managerStopped();
					} else {
						setProgressPanelContent("Stopping Acs", remoteFlowPanel);
						master.controller.executeAcs.stopRemote(master.giveComplexListener("Acs"));
						managerStopped();
					}
					break;
				case ModeType.JAVA_TYPE :
					// --- trigger two steps (code is a duplicate of the single actions)
					setProgressPanelContent("Stopping Manager", localJavaFlowPanel);
					flowDialog.bringUp();
					master.controller.executeManager.stopLocalJava();

					setProgressPanelContent("Stopping Cdb", singleStepFlowPanel);
					flowDialog.bringUp();
					master.controller.executeServices.stopLocalJava();

					managerStopped();
					break;
			}
		}
	}

	protected class ActionStartServices extends ActionBaseClass2 {

		protected ActionStartServices() {
			super("", master.icons.getStartIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					setProgressPanelContent("Starting Services", localScriptFlowPanel);
					master.controller.executeServices.startLocalScript(master.giveComplexListener("Services"));
					break;

				case ModeType.REMOTE_TYPE :
					setProgressPanelContent("Starting Services", remoteFlowPanel);
					master.controller.executeServices.startRemote(master.giveComplexListener("Services"));
					break;

				case ModeType.JAVA_TYPE :
					setProgressPanelContent("Starting Cdb", localJavaFlowPanel);
					master.controller.executeServices.startLocalJava(master.giveComplexListener("Services"));
					break;
			}
		}
	}

	protected class ActionStopServices extends ActionBaseClass2 {

		protected ActionStopServices() {
			super("", master.icons.getStopIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					setProgressPanelContent("Stopping Services", localScriptFlowPanel);
					master.controller.executeServices.stopLocalScript(master.giveComplexListener("Services"));
					break;

				case ModeType.REMOTE_TYPE :
					setProgressPanelContent("Stopping Services", remoteFlowPanel);
					master.controller.executeServices.stopRemote(master.giveComplexListener("Services"));
					break;

				case ModeType.JAVA_TYPE :
					setProgressPanelContent("Stopping Cdb", singleStepFlowPanel);
					master.controller.executeServices.stopLocalJava();
					break;

			}
		}
	}

	protected class ActionStartManager extends ActionBaseClass2 {

		protected ActionStartManager() {
			super("", master.icons.getStartIcon());
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
					setProgressPanelContent("Starting Manager", localScriptFlowPanel);
					master.controller.executeManager.startLocalScript(master.giveComplexListener("Manager"));
					managerStarted();
					break;

				case ModeType.REMOTE_TYPE :
					setProgressPanelContent("Starting Manager", remoteFlowPanel);
					master.controller.executeManager.startRemote(master.giveComplexListener("Manager"));
					managerStarted();
					break;

				case ModeType.JAVA_TYPE :
					setProgressPanelContent("Starting Manager", localJavaFlowPanel);
					master.controller.executeManager.startLocalJava(master.giveComplexListener("Manager"));
					managerStarted();
					break;
			}
		}
	}

	protected class ActionStopManager extends ActionBaseClass2 {

		protected ActionStopManager() {
			super("", master.icons.getStopIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			switch (getMode()) {
				case ModeType.LOCAL_TYPE :
					setProgressPanelContent("Stopping Manager", localScriptFlowPanel);
					master.controller.executeManager.stopLocalScript(master.giveComplexListener("Manager"));
					managerStopped();
					break;

				case ModeType.REMOTE_TYPE :
					setProgressPanelContent("Stopping Manager", remoteFlowPanel);
					master.controller.executeManager.stopRemote(master.giveComplexListener("Manager"));
					managerStopped();
					break;

				case ModeType.JAVA_TYPE :
					setProgressPanelContent("Stopping Manager", localJavaFlowPanel);
					master.controller.executeManager.stopLocalJava();
					managerStopped();
					break;

			}
		}
	}

	protected class ActionStartContainer extends ActionBaseClass2 {

		protected ActionStartContainer() {
			super("Start Cont");
		}

		@Override
		protected void actionPerformed () throws Throwable {
			int contNumber = master.controller.project.getContainers().getSelect();
			String contName = master.controller.project.getContainers().getContainer(contNumber).getName();
			boolean useCustomSettings = master.controller.project.getContainers().getContainer(contNumber).getUseDedicatedSettings();

			// msc (2004-11-16): gianluca finds it more reasonable that "custom" implies "remote"
			int mode = (useCustomSettings) ? ModeType.REMOTE_TYPE : getMode();
			
			// msc (2005-11-23): ExecuteContainer API has changed to allow concurrent starts of containers
			RunModel runmodel =  master.controller.model.createViewOnContainer(contNumber);

			switch (mode) {
				case ModeType.LOCAL_TYPE :
					setProgressPanelContent("Starting "+contName, localScriptFlowPanel);
					master.controller.executeContainer.startLocalScript(runmodel, master.giveComplexListener(contName));
					break;
				case ModeType.REMOTE_TYPE :
					if (Boolean.getBoolean("AcsCommandCenter.useContainerDaemon")) {
						setProgressPanelContent("Starting "+contName, remoteContainerDaemonFlowPanel);
						master.controller.executeContainer.startRemoteDemonic(runmodel, master.giveSimpleListener(contName));
					} else {
						setProgressPanelContent("Starting "+contName, remoteFlowPanel);
						master.controller.executeContainer.startRemote(runmodel, master.giveComplexListener(contName));
					}
					break;
				case ModeType.JAVA_TYPE :
					setProgressPanelContent("Starting "+contName, localJavaFlowPanel);
					master.controller.executeContainer.startLocalJava(runmodel, master.giveComplexListener(contName));
					break;
			}
		}
	}

	protected class ActionStopContainer extends ActionBaseClass2 {

		protected ActionStopContainer() {
			super("Stop Cont");
		}

		@Override
		protected void actionPerformed () throws Throwable {
			int contNumber = master.controller.project.getContainers().getSelect();
			String contName = master.controller.project.getContainers().getContainer(contNumber).getName();
			boolean useCustomSettings = master.controller.project.getContainers().getContainer(contNumber).getUseDedicatedSettings();

			// msc (2004-11-16): gianluca finds it more reasonable that "custom" implies "remote"
			int mode = (useCustomSettings) ? ModeType.REMOTE_TYPE : getMode();

			// msc (2005-11-23): ExecuteContainer API has changed to allow concurrent starts of containers
			RunModel runmodel =  master.controller.model.createViewOnContainer(contNumber);

			switch (mode) {
				case ModeType.LOCAL_TYPE :
					setProgressPanelContent("Stopping "+contName, localScriptFlowPanel);
					master.controller.executeContainer.stopLocalScript(runmodel, master.giveComplexListener(contName));
					break;
				case ModeType.REMOTE_TYPE :
					if (Boolean.getBoolean("AcsCommandCenter.useContainerDaemon")) {
						setProgressPanelContent("Stopping "+contName, remoteContainerDaemonFlowPanel);
						master.controller.executeContainer.stopRemoteDemonic(runmodel, master.giveSimpleListener(contName));
					} else {
						setProgressPanelContent("Stopping "+contName, remoteFlowPanel);
						master.controller.executeContainer.stopRemote(runmodel, master.giveComplexListener(contName));
					}
					break;
				case ModeType.JAVA_TYPE :
					setProgressPanelContent("Stopping "+contName, localJavaFlowPanel);
					master.controller.executeContainer.stopLocalJava(runmodel);
					break;
			}
		}
	}


	protected class ActionStartAllContainers extends ActionBaseClass {

		protected ActionStartAllContainers() {
			master.super("", master.icons.getStartIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			ContainersT conts = master.controller.project.getContainers();
			for (int contNumber = 0; contNumber < conts.getContainerCount(); contNumber++) {
				conts.setSelect(contNumber);
				setProgressPanelContent("Progress", new JLabel(" processing ... "));
				flowDialog.bringUp();
				actStartContainer.actionPerformed();
			}
		}
	}


	protected class ActionStopAllContainers extends ActionBaseClass {

		protected ActionStopAllContainers() {
			master.super("", master.icons.getStopIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			ContainersT conts = master.controller.project.getContainers();
			for (int contNumber = conts.getContainerCount() - 1; contNumber >= 0; contNumber--) {
				conts.setSelect(contNumber);
				setProgressPanelContent("Progress", new JLabel(" processing ... "));
				flowDialog.bringUp();
				actStopContainer.actionPerformed();
			}
		}
	}


	protected class ActionKillAcs extends ActionBaseClass2 {

		protected ActionKillAcs() {
			super("Kill", master.icons.getStopIconRed());
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
					setProgressPanelContent("Killing Acs", localScriptFlowPanel);
					master.controller.executeAcs.killLocalScript(master.giveComplexListener("Acs"));
					break;
				case ModeType.REMOTE_TYPE :
					setProgressPanelContent("Killing Acs", remoteFlowPanel);
					master.controller.executeAcs.killRemote(master.giveComplexListener("Acs"));
					break;
				case ModeType.JAVA_TYPE :
					// Should we exit the vm (that would guarantee all delegates die), or what?
					break;
			}
		}
	}

	protected class ActionConfigureAllContainers extends CommandCenterGui.ActionBaseClass {

		protected ActionConfigureAllContainers() {
			master.super("", master.icons.getConfigIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			master.showManagerLocationForContainersDialog();
		}
	}

	protected class ActionMoreContainers extends CommandCenterGui.ActionBaseClass {

		protected ActionMoreContainers() {
			master.super("", master.icons.getPlusIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			addContainerLine("", "", false);
			master.controller.moreContainers();
		}
	}

	protected class ActionLessContainers extends CommandCenterGui.ActionBaseClass {

		protected ActionLessContainers() {
			master.super("", master.icons.getMinusIcon());
		}

		@Override
		protected void actionPerformed () throws Throwable {
			master.controller.lessContainers();
			lessContainerLines();
		}
	}

	protected class ActionConfigureContainer extends CommandCenterGui.ActionBaseClass {

		protected ActionConfigureContainer() {
			master.super("Configure Container");
		}

		@Override
		protected void actionPerformed () throws Throwable {
			master.showContainerLocationDialog();
		}
	}

	protected class ActionMoveContainerUp extends CommandCenterGui.ActionBaseClass {

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

	protected class ActionMoveContainerDown extends CommandCenterGui.ActionBaseClass {

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
	
	
	protected class ActionShowAdvanced extends CommandCenterGui.ActionBaseClass {

		protected ActionShowAdvanced() {
			master.super("advanced", null);
		}

		@Override
		protected void actionPerformed () throws Throwable {
			boolean b = btnShowAdvanced.isSelected();
			setAdvancedVisible(b);
		}
	}

	protected class ActionUseDaemons extends CommandCenterGui.ActionBaseClass {

		protected ActionUseDaemons() {
			master.super("Use Acs 7.0 Daemons", null);
		}

		@Override
		protected void actionPerformed () throws Throwable {
			boolean b = chkUseDaemons.isSelected();
			System.setProperty("AcsCommandCenter.useServicesDaemon", String.valueOf(b));
			System.setProperty("AcsCommandCenter.useContainerDaemon", String.valueOf(b));
			if (b) { 
				String msg = " You have enabled daemon mode." //
						+ "\n\n" //
						+ "Acs Command Center will use Acs Daemons to start or stop:\n" //
						+ "   * Acs Suite\n" //
						+ "   * Containers\n" //
						+ "Note that daemons will not be used for the Services and Manager controls in the\n" //
						+ "advanced section. Instead, ssh will be used. The same is true for the \"Kill\" button.\n" //
						+ "\n"
						+ "In daemon mode, there's no need to specify\n" //
						+ "   * Username\n" //
						+ "   * Password\n" //
						+ "Instead, all Acs processes will run under the account of their respective daemon.\n" //
						+ "\n" //
						+ "Finally note that the output of processes started through Acs Daemons\n" //
						+ "will not be available in the Log Area of Acs Command Center" //
						;
				JOptionPane.showMessageDialog(master.frame, msg, "About Acs 7.0 Daemons", JOptionPane.OK_OPTION);
			}
		}
	}

	protected class ActionShowCdbChooser extends CommandCenterGui.ActionBaseClass {

		public ActionShowCdbChooser() {
			master.super("@");
		}

		@Override
		public void actionPerformed () {

			File result = master.showCdbChooser();

			if (result == null) {
				return;
			}

			// TODO(msc): put this logic into a method, analogously to showCdbBrowser()
			try {
				boolean success = master.controller.extract(result);
				if (success) {
					String newCdbRoot = master.cdbChooser.getCdbRoot();
					if (newCdbRoot != null)
						cdbrootF.setText(newCdbRoot);
					// otherwise we failed to compute the name
					return;
				}

				// in case of exception, or if success == false, show a message
			} catch (Exception exc) {}

			JOptionPane.showMessageDialog(master.frame, "Could not extract the Cdb. See console for details");

		}
	}

	protected class MyFocusListener extends FocusAdapter {

		@Override
		public void focusLost (FocusEvent evt) {
			master.writeFrontPanelToModel();
		}


	}

}
