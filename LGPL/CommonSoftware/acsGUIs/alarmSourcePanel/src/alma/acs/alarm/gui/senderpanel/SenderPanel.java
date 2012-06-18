/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2011
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarm.gui.senderpanel;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Collection;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.AlarmDescriptorType;
import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.Triplet;
import alma.acs.alarm.gui.senderpanel.table.AlarmsSentTable;
import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.ClientLogManager;
import cern.laser.source.alarmsysteminterface.FaultState;

/** 
 * A panel to send alarms.
 * 
 * The panel allows the user to send alarms with a simple GUI,
 * by triplet or selecting a file.
 * 
 * While sending by triplet the user defines the triplet, the activation mode
 * and the user properties. There is a button to send the alarm.
 * 
 * To send by file, the user select a text file with the definition of the alarms then it has 3 options
 * <UL>
 * 	<LI>activate all the alarms of the file
 *  <LI>terminate all the alarms of the file
 *  <LI>randomly activate/clear the alarms of the file
 * </UL>
 * 
 * The last option activates a thread that activates/clears the alarms defined in the the file. 
 * The user has to press again the button to stop sending alarms in this way.
 * 
 * @author acaproni
 */
public class SenderPanel extends JFrame implements ActionListener, DocumentListener, AlarmSentListener, SlowTaskListener {
	
	/**
	 * ACS component client
	 */
	private AdvancedComponentClient acsClient;
	
	/**
	 * The logger
	 */
	private Logger logger;
	
	/**
	 * ContainerServices
	 */
	private final ContainerServices contSvcs;
	
	/**
	 * The text file to insert the triplet
	 */
	private final JTextField tripletTF = new JTextField();
	
	/**
	 * The text file to insert the properties
	 */
	private final JTextField propsTF = new JTextField();
	
	/**
	 * The (active) state of the alarm to send
	 * <P>
	 * The name of the button contains the FaultState descriptor
	 */
	private final JRadioButton activeRB = new JRadioButton("Active");
	
	/**
	 * The (change) state of the alarm to send
	 * <P>
	 * The name of the button contains the FaultState descriptor
	 */
	private final JRadioButton changeRB = new JRadioButton("Change");
	
	/**
	 * The (terminate) state of the alarm to send
	 * <P>
	 * The name of the button contains the FaultState descriptor
	 */
	private final JRadioButton terminateRB = new JRadioButton("Terminate");
	
	/**
	 * The (instant) state of the alarm to send
	 * <P>
	 * The name of the button contains the FaultState descriptor
	 */
	private final JRadioButton instantRB = new JRadioButton("Instant");
	
	/**
	 * Select the sending mode by triplet
	 */
	private final JRadioButton sendFromTripletRB = new JRadioButton("Triplet");
	
	/**
	 * Select the sending mode by file
	 */
	private final JRadioButton sendFromFileRB = new JRadioButton("File");
	
	/**
	 * Select the sending mode by file
	 */
	private final JRadioButton sendFromCdbRB = new JRadioButton("TM/CDB");
	
	/**
	 * It contains the radio buttons for the mode of sending alarms
	 */
	private final ButtonGroup sendingBG = new ButtonGroup();
	
	/**
	 * The array with the buttons to facilitate getting their state
	 */
	private final JRadioButton[] descriptorBtns = {
			activeRB,terminateRB,changeRB,instantRB
	};
	
	/**
	 * It contains the radio buttons for the activation
	 * of an alarm
	 */
	private final ButtonGroup activationBG = new ButtonGroup();
	
	/**
	 * The button to send the alarm
	 */
	private final JButton sendTripletBtn = new JButton("Send");
	
	/**
	 * The button to choose the file
	 */
	private final JButton chooseFiletBtn = new JButton("File");
	
	/**
	 * The label with the name of the file
	 */
	private final JTextField fileNameTF = new JTextField();
	
	/**
	 * The label with the number of alarms read from TM/CDB
	 */
	private final JTextField cdbAlarmsTF = new JTextField();
	
	/**
	 * The button to activate all the alarms read from the file
	 */
	private final JButton activateFromFileBtn = new JButton("Activate all");
	
	/**
	 * The button to terminate all the alarms read from the file
	 */
	private final JButton terminateFromFileBtn = new JButton("Terminate all");
	
	/**
	 * The button to randomly activate/terminate the alarms contained in the file
	 */
	private final JToggleButton cycleFromFileBtn = new JToggleButton("Cycle");
	
	/**
	 * The button to activate all the alarms read from TM/CDB
	 */
	private final JButton activateFromCdbBtn = new JButton("Activate all");
	
	/**
	 * The button to terminate all the alarms read from TM/CDB
	 */
	private final JButton terminateFromCdbBtn = new JButton("Terminate all");
	
	/**
	 * The button to randomly activate/terminate the alarms read from TM/CDB
	 */
	private final JToggleButton cycleFromCdbBtn = new JToggleButton("Cycle");
	
	/**
	 * The list of the active alarms: the user can select
	 * and clear them
	 */
	private final AlarmsSentTable alarmsSent = new AlarmsSentTable();
	
	/**
	 * The button to clear the alarm selected int he list
	 */
	private final JButton clearSelectedAlarmsBtn = new JButton("Clear selected");
	
	/**
	 * The button to clear all the alarms
	 */
	private final JButton clearAllBtn = new JButton("Clear all");
	
	/**
	 * The button to close the application
	 */
	private final JButton closeBtn = new JButton("Done");
	
	/**
	 * The object to send alarms (ACS alarm sender)
	 */
	private final AlarmSender alarmSender;
	
	/**
	 * The helper to send alarms read from a file
	 */
	private final FileSender fileSender;
	
	/**
	 * The helper to send alarms read from TM/CDB
	 */
	private final CDBSender cdbSender;
	
	/**
	 * The progress bar for long lasting operations from file
	 */
	private final JProgressBar fileTasksPB = new JProgressBar();
	
	/**
	 * The progress bar for long lasting operations from TM/CDB
	 */
	private final JProgressBar cdbTasksPB = new JProgressBar();
	
	/**
	 * The number of alarm read from the TM/CDB
	 */
	private volatile int numOfAlarmsFromCDB=0;
	
	/**
	 * The number of alarm read from a file
	 */
	private volatile int numOfAlarmsFromFile=0;
	
	/**
	 * The panel with the widgets to send larms by triplet
	 */
	private final JPanel tripletWdgtsPnl = new JPanel();
	
	/**
	 * The panel with the widgets to send alarms by file
	 */
	private final JPanel fileWdgtsPnl = new JPanel();	
	
	/**
	 * The panel with the widgets to send alarms by TM/CDB
	 */
	private final JPanel cdbWdgtsPnl = new JPanel();

	/**
	 * Constructor
	 */
	public SenderPanel() throws Exception {
		super("Alarm sender panel");
		try {
			initACS();
		} catch (Throwable t) {
			JOptionPane.showMessageDialog(null, t.getMessage(), "Error initializing ACS", JOptionPane.ERROR_MESSAGE);
			throw new Exception("Error intializing the ACS client",t);
		}
		contSvcs=acsClient.getContainerServices();
		try {
			alarmSender = new AlarmSender(contSvcs);
		} catch (Throwable t) {
			JOptionPane.showMessageDialog(null, t.getMessage(), "Error initializing AlarmSender", JOptionPane.ERROR_MESSAGE);
			throw new Exception("Error intializing the alarm sender",t);
		} 
		initGUI();
		alarmSender.addListener(this);
		alarmSender.start();
		fileSender=new FileSender(this,contSvcs,alarmSender);
		fileSender.addSlowTaskListener(this);
		cdbSender=new CDBSender(this,contSvcs,alarmSender);
		cdbSender.addSlowTaskListener(this);
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initGUI() {
		super.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosed(WindowEvent e) {
				super.windowClosed(e);
				close();
			}
		});
	
		
		/////////////////////////////////////////////////////////
		// The panel to send alarms by triplet or file
		//
		// This panel contains:
		//  * one panel to send alarms by triplet
		//  * one to send alarms by file
		//  * one to send alarms by CDB
		// The user can select only one sending type by means
		// of a radio button.
		/////////////////////////////////////////////////////////
		
		/////////////////////////////////////////////////////////
		// The panel to send alarm by writing the triplet
		//
		// The panel has a radio button to select the sending by 
		// triplet and a panel that contains all the widgets 
		// needed by this sending mode.
		/////////////////////////////////////////////////////////
		JPanel tripletPnl = new JPanel(new BorderLayout());
		tripletPnl.add(sendFromTripletRB,BorderLayout.WEST);
		sendTripletBtn.setEnabled(false);
		
		// The panel with all the widgets to send alarms by triplets
		// It has 3 lines: triplet, descriptor and properties
		tripletWdgtsPnl.setBorder(BorderFactory.createTitledBorder("Triplet sender"));
		BoxLayout tripetWdgtsLayoout = new BoxLayout(tripletWdgtsPnl, BoxLayout.Y_AXIS);
		tripletWdgtsPnl.setLayout(tripetWdgtsLayoout);
		
		// Line 1: triplet text field and send button
		JPanel sendTripletPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		sendTripletPnl.add(tripletTF);
		tripletTF.setColumns(40);
		tripletTF.getDocument().addDocumentListener(this);
		tripletTF.setToolTipText("Insert triplet: FaultFamily,FaultMember,1");
		sendTripletBtn.setEnabled(false);
		sendTripletBtn.addActionListener(this);
		sendTripletPnl.add(sendTripletBtn);
		
		// Line 2: the descriptor
		JPanel descriptorWdgtPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		descriptorWdgtPnl.add(new JLabel("Descriptor: "));
		activeRB.setName(FaultState.ACTIVE);
		terminateRB.setName(FaultState.TERMINATE);
		changeRB.setName(FaultState.CHANGE);
		instantRB.setName(FaultState.INSTANT);
		descriptorWdgtPnl.add(activeRB);
		descriptorWdgtPnl.add(terminateRB);
		descriptorWdgtPnl.add(changeRB);
		descriptorWdgtPnl.add(instantRB);
		activeRB.setSelected(true);
		
		// Line 3: the properties
		JPanel propsWdgtsPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		propsWdgtsPnl.add(new JLabel("Properties"));
		propsWdgtsPnl.add(propsTF);
		propsTF.setColumns(40);
		propsTF.setToolTipText("User properties key=val, key2=val2,....");
		
		// Add the three lines of widgets
		tripletWdgtsPnl.add(sendTripletPnl);
		tripletWdgtsPnl.add(descriptorWdgtPnl);
		tripletWdgtsPnl.add(propsWdgtsPnl);
		
		tripletPnl.add(tripletWdgtsPnl,BorderLayout.CENTER);
		
		/////////////////////////////////////////////////////////
		// The panel to send alarm by selecting a file
		//
		// The panel has a radio button to select the sending by 
		// file and a panel that contains all the widgets 
		// needed by this sending mode.
		/////////////////////////////////////////////////////////
		JPanel filePnl = new JPanel(new BorderLayout());
		filePnl.add(sendFromFileRB,BorderLayout.WEST);
		sendFromFileRB.setPreferredSize(sendFromTripletRB.getPreferredSize());
		sendFromFileRB.setMinimumSize(sendFromTripletRB.getMinimumSize());
		// The panel with all the widgets to send alarms by file
		// It has 2 lines: one to select the file and another one with the control buttons
		fileWdgtsPnl.setBorder(BorderFactory.createTitledBorder("File sender"));
		BoxLayout fileWdgtsLayoout = new BoxLayout(fileWdgtsPnl, BoxLayout.Y_AXIS);
		fileWdgtsPnl.setLayout(fileWdgtsLayoout);
		
		// Line 1: widgets to select the file
		JPanel selectFilePnl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		fileNameTF.setColumns(40);
		fileNameTF.setEditable(false);
		fileNameTF.setText("");
		selectFilePnl.add(fileNameTF);
		selectFilePnl.add(chooseFiletBtn);
		
		// Line 2: widgets to control the sending mode
		JPanel ctrlsFilePnl = new JPanel(new BorderLayout());
		JPanel ctrlsButtonsPnl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ctrlsFilePnl.add(fileTasksPB,BorderLayout.CENTER);
		fileTasksPB.setValue(0);
		fileTasksPB.setIndeterminate(false);
		ctrlsButtonsPnl.add(activateFromFileBtn);
		ctrlsButtonsPnl.add(terminateFromFileBtn);
		ctrlsButtonsPnl.add(cycleFromFileBtn);
		ctrlsFilePnl.add(ctrlsButtonsPnl,BorderLayout.EAST);
		
		// Add the two lines of widgets
		fileWdgtsPnl.add(selectFilePnl);
		fileWdgtsPnl.add(ctrlsFilePnl);
		
		filePnl.add(fileWdgtsPnl,BorderLayout.CENTER);
		
		/////////////////////////////////////////////////////////
		// The panel to send alarm read from TM/CDB
		//
		// The panel has a radio button to select the sending by 
		// file and a panel that contains all the widgets 
		// needed by this sending mode.
		/////////////////////////////////////////////////////////
		JPanel cdbPnl = new JPanel(new BorderLayout());
		cdbPnl.add(sendFromCdbRB,BorderLayout.WEST);
		sendFromCdbRB.setPreferredSize(sendFromTripletRB.getPreferredSize());
		sendFromCdbRB.setMinimumSize(sendFromTripletRB.getMinimumSize());
		// The panel with all the widgets to send alarms by file
		// It has 2 lines: one to select the file and another one with the control buttons
		cdbWdgtsPnl.setBorder(BorderFactory.createTitledBorder("TM/CDB sender"));
		BoxLayout cdbWdgtsLayoout = new BoxLayout(cdbWdgtsPnl, BoxLayout.Y_AXIS);
		cdbWdgtsPnl.setLayout(cdbWdgtsLayoout);
		
		// Line 1: widgets to show the number of alarms read from TM/CDB
		JPanel showCdbPnl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		cdbAlarmsTF.setColumns(40);
		cdbAlarmsTF.setEditable(false);
		cdbAlarmsTF.setText("");
		showCdbPnl.add(cdbAlarmsTF);
		
		// Line 2: widgets to control the sending mode
		JPanel ctrlsCDBPnl = new JPanel(new BorderLayout());
		JPanel ctrlsCDBButtonsPnl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ctrlsCDBPnl.add(cdbTasksPB,BorderLayout.CENTER);
		cdbTasksPB.setValue(0);
		cdbTasksPB.setIndeterminate(false);
		ctrlsCDBButtonsPnl.add(activateFromCdbBtn);
		ctrlsCDBButtonsPnl.add(terminateFromCdbBtn);
		ctrlsCDBButtonsPnl.add(cycleFromCdbBtn);
		ctrlsCDBPnl.add(ctrlsCDBButtonsPnl,BorderLayout.EAST);
		
		// Add the two lines of widgets
		cdbWdgtsPnl.add(showCdbPnl);
		cdbWdgtsPnl.add(ctrlsCDBPnl);
		
		cdbPnl.add(cdbWdgtsPnl,BorderLayout.CENTER);
		
		// Finally add the triplet, the file and the CDB panels to the alarm sender panel
		JPanel alrmSenderPnl = new JPanel(new BorderLayout());
		alrmSenderPnl.add(tripletPnl,BorderLayout.NORTH);
		JPanel fileAndCDBSenderPnl = new JPanel(new BorderLayout());
		fileAndCDBSenderPnl.add(filePnl,BorderLayout.NORTH);
		fileAndCDBSenderPnl.add(cdbPnl,BorderLayout.SOUTH);
		alrmSenderPnl.add(fileAndCDBSenderPnl,BorderLayout.SOUTH);
		alrmSenderPnl.setBorder(BorderFactory.createTitledBorder("Send alarm"));
		add(alrmSenderPnl,BorderLayout.NORTH);
		
		/////////////////////////////////////////////////////////
		// The panel to clear alarms
		/////////////////////////////////////////////////////////
		JPanel alarmClearingPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		alarmClearingPnl.setBorder(BorderFactory.createTitledBorder("Alarm clearing"));
		JScrollPane scrollPane = new JScrollPane(alarmsSent);
		alarmsSent.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		alarmClearingPnl.add(scrollPane);
		JPanel clearingBtnsPnl = new JPanel();
		BoxLayout layout = new BoxLayout(clearingBtnsPnl, BoxLayout.Y_AXIS);
		clearingBtnsPnl.setLayout(layout);
		clearingBtnsPnl.add(clearAllBtn);
		Dimension minSize = new Dimension(5, 10);
		Dimension prefSize = new Dimension(5, 10);
		Dimension maxSize = new Dimension(Short.MAX_VALUE, 10);
		clearingBtnsPnl.add(new Box.Filler(minSize, prefSize, maxSize));
		clearingBtnsPnl.add(clearSelectedAlarmsBtn);
		clearAllBtn.setEnabled(false);
		clearAllBtn.addActionListener(this);
		clearSelectedAlarmsBtn.setEnabled(false);
		clearSelectedAlarmsBtn.addActionListener(this);
		alarmClearingPnl.add(clearingBtnsPnl);
		add(alarmClearingPnl,BorderLayout.CENTER);
		
		JPanel donePnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		donePnl.add(closeBtn);
		closeBtn.addActionListener(this);
		add(donePnl,BorderLayout.SOUTH);
		
		// Add the listeners
		sendFromTripletRB.setSelected(true);
		
		sendFromFileRB.addActionListener(this);
		sendFromTripletRB.addActionListener(this);
		sendFromCdbRB.addActionListener(this);
		chooseFiletBtn.addActionListener(this);
		activateFromFileBtn.addActionListener(this);
		terminateFromFileBtn.addActionListener(this);
		cycleFromFileBtn.addActionListener(this);
		activateFromCdbBtn.addActionListener(this);
		terminateFromCdbBtn.addActionListener(this);
		cycleFromCdbBtn.addActionListener(this);
		
		// Set the button groups
		sendingBG.add(sendFromTripletRB);
		sendingBG.add(sendFromFileRB);
		sendingBG.add(sendFromCdbRB);
		activationBG.add(activeRB);
		activationBG.add(changeRB);
		activationBG.add(terminateRB);
		activationBG.add(instantRB);
		
		pack();
		setVisible(true);
		ratioSenderModeButtons();
	}
	
	private void initACS() throws Exception {
		// Connect to ACS
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("SourcePanel",true);
		String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
                System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                System.exit(-1);
        }
       	acsClient = new AdvancedComponentClient(logger,managerLoc,"SourcePanel");
	}
	
	private void close() {
		if (cdbSender!=null) {
			cdbSender.close();
		}
		if (fileSender!=null) {
			fileSender.close();
		}
		if (alarmSender!=null) {
			alarmSender.close();
		}
		try {
			if (acsClient!=null) {
				acsClient.tearDown();
			}
		} catch (Throwable t) {
			System.err.println("Error closing ComponentClient: "+t.getMessage());
		}
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			setVisible(false);
			this.dispose();
		} else if (e.getSource()==sendTripletBtn) {
			sendAlarm(tripletTF.getText().trim(), getDescriptor(), propsTF.getText());
		} else if (e.getSource()==clearAllBtn) {
			clearAllAlarms();
		} else if (e.getSource()==clearSelectedAlarmsBtn) {
			clearSelectedAlarms();
		} else if (e.getSource()==sendFromFileRB) {
			ratioSenderModeButtons();
		} else if (e.getSource()==sendFromCdbRB) {
			ratioSenderModeButtons();
		} else if (e.getSource()==sendFromTripletRB) {
			ratioSenderModeButtons();
		} else if (e.getSource()==chooseFiletBtn) {
			fileSender.selectFile();
			cycleFromFileBtn.setEnabled(fileSender.size()>0);
			terminateFromFileBtn.setEnabled(fileSender.size()>0);
			activateFromFileBtn.setEnabled(fileSender.size()>0);
		} else if (e.getSource()==activateFromFileBtn) {
			fileSender.sendAlarms(true);
		} else if (e.getSource()==terminateFromFileBtn) {
			fileSender.sendAlarms(false);
		} else if (e.getSource()==cycleFromFileBtn) {
			boolean selected=cycleFromFileBtn.isSelected();
			if (selected) {
				fileSender.startSendingRandomly();
			} else {
				fileSender.stopThread();
			}
			terminateFromFileBtn.setEnabled(!selected);
			activateFromFileBtn.setEnabled(!selected);
			chooseFiletBtn.setEnabled(!selected);
			sendFromFileRB.setEnabled(!selected);
			sendFromTripletRB.setEnabled(!selected);
			sendFromCdbRB.setEnabled(!selected);
		} else if (e.getSource()==activateFromCdbBtn) {
			cdbSender.sendAlarms(true);
		} else if (e.getSource()==terminateFromCdbBtn) {
			cdbSender.sendAlarms(false);
		} else if (e.getSource()==cycleFromCdbBtn) {
			boolean selected=cycleFromCdbBtn.isSelected();
			if (selected) {
				cdbSender.startSendingRandomly();
			} else {
				cdbSender.stopThread();
			}
			terminateFromCdbBtn.setEnabled(!selected);
			activateFromCdbBtn.setEnabled(!selected);
			sendFromCdbRB.setEnabled(!selected);
			sendFromFileRB.setEnabled(!selected);
			sendFromTripletRB.setEnabled(!selected);
		} else {
			System.out.println("Unknown event source: "+e.getSource());
		}
	}
	
	public static void main(String[] args) {
		try {
			new SenderPanel();
		} catch (Throwable t) {
			System.err.println("Exception caught: "+t.getMessage());
			t.printStackTrace();
		}
	}

	@Override
	public void insertUpdate(DocumentEvent e) {
		sendTripletBtn.setEnabled(SenderPanelUtils.isATriplet(tripletTF.getText()) && alarmSender!=null);
	}

	@Override
	public void removeUpdate(DocumentEvent e) {
		sendTripletBtn.setEnabled(SenderPanelUtils.isATriplet(tripletTF.getText()) && alarmSender!=null);
	}

	@Override
	public void changedUpdate(DocumentEvent e) {
		sendTripletBtn.setEnabled(SenderPanelUtils.isATriplet(tripletTF.getText()) && alarmSender!=null);
	}
	
	/**
	 * Send an alarm by delegating to the {@link AlarmSender}.
	 * <P>
	 * This method must not be executed inside the swing thread.
	 * 
	 * @param triplet The triplet in the format FF,FM,FC
	 * @param descriptor The {@link FaultState} descriptor
	 * @param props The user properties
	 */
	private void sendAlarm(final String triplet, final String descriptor, final String props) {
		// Check if the triplet is well formed
		if (!SenderPanelUtils.isATriplet(triplet)) {
			JOptionPane.showMessageDialog(null, "Error building alarm triplet from "+triplet, "Syntax error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		// Check if the property string is well formed
		if (props!=null && !props.trim().isEmpty() && !SenderPanelUtils.isAStringOfProperties(props)) {
			JOptionPane.showMessageDialog(null, "Error building alarm props from "+props, "Syntax error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		// Get the properties and the Triplet
		Triplet alarmTriplet = null;
		Properties userProps=null;
		
		try {
			alarmTriplet = SenderPanelUtils.tripletFromString(triplet);
			userProps=SenderPanelUtils.propertiesFromString(props);	
		} catch (Throwable t) {
			// Should never happen!
			JOptionPane.showMessageDialog(null, "Error building alarm props from "+triplet+", props="+props, "Unknown error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		AlarmDescriptorType alarmActivationType=null;
		try {
			alarmActivationType=AlarmDescriptorType.fromDescriptor(descriptor);
		} catch (Throwable t) {
			// Should never happen!
			JOptionPane.showMessageDialog(null, "Error building alarm descriptor from "+descriptor, "Unknown error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		try {
			alarmSender.send(alarmTriplet,alarmActivationType,userProps);
		} catch (InterruptedException ie) {}
	}
	
	private void clearAllAlarms() {
		Collection<Triplet> triplets= alarmsSent.getAlarms();
		System.out.println("Alarms to terminate "+triplets.size());
		for (Triplet triplet: triplets) {
			String tripletStr=triplet.faultFamily+","+triplet.faultMember+","+triplet.faultCode;
			sendAlarm(tripletStr, FaultState.TERMINATE, null);
		}
	}
	
	private void clearSelectedAlarms() {
		Collection<Triplet> triplets= alarmsSent.getSelectedAlarms();
		System.out.println("Alarms to terminate "+triplets.size());
		for (Triplet triplet: triplets) {
			System.out.println("Terminating "+triplet);
			String tripletStr=triplet.faultFamily+","+triplet.faultMember+","+triplet.faultCode;
			sendAlarm(tripletStr, FaultState.TERMINATE, null);
		}
	}
	
	/**
	 * The state descriptor from the radio buttons
	 * 
	 * @return The state descriptor
	 */
	private String getDescriptor() {
		for (JRadioButton btn: descriptorBtns) {
			if (btn.isSelected()) {
				return btn.getName();
			}
		}
		throw new IllegalStateException("Descriptor not found");
	}
	
	
	
	/** 
	 * Enable/disable the widgets depending on the sending mode
	 */
	private void ratioSenderModeButtons() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				tripletTF.setEnabled(sendFromTripletRB.isSelected());
				sendTripletBtn.setEnabled(sendFromTripletRB.isSelected() && SenderPanelUtils.isATriplet(tripletTF.getText()));
				activeRB.setEnabled(sendFromTripletRB.isSelected());
				changeRB.setEnabled(sendFromTripletRB.isSelected());
				terminateRB.setEnabled(sendFromTripletRB.isSelected());
				instantRB.setEnabled(sendFromTripletRB.isSelected());
				propsTF.setEnabled(sendFromTripletRB.isSelected());
				
				chooseFiletBtn.setEnabled(sendFromFileRB.isSelected());
				fileNameTF.setEnabled(sendFromFileRB.isSelected());
				activateFromFileBtn.setEnabled(sendFromFileRB.isSelected() && numOfAlarmsFromFile>0);
				terminateFromFileBtn.setEnabled(sendFromFileRB.isSelected() && numOfAlarmsFromFile>0);
				cycleFromFileBtn.setEnabled(sendFromFileRB.isSelected() && numOfAlarmsFromFile>0);
				
				cdbAlarmsTF.setEnabled(sendFromCdbRB.isSelected());
				activateFromCdbBtn.setEnabled(sendFromCdbRB.isSelected() && numOfAlarmsFromCDB>0);
				terminateFromCdbBtn.setEnabled(sendFromCdbRB.isSelected() && numOfAlarmsFromCDB>0);
				cycleFromCdbBtn.setEnabled(sendFromCdbRB.isSelected() && numOfAlarmsFromCDB>0);
			}
		});
	}

	@Override
	public void alarmSent(Triplet triplet, AlarmDescriptorType descriptor, boolean success) {
		if (success) {
			final int nAlarms=alarmsSent.alarmSent(triplet, descriptor==AlarmDescriptorType.ACTIVE);
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					clearAllBtn.setEnabled(nAlarms>0);
					clearSelectedAlarmsBtn.setEnabled(nAlarms>0);
				}
			});	
		} else {
			String msg ="Error returned sending "+triplet.toString();
			JOptionPane.showMessageDialog(null, msg, "Error sending alarm alarm", JOptionPane.ERROR_MESSAGE);
		}
	}
	
	/**
	 * Animate the passed progress bar 
	 * 
	 * @param bar The bar to animate
	 * @param nSteps The number of steps to perform. 
	 * 				 <code>null</code> means that the number is undefined
	 */
	private void animateProgressBar(final JProgressBar bar, final Integer nSteps) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				if (nSteps==null) {
					bar.setIndeterminate(true);
					bar.setMaximum(0);
					bar.setMinimum(0);
				} else {
					bar.setIndeterminate(false);
					bar.setMaximum(nSteps.intValue());
					bar.setMinimum(0);
				}
				bar.setValue(0);		
			}
		});
	}

	/**
	 * @see SlowTaskListener
	 */
	@Override
	public void slowTaskStarted(Object source, Integer nSteps) {
		if (source==fileSender) {
			animateProgressBar(fileTasksPB, nSteps);
		} else {
			animateProgressBar(cdbTasksPB, nSteps);
		}
	}
	
	/**
	 * Stop the passed progress bar
	 * 
	 * @param bar The bar to stop
	 */
	private void stopProgressBar(final JProgressBar bar) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				bar.setIndeterminate(false);
				bar.setValue(0);
		
			}
		});
	}

	/**
	 * @see SlowTaskListener
	 */
	@Override
	public void slowTaskFinished(Object source) {
		if (source==fileSender) {
			stopProgressBar(fileTasksPB);
		} else {
			stopProgressBar(cdbTasksPB);
		}
	}
	
	/**
	 * Update the passed progress bar
	 * 
	 * @param bar The bar to update
	 * @param step The current step
	 */
	private void updateProgressBar(final JProgressBar bar, final int step) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				bar.setValue(step);
			}
		});
	}

	/**
	 * @see SlowTaskListener
	 */
	@Override
	public void slowTaskProgress(Object source, int progess) {
		if (source==fileSender) {
			updateProgressBar(fileTasksPB,progess);
		} else {
			updateProgressBar(cdbTasksPB,progess);
		}
	}
	
	/**
	 * @see SlowTaskListener
	 */
	@Override
	public void alarmsRead(Object source, int numOfAlarmsRead) {
		if (source==cdbSender) {
			numOfAlarmsFromCDB=numOfAlarmsRead;
			cdbAlarmsTF.setText(""+numOfAlarmsFromCDB+" alarms read from TM/CDB");
		} else {
			numOfAlarmsFromFile=numOfAlarmsRead;
			fileNameTF.setText(fileSender.getFileName()+" - "+numOfAlarmsFromFile+" alarms loaded");
		}
	}
}
