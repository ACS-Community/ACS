package com.cosylab.logging.settings;

import java.awt.BorderLayout;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRootPane;

import alma.acs.logging.preferences.UserPreferences;

/**
 * A dialog to setup the preferences
 * 
 * @author acaproni
 *
 */
public class ExpertPrefsDlg extends JDialog implements ActionListener {
	
	// The ok and cancel buttons
	private JButton okBtn;
	private JButton cancelBtn;
	
	// Say if the user pressed the OK or the Cancel button
	private boolean okBtnPressed=false;
	
	// Options for the time frame 
	private JComboBox timeCB;
	
	
	// Options for the max number of logs
	private JComboBox maxLogNumCB;
	
	/**
	 * Constructor
	 * 
	 * @param owner The owner of the dialog
	 * @param initialNumOfLogs The initial value for the num of logs
	 * @param initialTimeFrame The initial value for the time frame (minutes)
	 */
	public ExpertPrefsDlg(int initialNumOfLogs, int initialTimeFrame) {
		super();
		setTitle("Preferences");
		setName("ExpertPrefsDlg");
		setModal(true);
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		initGUI();
		initCombos(initialNumOfLogs,initialTimeFrame);
		setBounds(50,50,50,50);
		pack();
		setVisible(true);
	}
	
	/**
	 * @see java.awt.event.ActionListener
	 * @see java.awt.event.ActionEvent
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==okBtn) {
			okBtnPressed=true;
			setVisible(false);
			dispose();
		} else if (e.getSource()==cancelBtn) {
			setVisible(false);
			dispose();
		} else {
			System.err.println("Event not handled");
		}
	}
	
	/**
	 * Builds the GUI
	 *
	 */
	private void initGUI() {
		JRootPane mainPnl = this.getRootPane();
		mainPnl.setLayout(new BorderLayout());
		
		// The panel with the options
		JPanel optionsPanel = new JPanel();
		GridBagLayout prefsLayout = new GridBagLayout();
		GridBagConstraints c = new GridBagConstraints();
		optionsPanel.setLayout(prefsLayout);
		JLabel timeLbl = new JLabel("Time frame:");
		timeLbl.setEnabled(false);
		JLabel numLbl =  new JLabel("Max num. of logs:");
		c.gridx=0; c.gridy=0; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPanel.add(timeLbl,c);
		c.gridx=0; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPanel.add(numLbl,c);
		// The panel with the controls
		timeCB = new JComboBox(UserPreferences.timeOptions);
		timeCB.setEditable(false);
		timeCB.setEnabled(false);
		maxLogNumCB  = new JComboBox(UserPreferences.maxLogNumOptions);
		maxLogNumCB.setEditable(false);
		c.gridx=1; c.gridy=0; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPanel.add(timeCB,c);
		c.gridx=1; c.gridy=1; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
		optionsPanel.add(maxLogNumCB,c);
		// Add the label and prefs panel
		mainPnl.add(optionsPanel,BorderLayout.CENTER);
		
		// Add the OK, CANCEL buttons
		JPanel btnPnl = new JPanel();
		btnPnl.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
		BoxLayout boxLayout = new BoxLayout(btnPnl,BoxLayout.LINE_AXIS);
		btnPnl.setLayout(boxLayout);
		btnPnl.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
		okBtn = new JButton("Ok");
		okBtn.addActionListener(this);
		cancelBtn = new JButton("Cancel");
		cancelBtn.addActionListener(this);
		btnPnl.add(okBtn,BorderLayout.WEST);
		btnPnl.add(Box.createRigidArea(new Dimension(10, 0)));
		btnPnl.add(cancelBtn,BorderLayout.EAST);
		mainPnl.add(btnPnl,BorderLayout.SOUTH);
	}
	
	/**
	 * Set the combos to their initial value
	 * If the specified values do not exist, it selects a
	 * default value
	 * 
	 * @param numbOfLogs The number of logs
	 * @param timeFrame The number of minutes of the time frame
	 */
	private void initCombos(int numbOfLogs, int timeFrame) {
		// Set the defaults 
		maxLogNumCB.setSelectedIndex(2);
		timeCB.setSelectedIndex(2);
		for (int t=0; t<UserPreferences.maxLogNumOptions.length; t++) {
			if (UserPreferences.maxLogNumOptions[t].equals(numbOfLogs)) {
				maxLogNumCB.setSelectedItem(UserPreferences.maxLogNumOptions[t]);
			}
		}
		for (int t=0; t<UserPreferences.timeOptions.length; t++) {
			if (UserPreferences.timeOptions[t].equals(timeFrame)) {
				timeCB.setSelectedItem(UserPreferences.timeOptions[t]);
			}
		}
	}
	
	/**
	 * Return true if the user pressed the Ok button
	 * to approve the changes
	 *
	 */
	public boolean okPressed() {
		return okBtnPressed;
	}
	
	/**
	 * 
	 * @return The time frame selcted in the CB
	 */
	public int getTimeFrame() {
		return ((UserPreferences.TimeOption)timeCB.getSelectedItem()).getTimeFrame();
	}
	
	/**
	 * 
	 * @return The max number of logs selected in the CB
	 */
	public int getMaxNumOfLogs() {
		return ((UserPreferences.NumberOption)maxLogNumCB.getSelectedItem()).getNumOfLogs();
	}
}
