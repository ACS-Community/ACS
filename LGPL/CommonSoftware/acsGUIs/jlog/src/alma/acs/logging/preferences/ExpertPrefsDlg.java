/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.logging.preferences;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.InputVerifier;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JPanel;
import javax.swing.JRootPane;

/**
 * A dialog to setup (expert) preferences.
 * 
 * @author acaproni
 *
 */
public class ExpertPrefsDlg extends JDialog implements ActionListener {
	
	/**
	 * A class containing the options for the number of logs.
	 *  <P>
	 * Each option is a couple <code><label, value></code> where
	 *	<UL>
	 * 		<LI> value is the value of the option
	 * 		<LI> label is a label to show to the user for that option
	 *	</UL>
	 */ 
	public enum NumberOption {
		
		UNLIMITED("Unlimited",0),
		K100("100K",100000),
		K200("200K",200000),
		K300("300K",300000),
		K400("400K",400000),
		K500("500K",500000);
		
		public final String label;
		public final int value;
		
		/**
		 * Constructor
		 * 
		 * @param lbl The label to show in the combo box
		 * @param val The number of minutes
		 */
		private NumberOption(String lbl, int val) {
			label=lbl;
			value=val;
		}
		
		public String toString() {
			return label;
		}
		
		/**
		 * Return the <code>TimeOption</code> having the parameter 
		 * as value;
		 * 
		 * @param val The value to get the <code>TimeOption</code> from
		 * @return the <code>TimeOption</code> having the parameter as value or
		 * 			<code>null</code> if there is no <code>TimeOption</code> 
		 * 			having the parameter as value
		 */
		public static NumberOption fromInt(int val) {
			for (NumberOption nOpt: NumberOption.values()) {
				if (nOpt.value==val) {
					return nOpt;
				}
			}
			return null;
		}
	}
	
	/**
	 * A class containing the options for the time frame of logs
	 * <P>
	 * Each option is a couple <code><label, value></code> where
	 *	<UL>
	 * 		<LI> value is the value of the option
	 * 		<LI> label is a label to show to the user for that option
	 *	</UL>
	 */ 
	public enum TimeOption {
		
		UNLIMITED("Unlimited",0),
		H1("1h",60),
		H3("3h",180),
		H5("5h",300),
		H12("12h",720),
		D1("1d",1440);
		
		public final String label;
		public final int value;
		
		/**
		 * Constructor 
		 * 
		 * @param lbl The label to show
		 * @param val The number of logs
		 */
		private TimeOption(String lbl, int val) {
			label=lbl;
			value=val;
		}
		
		public String toString() {
			return label;
		}
		
		/**
		 * Return the <code>TimeOption</code> having the parameter 
		 * as value;
		 * 
		 * @param val The value to get the <code>TimeOption</code> from
		 * @return the <code>TimeOption</code> having the parameter as value or
		 * 			<code>null</code> if there is no <code>TimeOption</code> 
		 * 			having the parameter as value
		 */
		public static TimeOption fromInt(int val) {
			for (TimeOption tOpt: TimeOption.values()) {
				if (tOpt.value==val) {
					return tOpt;
				}
			}
			return null;
		}
		
	}
	
	/**
	 * The verifier of the value written in a text field
	 * 
	 * @author acaproni
	 *
	 */
	class PassVerifier extends InputVerifier {
		/**
		 * Low limit (inclusive)
		 */
		private final int limit;
		
		public PassVerifier(int lowLimit) {
			limit=lowLimit;
		}
		
        public boolean verify(JComponent input) {
              JTextField tf = (JTextField) input;
              Integer val;
              try { 
            	  val=Integer.parseInt(tf.getText());
            	  boolean ret= val>=limit;
            	  if (ret) {
  	        		input.setForeground(Color.black);
  	        	} else {
  	        		input.setForeground(Color.red);
  	        	}
            	return ret;
              } catch (Exception e) {
            	  input.setForeground(Color.red);
            	  return false;
              }
        }
    }
	
	
	/**
	 * <code>OptionWidgets</code> is essentially used to access the check boxes with a 
	 * meaningful name.
	 * 
	 * @author acaproni
	 *
	 */
	public enum OptionWidgets {
		MAX_NUM_OF_LOGS("Max num. of logs:",null),
		TIME_FRAME("Time frame:",null),
		MAX_INPUT_RATE(
				"Max rate of logs from NC: ",
				"<HTML><FONT color=red>!</FONT> Use with care: can cause loss of logs"),
		MAX_OUTPUT_RATE(
				"Max rate of logs in table: ",
				"<HTML><FONT color=red>!</FONT> Use with care: can cause out of memory"),
		DYNAMIC_DISCARD_LEVEL(
				"Dynamic discard level: ",
				"<HTML><FONT color=red>!</FONT> Use with care: can cause out of memory");
		
		/**
		 * The check box for this option
		 * <P>
		 * The check box is used to enable, disable an option
		 */
		public final JCheckBox enableCB=new JCheckBox();
		
		/**
		 * Constructor
		 * 
		 * @param label The text of the check box
		 * @param tooltip The tooltip
		 */
		private OptionWidgets(String label, String tooltip) {
			enableCB.setText(label);
			enableCB.setToolTipText(tooltip);
		}
		
		/**
		 * 
		 * @return <code>true</code> if this option is enabled
		 */
		public boolean isOptionEnabled() {
			return enableCB.isSelected();
		}
		
		/**
		 * Return the <code>OptionWidgets</code> whose <code>JCheckBox</code>
		 * is equal to the passed parameter.
		 * Otherwise throws an <code>IllegalArgumentException</code>
		 * 
		 * @param cb The <code>JCheckBox</code> to get the option from
		 * @return the <code>OptionWidgets</code> whose <code>JCheckBox</code>
		 * 			is equal to the passed parameter. 
		 * 			Otherwise throws an <code>IllegalArgumentException</code>
		 */
		public static OptionWidgets fromCheckBox(JCheckBox cb) {
			if (cb==null) {
				throw new IllegalArgumentException("cb can't be null");
			}
			for (OptionWidgets opt: OptionWidgets.values()) {
				if (opt.enableCB==cb) {
					return opt;
				}
			}
			throw new IllegalArgumentException("The passed is not part of TimeOptions.enableCB");
		}
		
	}
	
	/**
	 * The preferences shown and changed by this panel
	 * <P>
	 * This is a copy of the object received in the constructor.
	 * <BR>
	 * In this implementation this property is filled with the values 
	 * from the GUI by <code>getPreferences()</code>
	 * 
	 * @see getPreferences()
	 */
	private UserPreferences preferences;
	
	/**
	 * The preferences received in the constructor and used to reset.
	 */
	private UserPreferences originalPreferences;
	
	// The ok and cancel buttons
	private JButton okBtn;
	private JButton cancelBtn;
	private JButton restoreBtn;
	
	/** 
	 * Say if the user pressed the OK or the Cancel button
	 */
	private boolean okBtnPressed=false;
	
	/** 
	 * The component to show this dialog over
	 */
	private Component owner;
	
	/**
	 * The max number of logs in table
	 */
	private JComboBox maxLogsInTableCB = new JComboBox(NumberOption.values());
	
	/**
	 * The max time frame to keep in the table
	 */
	private JComboBox timeFrameCB = new JComboBox(TimeOption.values());
	
	/**
	 * The rate of logs from the NC
	 */
	private JTextField inputRateTF = new JTextField("0",8);
	
	/**
	 * The rate of logs into the table
	 */
	private JTextField outputRateTF = new JTextField("0",8);
	
	/**
	 * The threshold for the dynamic discarding of logs
	 */
	private JTextField dynThresholdTF = new JTextField("0",6);
	
	/**
	 * The damping factor for the dynamic discarding of logs
	 */
	private JTextField dynDampingTF = new JTextField("0",6);
	
	/**
	 * The time interval for the dynamic discarding of logs
	 */
	private JTextField dynIntervalTF = new JTextField("0",6);
	
	/**
	 * Constructor
	 * 
	 * @param owner The owner of the dialog
	 * @param initialNumOfLogs The initial value for the num of logs
	 * @param initialTimeFrame The initial value for the time frame (minutes)
	 */
	public ExpertPrefsDlg(Component owner, UserPreferences prefs) {
		super();
		if (owner==null) {
			throw new IllegalArgumentException("The owner can't be null");
		}
		if (prefs==null) {
			throw new IllegalArgumentException("The preferences can't be null");
		}
		setTitle("Preferences");
		setName("ExpertPrefsDlg");
		this.owner=owner;
		this.originalPreferences=prefs;
		try {
			this.preferences=prefs.clone();
			System.out.println("Prefs: "+preferences.getMaxNumOfLogs()+", "+preferences.getMinuteTimeFrame()+", "+preferences.getMaxInputRate()+", "+preferences.getMaxOutputRate());
		} catch (CloneNotSupportedException e) {
			// This should not happen because UserPreferences implements Cloneable
			this.preferences=new UserPreferences(0,0,Integer.MAX_VALUE, Integer.MAX_VALUE);
		}
		setModal(true);
		this.setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		buildWidgets();
		initGUI();
		ratioWidgets();
		setVisible(true);
	}
	
	/**
	 * @see java.awt.event.ActionListener
	 * @see java.awt.event.ActionEvent
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof JCheckBox) {
			enableOption((JCheckBox)e.getSource());
		} else if (e.getSource()==okBtn) {
			okBtnPressed=true;
			setVisible(false);
			dispose();
		} else if (e.getSource()==cancelBtn) {
			setVisible(false);
			dispose();
		} else if (e.getSource()==restoreBtn) {
			try {
				preferences=originalPreferences.clone();
			} catch (CloneNotSupportedException ce) {
				preferences=new UserPreferences(0,0,Integer.MAX_VALUE, Integer.MAX_VALUE);
			}
			ratioWidgets();
		}else {
			System.err.println("Event not handled "+e.getSource());
		}
	}
	
	/**
	 * Builds the GUI
	 *
	 */
	private void initGUI() {
		JRootPane mainPnl = this.getRootPane();
		mainPnl.setLayout(new BorderLayout());
		
		/////////////////////////////////////////////////////////////
		// Add the table constraints (max num of logs and time frame)
		///////////////////////////////////////////////////////////
		JPanel tablePnl = new JPanel();
		tablePnl.setBorder(BorderFactory.createTitledBorder("Table constraints"));
		tablePnl.setLayout(new GridBagLayout());
		GridBagConstraints constr = new GridBagConstraints();
		// Num of logs
		constr.gridx=0; constr.gridy=0; constr.fill=GridBagConstraints.HORIZONTAL;
		constr.anchor=GridBagConstraints.WEST; constr.insets = new Insets(5,5,5,5);
		tablePnl.add(OptionWidgets.MAX_NUM_OF_LOGS.enableCB,constr);
		constr.gridx=1; constr.gridy=0; 
		constr.anchor=GridBagConstraints.LINE_START; constr.insets = new Insets(5,5,5,5);
		constr.fill=GridBagConstraints.HORIZONTAL;
		tablePnl.add(maxLogsInTableCB,constr);
		// Time frame
		constr.gridx=0; constr.gridy=1; 
		constr.anchor=GridBagConstraints.WEST; constr.insets = new Insets(5,5,5,5);
		tablePnl.add(OptionWidgets.TIME_FRAME.enableCB,constr);
		constr.gridx=1; constr.gridy=1; 
		constr.anchor=GridBagConstraints.LINE_START; constr.insets = new Insets(5,5,5,5);
		tablePnl.add(timeFrameCB,constr);
		timeFrameCB.setEnabled(false); /// DISABLED
		OptionWidgets.TIME_FRAME.enableCB.setEnabled(false); // DISABLED
		
		///////////////////////////////////////////////////////////
		// Add engine constraints
		///////////////////////////////////////////////////////////
		JPanel enginePnl = new JPanel();
		enginePnl.setBorder(BorderFactory.createTitledBorder("Engine constraints"));
		enginePnl.setLayout(new GridBagLayout());
		
		// INPUT RATE
		constr.gridx=0; constr.gridy=0; 
		constr.anchor=GridBagConstraints.LAST_LINE_START; constr.insets = new Insets(5,5,5,5);
		enginePnl.add(OptionWidgets.MAX_INPUT_RATE.enableCB,constr);
		constr.gridx=1; constr.gridy=0; 
		constr.anchor=GridBagConstraints.LAST_LINE_START; constr.insets = new Insets(5,5,5,5);
		enginePnl.add(inputRateTF,constr);
		// Output RATE
		constr.gridx=0; constr.gridy=1; 
		constr.anchor=GridBagConstraints.LAST_LINE_START; constr.insets = new Insets(5,5,5,5);
		enginePnl.add(OptionWidgets.MAX_OUTPUT_RATE.enableCB,constr);
		constr.gridx=1; constr.gridy=1; 
		constr.anchor=GridBagConstraints.LAST_LINE_START; constr.insets = new Insets(5,5,5,5);
		enginePnl.add(outputRateTF,constr);
		// DYNAMIC DISCARD LEVEL
		JPanel pnl = new JPanel();
		pnl.setLayout(new GridLayout(3,2));
		pnl.add(new JLabel("Threshold: "),"1");
		pnl.add(dynThresholdTF,"2");
		pnl.add(new JLabel("Damping: "),"3");
		pnl.add(dynDampingTF,"4");
		pnl.add(new JLabel("Time: "),"5");
		pnl.add(dynIntervalTF,"6");
		
		constr.gridx=0; constr.gridy=2; constr.fill=GridBagConstraints.VERTICAL;
		constr.anchor=GridBagConstraints.LAST_LINE_START; constr.insets = new Insets(5,5,5,5);
		enginePnl.add(OptionWidgets.DYNAMIC_DISCARD_LEVEL.enableCB,constr);
		constr.gridx=1; constr.gridy=2; 
		constr.anchor=GridBagConstraints.LAST_LINE_START; constr.insets = new Insets(5,5,5,5);
		enginePnl.add(pnl,constr);
		
		
		// Add the table and engine panels to the main panel
		mainPnl.add(tablePnl,BorderLayout.CENTER);
		mainPnl.add(enginePnl,BorderLayout.NORTH);
		
		// Add the OK, CANCEL buttons
		JPanel buttonsPnl = new JPanel(new BorderLayout());
		JPanel okCancelPnl = new JPanel();
		okCancelPnl.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
		BoxLayout boxLayout = new BoxLayout(okCancelPnl,BoxLayout.LINE_AXIS);
		okCancelPnl.setLayout(boxLayout);
		okCancelPnl.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
		okBtn = new JButton("Ok");
		okBtn.addActionListener(this);
		cancelBtn = new JButton("Cancel");
		cancelBtn.addActionListener(this);
		okCancelPnl.add(okBtn,BorderLayout.WEST);
		okCancelPnl.add(Box.createRigidArea(new Dimension(10, 0)));
		okCancelPnl.add(cancelBtn,BorderLayout.EAST);
		
		JPanel restoreBtnPnl=new JPanel(new FlowLayout());
		restoreBtn = new JButton("Restore");
		restoreBtn.addActionListener(this);
		restoreBtnPnl.add(restoreBtn);
		buttonsPnl.add(restoreBtnPnl,BorderLayout.WEST);
		buttonsPnl.add(okCancelPnl,BorderLayout.EAST);
		
		mainPnl.add(buttonsPnl,BorderLayout.SOUTH);
		
		pack();
	}
	
	/**
	 * Build the widgets shown in the dialog
	 */
	private void buildWidgets() {
		// NUM OF LOGS
		maxLogsInTableCB.setEditable(false);
		maxLogsInTableCB.setMaximumRowCount(NumberOption.values().length);

		// TIME FRAME
		timeFrameCB.setEditable(false);
		timeFrameCB.setMaximumRowCount(TimeOption.values().length);
		
		// INPUT RATE
		inputRateTF.setInputVerifier(new PassVerifier(1));
		
		// OUTPUT RATE
		outputRateTF.setInputVerifier(new PassVerifier(1));
		
		// THRESHOLD
		dynThresholdTF.setInputVerifier(new PassVerifier(1024));
		dynDampingTF.setInputVerifier(new PassVerifier(0));
		dynIntervalTF.setInputVerifier(new PassVerifier(1));
		
		// Add the listener to the check boxes
		for (OptionWidgets widget: OptionWidgets.values()) {
			widget.enableCB.addActionListener(this);
		}
	}
	
	/**
	 * Ratio the content of the widgets getting their values from
	 * <code>preferences</code>.
	 * 
	 * 
	 * @param numbOfLogs The number of logs
	 * @param timeFrame The number of minutes of the time frame
	 * 
	 * @see prefrences
	 */
	private void ratioWidgets() {
		// Set the max num of logs 
		NumberOption nOpt = NumberOption.fromInt(preferences.getMaxNumOfLogs());
		if (nOpt==null) {
			maxLogsInTableCB.setSelectedIndex(2);
		} else {
			maxLogsInTableCB.setSelectedIndex(nOpt.ordinal());
		}
		OptionWidgets.MAX_NUM_OF_LOGS.enableCB.setSelected(maxLogsInTableCB.getSelectedIndex()!=0);
		
		// Set the time frame
		TimeOption tOpt = TimeOption.fromInt(preferences.getMinuteTimeFrame());
		if (tOpt==null) {
			timeFrameCB.setSelectedIndex(0);
		} else {
			timeFrameCB.setSelectedIndex(tOpt.ordinal());
		}
		OptionWidgets.TIME_FRAME.enableCB.setSelected(timeFrameCB.getSelectedIndex()!=0);
		
		
		if (preferences.getMaxInputRate()==Integer.MAX_VALUE) {
			inputRateTF.setText("0");
		} else {
			inputRateTF.setText(""+preferences.getMaxInputRate());
		}
		OptionWidgets.MAX_INPUT_RATE.enableCB.setSelected(preferences.getMaxInputRate()!=Integer.MAX_VALUE);
		
		if (preferences.getMaxOutputRate()==Integer.MAX_VALUE) {
			outputRateTF.setText("0");
		} else {
			outputRateTF.setText(""+preferences.getMaxOutputRate());
		}
		OptionWidgets.MAX_OUTPUT_RATE.enableCB.setSelected(preferences.getMaxOutputRate()!=Integer.MAX_VALUE);
		
		if (preferences.getDynThreshold()==Integer.MAX_VALUE) {
			dynThresholdTF.setText("8192");
			dynDampingTF.setText("2048");
			dynIntervalTF.setText("30");
		}
		OptionWidgets.DYNAMIC_DISCARD_LEVEL.enableCB.setSelected(preferences.getDynThreshold()!=Integer.MAX_VALUE);
		
		for (OptionWidgets opt: OptionWidgets.values()) {
			enableOption(opt.enableCB);
		}
	}
	
	/**
	 * Enable or disable the option of the given check box.
	 * 
	 * @param cB The chackbox the enable/disable the option
	 */
	private void enableOption(JCheckBox cB) {
		if (cB==null) {
			throw new IllegalArgumentException("The JCheckBox can't be null");
		}
		OptionWidgets opt = OptionWidgets.fromCheckBox(cB);
		switch (opt) {
		case MAX_NUM_OF_LOGS:
			maxLogsInTableCB.setEnabled(cB.isSelected());
			break;
		case TIME_FRAME:
			timeFrameCB.setEnabled(cB.isSelected());
			break;
		case MAX_INPUT_RATE:
			inputRateTF.setEnabled(cB.isSelected());
			break;
		case MAX_OUTPUT_RATE:
			outputRateTF.setEnabled(cB.isSelected());
			break;
		case DYNAMIC_DISCARD_LEVEL:
			dynThresholdTF.setEnabled(cB.isSelected());
			dynDampingTF.setEnabled(cB.isSelected());
			dynIntervalTF.setEnabled(cB.isSelected());
			break;
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
	 * Return the user preferences defined in the dialog.
	 * 
	 * @return The user preferences defined in the dialog.
	 * @see preferences
	 */
	public UserPreferences getPreferences() {
		// Put the values in the GUI into preferences
		if (OptionWidgets.MAX_NUM_OF_LOGS.isOptionEnabled()) {
			System.out.println("A");
			NumberOption opt = (NumberOption)maxLogsInTableCB.getSelectedItem();
			preferences.setMaxLogs(opt.value);
		} else {
			System.out.println("B");
			preferences.setMaxLogs(0);
		}
		if (OptionWidgets.TIME_FRAME.isOptionEnabled()) {
			TimeOption opt = (TimeOption)timeFrameCB.getSelectedItem();
			preferences.setTimeFrame(opt.value);
		} else {
			preferences.setTimeFrame(0);
		}
		if (OptionWidgets.MAX_INPUT_RATE.isOptionEnabled()) {
			String val = inputRateTF.getText();
			preferences.setMaxInputRate(Integer.parseInt(val));
		} else {
			preferences.setMaxInputRate(Integer.MAX_VALUE);
		}
		if (OptionWidgets.MAX_OUTPUT_RATE.isOptionEnabled()) {
			String val = outputRateTF.getText();
			preferences.setMaxOutputRate(Integer.parseInt(val));
		} else {
			preferences.setMaxOutputRate(Integer.MAX_VALUE);
		}
		
		if (OptionWidgets.DYNAMIC_DISCARD_LEVEL.isOptionEnabled()) {
			preferences.setDynThreshold(Integer.parseInt(dynThresholdTF.getText()));
			preferences.setDynDamping(Integer.parseInt(dynDampingTF.getText()));
			preferences.setDynTime(Integer.parseInt(dynIntervalTF.getText()));
		} else {
			preferences.setDynThreshold(Integer.MAX_VALUE);
			preferences.setDynDamping(0);
			preferences.setDynTime(30);
		}
		return preferences;
	}
	
	/**
	 * Override <code>JDialog.setVisible</code> to show this dialog over
	 * the <code>LogsingClient</code> component.
	 */
	@Override
	public void setVisible(boolean visible) {
		setLocationRelativeTo(owner);
		pack();
		super.setVisible(visible);
		toFront();
	}
}
