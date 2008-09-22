package alma.acs.logging.preferences;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.InputVerifier;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JTextField;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JTextField;


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
	 * Each row of this dialog is composed of a
	 * <UL>
	 * 	<LI>a CheckBox to activate/deactivate the option
	 * 	<LI>a label explaining the option
	 * 	<LI>a component with the value of the option
	 * </UL>
	 * <P>
	 * The rows will be displayed following the order of their definitions
	 * in <code>PrefsWidget</code>.
	 * 
	 * @author acaproni
	 *
	 */
	public enum PrefsWidget {
		MAX_NUM_OF_LOGS(
				"Max num. of logs:",
				null,
				new JComboBox(NumberOption.values()),
				0),
		TIME_FRAME(
				"Time frame:",
				null,
				new JComboBox(TimeOption.values()),
				0),
		MAX_INPUT_RATE(
				"Max rate of logs from NC: ",
				"<HTML><FONT color=red>!</FONT> Use with care: can cause loss of logs",
				new JTextField(),
				1),
		MAX_OUTPUT_RATE(
				"Max rate of logs in table: ",
				"<HTML><FONT color=red>!</FONT> Use with care: can cause out of memory",
				new JTextField(),
				1);
		
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
		 *  The checkbox to activate/de-activate the feature
		 */
		public JCheckBox enableCB = new JCheckBox();
		
		/**
		 * The label shown 
		 */
		public JLabel label = new JLabel();
		
		/**
		 * The (editable) component with the actual value
		 * of the option
		 */
		public JComponent component;
		
		/**
		 * Constructor 
		 * 
		 * @param lbl The text to show in the label
		 * @param tootltip The tooltip set in the label and in the component
		 * @param comp The component to set options
		 * @param lowLimit The low limit (inclusive) for the input in text fields
		 */
		private PrefsWidget(String lbl, String tootltip, JComponent comp, int lowLimit) {
			label.setText(lbl);
			label.setToolTipText(tootltip);
			component=comp;
			comp.setToolTipText(tootltip);
			
			if (component instanceof JTextField) {
				((JTextField)component).setColumns(8);
				((JTextField)component).setText("0");
				component.setInputVerifier(new PassVerifier(lowLimit));
			}
			
			enableCB.addActionListener(new ActionListener(){

				@Override
				public void actionPerformed(ActionEvent e) {
					// TODO Auto-generated method stub
					component.setEnabled(!enableCB.isSelected());
					enableCB.setToolTipText("Enable/disable this feature");
				}
				
			});
		}
		
		/**
		 * Enable/disable the widget
		 * 
		 * @param enable If <code>true</code> enable the widget
		 */
		public void enableWidget(boolean enable) {
			enableCB.setEnabled(enable);
			label.setEnabled(enable);
			component.setEnabled(enable);
		}
		
		/**
		 * Enable/disable the option.
		 * <P>
		 * If the option is disabled the combobox is selected and 
		 * the component disabled.
		 * <B>
		 * If the option is enabled then the checkbox is unselected
		 * and the component enabled.
		 * 
		 * @param enable
		 */
		public void enableOption(boolean enable) {
			enableCB.setSelected(enable);
			component.setEnabled(!enable);
		}
		
		/**
		 * 
		 * @return <code>true</code> if this option is enabled
		 */
		public boolean isOptionEnabled() {
			return !enableCB.isSelected();
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
		initGUI();
		ratioWidgets();
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
		
		// The panel with the options
		JPanel optionsPanel = new JPanel();
		GridBagLayout prefsLayout = new GridBagLayout();
		GridBagConstraints c = new GridBagConstraints();
		optionsPanel.setLayout(prefsLayout);
		int row=0;
		for (PrefsWidget widget: PrefsWidget.values()) {
			c.gridx=0; c.gridy=row; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
			optionsPanel.add(widget.enableCB,c);
			c.gridx=1; c.gridy=row; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
			optionsPanel.add(widget.label,c);
			c.gridx=2; c.gridy=row; c.anchor=GridBagConstraints.LAST_LINE_START; c.insets = new Insets(5,5,5,5);
			optionsPanel.add(widget.component,c);
			row++;
		}
		
		PrefsWidget.TIME_FRAME.enableWidget(false);
		
		// Add the label and prefs panel
		mainPnl.add(optionsPanel,BorderLayout.CENTER);
		
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
			((JComboBox)(PrefsWidget.MAX_NUM_OF_LOGS.component)).setSelectedIndex(2);
		} else {
			((JComboBox)(PrefsWidget.MAX_NUM_OF_LOGS.component)).setSelectedIndex(nOpt.ordinal());
		}
		PrefsWidget.MAX_NUM_OF_LOGS.enableOption(((JComboBox)(PrefsWidget.MAX_NUM_OF_LOGS.component)).getSelectedIndex()==0);
		
		// Set the time frame
		TimeOption tOpt = TimeOption.fromInt(preferences.getMinuteTimeFrame());
		if (tOpt==null) {
			((JComboBox)(PrefsWidget.TIME_FRAME.component)).setSelectedIndex(0);
		} else {
			((JComboBox)(PrefsWidget.TIME_FRAME.component)).setSelectedIndex(tOpt.ordinal());
		}
		PrefsWidget.TIME_FRAME.enableOption(((JComboBox)(PrefsWidget.TIME_FRAME.component)).getSelectedIndex()==0);
		
		if (preferences.getMaxInputRate()==Integer.MAX_VALUE) {
			((JTextField)PrefsWidget.MAX_INPUT_RATE.component).setText("0");
		} else {
			((JTextField)PrefsWidget.MAX_INPUT_RATE.component).setText(""+preferences.getMaxInputRate());
		}
		PrefsWidget.MAX_INPUT_RATE.enableOption(preferences.getMaxInputRate()==Integer.MAX_VALUE);
		
		if (preferences.getMaxOutputRate()==Integer.MAX_VALUE) {
			((JTextField)PrefsWidget.MAX_OUTPUT_RATE.component).setText("0");
		} else {
			((JTextField)PrefsWidget.MAX_OUTPUT_RATE.component).setText(""+preferences.getMaxOutputRate());
		}
		PrefsWidget.MAX_OUTPUT_RATE.enableOption(preferences.getMaxOutputRate()==Integer.MAX_VALUE);
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
		if (PrefsWidget.MAX_NUM_OF_LOGS.isOptionEnabled()) {
			NumberOption opt = (NumberOption)((JComboBox)(PrefsWidget.MAX_NUM_OF_LOGS.component)).getSelectedItem();
			preferences.setMaxLogs(opt.value);
		} else {
			preferences.setMaxLogs(0);
		}
		if (PrefsWidget.TIME_FRAME.isOptionEnabled()) {
			TimeOption opt = (TimeOption)((JComboBox)(PrefsWidget.TIME_FRAME.component)).getSelectedItem();
			preferences.setTimeFrame(opt.value);
		} else {
			preferences.setTimeFrame(0);
		}
		if (PrefsWidget.MAX_INPUT_RATE.isOptionEnabled()) {
			String val = (String)((JTextField)PrefsWidget.MAX_INPUT_RATE.component).getText();
			preferences.setMaxInputRate(Integer.parseInt(val));
			System.out.println("===>"+val);
		} else {
			preferences.setMaxInputRate(Integer.MAX_VALUE);
		}
		if (PrefsWidget.MAX_OUTPUT_RATE.isOptionEnabled()) {
			String val = (String)((JTextField)PrefsWidget.MAX_OUTPUT_RATE.component).getText();
			preferences.setMaxOutputRate(Integer.parseInt(val));
		} else {
			preferences.setMaxOutputRate(Integer.MAX_VALUE);
		}
		System.out.println("Returning: "+preferences.getMaxNumOfLogs()+", "+preferences.getMinuteTimeFrame()+", "+preferences.getMaxInputRate()+", "+preferences.getMaxOutputRate());
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
