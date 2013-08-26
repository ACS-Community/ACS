package com.cosylab.gui.components.r2.chart;

/**
 * This type is GUI for costumizing chart. 
 */
public class BaseChartCustomizer extends javax.swing.JFrame {
	private javax.swing.JButton ivjCancelButton = null;
	private javax.swing.JPanel ivjJFrameContentPane = null;
	private javax.swing.JLabel ivjJLabel1 = null;
	private javax.swing.JLabel ivjJLabel2 = null;
	private javax.swing.JLabel ivjJLabel3 = null;
	private javax.swing.JLabel ivjJLabel4 = null;
	private javax.swing.JLabel ivjMaxValueLabel = null;
	private javax.swing.JTextField ivjMaxXText = null;
	private javax.swing.JTextField ivjMaxYText = null;
	private javax.swing.JLabel ivjMinValueLabel = null;
	private javax.swing.JTextField ivjMinXText = null;
	private javax.swing.JTextField ivjMinYText = null;
	private javax.swing.JButton ivjOKButton = null;
	private BaseChart chart = null;
	private javax.swing.JCheckBox ivjPreferedXCheckBox = null;
	private javax.swing.JCheckBox ivjPreferedYCheckBox = null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private java.text.DecimalFormat format = new java.text.DecimalFormat();

class IvjEventHandler implements java.awt.event.ActionListener, java.awt.event.WindowListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == BaseChartCustomizer.this.getOKButton()) 
				connEtoM5(e);
			if (e.getSource() == BaseChartCustomizer.this.getCancelButton()) 
				connEtoM6(e);
			if (e.getSource() == BaseChartCustomizer.this.getMinXText()) 
				connEtoM1(e);
			if (e.getSource() == BaseChartCustomizer.this.getMaxXText()) 
				connEtoM2(e);
			if (e.getSource() == BaseChartCustomizer.this.getMinYText()) 
				connEtoM3(e);
			if (e.getSource() == BaseChartCustomizer.this.getMaxYText()) 
				connEtoM4(e);
		};
		public void windowActivated(java.awt.event.WindowEvent e) {};
		public void windowClosed(java.awt.event.WindowEvent e) {};
		public void windowClosing(java.awt.event.WindowEvent e) {};
		public void windowDeactivated(java.awt.event.WindowEvent e) {};
		public void windowDeiconified(java.awt.event.WindowEvent e) {};
		public void windowIconified(java.awt.event.WindowEvent e) {};
		public void windowOpened(java.awt.event.WindowEvent e) {
			if (e.getSource() == BaseChartCustomizer.this) 
				connEtoM7(e);
		};
	};
/**
 * BaseChartCustomizer constructor, which takes no argument and constructs 
 * BaseChartCustomizer with default values.
 */
public BaseChartCustomizer() {
	super();
	initialize();
}
/**
 * BaseChartCustomizer constructor, which takes BaseChart as argument and constructs 
 * BaseChartCustomizer with default values, except BaseChart is specified as parameter.
 */
public BaseChartCustomizer(BaseChart ch) {
	this();
	chart=ch;
}
/**
 * connEtoM1:  (MinXText.action.actionPerformed(java.awt.event.ActionEvent) --> PreferedXCheckBox.selected)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM1(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getPreferedXCheckBox().setSelected(false);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM2:  (MaxXText.action.actionPerformed(java.awt.event.ActionEvent) --> PreferedXCheckBox.selected)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM2(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getPreferedXCheckBox().setSelected(false);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM3:  (MinYText.action.actionPerformed(java.awt.event.ActionEvent) --> PreferedYCheckBox.selected)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM3(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getPreferedYCheckBox().setSelected(false);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM4:  (MaxYText.action.actionPerformed(java.awt.event.ActionEvent) --> PreferedYCheckBox.selected)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM4(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getPreferedYCheckBox().setSelected(false);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM5:  (OKButton.action.actionPerformed(java.awt.event.ActionEvent) --> BaseChartCustomizer.oKButton_ActionPerformed(Ljava.awt.event.ActionEvent;)V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM5(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.oKButton_ActionPerformed(arg1);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM6:  (CancelButton.action.actionPerformed(java.awt.event.ActionEvent) --> BaseChartCustomizer.dispose()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM6(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.dispose();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM7:  (BaseChartCustomizer.window.windowOpened(java.awt.event.WindowEvent) --> BaseChartCustomizer.updateCustomizer()V)
 * @param arg1 java.awt.event.WindowEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM7(java.awt.event.WindowEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.updateCustomizer();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
private String format(String form, double d) {
	if (Double.isNaN(d)) return "NaN";
	if (Double.isInfinite(d)) return "Infinite";
	// this formatting procedure should be changed to use of native Java Format object
//	return si.ijs.anka.utilities.Format.sprint(form,d);
	format.applyPattern(form);
	return format.format(d);
}
/**
 * Return the CancelButton property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getCancelButton() {
	if (ivjCancelButton == null) {
		try {
			ivjCancelButton = new javax.swing.JButton();
			ivjCancelButton.setName("CancelButton");
			ivjCancelButton.setText("Cancel");
			ivjCancelButton.setMaximumSize(new java.awt.Dimension(59, 25));
			ivjCancelButton.setActionCommand("Cancel");
			ivjCancelButton.setMargin(new java.awt.Insets(2, 7, 2, 7));
			ivjCancelButton.setMinimumSize(new java.awt.Dimension(59, 25));
			ivjCancelButton.setBounds(210, 168, 70, 25);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjCancelButton;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 22:46:41)
 * @return si.ijs.anka.databush.utilities.BaseChart
 */
public BaseChart getChart() {
	return chart;
}
/**
 * Return the JFrameContentPane property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJFrameContentPane() {
	if (ivjJFrameContentPane == null) {
		try {
			ivjJFrameContentPane = new javax.swing.JPanel();
			ivjJFrameContentPane.setName("JFrameContentPane");
			ivjJFrameContentPane.setLayout(null);
			ivjJFrameContentPane.setMinimumSize(new java.awt.Dimension(0, 0));
			getJFrameContentPane().add(getJLabel1(), getJLabel1().getName());
			getJFrameContentPane().add(getMinXText(), getMinXText().getName());
			getJFrameContentPane().add(getJLabel2(), getJLabel2().getName());
			getJFrameContentPane().add(getMaxXText(), getMaxXText().getName());
			getJFrameContentPane().add(getJLabel3(), getJLabel3().getName());
			getJFrameContentPane().add(getJLabel4(), getJLabel4().getName());
			getJFrameContentPane().add(getMinYText(), getMinYText().getName());
			getJFrameContentPane().add(getMaxYText(), getMaxYText().getName());
			getJFrameContentPane().add(getMinValueLabel(), getMinValueLabel().getName());
			getJFrameContentPane().add(getMaxValueLabel(), getMaxValueLabel().getName());
			getJFrameContentPane().add(getCancelButton(), getCancelButton().getName());
			getJFrameContentPane().add(getOKButton(), getOKButton().getName());
			getJFrameContentPane().add(getPreferedXCheckBox(), getPreferedXCheckBox().getName());
			getJFrameContentPane().add(getPreferedYCheckBox(), getPreferedYCheckBox().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJFrameContentPane;
}
/**
 * Return the JLabel1 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel1() {
	if (ivjJLabel1 == null) {
		try {
			ivjJLabel1 = new javax.swing.JLabel();
			ivjJLabel1.setName("JLabel1");
			ivjJLabel1.setText("X min");
			ivjJLabel1.setMaximumSize(new java.awt.Dimension(32, 14));
			ivjJLabel1.setBounds(20, 17, 45, 21);
			ivjJLabel1.setMinimumSize(new java.awt.Dimension(32, 14));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel1;
}
/**
 * Return the JLabel2 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel2() {
	if (ivjJLabel2 == null) {
		try {
			ivjJLabel2 = new javax.swing.JLabel();
			ivjJLabel2.setName("JLabel2");
			ivjJLabel2.setText("X max");
			ivjJLabel2.setMaximumSize(new java.awt.Dimension(36, 14));
			ivjJLabel2.setBounds(20, 43, 45, 21);
			ivjJLabel2.setMinimumSize(new java.awt.Dimension(36, 14));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel2;
}
/**
 * Return the JLabel3 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel3() {
	if (ivjJLabel3 == null) {
		try {
			ivjJLabel3 = new javax.swing.JLabel();
			ivjJLabel3.setName("JLabel3");
			ivjJLabel3.setText("Y min");
			ivjJLabel3.setMaximumSize(new java.awt.Dimension(31, 14));
			ivjJLabel3.setBounds(190, 17, 45, 21);
			ivjJLabel3.setMinimumSize(new java.awt.Dimension(31, 14));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel3;
}
/**
 * Return the JLabel4 property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getJLabel4() {
	if (ivjJLabel4 == null) {
		try {
			ivjJLabel4 = new javax.swing.JLabel();
			ivjJLabel4.setName("JLabel4");
			ivjJLabel4.setText("Y max");
			ivjJLabel4.setMaximumSize(new java.awt.Dimension(35, 14));
			ivjJLabel4.setBounds(190, 43, 45, 21);
			ivjJLabel4.setMinimumSize(new java.awt.Dimension(35, 14));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJLabel4;
}
/**
 * Return the MaxValueLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getMaxValueLabel() {
	if (ivjMaxValueLabel == null) {
		try {
			ivjMaxValueLabel = new javax.swing.JLabel();
			ivjMaxValueLabel.setName("MaxValueLabel");
			ivjMaxValueLabel.setText("Maximum (x,y)");
			ivjMaxValueLabel.setMaximumSize(new java.awt.Dimension(89, 20));
			ivjMaxValueLabel.setBounds(5, 134, 340, 19);
			ivjMaxValueLabel.setMinimumSize(new java.awt.Dimension(89, 20));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjMaxValueLabel;
}
/**
 * Return the MaxXText property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getMaxXText() {
	if (ivjMaxXText == null) {
		try {
			ivjMaxXText = new javax.swing.JTextField();
			ivjMaxXText.setName("MaxXText");
			ivjMaxXText.setText("");
			ivjMaxXText.setMaximumSize(new java.awt.Dimension(32767, 25));
			ivjMaxXText.setFont(new java.awt.Font("dialog", 0, 12));
			ivjMaxXText.setEditable(true);
			ivjMaxXText.setMinimumSize(new java.awt.Dimension(120, 25));
			ivjMaxXText.setBounds(65, 43, 100, 21);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjMaxXText;
}
/**
 * Return the MaxYText property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getMaxYText() {
	if (ivjMaxYText == null) {
		try {
			ivjMaxYText = new javax.swing.JTextField();
			ivjMaxYText.setName("MaxYText");
			ivjMaxYText.setText("");
			ivjMaxYText.setMaximumSize(new java.awt.Dimension(32767, 25));
			ivjMaxYText.setFont(new java.awt.Font("dialog", 0, 12));
			ivjMaxYText.setEditable(true);
			ivjMaxYText.setMinimumSize(new java.awt.Dimension(120, 25));
			ivjMaxYText.setBounds(235, 43, 100, 21);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjMaxYText;
}
/**
 * Return the MinValueLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getMinValueLabel() {
	if (ivjMinValueLabel == null) {
		try {
			ivjMinValueLabel = new javax.swing.JLabel();
			ivjMinValueLabel.setName("MinValueLabel");
			ivjMinValueLabel.setText("Minimum  (x.y)");
			ivjMinValueLabel.setMaximumSize(new java.awt.Dimension(88, 20));
			ivjMinValueLabel.setBounds(5, 110, 340, 19);
			ivjMinValueLabel.setMinimumSize(new java.awt.Dimension(88, 20));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjMinValueLabel;
}
/**
 * Return the MinXText property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getMinXText() {
	if (ivjMinXText == null) {
		try {
			ivjMinXText = new javax.swing.JTextField();
			ivjMinXText.setName("MinXText");
			ivjMinXText.setText("");
			ivjMinXText.setMaximumSize(new java.awt.Dimension(32767, 25));
			ivjMinXText.setFont(new java.awt.Font("dialog", 0, 12));
			ivjMinXText.setEditable(true);
			ivjMinXText.setMinimumSize(new java.awt.Dimension(120, 25));
			ivjMinXText.setBounds(65, 17, 100, 21);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjMinXText;
}
/**
 * Return the MinYText property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getMinYText() {
	if (ivjMinYText == null) {
		try {
			ivjMinYText = new javax.swing.JTextField();
			ivjMinYText.setName("MinYText");
			ivjMinYText.setText("");
			ivjMinYText.setMaximumSize(new java.awt.Dimension(32767, 25));
			ivjMinYText.setFont(new java.awt.Font("dialog", 0, 12));
			ivjMinYText.setEditable(true);
			ivjMinYText.setMinimumSize(new java.awt.Dimension(120, 25));
			ivjMinYText.setBounds(235, 17, 100, 21);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjMinYText;
}
/**
 * Return the OKButton property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getOKButton() {
	if (ivjOKButton == null) {
		try {
			ivjOKButton = new javax.swing.JButton();
			ivjOKButton.setName("OKButton");
			ivjOKButton.setText("OK");
			ivjOKButton.setMaximumSize(new java.awt.Dimension(37, 25));
			ivjOKButton.setActionCommand("OK");
			ivjOKButton.setMargin(new java.awt.Insets(2, 7, 2, 7));
			ivjOKButton.setMinimumSize(new java.awt.Dimension(37, 25));
			ivjOKButton.setBounds(70, 168, 70, 25);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjOKButton;
}
/**
 * Return the PreferedXCheckBox property value.
 * @return javax.swing.JCheckBox
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JCheckBox getPreferedXCheckBox() {
	if (ivjPreferedXCheckBox == null) {
		try {
			ivjPreferedXCheckBox = new javax.swing.JCheckBox();
			ivjPreferedXCheckBox.setName("PreferedXCheckBox");
			ivjPreferedXCheckBox.setText("Prefered");
			ivjPreferedXCheckBox.setMaximumSize(new java.awt.Dimension(75, 22));
			ivjPreferedXCheckBox.setActionCommand("Prefered");
			ivjPreferedXCheckBox.setSelected(true);
			ivjPreferedXCheckBox.setBounds(20, 74, 97, 21);
			ivjPreferedXCheckBox.setMinimumSize(new java.awt.Dimension(75, 22));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPreferedXCheckBox;
}
/**
 * Return the PreferedYCheckBox property value.
 * @return javax.swing.JCheckBox
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JCheckBox getPreferedYCheckBox() {
	if (ivjPreferedYCheckBox == null) {
		try {
			ivjPreferedYCheckBox = new javax.swing.JCheckBox();
			ivjPreferedYCheckBox.setName("PreferedYCheckBox");
			ivjPreferedYCheckBox.setText("Prefered");
			ivjPreferedYCheckBox.setMaximumSize(new java.awt.Dimension(75, 22));
			ivjPreferedYCheckBox.setActionCommand("Prefered");
			ivjPreferedYCheckBox.setSelected(true);
			ivjPreferedYCheckBox.setBounds(190, 74, 97, 21);
			ivjPreferedYCheckBox.setMinimumSize(new java.awt.Dimension(75, 22));
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjPreferedYCheckBox;
}
/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(java.lang.Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	System.out.println("--------- UNCAUGHT EXCEPTION ---------");
	exception.printStackTrace(System.out);
}
/**
 * Initializes connections
 * @exception java.lang.Exception The exception description.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initConnections() throws java.lang.Exception {
	// user code begin {1}
	// user code end
	getOKButton().addActionListener(ivjEventHandler);
	getCancelButton().addActionListener(ivjEventHandler);
	this.addWindowListener(ivjEventHandler);
	getMinXText().addActionListener(ivjEventHandler);
	getMaxXText().addActionListener(ivjEventHandler);
	getMinYText().addActionListener(ivjEventHandler);
	getMaxYText().addActionListener(ivjEventHandler);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("BaseChartCustomizer");
		setDefaultCloseOperation(2);
		setTitle("Chart Customizer");
		setBounds(new java.awt.Rectangle(0, 0, 356, 232));
		setSize(356, 232);
		setResizable(false);
		setContentPane(getJFrameContentPane());
		initConnections();
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	try {
		BaseChartCustomizer aFlowCharCustomizer;
		aFlowCharCustomizer = new BaseChartCustomizer();
		aFlowCharCustomizer.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		aFlowCharCustomizer.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JFrame");
		exception.printStackTrace(System.out);
	}
}
/**
 * This method sets new scale in x and y directions, if not selected Prefered CheckBox.
 */
public void oKButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	if (chart==null) dispose();
	try {
		if (getPreferedXCheckBox().isSelected()) chart.getViewManager().setUserXScaleUsed(true);
		else chart.getViewManager().setXScale(new Interval(Double.valueOf(getMinXText().getText()).doubleValue(),Double.valueOf(getMaxXText().getText()).doubleValue()));
		if (getPreferedYCheckBox().isSelected()) chart.getViewManager().setUserYScaleUsed(true);
		else chart.getViewManager().setYScale(new Interval(Double.valueOf(getMinYText().getText()).doubleValue(),Double.valueOf(getMaxYText().getText()).doubleValue()));
		chart.updateChart(ChartUpdateRequest.UPDATE_ALL);
		dispose();
	} catch (NumberFormatException e) {
		javax.swing.JOptionPane.showConfirmDialog(this,"Parsing number exception: "+e.toString(),"Error",javax.swing.JOptionPane.OK_OPTION,javax.swing.JOptionPane.ERROR_MESSAGE);
	}
}
/**
 * This method sets BaseChart to this BaseChartCustomizer.
 */
public void setChart(BaseChart newChart) {
	chart = newChart;
}
/**
 * This method update this BaseChartCustomizer with settings from own BaseChart.
 */
public void updateCustomizer() {
	if (chart==null) return;
	getMinXText().setText(Double.toString(chart.getViewManager().getXScale().min()));
	getMaxXText().setText(Double.toString(chart.getViewManager().getXScale().max()));
	getMinYText().setText(Double.toString(chart.getViewManager().getYScale().min()));
	getMaxYText().setText(Double.toString(chart.getViewManager().getYScale().max()));
	getPreferedXCheckBox().setSelected(chart.getViewManager().isUserXScaleUsed());
	getPreferedYCheckBox().setSelected(chart.getViewManager().isUserYScaleUsed());
	getMinValueLabel().setText("Min Value ("+format("#.####",chart.getChartArea().getMinValue().x)+","+format("#.####",chart.getChartArea().getMinValue().y)+")");
	getMaxValueLabel().setText("Max Value ("+format("#.####",chart.getChartArea().getMaxValue().x)+","+format("#.####",chart.getChartArea().getMaxValue().y)+")");
}
}
