package si.ijs.acs.objectexplorer.engine.BACI;

import javax.swing.*;
/**
 * Insert the type's description here.
 * Creation date: (30.11.2000 21:56:24)
 * @author: 
 */
public class CorbalocDialog extends JDialog {
	private JLabel ivjHelpLabel = null;
	private JLabel ivjInstructionLabel = null;
	private JLabel ivjIRLabel = null;
	private JPanel ivjJDialogContentPane = null;
	private JPanel ivjJPanel1 = null;
	private JTextField ivjManagerField = null;
	private JLabel ivjManagerLabel = null;
	private JButton ivjOKButton = null;
	private JTextField ivjRepositoryField = null;
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private JCheckBox ivjDebug = null;
	private boolean isOKed = false;

class IvjEventHandler implements java.awt.event.ActionListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == CorbalocDialog.this.getOKButton()) 
			{
				oKButton_ActionPerformed(e);
				connEtoM1(e);
			}
			if (e.getSource() == CorbalocDialog.this.getManagerField()) 
				connEtoM2(e);
			if (e.getSource() == CorbalocDialog.this.getRepositoryField()) 
				connEtoM3(e);
		};
	};
/**
 * CorbalocDialog constructor comment.
 */
public CorbalocDialog() {
	super();
	initialize();
}
/**
 * CorbalocDialog constructor comment.
 * @param owner java.awt.Dialog
 */
public CorbalocDialog(java.awt.Dialog owner) {
	super(owner);
}
/**
 * CorbalocDialog constructor comment.
 * @param owner java.awt.Dialog
 * @param title java.lang.String
 */
public CorbalocDialog(java.awt.Dialog owner, String title) {
	super(owner, title);
}
/**
 * CorbalocDialog constructor comment.
 * @param owner java.awt.Dialog
 * @param title java.lang.String
 * @param modal boolean
 */
public CorbalocDialog(java.awt.Dialog owner, String title, boolean modal) {
	super(owner, title, modal);
}
/**
 * CorbalocDialog constructor comment.
 * @param owner java.awt.Dialog
 * @param modal boolean
 */
public CorbalocDialog(java.awt.Dialog owner, boolean modal) {
	super(owner, modal);
}
/**
 * CorbalocDialog constructor comment.
 * @param owner java.awt.Frame
 */
public CorbalocDialog(java.awt.Frame owner) {
	super(owner);
}
/**
 * CorbalocDialog constructor comment.
 * @param owner java.awt.Frame
 * @param title java.lang.String
 */
public CorbalocDialog(java.awt.Frame owner, String title) {
	super(owner, title);
}
/**
 * CorbalocDialog constructor comment.
 * @param owner java.awt.Frame
 * @param title java.lang.String
 * @param modal boolean
 */
public CorbalocDialog(java.awt.Frame owner, String title, boolean modal) {
	super(owner, title, modal);
}
/**
 * CorbalocDialog constructor comment.
 * @param owner java.awt.Frame
 * @param modal boolean
 */
public CorbalocDialog(java.awt.Frame owner, boolean modal) {
	super(owner, modal);
}
/**
 * connEtoM1:  (OKButton.action.actionPerformed(java.awt.event.ActionEvent) --> CorbalocDialog.dispose()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM1(java.awt.event.ActionEvent arg1) {
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
 * connEtoM2:  (ManagerField.action.actionPerformed(java.awt.event.ActionEvent) --> RepositoryField.requestFocus()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM2(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getRepositoryField().requestFocus();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM3:  (RepositoryField.action.actionPerformed(java.awt.event.ActionEvent) --> CorbalocDialog.dispose()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM3(java.awt.event.ActionEvent arg1) {
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
 * Return the Debug property value.
 * @return javax.swing.JCheckBox
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JCheckBox getDebug() {
	if (ivjDebug == null) {
		try {
			ivjDebug = new javax.swing.JCheckBox();
			ivjDebug.setName("Debug");
			ivjDebug.setText("ORB debug");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjDebug;
}
/**
 * Method generated to support the promotion of the debugSelected attribute.
 * @return boolean
 */
public boolean getDebugSelected() {
	return getDebug().isSelected();
}
/**
 * Return the HelpLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getHelpLabel() {
	if (ivjHelpLabel == null) {
		try {
			ivjHelpLabel = new javax.swing.JLabel();
			ivjHelpLabel.setName("HelpLabel");
			ivjHelpLabel.setFont(new java.awt.Font("Arial", 2, 12));
			ivjHelpLabel.setText("(Use the form corbaloc::<host>:<port>/<ObjectID>)");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjHelpLabel;
}
/**
 * Return the InstructionLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getInstructionLabel() {
	if (ivjInstructionLabel == null) {
		try {
			ivjInstructionLabel = new javax.swing.JLabel();
			ivjInstructionLabel.setName("InstructionLabel");
			ivjInstructionLabel.setText("Enter Manager and InterfaceRepository CORBALOC");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjInstructionLabel;
}
/**
 * Return the IRLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getIRLabel() {
	if (ivjIRLabel == null) {
		try {
			ivjIRLabel = new javax.swing.JLabel();
			ivjIRLabel.setName("IRLabel");
			ivjIRLabel.setText("Repository:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjIRLabel;
}
/**
 * Return the JDialogContentPane property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJDialogContentPane() {
	if (ivjJDialogContentPane == null) {
		try {
			ivjJDialogContentPane = new javax.swing.JPanel();
			ivjJDialogContentPane.setName("JDialogContentPane");
			ivjJDialogContentPane.setLayout(new java.awt.GridBagLayout());

			java.awt.GridBagConstraints constraintsManagerLabel = new java.awt.GridBagConstraints();
			constraintsManagerLabel.gridx = 0; constraintsManagerLabel.gridy = 2;
			constraintsManagerLabel.anchor = java.awt.GridBagConstraints.WEST;
			constraintsManagerLabel.insets = new java.awt.Insets(4, 4, 4, 4);
			getJDialogContentPane().add(getManagerLabel(), constraintsManagerLabel);

			java.awt.GridBagConstraints constraintsInstructionLabel = new java.awt.GridBagConstraints();
			constraintsInstructionLabel.gridx = 0; constraintsInstructionLabel.gridy = 0;
			constraintsInstructionLabel.gridwidth = 2;
			constraintsInstructionLabel.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsInstructionLabel.insets = new java.awt.Insets(4, 4, 4, 4);
			getJDialogContentPane().add(getInstructionLabel(), constraintsInstructionLabel);

			java.awt.GridBagConstraints constraintsHelpLabel = new java.awt.GridBagConstraints();
			constraintsHelpLabel.gridx = 0; constraintsHelpLabel.gridy = 1;
			constraintsHelpLabel.gridwidth = 2;
			constraintsHelpLabel.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsHelpLabel.insets = new java.awt.Insets(4, 4, 15, 4);
			getJDialogContentPane().add(getHelpLabel(), constraintsHelpLabel);

			java.awt.GridBagConstraints constraintsManagerField = new java.awt.GridBagConstraints();
			constraintsManagerField.gridx = 1; constraintsManagerField.gridy = 2;
			constraintsManagerField.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsManagerField.anchor = java.awt.GridBagConstraints.EAST;
			constraintsManagerField.weightx = 1.0;
			constraintsManagerField.insets = new java.awt.Insets(4, 4, 4, 4);
			getJDialogContentPane().add(getManagerField(), constraintsManagerField);

			java.awt.GridBagConstraints constraintsIRLabel = new java.awt.GridBagConstraints();
			constraintsIRLabel.gridx = 0; constraintsIRLabel.gridy = 3;
			constraintsIRLabel.anchor = java.awt.GridBagConstraints.WEST;
			constraintsIRLabel.insets = new java.awt.Insets(4, 4, 4, 4);
			getJDialogContentPane().add(getIRLabel(), constraintsIRLabel);

			java.awt.GridBagConstraints constraintsRepositoryField = new java.awt.GridBagConstraints();
			constraintsRepositoryField.gridx = 1; constraintsRepositoryField.gridy = 3;
			constraintsRepositoryField.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsRepositoryField.anchor = java.awt.GridBagConstraints.EAST;
			constraintsRepositoryField.weightx = 1.0;
			constraintsRepositoryField.insets = new java.awt.Insets(4, 4, 4, 4);
			getJDialogContentPane().add(getRepositoryField(), constraintsRepositoryField);

			java.awt.GridBagConstraints constraintsJPanel1 = new java.awt.GridBagConstraints();
			constraintsJPanel1.gridx = 0; constraintsJPanel1.gridy = 5;
			constraintsJPanel1.gridwidth = 2;
			constraintsJPanel1.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJPanel1.weightx = 0.6;
			constraintsJPanel1.weighty = 1.0;
			constraintsJPanel1.insets = new java.awt.Insets(4, 4, 4, 4);
			getJDialogContentPane().add(getJPanel1(), constraintsJPanel1);

			java.awt.GridBagConstraints constraintsDebug = new java.awt.GridBagConstraints();
			constraintsDebug.gridx = 1; constraintsDebug.gridy = 4;
			constraintsDebug.anchor = java.awt.GridBagConstraints.WEST;
			constraintsDebug.insets = new java.awt.Insets(4, 4, 4, 4);
			getJDialogContentPane().add(getDebug(), constraintsDebug);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJDialogContentPane;
}
/**
 * Return the JPanel1 property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJPanel1() {
	if (ivjJPanel1 == null) {
		try {
			ivjJPanel1 = new javax.swing.JPanel();
			ivjJPanel1.setName("JPanel1");
			ivjJPanel1.setLayout(new java.awt.FlowLayout());
			getJPanel1().add(getOKButton(), getOKButton().getName());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJPanel1;
}
/**
 * Return the ManagerField property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getManagerField() {
	if (ivjManagerField == null) {
		try {
			ivjManagerField = new javax.swing.JTextField();
			ivjManagerField.setName("ManagerField");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjManagerField;
}
/**
 * Method generated to support the promotion of the managerFieldText attribute.
 * @return java.lang.String
 */
public java.lang.String getManagerFieldText() {
	return getManagerField().getText();
}
/**
 * Return the ManagerLabel property value.
 * @return javax.swing.JLabel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JLabel getManagerLabel() {
	if (ivjManagerLabel == null) {
		try {
			ivjManagerLabel = new javax.swing.JLabel();
			ivjManagerLabel.setName("ManagerLabel");
			ivjManagerLabel.setText("Manager:");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjManagerLabel;
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
			ivjOKButton.setPreferredSize(new java.awt.Dimension(80, 25));
			ivjOKButton.setText("OK");
			ivjOKButton.setMaximumSize(new java.awt.Dimension(80, 25));
			ivjOKButton.setMinimumSize(new java.awt.Dimension(80, 25));
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
 * Return the RepositoryField property value.
 * @return javax.swing.JTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextField getRepositoryField() {
	if (ivjRepositoryField == null) {
		try {
			ivjRepositoryField = new javax.swing.JTextField();
			ivjRepositoryField.setName("RepositoryField");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjRepositoryField;
}
/**
 * Method generated to support the promotion of the repositoryFieldText attribute.
 * @return java.lang.String
 */
public java.lang.String getRepositoryFieldText() {
	return getRepositoryField().getText();
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
	getManagerField().addActionListener(ivjEventHandler);
	getRepositoryField().addActionListener(ivjEventHandler);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("CorbalocDialog");
		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		setSize(536, 230);
		setModal(true);
		setTitle("BACIRemoteAccess Initialization");
		setContentPane(getJDialogContentPane());
		initConnections();
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
}
/**
 * Insert the method's description here.
 * Creation date: (6/29/2001 11:21:00 AM)
 * @return boolean
 */
public boolean isOKed() {
	return isOKed;
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	try {
		CorbalocDialog aCorbalocDialog;
		aCorbalocDialog = new CorbalocDialog();
		aCorbalocDialog.setModal(true);
		aCorbalocDialog.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
		aCorbalocDialog.show();
		java.awt.Insets insets = aCorbalocDialog.getInsets();
		aCorbalocDialog.setSize(aCorbalocDialog.getWidth() + insets.left + insets.right, aCorbalocDialog.getHeight() + insets.top + insets.bottom);
		aCorbalocDialog.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of javax.swing.JDialog");
		exception.printStackTrace(System.out);
	}
}
/**
 * Comment
 */
public void oKButton_ActionPerformed(java.awt.event.ActionEvent actionEvent) {
	isOKed = true;
	return;
}
/**
 * Method generated to support the promotion of the debugSelected attribute.
 * @param arg1 boolean
 */
public void setDebugSelected(boolean arg1) {
	getDebug().setSelected(arg1);
}
/**
 * Method generated to support the promotion of the managerFieldText attribute.
 * @param arg1 java.lang.String
 */
public void setManagerFieldText(java.lang.String arg1) {
	getManagerField().setText(arg1);
}
/**
 * Method generated to support the promotion of the repositoryFieldText attribute.
 * @param arg1 java.lang.String
 */
public void setRepositoryFieldText(java.lang.String arg1) {
	getRepositoryField().setText(arg1);
}
}
