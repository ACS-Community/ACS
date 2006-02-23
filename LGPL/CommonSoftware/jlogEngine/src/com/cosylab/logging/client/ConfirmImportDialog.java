/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package com.cosylab.logging.client;

import com.cosylab.gui.components.r2.JIntegerTextField;

/**
 * Uses ActionListener, PropertyChangeListener and WindowsListener for user's input. 
 * Used by LogImportTask to indicate the number of logs a file contains and 
 * the option to load less logs for performance reasons, as described in its method
 * showConfirmationDialog.
 * Creation date: (4/14/2002 17:26:58)
 * @author: 
 */
public class ConfirmImportDialog extends com.cosylab.logging.settings.JModalDialog {
	IvjEventHandler ivjEventHandler = new IvjEventHandler();
	private javax.swing.JButton ivjJButton1 = null;
	private javax.swing.JButton ivjJButton2 = null;
	private javax.swing.JCheckBox ivjJCheckBox1 = null;
	private javax.swing.JPanel ivjJModalDialogContentPane = null;
	private javax.swing.JPanel ivjJPanel1 = null;
	private javax.swing.JPanel ivjJPanel2 = null;
	private JIntegerTextField ivjLogLimitField = null;
	private javax.swing.JTextPane ivjJTextPane1 = null;

class IvjEventHandler implements java.awt.event.ActionListener, java.beans.PropertyChangeListener {
		public void actionPerformed(java.awt.event.ActionEvent e) {
			if (e.getSource() == ConfirmImportDialog.this.getJButton1()) 
				connEtoM1(e);
			if (e.getSource() == ConfirmImportDialog.this.getJButton2()) 
				connEtoM2(e);
		};
		public void propertyChange(java.beans.PropertyChangeEvent evt) {
			if (evt.getSource() == ConfirmImportDialog.this.getLogLimitField() && (evt.getPropertyName().equals("integerValue"))) 
				connEtoM3(evt);
		};
	};
/**
 * ConfirmImportDialog constructor comment.
 */
public ConfirmImportDialog() {
	super();
	initialize();
}
/**
 * ConfirmImportDialog constructor comment.
 * @param owner java.awt.Dialog
 */
public ConfirmImportDialog(java.awt.Dialog owner) {
	super(owner);
}
/**
 * ConfirmImportDialog constructor comment.
 * @param owner java.awt.Dialog
 * @param title java.lang.String
 */
public ConfirmImportDialog(java.awt.Dialog owner, String title) {
	super(owner, title);
}
/**
 * ConfirmImportDialog constructor comment.
 * @param owner java.awt.Dialog
 * @param title java.lang.String
 * @param modal boolean
 */
public ConfirmImportDialog(java.awt.Dialog owner, String title, boolean modal) {
	super(owner, title, modal);
}
/**
 * ConfirmImportDialog constructor comment.
 * @param owner java.awt.Dialog
 * @param modal boolean
 */
public ConfirmImportDialog(java.awt.Dialog owner, boolean modal) {
	super(owner, modal);
}
/**
 * ConfirmImportDialog constructor comment.
 * @param owner java.awt.Frame
 */
public ConfirmImportDialog(java.awt.Frame owner) {
	super(owner);
}
/**
 * ConfirmImportDialog constructor comment.
 * @param owner java.awt.Frame
 * @param title java.lang.String
 */
public ConfirmImportDialog(java.awt.Frame owner, String title) {
	super(owner, title);
}
/**
 * ConfirmImportDialog constructor comment.
 * @param owner java.awt.Frame
 * @param title java.lang.String
 * @param modal boolean
 */
public ConfirmImportDialog(java.awt.Frame owner, String title, boolean modal) {
	super(owner, title, modal);
}
/**
 * ConfirmImportDialog constructor comment.
 * @param owner java.awt.Frame
 * @param modal boolean
 */
public ConfirmImportDialog(java.awt.Frame owner, boolean modal) {
	super(owner, modal);
}
/**
 * connEtoM1:  (JButton1.action.actionPerformed(java.awt.event.ActionEvent) --> ConfirmImportDialog.returnModalOK()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM1(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.returnModalOK();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM2:  (JButton2.action.actionPerformed(java.awt.event.ActionEvent) --> ConfirmImportDialog.returnModalCancel()V)
 * @param arg1 java.awt.event.ActionEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM2(java.awt.event.ActionEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		this.returnModalCancel();
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * connEtoM3:  (LogLimitField.integerValue --> JCheckBox1.selected)
 * @param arg1 java.beans.PropertyChangeEvent
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void connEtoM3(java.beans.PropertyChangeEvent arg1) {
	try {
		// user code begin {1}
		// user code end
		getJCheckBox1().setSelected(true);
		// user code begin {2}
		// user code end
	} catch (java.lang.Throwable ivjExc) {
		// user code begin {3}
		// user code end
		handleException(ivjExc);
	}
}
/**
 * Return the JButton1 property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getJButton1() {
	if (ivjJButton1 == null) {
		try {
			ivjJButton1 = new javax.swing.JButton();
			ivjJButton1.setName("JButton1");
			ivjJButton1.setText("Continue");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJButton1;
}
/**
 * Return the JButton2 property value.
 * @return javax.swing.JButton
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JButton getJButton2() {
	if (ivjJButton2 == null) {
		try {
			ivjJButton2 = new javax.swing.JButton();
			ivjJButton2.setName("JButton2");
			ivjJButton2.setText("Cancel");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJButton2;
}
/**
 * Return the JCheckBox1 property value.
 * @return javax.swing.JCheckBox
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JCheckBox getJCheckBox1() {
	if (ivjJCheckBox1 == null) {
		try {
			ivjJCheckBox1 = new javax.swing.JCheckBox();
			ivjJCheckBox1.setName("JCheckBox1");
			ivjJCheckBox1.setText("Only import last");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJCheckBox1;
}
/**
 * Return the JModalDialogContentPane property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJModalDialogContentPane() {
	if (ivjJModalDialogContentPane == null) {
		try {
			ivjJModalDialogContentPane = new javax.swing.JPanel();
			ivjJModalDialogContentPane.setName("JModalDialogContentPane");
			ivjJModalDialogContentPane.setLayout(new java.awt.BorderLayout());
			getJModalDialogContentPane().add(getJPanel1(), "South");
			getJModalDialogContentPane().add(getJPanel2(), "Center");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJModalDialogContentPane;
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
			ivjJPanel1.setPreferredSize(new java.awt.Dimension(0, 40));
			ivjJPanel1.setLayout(new java.awt.GridBagLayout());
			ivjJPanel1.setMinimumSize(new java.awt.Dimension(0, 150));

			java.awt.GridBagConstraints constraintsJButton1 = new java.awt.GridBagConstraints();
			constraintsJButton1.gridx = 0; constraintsJButton1.gridy = 0;
			constraintsJButton1.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel1().add(getJButton1(), constraintsJButton1);

			java.awt.GridBagConstraints constraintsJButton2 = new java.awt.GridBagConstraints();
			constraintsJButton2.gridx = 1; constraintsJButton2.gridy = 0;
			constraintsJButton2.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel1().add(getJButton2(), constraintsJButton2);
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
 * Return the JPanel2 property value.
 * @return javax.swing.JPanel
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JPanel getJPanel2() {
	if (ivjJPanel2 == null) {
		try {
			ivjJPanel2 = new javax.swing.JPanel();
			ivjJPanel2.setName("JPanel2");
			ivjJPanel2.setLayout(new java.awt.GridBagLayout());

			java.awt.GridBagConstraints constraintsJCheckBox1 = new java.awt.GridBagConstraints();
			constraintsJCheckBox1.gridx = 0; constraintsJCheckBox1.gridy = 1;
			constraintsJCheckBox1.anchor = java.awt.GridBagConstraints.WEST;
			constraintsJCheckBox1.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel2().add(getJCheckBox1(), constraintsJCheckBox1);

			java.awt.GridBagConstraints constraintsLogLimitField = new java.awt.GridBagConstraints();
			constraintsLogLimitField.gridx = 1; constraintsLogLimitField.gridy = 1;
			constraintsLogLimitField.fill = java.awt.GridBagConstraints.HORIZONTAL;
			constraintsLogLimitField.weightx = 1.0;
			constraintsLogLimitField.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel2().add(getLogLimitField(), constraintsLogLimitField);

			java.awt.GridBagConstraints constraintsJTextPane1 = new java.awt.GridBagConstraints();
			constraintsJTextPane1.gridx = 0; constraintsJTextPane1.gridy = 0;
			constraintsJTextPane1.gridwidth = 2;
			constraintsJTextPane1.fill = java.awt.GridBagConstraints.BOTH;
			constraintsJTextPane1.weightx = 1.0;
			constraintsJTextPane1.weighty = 1.0;
			constraintsJTextPane1.insets = new java.awt.Insets(4, 4, 4, 4);
			getJPanel2().add(getJTextPane1(), constraintsJTextPane1);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJPanel2;
}
/**
 * Return the JTextPane1 property value.
 * @return javax.swing.JTextPane
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private javax.swing.JTextPane getJTextPane1() {
	if (ivjJTextPane1 == null) {
		try {
			ivjJTextPane1 = new javax.swing.JTextPane();
			ivjJTextPane1.setName("JTextPane1");
			ivjJTextPane1.setDocument(new javax.swing.text.html.HTMLDocument());
			ivjJTextPane1.setText("<html><body><h1><b>Test</b></body></html>");
			ivjJTextPane1.setEditable(false);
			ivjJTextPane1.setContentType("text/html");
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjJTextPane1;
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 18:45:51)
 * @return int
 */
public int getLogLimit() {
	return getLogLimitField().getIntegerValue();
}
/**
 * Return the LogLimitField property value.
 * @return com.cosylab.gui.components.JIntegerTextField
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private JIntegerTextField getLogLimitField() {
	if (ivjLogLimitField == null) {
		try {
			ivjLogLimitField = new JIntegerTextField();
			ivjLogLimitField.setName("LogLimitField");
			ivjLogLimitField.setIntegerValue(500);
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {2}
			// user code end
			handleException(ivjExc);
		}
	}
	return ivjLogLimitField;
}
/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(java.lang.Throwable exception) {

	/* Uncomment the following lines to print uncaught exceptions to stdout */
	// System.out.println("--------- UNCAUGHT EXCEPTION ---------");
	// exception.printStackTrace(System.out);
}
/**
 * Initializes connections
 * @exception java.lang.Exception The exception description.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initConnections() throws java.lang.Exception {
	// user code begin {1}
	// user code end
	getJButton1().addActionListener(ivjEventHandler);
	getJButton2().addActionListener(ivjEventHandler);
	getLogLimitField().addPropertyChangeListener(ivjEventHandler);
}
/**
 * Initialize the class.
 */
/* WARNING: THIS METHOD WILL BE REGENERATED. */
private void initialize() {
	try {
		// user code begin {1}
		// user code end
		setName("ConfirmImportDialog");
		setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
		setSize(304, 240);
		setTitle("Please confirm file import");
		setContentPane(getJModalDialogContentPane());
		initConnections();
	} catch (java.lang.Throwable ivjExc) {
		handleException(ivjExc);
	}
	// user code begin {2}
	// user code end
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 18:45:25)
 * @return boolean
 */
public boolean isTrimChecked() {
	return getJCheckBox1().isSelected();
}
/**
 * main entrypoint - starts the part when it is run as an application
 * @param args java.lang.String[]
 */
public static void main(java.lang.String[] args) {
	try {
		ConfirmImportDialog aConfirmImportDialog;
		aConfirmImportDialog = new ConfirmImportDialog();
		aConfirmImportDialog.setModal(true);
		aConfirmImportDialog.addWindowListener(new java.awt.event.WindowAdapter() {
			public void windowClosing(java.awt.event.WindowEvent e) {
				System.exit(0);
			};
		});
//		aConfirmImportDialog.show();
		java.awt.Insets insets = aConfirmImportDialog.getInsets();
		aConfirmImportDialog.setSize(aConfirmImportDialog.getWidth() + insets.left + insets.right, aConfirmImportDialog.getHeight() + insets.top + insets.bottom);
		aConfirmImportDialog.setVisible(true);
	} catch (Throwable exception) {
		System.err.println("Exception occurred in main() of com.cosylab.logging.settings.JModalDialog");
		exception.printStackTrace(System.out);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (4/14/2002 17:39:30)
 * @return int
 * @param filename java.lang.String
 * @param numberOfLogs int
 */
public int showConfirmationDialog(String filename, int numberOfLogs) {
	String warning = "<html><body>";
	warning += "Specified file ";
	if (filename != null) {
		warning += "<b>" + filename + "</b> ";
	}
	warning += "contains <b>"+numberOfLogs+"</b> log entries.<BR>";
	warning += "This may put excessive load on the system resources and could take a very long time.<BR>";
	warning += "To import only a limited number of most recent logs, enter the desired value in the field below.";
	warning += "</body></html>";
	getJTextPane1().setText(warning);
	setBounds(50,50,50,50);
	pack();
	return showModal();
}
}
