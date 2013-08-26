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
package com.cosylab.logging.settings;

import java.awt.Dialog;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import com.cosylab.logging.engine.Filter;

/**
 * Serves the purpose of filtering according to the parameter. Used by
 * LogEntryTable. Creation date: (2/7/02 12:36:55 PM)
 * 
 * @author:
 */
public class FilterParameterDialog extends JDialog implements ActionListener {
	private FilterDatePanel ivjDateEditor = null;
	private FieldClassChooser fieldIndexChooser = null;
	private FilterIntegerPanel ivjIntegerEditor = null;
	private FilterTypePanel typeEditor = null;
	private JLabel ivjJLabel2 = null;
	private JPanel ivjJModalDialogContentPane = null;
	private JPanel ivjJPanel1 = null;
	private JPanel ivjJPanel2 = null;
	private FilterStringPanel ivjStringEditor = null;
	private JButton ivjCancelButton = null;
	private JLabel ivjErrorLabel = null;
	private JButton ivjOKButton = null;
	
	/**
	 * Used to signal the caller if the user pressed OK
	 */
	private boolean okPressed;
	
	/**
	 * The Dialog to display this dialog over
	 */
	private final Dialog ownerDialog;

	/**
	 * FilterParameterDialog constructor comment.
	 * 
	 * @param comp The component used to show this panel over
	 */
	public FilterParameterDialog(Dialog owner) {
		super(owner,true);
		if (owner==null) {
			throw new IllegalArgumentException("LoggingClient can't be null");
		}
		ownerDialog=owner;
		initialize();
	}

	/**
	 * connPtoP4SetTarget: (JPanel2.this <--> FieldIndexChooser.mainPanel)
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private void connPtoP4SetTarget() {
		/* Set the target from the source */
		try {
			getFieldIndexChooser().setMainPanel(getJPanel2());
			// user code begin {1}
			// user code end
		} catch (java.lang.Throwable ivjExc) {
			// user code begin {3}
			// user code end
			handleException(ivjExc);
		}
	}

	/**
	 * Return the JButton2 property value.
	 * 
	 * @return javax.swing.JButton
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private javax.swing.JButton getCancelButton() {
		if (ivjCancelButton == null) {
			try {
				ivjCancelButton = new javax.swing.JButton();
				ivjCancelButton.setName("CancelButton");
				ivjCancelButton.setText("Cancel");
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
	 * Return the DateEditor property value.
	 * 
	 * @return com.cosylab.logging.settings.FilterDatePanel
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private FilterDatePanel getDateEditor() {
		if (ivjDateEditor == null) {
			try {
				ivjDateEditor = new com.cosylab.logging.settings.FilterDatePanel();
				ivjDateEditor.setName("DateEditor");
				ivjDateEditor.setBounds(371, 319, 236, 292);
				// user code begin {1}
				// user code end
			} catch (java.lang.Throwable ivjExc) {
				// user code begin {2}
				// user code end
				handleException(ivjExc);
			}
		}
		return ivjDateEditor;
	}

	/**
	 * Return the FieldErrorLabel property value.
	 * 
	 * @return javax.swing.JLabel
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private javax.swing.JLabel getErrorLabel() {
		if (ivjErrorLabel == null) {
			try {
				ivjErrorLabel = new javax.swing.JLabel();
				ivjErrorLabel.setName("ErrorLabel");
				ivjErrorLabel.setText("");
				// user code begin {1}
				// user code end
			} catch (java.lang.Throwable ivjExc) {
				// user code begin {2}
				// user code end
				handleException(ivjExc);
			}
		}
		return ivjErrorLabel;
	}

	/**
	 * Return the FieldIndexChooser property value.
	 * 
	 * @return com.cosylab.logging.settings.FieldClassChooser
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private FieldClassChooser getFieldIndexChooser() {
		if (fieldIndexChooser == null) {
			try {
				fieldIndexChooser = new com.cosylab.logging.settings.FieldClassChooser();
				fieldIndexChooser.setName("FieldIndexChooser");
				// user code begin {1}
				// user code end
			} catch (java.lang.Throwable ivjExc) {
				// user code begin {2}
				// user code end
				handleException(ivjExc);
			}
		}
		return fieldIndexChooser;
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 2:12:27 PM)
	 * 
	 * @return com.cosylab.logging.engine.Filter
	 */
	public Filter getFilter() {
		return getFieldIndexChooser().getFilter();
	}

	/**
	 * Return the IntegerEditor property value.
	 * 
	 * @return com.cosylab.logging.settings.FilterIntegerPanel
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private FilterIntegerPanel getIntegerEditor() {
		if (ivjIntegerEditor == null) {
			try {
				ivjIntegerEditor = new com.cosylab.logging.settings.FilterIntegerPanel();
				ivjIntegerEditor.setName("IntegerEditor");
				ivjIntegerEditor.setBounds(373, 21, 213, 166);
				// user code begin {1}
				// user code end
			} catch (java.lang.Throwable ivjExc) {
				// user code begin {2}
				// user code end
				handleException(ivjExc);
			}
		}
		return ivjIntegerEditor;
	}

	private FilterTypePanel getTypeEditor() {
		if (typeEditor == null) {
			try {
				typeEditor = new FilterTypePanel();
				typeEditor.setName("TypeEditor");
				typeEditor.setBounds(373, 21, 213, 166);
			} catch (java.lang.Throwable ivjExc) {
				ivjExc.printStackTrace();
				handleException(ivjExc);
			}
		}
		return typeEditor;
	}

	/**
	 * Return the JLabel2 property value.
	 * 
	 * @return javax.swing.JLabel
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private javax.swing.JLabel getJLabel2() {
		if (ivjJLabel2 == null) {
			try {
				ivjJLabel2 = new javax.swing.JLabel();
				ivjJLabel2.setName("JLabel2");
				ivjJLabel2.setText("Select field for new filter");
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
	 * Return the JModalDialogContentPane property value.
	 * 
	 * @return javax.swing.JPanel
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private javax.swing.JPanel getJModalDialogContentPane() {
		if (ivjJModalDialogContentPane == null) {
			try {
				ivjJModalDialogContentPane = new javax.swing.JPanel();
				ivjJModalDialogContentPane.setName("JModalDialogContentPane");
				ivjJModalDialogContentPane
						.setLayout(new java.awt.GridBagLayout());

				java.awt.GridBagConstraints constraintsFieldIndexChooser = new java.awt.GridBagConstraints();
				constraintsFieldIndexChooser.gridx = 0;
				constraintsFieldIndexChooser.gridy = 1;
				constraintsFieldIndexChooser.fill = java.awt.GridBagConstraints.HORIZONTAL;
				constraintsFieldIndexChooser.weightx = 1.0;
				constraintsFieldIndexChooser.insets = new java.awt.Insets(4, 4,
						4, 4);
				getJModalDialogContentPane().add(getFieldIndexChooser(),
						constraintsFieldIndexChooser);

				java.awt.GridBagConstraints constraintsJPanel1 = new java.awt.GridBagConstraints();
				constraintsJPanel1.gridx = 0;
				constraintsJPanel1.gridy = 4;
				constraintsJPanel1.fill = java.awt.GridBagConstraints.BOTH;
				constraintsJPanel1.weightx = 1.0;
				constraintsJPanel1.insets = new java.awt.Insets(4, 4, 4, 4);
				getJModalDialogContentPane().add(getJPanel1(),
						constraintsJPanel1);

				java.awt.GridBagConstraints constraintsJPanel2 = new java.awt.GridBagConstraints();
				constraintsJPanel2.gridx = 0;
				constraintsJPanel2.gridy = 2;
				constraintsJPanel2.fill = java.awt.GridBagConstraints.BOTH;
				constraintsJPanel2.weightx = 1.0;
				constraintsJPanel2.weighty = 1.0;
				constraintsJPanel2.insets = new java.awt.Insets(4, 4, 4, 4);
				getJModalDialogContentPane().add(getJPanel2(),
						constraintsJPanel2);

				java.awt.GridBagConstraints constraintsErrorLabel = new java.awt.GridBagConstraints();
				constraintsErrorLabel.gridx = 0;
				constraintsErrorLabel.gridy = 3;
				constraintsErrorLabel.insets = new java.awt.Insets(4, 4, 4, 4);
				getJModalDialogContentPane().add(getErrorLabel(),
						constraintsErrorLabel);

				java.awt.GridBagConstraints constraintsJLabel2 = new java.awt.GridBagConstraints();
				constraintsJLabel2.gridx = 0;
				constraintsJLabel2.gridy = 0;
				constraintsJLabel2.anchor = java.awt.GridBagConstraints.WEST;
				constraintsJLabel2.insets = new java.awt.Insets(4, 4, 4, 4);
				getJModalDialogContentPane().add(getJLabel2(),
						constraintsJLabel2);
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
	 * 
	 * @return javax.swing.JPanel
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private javax.swing.JPanel getJPanel1() {
		if (ivjJPanel1 == null) {
			try {
				ivjJPanel1 = new javax.swing.JPanel();
				ivjJPanel1.setName("JPanel1");
				ivjJPanel1.setLayout(new java.awt.GridBagLayout());

				java.awt.GridBagConstraints constraintsOKButton = new java.awt.GridBagConstraints();
				constraintsOKButton.gridx = 0;
				constraintsOKButton.gridy = 0;
				constraintsOKButton.insets = new java.awt.Insets(4, 4, 4, 4);
				getJPanel1().add(getOKButton(), constraintsOKButton);

				java.awt.GridBagConstraints constraintsCancelButton = new java.awt.GridBagConstraints();
				constraintsCancelButton.gridx = 1;
				constraintsCancelButton.gridy = 0;
				constraintsCancelButton.insets = new java.awt.Insets(4, 4, 4, 4);
				getJPanel1().add(getCancelButton(), constraintsCancelButton);
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
	 * 
	 * @return javax.swing.JPanel
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private javax.swing.JPanel getJPanel2() {
		if (ivjJPanel2 == null) {
			try {
				ivjJPanel2 = new JPanel();
				ivjJPanel2.setName("JPanel2");
				ivjJPanel2.setLayout(new GridBagLayout());
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
	 * Return the JButton1 property value.
	 * 
	 * @return javax.swing.JButton
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private javax.swing.JButton getOKButton() {
		if (ivjOKButton == null) {
			try {
				ivjOKButton = new JButton();
				ivjOKButton.setName("OKButton");
				ivjOKButton.setText("OK");
				// user code begin {1}
				// user code end
			} catch (Throwable ivjExc) {
				// user code begin {2}
				// user code end
				handleException(ivjExc);
			}
		}
		return ivjOKButton;
	}

	/**
	 * Return the StringEditor property value.
	 * 
	 * @return com.cosylab.logging.settings.FilterStringPanel
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private FilterStringPanel getStringEditor() {
		if (ivjStringEditor == null) {
			try {
				ivjStringEditor = new FilterStringPanel();
				ivjStringEditor.setName("StringEditor");
				ivjStringEditor.setBounds(373, 193, 215, 120);
				// user code begin {1}
				// user code end
			} catch (java.lang.Throwable ivjExc) {
				// user code begin {2}
				// user code end
				handleException(ivjExc);
			}
		}
		return ivjStringEditor;
	}

	/**
	 * Called whenever the part throws an exception.
	 * 
	 * @param exception
	 *            java.lang.Throwable
	 */
	private void handleException(java.lang.Throwable exception) {

		/* Uncomment the following lines to print uncaught exceptions to stdout */
		// System.out.println("--------- UNCAUGHT EXCEPTION ---------");
		// exception.printStackTrace(System.out);
	}

	/**
	 * Initializes connections
	 * 
	 * @exception java.lang.Exception
	 *                The exception description.
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private void initConnections() throws java.lang.Exception {
		// user code begin {1}
		// user code end
		getOKButton().addActionListener(this);
		getCancelButton().addActionListener(this);

		connPtoP4SetTarget();
		// Executing the set<type>Editor method of the fieldIndexChooser
		// object an editor panel of each type is created
		getFieldIndexChooser().setIntEditor(getIntegerEditor());
		getFieldIndexChooser().setDateEditor(getDateEditor());
		getFieldIndexChooser().setStringEditor(getStringEditor());
		getFieldIndexChooser().setTypeEditor(getTypeEditor());
	}

	/**
	 * Initialize the class.
	 */
	/* WARNING: THIS METHOD WILL BE REGENERATED. */
	private void initialize() {
		try {
			// user code begin {1}
			// user code end
			setName("FilterParameterDialog");
			setDefaultCloseOperation(DISPOSE_ON_CLOSE);
			setSize(324, 513);
			setTitle("Filter properties");
			setContentPane(getJModalDialogContentPane());
			initConnections();
		} catch (java.lang.Throwable ivjExc) {
			handleException(ivjExc);
		}
		// user code begin {2}
		// user code end
	}

	/**
	 * Check if the filter is valid.
	 * 
	 * @return <code>true</code> if the form is well formed
	 */
	private boolean checkFilter() {
		getErrorLabel().setText("");
		// If the following line throws exception,
		// the specified parameters are incorrect.
		try {
			Filter f = getFieldIndexChooser().getFilter();
		} catch (Exception e) {
			// The filter is invalid: an error is shown
			JOptionPane.showMessageDialog(this, e.getMessage(),
					"Error editing the filter", JOptionPane.ERROR_MESSAGE);
			return false;// Force the user to create well defined filters before
					// accepting
		}
		// The filter is ok
		return true;
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 2:13:29 PM)
	 * 
	 * @param f
	 *            com.cosylab.logging.engine.Filter
	 */
	public void setFilter(Filter f) {
		getFieldIndexChooser().setFilter(f);
	}
	
	/**
	 * Override <code>JDialog.setVisible</code> to show this dialog over
	 * the <code>LogsingClient</code> component.
	 */
	@Override
	public void setVisible(boolean visible) {
		setLocationRelativeTo(ownerDialog);
		pack();
		super.setVisible(visible);
		toFront();
	}
	
	/**
	 * 
	 * @return <code>true</code> if the user pressed Ok.
	 */
	public boolean okPressed() {
		return okPressed;
	}
	
	/**
	 * @see {@link ActionListener}
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource() == FilterParameterDialog.this.getOKButton()) {
			if (checkFilter()) {
				okPressed=true;
				setVisible(false);
				dispose();
			}
		} else if (e.getSource() == FilterParameterDialog.this.getCancelButton()) {
			okPressed=false;
			setVisible(false);
			dispose();
		}
	};
}
