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

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;

import alma.acs.gui.util.threadsupport.EDTExecutor;
import alma.acs.gui.widgets.CheckList;

/**
 * Serves the purpose of selecting the right fields (Timestamp, File, Thread,
 * etc.) to be displayed in the table according to the user's preferences. Used
 * by LogEntryTable.
 * <p>
 * Creation date: (1/2/2002 22:53:33)
 * 
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class FieldChooserDialog extends JDialog {
	
	/**
	 * The list with the fields of the logs 
	 */
	private final CheckList fieldList = new CheckList();
	
	/**
	 * The OK button
	 */
	private final JButton buttonOK =  new JButton("OK");
	
	/**
	 * The button to cancel
	 */
	private JButton buttonCancel = new JButton("Cancel");

	//private JPanel contentPane = null;

	private Insets defaultInsets = new Insets(4, 4, 4, 4);
	private int modalResult = 0;

	// The dialog is positioned over this component when it is made visible
	// (or at the center of the screen if it is null)
	private Component displayHelperComponent = null;

	private class ButtonListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			if (e.getSource() == buttonOK) {
				modalResult = 1;
			} else {
				modalResult = 0;
			}
			FieldChooserDialog.this.setVisible(false);
		}
	}

	/**
	 * FieldChooserDialog constructor comment.
	 * 
	 * @param displayOverComponent
	 *            The component over which this dialog will be shown
	 */
	public FieldChooserDialog(Component displayOverComponent) {
		if (displayOverComponent == null) {
			throw new IllegalArgumentException(
					"Invalid null Component in constructor");
		}

		displayHelperComponent = displayOverComponent;
		EDTExecutor.instance().execute(new Runnable() {
			@Override
			public void run() {
				initGUI();
			}
		});
	}
	
	/**
	 * initialize the GUI
	 */
	private void initGUI() {
		setTitle("Field chooser");
		setModal(true);

		GridBagLayout gb = new GridBagLayout();
		JPanel contentPane = new JPanel();

		this.setContentPane(contentPane);
		contentPane.setLayout(gb);

		GridBagConstraints constraints = null;

		constraints = new GridBagConstraints();

		constraints.gridx = 0;
		constraints.gridy = 0;
		constraints.insets = defaultInsets;
		constraints.anchor = GridBagConstraints.WEST;
		constraints.gridheight = 1;
		constraints.gridwidth = 2;
		contentPane.add(new JLabel("Select fields to display"), constraints);

		constraints.gridx = 0;
		constraints.gridy = 1;
		constraints.gridheight = 1;
		constraints.insets = defaultInsets;
		constraints.gridwidth = 2;
		constraints.weightx = 1.0;
		constraints.weighty = 1.0;
		constraints.fill = GridBagConstraints.BOTH;
		contentPane.add(new JScrollPane(fieldList), constraints);

		ButtonListener buttonListener = new ButtonListener();

		constraints = new GridBagConstraints();
		constraints.gridx = 0;
		constraints.gridy = 2;
		constraints.insets = defaultInsets;
		constraints.weightx = 1.0;
		constraints.gridheight = 1;
		constraints.gridwidth = 1;

		buttonOK.setHorizontalAlignment(SwingConstants.HORIZONTAL);
		buttonOK.addActionListener(buttonListener);
		contentPane.add(buttonOK, constraints);

		constraints = new GridBagConstraints();
		constraints.gridx = 1;
		constraints.insets = defaultInsets;
		constraints.gridy = 2;
		constraints.weightx = 1.0;
		constraints.gridheight = 1;
		constraints.gridwidth = 1;
		buttonCancel.setHorizontalAlignment(SwingConstants.HORIZONTAL);
		buttonCancel.addActionListener(buttonListener);
		contentPane.add(buttonCancel, constraints);
	}

	/**
	 * @return The activation states (check of unchecked) from the widget
	 */
	public boolean[] getFields() {
		return fieldList.getActivationStates();
	}
	
	/**
	 * Insert the method's description here.
	 * <p>
	 * Creation date: (1/2/2002 23:26:28)
	 * 
	 * @return int
	 */
	public int getModalResult() {
		return modalResult;
	}

	/**
	 * Insert the method's description here.
	 * <p>
	 * Creation date: (1/2/2002 23:34:08)
	 * 
	 * @param fieldNames
	 *            java.lang.String[]
	 * @param checked
	 *            boolean[]
	 */
	public void setupFields(String[] fieldNames, boolean[] checked) {
		fieldList.clear();
		int n = fieldNames.length;
		for (int i = 0; i < n; i++) {
			fieldList.addElement(checked[i], fieldNames[i]);
		}
	}

	public void setVisible(boolean visible) {
		setLocationRelativeTo(displayHelperComponent);
		pack();
		super.setVisible(visible);
		toFront();
	}
}
