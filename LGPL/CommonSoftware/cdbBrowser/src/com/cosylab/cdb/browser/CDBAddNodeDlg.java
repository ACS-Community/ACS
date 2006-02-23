/*******************************************************************************
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 */
/*
 * Created on Feb 15, 2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package com.cosylab.cdb.browser;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;

/**
 * @author dvitas
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class CDBAddNodeDlg extends JDialog implements ActionListener {
	private JTextField curlCtrl;
	private JLabel curlLabel;
	private JTextArea xmlDataCtrl;
	private JButton okBtn;
	private JButton cancelBtn;
	private boolean okButtonPressed = false;
	
	public CDBAddNodeDlg(Frame parent) {
		super(parent, true);
		initComponents();
	}
    
	private void initComponents() {
		GridBagConstraints gridBagConstraints;

		curlLabel = new JLabel();
		curlCtrl = new JTextField();
		xmlDataCtrl = new JTextArea();

		getContentPane().setLayout(new GridBagLayout());

		setTitle("Add new node");
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent evt) {
				closeDialog(evt);
			}
		});

		curlLabel.setText("Curl:");
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.insets = new Insets(11, 11, 11, 11);
		gridBagConstraints.anchor = GridBagConstraints.WEST;
		getContentPane().add(curlLabel, gridBagConstraints);

		curlCtrl.setBorder(new javax.swing.border.EtchedBorder());
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 1;
		gridBagConstraints.gridy = 0;
		gridBagConstraints.fill = GridBagConstraints.HORIZONTAL;
		gridBagConstraints.insets = new Insets(11, 11, 11, 11);
		gridBagConstraints.weightx = 1.0;
		getContentPane().add(curlCtrl, gridBagConstraints);

		xmlDataCtrl.setBorder(new EtchedBorder());
		xmlDataCtrl.setPreferredSize(new Dimension(400, 300));
		gridBagConstraints = new GridBagConstraints();
		gridBagConstraints.gridx = 0;
		gridBagConstraints.gridy = 1;
		gridBagConstraints.gridwidth = GridBagConstraints.REMAINDER;
		gridBagConstraints.gridheight = GridBagConstraints.RELATIVE;
		gridBagConstraints.fill = GridBagConstraints.BOTH;
		gridBagConstraints.insets = new Insets(11, 11, 11, 11);
		gridBagConstraints.anchor = GridBagConstraints.WEST;
		gridBagConstraints.weightx = 1.0;
		gridBagConstraints.weighty = 1.0;
		getContentPane().add(xmlDataCtrl, gridBagConstraints);

		JPanel buttonsPanel = new JPanel();
		buttonsPanel.setLayout(new FlowLayout(FlowLayout.RIGHT));

		okBtn = new JButton();
		cancelBtn = new JButton();
		
		okBtn.addActionListener(this);
		okBtn.setText("OK");
		buttonsPanel.add(okBtn);

		cancelBtn.addActionListener(this);
		cancelBtn.setText("Cancel");
		buttonsPanel.add(cancelBtn);

		gridBagConstraints = new java.awt.GridBagConstraints();
		gridBagConstraints.gridwidth = 2;
		gridBagConstraints.fill = java.awt.GridBagConstraints.HORIZONTAL;
		getContentPane().add(buttonsPanel, gridBagConstraints);

		setBounds(70,70,70,70);
		pack();
	}
    
	/** Closes the dialog */
	private void closeDialog(WindowEvent evt) {
		setVisible(false);
		dispose();
	}

	public void actionPerformed(ActionEvent ae)
	{
		if (ae.getSource() == okBtn) {
			okButtonPressed = true;
		}
		closeDialog(null);
	}
    
	/**
	 * @param args the command line arguments
	 */
	public static void main(String args[]) {
		CDBAddNodeDlg dlg = new CDBAddNodeDlg(new JFrame());
		dlg.setLocationRelativeTo(null);
		dlg.show();
	}

	/**
	 * @return
	 */
	public int showModal() {
		show(); // we are already modal
		if(okButtonPressed)
			return JOptionPane.OK_OPTION;
		return JOptionPane.CANCEL_OPTION;
	}

	/**
	 * @param curl
	 */
	public void setCurl(String curl) {
		curlCtrl.setText(curl);
	}

	/**
	 * @param xml
	 */
	public void setXML(String xml) {
		xmlDataCtrl.setText(xml);
	}

	/**
	 * @return
	 */
	public String getCurl() {
		return curlCtrl.getText();
	}

	/**
	 * @return
	 */
	public String getXML() {
		return xmlDataCtrl.getText();
	}
    
}
