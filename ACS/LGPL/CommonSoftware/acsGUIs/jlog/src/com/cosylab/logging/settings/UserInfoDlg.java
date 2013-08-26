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

import javax.swing.JDialog;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;
import javax.swing.JOptionPane;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import com.cosylab.logging.LoggingClient;

/**
 * The class to add info (<name, value> to a log
 * 
 * @author acaproni
 *
 */
public class UserInfoDlg extends JDialog implements ActionListener {
	/**
	 * The text field where the user inserts the name (key)
	 */
	private JTextField nameTF = new JTextField(20);
	
	private JTextArea valueTA = new JTextArea(10,20);
	
	// The OK button
	private JButton okB = new JButton("Ok");
	
	// The cancel button
	private JButton cancelB = new JButton("Cancel");
	
	// true if the user pressed the OK button 
	private boolean exitOk=false;

	public UserInfoDlg() {
		super();
		setTitle("Add info");
		setModal(true);
		initGUI();
		setVisible(true);
	}
	
	/**
	 * Init the GUI
	 */
	private void initGUI() {
		// Add the Ok/Cancel buttons in the bottom
		okB.addActionListener(this);
		cancelB.addActionListener(this);
		JPanel buttonPnl = new JPanel(new BorderLayout());
		buttonPnl.add(okB,BorderLayout.WEST);
		buttonPnl.add(cancelB,BorderLayout.EAST);
		add(buttonPnl,BorderLayout.SOUTH);
		
		// Add the value field in the center
		JPanel valuePnl = new JPanel(new BorderLayout());
		valuePnl.add(new JLabel("Info "),BorderLayout.WEST);
		JScrollPane scrollP = new JScrollPane(valueTA);
		valuePnl.add(scrollP,BorderLayout.CENTER);
		add(valuePnl,BorderLayout.CENTER);
		
		// Add the text field for the key/name
		JPanel keyPnl = new JPanel(new BorderLayout());
		keyPnl.add(new JLabel("Name "),BorderLayout.WEST);
		keyPnl.add(nameTF,BorderLayout.CENTER);
		add(keyPnl,BorderLayout.NORTH);
		
		((BorderLayout)getContentPane().getLayout()).setVgap(10);
		// Move the window to the center of the screen
		pack();
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension windowSize = getSize();
        setLocation(Math.max(0,(screenSize.width -windowSize.width)/2), 
        Math.max(0,(screenSize.height-windowSize.height)/2));
	}
	
	/**
	 * @see java.awt.event.ActionListener;
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==okB) {
			if (nameTF.getText().length()==0 || valueTA.getText().length()==0) {
				JOptionPane.showMessageDialog(null,"Name and Info can't be empty!","Invalid operation",JOptionPane.INFORMATION_MESSAGE);
				return;
			}
			exitOk=true;
		}
		setVisible(false);
		dispose();
	}
	
	
	
	
	public boolean okPressed() {
		return exitOk;
	}
	
	/**
	 *  @return the name for the info to add
	 */
	public String getInfoName() {
		return nameTF.getText();
	}
	
	/**
	 *  @return the info to add
	 */
	public String getInfo() {
		return valueTA.getText();
	}
}
