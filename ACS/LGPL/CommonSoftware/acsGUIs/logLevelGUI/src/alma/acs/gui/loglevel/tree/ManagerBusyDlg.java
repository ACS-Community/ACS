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
package alma.acs.gui.loglevel.tree;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

/**
 * The dialog is shown when tha manager is busy
 * (CORBA TRANSIENT exception)
 * 
 * @author acaproni
 *
 */
public class ManagerBusyDlg extends JDialog implements ActionListener {

	private static final long serialVersionUID = 234195167449794360L;

	// The AdministratorClient
	private AdministratorClient adminClient;
	
	// When the user presses this button the client is stopped
	// and the dialog disposed
	private JButton stopBtn = new JButton("Interrupt");
	
	private JLabel msgLbl = new JLabel();
	
	/**
	 * Constructor
	 *
	 * @param msg The message to show to the user
	 */
	public ManagerBusyDlg(AdministratorClient client, String msg) {
		super();
		if (client==null) {
			throw new IllegalArgumentException("Inavlid null AdministratorClient in constructor");
		}
		if (msg==null) {
			throw new IllegalArgumentException("Inavlid null message in constructor");
		}
		adminClient=client;
		setTitle("Manager busy");
		initialize(msg);
	}
	
	private void initialize(String msg) {
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		
		msgLbl.setText(msg);
		JPanel msgPnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		msgPnl.add(msgLbl);
		add(msgPnl,BorderLayout.NORTH);
		
		JProgressBar bar = new JProgressBar();
		bar.setPreferredSize(new Dimension(150,25));
		add(bar,BorderLayout.CENTER);
		
		JPanel btnPnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		btnPnl.add(stopBtn);
		stopBtn.addActionListener(this);
		add(btnPnl,BorderLayout.SOUTH);
		setLocation(50,50);
		pack();
		setVisible(true);
		bar.setIndeterminate(true);
	}
	
	/**
	 * Update the message for the user
	 * 
	 * @param newMsg The new message to show
	 */
	public void updateMessage(String newMsg) {
		if (newMsg==null) {
			throw new IllegalArgumentException("Inavlid null message");
		}
		msgLbl.setText(newMsg);
		pack();
	}
	
	/**
	 * Signal this dialog that the manager is not anymore busy
	 *
	 *The dialog is disposed
	 */
	public synchronized void managerNotBusy() {
		setVisible(false);
		adminClient=null;
		dispose();
	}
	
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==stopBtn) {
			adminClient.interruptManagerBusy();
			setVisible(false);
			dispose();
		}
	}
}
