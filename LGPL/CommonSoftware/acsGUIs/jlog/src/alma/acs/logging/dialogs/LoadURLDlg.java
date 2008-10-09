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
package alma.acs.logging.dialogs;

import java.awt.BorderLayout;
import java.awt.Dialog;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JTextField;

import alma.acs.logging.io.LoadSwitchesPanel;

import com.cosylab.logging.LoggingClient;

/**
 * The dialog to get the URL to load
 * 
 * @author acaproni
 *
 */
public class LoadURLDlg extends JDialog implements ActionListener {
	// The URL
	private JTextField urlTF = new JTextField(40);
	
	// The switches to clear the table and disconnect from the NC 
	// before submitting a query
	private LoadSwitchesPanel guiSwitches;
	
	// The load/cancel buttons
	private JButton loadBtn = new JButton("Load URL");
	private JButton cancelBtn = new JButton("Cancel");
	
	// The URL to return to the caller (it is null
	// until the user presses Load)
	private URL url=null;
	
	// The logging client
	private LoggingClient loggingClient=null;
	
	/** 
	 * Constructor
	 * 
	 * @param initialValue The initial value of the URL
	 */
	public LoadURLDlg(String initialValue, LoggingClient client) {
		super();
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		loggingClient=client;
		setTitle("Load from URL");
		setModal(true);
		initGUI();
		urlTF.setText(initialValue);
		//setBounds(50,50,50,50);
		pack();
	}
	
	/**
	 * Setup the GUI
	 */
	private void initGUI() {
		JRootPane mainPnl = this.getRootPane();
		mainPnl.setLayout(new BorderLayout());
		
		guiSwitches = new LoadSwitchesPanel(loggingClient);
		mainPnl.add(guiSwitches,BorderLayout.NORTH);
		
		JPanel urlPnl = new JPanel(new FlowLayout());
		urlPnl.add(new JLabel("URL of the file: "));
		urlPnl.add(urlTF);
		mainPnl.add(urlPnl,BorderLayout.CENTER);
		
		JPanel btnPnl = new JPanel(new BorderLayout());
		loadBtn.addActionListener(this);
		cancelBtn.addActionListener(this);
		btnPnl.add(loadBtn,BorderLayout.WEST);
		btnPnl.add(cancelBtn,BorderLayout.EAST);
		mainPnl.add(btnPnl,BorderLayout.SOUTH);
	}
	
	/**
	 * Return the URL inserted by the user
	 *  
	 * @return the URL inserted by the user if the user pressed the load button
	 *         null if the user pressed cancel 
	 */
	public URL getURL() {
		return url;
	}
	
	/**
	 * @see ActionListener
	 * 
	 * @param e
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==loadBtn) {
			// Build the URL (informing the user if the URL is malformed)
			try {
				url = new URL(urlTF.getText());
			} catch (MalformedURLException me) {
				JOptionPane.showMessageDialog(this,me.getMessage(),"Malformed URL",JOptionPane.ERROR_MESSAGE);
				return;
			}
			setVisible(false);
			dispose();
		} else if (e.getSource()==cancelBtn) {
			setVisible(false);
			dispose();
		} else {
			System.err.println("Unknown source: "+e.getSource());
		}
	}
}
