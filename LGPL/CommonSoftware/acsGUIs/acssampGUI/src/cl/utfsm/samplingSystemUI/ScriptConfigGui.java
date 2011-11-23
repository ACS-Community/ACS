/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package cl.utfsm.samplingSystemUI;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;



//VS4E -- DO NOT REMOVE THIS LINE!
public class ScriptConfigGui extends JDialog implements WindowListener {

	private static final long serialVersionUID = 1L;
	private JTextField LocationField;
	private JTextField ArgsField;
	private JButton BrowseButton;
	private JLabel LocationLabel;
	private JLabel ArgsLabel;
	private JButton CancelButton;
	private JButton AccepButton;
	private ScriptExecutor scriptExec;

	public ScriptExecutor getScriptExec() {
		return scriptExec;
	}

	public ScriptConfigGui(ScriptExecutor script) {
		scriptExec = script;
		this.setModal(true);
		this.setResizable(false);
		this.setTitle("Select script to run");
		initComponents();
	}

	private void initComponents() {
		
		this.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		c.insets = new Insets(5,5,5,5);
		c.anchor = GridBagConstraints.WEST;
		c.weighty = 0;
		c.weightx = 0;
		c.gridwidth = 1;
		
		// First Row
		c.gridy = 0; c.gridx = 0;
		c.anchor = GridBagConstraints.EAST;
		this.add(getLocationLabel(),c);
		c.gridx = 1;
		c.gridwidth = 2;
		c.anchor = GridBagConstraints.WEST;
		c.ipadx = 170;
		this.add(getLocationField(),c);
		c.gridwidth = 1;
		c.ipadx = 0;
		c.gridx = 3;
		this.add(getBrowseButton(),c);
		
		// Second Row
		c.gridy = 1; c.gridx = 0;
		
		c.anchor = GridBagConstraints.EAST;
		this.add(getArgsLabel(),c);
		c.gridx = 1;
		c.gridwidth = 2;
		c.anchor = GridBagConstraints.WEST;
		c.ipadx = 170;
		this.add(getArgsField(),c);
		c.ipadx = 0;
		c.gridwidth = 1;

		// Third Row
		c.gridy=2; c.gridx = 0;
		c.anchor = GridBagConstraints.WEST;
		this.add(getAcceptButton(),c);
		c.gridx = 3;
		c.anchor = GridBagConstraints.EAST;
		this.add(getCancelButton(),c);
		
		this.pack();
		this.setVisible(true);
	}
	
	private void closePanel(){
		this.setVisible(false);
	}

	private JButton getAcceptButton() {
		if (AccepButton == null) {
			AccepButton = new JButton();
			AccepButton.setText("Accept");
			AccepButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					if( getLocationField().getText().equals("") ){
						JOptionPane.showMessageDialog(
								null, 
								"Please select a script to run.\n" +
								"If you wish to abort, use the Cancel button", 
								"Please select a script", JOptionPane.WARNING_MESSAGE);
					}else{
						scriptExec = new ScriptExecutor( getLocationField().getText(), getArgsField().getText() );
					    closePanel();					
					}
					
				}
			});
		}
		return AccepButton;
	}

	private JButton getCancelButton() {
		if (CancelButton == null) {
			CancelButton = new JButton();
			CancelButton.setText("Cancel");
			CancelButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
				    closePanel();
				}
			});
		}
		return CancelButton;
	}

	private JLabel getArgsLabel() {
		if (ArgsLabel == null) {
			ArgsLabel = new JLabel();
			ArgsLabel.setText("Script Arguments:");
			ArgsLabel.setHorizontalAlignment(JLabel.RIGHT);
		}
		return ArgsLabel;
	}

	private JLabel getLocationLabel() {
		if (LocationLabel == null) {
			LocationLabel = new JLabel();
			LocationLabel.setText("Script Location:");
			LocationLabel.setHorizontalAlignment(JLabel.RIGHT);
		}
		return LocationLabel;
	}

	private JButton getBrowseButton() {
		if (BrowseButton == null) {
			BrowseButton = new JButton();
			BrowseButton.setIcon( new ImageIcon(getClass().getClassLoader().getResource("cl/utfsm/samplingSystemUI/img/fileopen.png")) );
			BrowseButton.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
				    JFileChooser chooser = new JFileChooser();
				    ScriptFilter filter = new ScriptFilter();
				    chooser.setFileFilter(filter);
				    int returnVal = chooser.showOpenDialog(ScriptConfigGui.this);
				    if(returnVal == JFileChooser.APPROVE_OPTION) {
				    	getLocationField().setText(chooser.getSelectedFile().getAbsolutePath());
				    }
				}
			});
		}
		return BrowseButton;
	}


	private JTextField getArgsField() {
		if (ArgsField == null) {
			ArgsField = new JTextField();
		}
		return ArgsField;
	}

	private JTextField getLocationField() {
		if (LocationField == null) {
			LocationField = new JTextField();
			LocationField.setText("");
		}
		return LocationField;
	}

	@Override
	public void windowActivated(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowClosed(WindowEvent e) {
		// TODO Auto-generated method stub
		
		
	}

	@Override
	public void windowClosing(WindowEvent e) {
		closePanel();
	}

	@Override
	public void windowDeactivated(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowDeiconified(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowIconified(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowOpened(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

}

