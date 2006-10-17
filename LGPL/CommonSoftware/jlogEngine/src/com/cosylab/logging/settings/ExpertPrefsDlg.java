package com.cosylab.logging.settings;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JRootPane;

/**
 * A dialog to setup the preferences
 * 
 * @author acaproni
 *
 */
public class ExpertPrefsDlg extends JDialog implements ActionListener {
	
	private JButton okBtn;
	private JButton cancelBtn;
	
	/**
	 * Constructor
	 * 
	 * @param owner The owner of the dialog
	 * @param title The title
	 * @param modal Modal type
	 */
	public ExpertPrefsDlg(Frame owner, String title, boolean modal) {
		super(owner, title, modal);
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
		initGUI();
		pack();
	}
	
	/**
	 * @see java.awt.event.ActionListener
	 * @see java.awt.event.ActionEvent
	 */
	public void actionPerformed(ActionEvent e) {
	}
	
	/**
	 * Builds the GUI
	 *
	 */
	private void initGUI() {
		JRootPane mainPnl = this.getRootPane();
		mainPnl.setLayout(new BorderLayout());
		
		// Add the OK, CANCEL buttons
		JPanel btnPnl = new JPanel(new BorderLayout());
		okBtn = new JButton("Ok");
		okBtn.addActionListener(this);
		cancelBtn = new JButton("Cancel");
		cancelBtn.addActionListener(this);
		btnPnl.add(okBtn,BorderLayout.WEST);
		btnPnl.add(cancelBtn,BorderLayout.EAST);
		mainPnl.add(btnPnl,BorderLayout.SOUTH);
	}
}
