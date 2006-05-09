package com.cosylab.logging.client;

import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JDialog;
import javax.swing.JScrollPane;
import javax.swing.JComponent;
import javax.swing.JCheckBox;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import com.cosylab.logging.LoggingClient;

import java.io.File;

/**
 * A FileChooser customized to show hidden files
 * 
 * @author acaproni
 *
 */
public class CustomFileChooser extends JDialog implements ActionListener {
	
	private JFileChooser fc;
	
	private CustomFileChooser(File currentDir,String title) {
		super(LoggingClient.getInstance(),title,true);
		initialize(currentDir);
		pack();
		setVisible(true);
	}
	
	private JCheckBox viewHiddenFiles = new JCheckBox("Show hidden files",false);
	
	/**
	 * Initialize the GUI
	 */
	private void initialize(File curDir) {
		rootPane.setLayout(new BorderLayout());
		
		viewHiddenFiles.addActionListener(this);
		rootPane.add(viewHiddenFiles,BorderLayout.NORTH);
		
		fc = new JFileChooser(curDir);
		fc.setFileHidingEnabled(!viewHiddenFiles.isSelected());
		fc.setFileSelectionMode(JFileChooser.FILES_ONLY);
		JComponent fcComponent = fc;
		fcComponent.setOpaque(true);
		rootPane.add(fcComponent,BorderLayout.CENTER);
	}
	
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource()==viewHiddenFiles) {
			fc.setFileHidingEnabled(!viewHiddenFiles.isSelected());
			fc.rescanCurrentDirectory();
		}
	}
}
