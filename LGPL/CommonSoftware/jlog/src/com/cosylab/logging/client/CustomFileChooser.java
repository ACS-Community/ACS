package com.cosylab.logging.client;

import javax.swing.JFileChooser;
import javax.swing.JDialog;
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
	
	/**
	 * The file selected by the user
	 * It is null if the file has not been selected or the user pressed cancel
	 */
	private File selectedFile=null;
	
	/**
	 * The JFileChooser shown in the center of  the main panel
	 */
	private JFileChooser fc;
	
	/**
	 * The checkbox to make visible hidden files and folfers
	 *The default is false (i.e. hidden folders are not shown)
	 */
	private JCheckBox viewHiddenFiles = new JCheckBox("Show hidden files",false);
	
	/**
	 * Constructor: build and show the modal dialog
	 *  
	 * @param currentDir The dir whose content is shown at startup
	 * @param title The title of the window (it appears also in the button)
	 */
	public CustomFileChooser(File currentDir,String title) {
		super(LoggingClient.getInstance(),title,true);
		initialize(currentDir);
		pack();
		setVisible(true);
	}
	
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
		fc.addActionListener(this);
		JComponent fcComponent = fc;
		fcComponent.setOpaque(true);
		rootPane.add(fcComponent,BorderLayout.CENTER);
	}
	
	/**
	 * 
	 * @return The file selected by the user or null if the user pressed Cancel
	 */
	public File getSelectedFile() {
		return selectedFile;
	}
	
	/**
	 * @return The current directory
	 */
	public File getCurrentDirectory() {
		return fc.getCurrentDirectory();
	}
	
	/**
	 * @see java.awt.event.ActionListener
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource()==viewHiddenFiles) {
			fc.setFileHidingEnabled(!viewHiddenFiles.isSelected());
			fc.rescanCurrentDirectory();
		} else {
			if (evt.getActionCommand().equals("ApproveSelection")) {
				selectedFile=fc.getSelectedFile();
				setVisible(false);
				dispose();
			} else if (evt.getActionCommand().equals("CancelSelection")) {
				selectedFile=null;
				setVisible(false);
				dispose();
			}
		}
	}
}
