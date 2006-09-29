package com.cosylab.logging.settings;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;

public class ErrorLogDialog extends JDialog implements ActionListener {
	
	// The log text are appears into a scroll pane
	private JScrollPane logSP;
	private JTextArea logTA;
	
	// The button to close (hide) the dialog
	private JButton closeBtn;
	
	// The button to erase the content of the text area
	private JButton cleanBtn;
	private ImageIcon cleanIcon=new ImageIcon(ErrorLogDialog.class.getResource("/erase.gif"));
	
	// The button to save the content of the test area into a text file
	private JButton saveBtn;
	private ImageIcon saveIcon=new ImageIcon(ErrorLogDialog.class.getResource("/save.gif"));
	
	// The toolbar
	private JToolBar toolBar;
	
	private StringBuilder content = new StringBuilder("");

	/**
	 * Constructor
	 * 
	 * @param owner The owner of the dialog
	 * @param title The title
	 * @param modal Modal type
	 */
	public ErrorLogDialog(Frame owner, String title, boolean modal) {
		super(owner, title, modal);
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
		initGUI();
		pack();
	}
	
	/**
	 * Builds the content of the GUI
	 *
	 */
	private void initGUI() {
		logTA = new JTextArea("", 25, 85);
		logTA.setEditable(false);
		logSP = new JScrollPane(logTA);
		
		JRootPane mainPnl = this.getRootPane();
		mainPnl.setLayout(new BorderLayout());
		
		mainPnl.add(logSP,BorderLayout.CENTER);
		
		JPanel btnPnl = new JPanel(new FlowLayout());
		closeBtn = new JButton("Close");
		closeBtn.addActionListener(this);
		btnPnl.add(closeBtn);
		mainPnl.add(btnPnl,BorderLayout.SOUTH);
		
		// Build the toolbar
		toolBar = new JToolBar();
		JPanel toolBarPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		cleanBtn = new JButton("<HTML><Font size=\"-1\">Clean</FONT></HTML>",cleanIcon);
		cleanBtn.addActionListener(this);
		saveBtn = new JButton("<HTML><Font size=\"-1\">Save</FONT></HTML>",saveIcon);
		saveBtn.addActionListener(this);
		toolBarPanel.add(cleanBtn);
		toolBarPanel.add(saveBtn);
		toolBar.add(toolBarPanel);
		mainPnl.add(toolBar, BorderLayout.PAGE_START);
	}
	
	/** 
	 * Add a String to the error log and ensure the dialog is
	 * visible
	 * 
	 * @param str The string to append in the TextArea
	 */
	public synchronized void appendText(String str) {
		appendText(str,true);
	}
	
	/**
	 * Add a String to the error log and show/hide the dialog
	 * 
	 * @param str The string to append in the TextArea
	 * @param show Show/hide the dialog
	 */
	public synchronized void appendText(String str, boolean show) {
		synchronized(content) {
			content.append(str);
		}
		logTA.setText(content.toString());
		setVisible(show);
	}
	
	public synchronized void clear() {
		synchronized (content) {
			if (content.length()>0) {
				content.delete(0, content.length()-1);
			}
		}
		logTA.setText(content.toString());
	}
	
	/**
	 * @see java.awt.event.ActionListener
	 * @see java.awt.event.ActionEvent
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			setVisible(false);
		} else if (e.getSource()==saveBtn) {
			saveLog();
		} else if (e.getSource()==cleanBtn) {
			clear();
		} else {
			System.err.println("Action unknown");
		}
	}
	
	/**
	 * Save the log on a text file
	 *
	 */
	private void saveLog() {
		File f=null;
		JFileChooser chooser = new JFileChooser();
		chooser.setMultiSelectionEnabled(false);
		chooser.setDialogTitle("Save error log");
		int returnVal = chooser.showOpenDialog(this);
	    if(returnVal == JFileChooser.APPROVE_OPTION) {
	    	f = chooser.getSelectedFile();
	    } else {
	    	return;
	    }
	    FileOutputStream fStream=null;
	    try {
	    	fStream = new FileOutputStream(f);
	    } catch (FileNotFoundException fnfe) {
	    	JOptionPane.showMessageDialog(this, "Error opening the file", "Error saving the log", JOptionPane.ERROR_MESSAGE);
	    	System.out.println(fnfe.getMessage());
    		fnfe.printStackTrace();
	    	return;
	    }
	    BufferedOutputStream bufOutStream = new BufferedOutputStream(fStream);
	    synchronized (content) {
	    	try {
	    		bufOutStream.write(content.toString().getBytes());
	    	} catch (IOException ioe) {
	    		JOptionPane.showMessageDialog(this, "Error writing the file", "Error saving the log", JOptionPane.ERROR_MESSAGE);
	    		System.out.println(ioe.getMessage());
	    		ioe.printStackTrace();
	    		return;
	    	}
	    }
	    try {
	    	bufOutStream.flush();
	    	bufOutStream.close();
	    } catch (IOException ioe) {
    		JOptionPane.showMessageDialog(this, "Error closing the file", "Error saving the log", JOptionPane.ERROR_MESSAGE);
    		System.out.println(ioe.getMessage());
    		ioe.printStackTrace();
    		return;
    	}
	}
	
}
