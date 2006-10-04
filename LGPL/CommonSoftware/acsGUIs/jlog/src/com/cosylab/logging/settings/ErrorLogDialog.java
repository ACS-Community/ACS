package com.cosylab.logging.settings;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
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

/**
 * The dialog to show the errors.
 * The dialog store till maxLength chars. If the log exceeds that number,
 * it is automatically flushed in ACS_TMP.
 * 
 * The dimension defaults to ERROR_LOG_DEFAULT_SIZE and can be changed seeting
 * the ERRORLOG_SIZE_PROP_NAME property.
 * 
 * The dialog is instantiated only one. Its content can be saved and cleared.
 * 
 * The dimension of the file on disk can grows too much.
 * 
 * @author acaproni
 *
 */
public class ErrorLogDialog extends JDialog implements ActionListener {
	
	// The log text are appears into a scroll pane
	private JScrollPane logSP;
	private JTextArea logTA;
	
	// The button to close (hide) the dialog
	private JButton closeBtn;
	
	// The button to erase all the logs (i.e. the logs shown in the window and those
	// flushed on the temporary file
	private JButton cleanAllBtn;
	private ImageIcon cleanIcon=new ImageIcon(ErrorLogDialog.class.getResource("/erase.gif"));
	
	// The button to save all the logs in a file (it saves the content of
	// the temporary log file on disk and the content of the text area)
	// The button is eabled only if some log has been flushed on disk
	private JButton saveAllBtn;
	private ImageIcon saveIcon=new ImageIcon(ErrorLogDialog.class.getResource("/save.gif"));

	
	// The toolbar
	private JToolBar toolBar;
	
	// Tne content of the text area
	private StringBuilder content = new StringBuilder("");
	
	// The maximum dimension of the log to keep in memory
	// Its value is read from a property.
	// If the value is 0, the error log is unlimited
	private long maxLength;
	
	// The name of the property definig the size of the error log
	private final String ERRORLOG_SIZE_PROP_NAME = "jlog.errorlog.size";
	
	// The default maximum size of the ERROR LOG
	private final long ERROR_LOG_DEFAULT_SIZE = 50 *1000000;
	
	// The file to flush the logs
	// it is created only if there is a log to flush
	private File tmpFile = null;
	private FileOutputStream outF=null;
	
	private static ErrorLogDialog instance =null;

	/**
	 * Constructor
	 * 
	 * @param owner The owner of the dialog
	 * @param title The title
	 * @param modal Modal type
	 */
	private ErrorLogDialog(Frame owner, String title, boolean modal) {
		super(owner, title, modal);
		this.setDefaultCloseOperation(HIDE_ON_CLOSE);
		initGUI();
		pack();
		maxLength = Long.getLong(ERRORLOG_SIZE_PROP_NAME, ERROR_LOG_DEFAULT_SIZE);
		System.out.println("Max length of in-memory error log "+maxLength);
	}
	
	/**
	 * Builds the content of the GUI
	 *
	 */
	private void initGUI() {
		logTA = new JTextArea("", 20, 60);
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
		cleanAllBtn = new JButton("<HTML><Font size=\"-1\">Clean all</FONT></HTML>",cleanIcon);
		cleanAllBtn.addActionListener(this);
		cleanAllBtn.setToolTipText("Delete the log in the window and those flushed on disk");
		saveAllBtn = new JButton("<HTML><Font size=\"-1\">Save all</FONT></HTML>",saveIcon);
		saveAllBtn.addActionListener(this);
		saveAllBtn.setToolTipText("Save the log in the window and those flushed on disk");
		
		toolBarPanel.add(cleanAllBtn);
		toolBarPanel.add(saveAllBtn);
		
		toolBar.add(toolBarPanel);
		mainPnl.add(toolBar, BorderLayout.PAGE_START);
		
		rationalizeButtons();
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
			// Check if the content of the log is too big
			if (maxLength>0 && content.length()>maxLength) {
				// The string to flush in the file
				String strToFlush = content.substring(0,(int)(content.length()-maxLength));
				// Cut the text in the log
				content.delete(0,(int)(content.length()-maxLength));
				// Flush the string on disk
				flush(strToFlush);
			}
		}
		logTA.setText(content.toString());
		setVisible(show);
		rationalizeButtons();
	}
	
	
	/**
	 * Clear the log in the windows and the log in the file
	 *
	 */
	public synchronized void clearAll() {
		synchronized (content) {
			if (content.length()>0) {
				content.delete(0, content.length());
			}
		}
		logTA.setText(content.toString());
		try {
			if (outF!=null) {
				outF.close();
			}
		} catch (IOException ioe) {
			System.out.println("Exception deleting temporary file: "+ioe.getMessage());
			ioe.printStackTrace();
		}
		outF=null;
		tmpFile=null;
		rationalizeButtons();
	}
	
	/**
	 * @see java.awt.event.ActionListener
	 * @see java.awt.event.ActionEvent
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			setVisible(false);
		} else if (e.getSource()==saveAllBtn) {
			saveAllLog();
		} else if (e.getSource()==cleanAllBtn) {
			clearAll();
		} else {
			System.err.println("Action unknown");
		}
	}
	
	/**
	 * Save the content of the temporary file and the content of the text area 
	 * into a file
	 *
	 */
	private void saveAllLog() {
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
	    
	    if (tmpFile!=null) {
		    FileInputStream inF =null;
		    try {
		    	inF= new FileInputStream(tmpFile);
		    } catch (IOException ioe) {
		    	System.out.println("Error opening the log on disk: "+ioe.getMessage());
		    	JOptionPane.showMessageDialog(this, "Error opening the input file", "Error saving the log", JOptionPane.ERROR_MESSAGE);
		    	ioe.printStackTrace();
		    	return;
		    }
		    
		    // Copy the content of the temporary file
		    byte[] buffer = new byte[512];
		    
	    	int bytesRead;
	    	do {
	    		try {
	    			bytesRead=inF.read(buffer);
	    		}  catch (IOException ioe) {
	    	    	System.out.println("Error reading from the log on disk: "+ioe.getMessage());
	    	    	JOptionPane.showMessageDialog(this, "Error reading log from disk", "Error saving the log", JOptionPane.ERROR_MESSAGE);
	    	    	ioe.printStackTrace();
	    	    	return;
	    	    }
	    		if (bytesRead>0) {
	    			try {
	    				bufOutStream.write(buffer, 0, bytesRead);
	    			}  catch (IOException ioe) {
	    		    	System.out.println("Error writing on file: "+ioe.getMessage());
	    		    	JOptionPane.showMessageDialog(this, "Error writing on file", "Error saving the log", JOptionPane.ERROR_MESSAGE);
	    		    	ioe.printStackTrace();
	    		    	return;
	    		    }
	    		}
	    	} while (bytesRead>=0);
	    }
	    
	    // Write the content of the window
	    synchronized (content) {
	    	try {
	    		bufOutStream.write(content.toString().getBytes());
	    	} catch (IOException ioe) {
	    		JOptionPane.showMessageDialog(this, "Error writing on file", "Error saving the log", JOptionPane.ERROR_MESSAGE);
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
	
	/**
	 * Write the given string on the file
	 * 
	 * @param str The string to flush
	 */
	private void flush(String str) {
		if (outF==null) {
			try {
				initTmpFile();
			} catch (IOException ioe) {
				System.out.println("Error creating the temporay log file: "+ioe.getMessage());
				ioe.printStackTrace();
				return;
			}
		}
		try {
			outF.write(str.getBytes());
		} catch (IOException ioe) {
			System.out.println("Error writing to the temporay log file: "+ioe.getMessage());
			ioe.printStackTrace();
		}
	}
	
	/**
	 * Create the temporary file for flushing the log
	 * 
	 * @return The newly create file
	 * 
	 * @throws IOException 
	 */
	private void initTmpFile() throws IOException {
		// Get a name for the file
		String tmpDir = System.getProperty("ACS.tmp");
		if (tmpDir==null || tmpDir.length()==0) {
			tmpDir = ".";
		}
		File dir = new File(tmpDir);
		tmpFile = File.createTempFile("jlog.", ".error.log", dir);
		outF = new FileOutputStream(tmpFile);
	}
	
	/**
	 * Flush and close the file when the object is destroyed by the GC
	 */
	protected void finalize() throws Throwable {
		if (outF!=null) {
			outF.flush();
			outF.close();
			outF=null;
		}
		super.finalize();
	}
	
	/**
	 * Enable/Disable the buttons in the toolbar depending on the
	 * content of the text area and the existence of the temporary
	 * file
	 */
	private void rationalizeButtons() {
		boolean b;
		synchronized (content) {
			b = content.length()>0;
		}
		
		saveAllBtn.setEnabled(b);
		cleanAllBtn.setEnabled(b);
	}
	
	/**
	 * 
	 * @return A reference to the Error log dialog
	 */
	public synchronized static ErrorLogDialog getErrorLogDlg() {
		if (instance==null) {
			instance = new ErrorLogDialog(null,"Error log",false);
		}
		return instance;
	}
	
}
