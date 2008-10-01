/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.logging.archive.zoom;

import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRootPane;
import javax.swing.JSeparator;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

/**
 * The dialog to set preferences for zooming
 * 
 * @author acaproni
 *
 */
public class ZoomPrefsDlg extends JDialog implements ActionListener {
	
	/**
	 * The logging client
	 */
	private final LoggingClient loggingClient;
	
	/**
	 * The manager for zooming
	 */
	private final ZoomManager zoomManager;
	
	/**
	 * The combobox to choose  minimum log level while zooming (inclusive)
	 */
	private JComboBox minLevelCB;
	
	/**
	 * The combobox to choose maximum log level while zooming (inclusive)
	 */
	private JComboBox maxLevelCB;
	
	/**
	 * The path of the folder to show in the label. 
	 */
	private String folder;
	
	/**
	 * The label with the folder of XML files of logs.
	 * <P>
	 * The content of the label is built from the value of
	 * <code>folder</code>.
	 */
	private JLabel folderLbl = new JLabel();
	
	/**
	 * The button to choose the folder of XML files of logs
	 */
	private JButton folderBtn=new JButton("Change...");
	
	/**
	 * The OK button
	 */
	private JButton okBtn = new JButton("Ok");
	
	/**
	 * The Cancel button
	 */
	private JButton cancelBtn = new JButton("Cancel");
	
	/**
	 * Constructor
	 * 
	 * @param logCli The <code>LoggingClient</code>
	 * @param manager The manager for zooming
	 */
	public ZoomPrefsDlg(LoggingClient logCli,ZoomManager manager) {
		if (logCli==null) {
			throw new IllegalArgumentException("The LoggingClient can't be null");
		}
		if (manager==null) {
			throw new IllegalArgumentException("The manager can't be null");
		}
		loggingClient=logCli;
		zoomManager=manager;
		folder=zoomManager.getRepository();
		initialize();
		pack();
		setVisible(true);
	}
	
	/**
	 * Init the GUI
	 */
	private void initialize() {
		// Window setup
		setTitle("Drill down preferences");
		ImageIcon zoomIcon = new ImageIcon(LogTypeHelper.class.getResource("/zoom.png"));
		setIconImage(zoomIcon.getImage());	
		setModal(true);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		// Window content setup
		
		JPanel contentPanel = new JPanel();
		contentPanel.setLayout(new BoxLayout(contentPanel,BoxLayout.Y_AXIS));
		
		// The panel with levels
		JPanel levelsPnl = new JPanel();
		levelsPnl.setBorder(BorderFactory.createTitledBorder("Levels"));
		levelsPnl.setLayout(new BoxLayout(levelsPnl,BoxLayout.Y_AXIS));
		
		JPanel minPnl = new JPanel();
		minPnl.add(new JLabel("Min log level to load: "));
		minLevelCB=buildLogLevelCB(zoomManager.getMinLevel());
		minPnl.add(minLevelCB);
		levelsPnl.add(minPnl);
		
		JPanel maxPnl = new JPanel();
		maxPnl.add(new JLabel("Max log level to load: "));
		maxLevelCB=buildLogLevelCB(zoomManager.getMaxLevel());
		maxPnl.add(maxLevelCB);
		levelsPnl.add(maxPnl);
		
		contentPanel.add(levelsPnl);
		
		// The panel with the folder
		JPanel folderPnl = new JPanel(new BorderLayout());
		folderPnl.setBorder(BorderFactory.createTitledBorder("Folder of XML files"));
		folderPnl.add(folderBtn,BorderLayout.EAST);
		folderBtn.addActionListener(this);
		folderPnl.add(folderLbl,BorderLayout.CENTER);
		setupFolderLbl();
		
		contentPanel.add(folderPnl);
		
		// The Ok/Cancel buttons
		contentPanel.add(new JSeparator());
		JPanel buttonPnl = new JPanel(new BorderLayout());
		buttonPnl.add(cancelBtn,BorderLayout.WEST);
		cancelBtn.addActionListener(this);
		buttonPnl.add(okBtn,BorderLayout.EAST);
		okBtn.addActionListener(this);
		okBtn.setEnabled(folder!=null);
		contentPanel.add(buttonPnl);
		
		rootPane.setContentPane(contentPanel);
	}
	
	/**
     * Build a log level combobox.
     * 
     * @param initialLevel The initial log level
     * @return The log level CB
     */
    private JComboBox buildLogLevelCB(LogTypeHelper initialLevel) {
    	JComboBox temp;
    	// Add the ComboBox for the log level
		LogTypeHelper[] types = LogTypeHelper.values();
		int t=0;
		temp = new JComboBox(types);
        
        // Build the renderer for the combo boxes
        LogTypeRenderer rendererCB = new LogTypeRenderer();
        
        if (initialLevel!=null) {
        	temp.setSelectedItem(initialLevel);
        } else {
        	temp.setSelectedItem(LogTypeHelper.TRACE);
        }
        temp.setEditable(false);
        temp.setMaximumRowCount(LogTypeHelper.values().length);
        temp.setRenderer(rendererCB);
    	return temp;
    }

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==okBtn) {
			if (okPressed()) {
				setVisible(false);
				dispose();
			}
		} else if (e.getSource()==cancelBtn) {
			setVisible(false);
			dispose();
		} else if (e.getSource()==folderBtn) {
			JFileChooser fileChooser = new JFileChooser(folder);
			fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			int returnVal = fileChooser.showOpenDialog(this);
			if (returnVal==JFileChooser.APPROVE_OPTION) {
				File f = fileChooser.getSelectedFile();
				folder=f.getAbsolutePath();
				setupFolderLbl();
			}
			okBtn.setEnabled(folder!=null);
		} else {
			System.out.println("Unknown event: "+e.getSource());
		}
	}
	
	/**
	 * Read the values from the widgets and update
	 * the <code>ZoomManager</code>.
	 * 
	 * @return <code>true</code> if the values has been accepted
	 */
	private boolean okPressed() {
		try {
			zoomManager.setFilesRepository(folder);	
		} catch (ZoomException ze) {
			JOptionPane.showMessageDialog(this, "Invalid folder: "+ze.getMessage(), "Error selecting folder", JOptionPane.ERROR_MESSAGE);
			return false;
		}
		try {
			zoomManager.setLevels((LogTypeHelper)minLevelCB.getSelectedItem(), (LogTypeHelper)maxLevelCB.getSelectedItem());
		} catch (Exception e) {
			JOptionPane.showMessageDialog(this, "Invalid levels", "Error setting levels", JOptionPane.ERROR_MESSAGE);
			return false;
		}
		if (!zoomManager.isAvailable()) {
			int ret=JOptionPane.showConfirmDialog(
					this, 
					"Current setup seems wrong.\nAccept anyhow?", 
					"Are the values right?", 
					JOptionPane.YES_NO_OPTION, 
					JOptionPane.WARNING_MESSAGE);
			return ret==JOptionPane.YES_OPTION;
		}
		return true;
	}
	
	/**
	 * Override <code>setVisible()</code> to move the dialog
	 * over the logging client and in front of other windows
	 */
	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		// Move the statistic win on top of jlog
		if (visible && isShowing()) {
			Point loggingPos = loggingClient.getLocationOnScreen();
			setLocation(loggingPos);
			toFront();
		}
	}
	
	/**
	 * Setup the content of the label depending on the parameter
	 * 
	 * @param folder The path of the folder of XML files
	 * 				(can be <code>null</code>)
	 */
	private void setupFolderLbl() {
		if (folder==null || folder.isEmpty()) {
			folderLbl.setText("<UNDEFINED>");
		} else {
			folderLbl.setText(folder);
		}
	}
}
