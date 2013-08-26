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
package alma.acs.logging.io;

import javax.swing.JFileChooser;
import javax.swing.JComponent;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

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
public class LoadFileChooser extends LogsFileChooser implements ActionListener {
	/**
	 * The checkbox to make visible hidden files and folders
	 *The default is false (i.e. hidden folders are not shown)
	 */
	private JCheckBox viewHiddenFiles = new JCheckBox("Show hidden files",false);
	
	// The switches to clear the logs and disconnect the engine from the NC
	private LoadSwitchesPanel guiSwitches;
	
	// The logging client
	private LoggingClient loggingClient=null;
	
	/**
	 * Constructor: build and show the modal dialog
	 *  
	 * @param currentDir The dir whose content is shown at startup
	 * @param title The title of the window (it appears also in the button)
	 * @param extensions The (case insensitive) extensions of the files to show in the chooser
	 * 					(can be <code>null</code> or empty in case of no filtering)
	 * @param client The <code>LoggingClient</code> invoking this file chooser
	 */
	public LoadFileChooser(File currentDir,String title, String[] extensions, LoggingClient client) {
		super(title, currentDir,true);
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		
		loggingClient=client;
		setModal(true);
		initialize(currentDir);
		if (extensions!=null && extensions.length>0) {
			fileChooser.setFileFilter(new FileChooserFilter(extensions));
		}
		pack();
		setVisible(true);
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initialize(File curDir) {
		rootPane.setLayout(new BorderLayout());
		
		guiSwitches = new LoadSwitchesPanel(loggingClient);
		JPanel pnl = new JPanel(new BorderLayout());
		viewHiddenFiles.addActionListener(this);
		pnl.add(guiSwitches,BorderLayout.NORTH);
		pnl.add(viewHiddenFiles,BorderLayout.SOUTH);
		rootPane.add(pnl,BorderLayout.NORTH);
		
		if (curDir==null) {
			curDir = new File(".");
		}
		fileChooser = new JFileChooser(curDir);
		fileChooser.setFileHidingEnabled(!viewHiddenFiles.isSelected());
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.addActionListener(this);
		JComponent fcComponent = fileChooser;
		fcComponent.setOpaque(true);
		rootPane.add(fcComponent,BorderLayout.CENTER);
	}
	
	/**
	 * @see java.awt.event.ActionListener
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==viewHiddenFiles) {
			fileChooser.setFileHidingEnabled(!viewHiddenFiles.isSelected());
			fileChooser.rescanCurrentDirectory();
		} else {
			super.actionPerformed(e);
		}
	}

	/**
	 * 
	 * @see alma.acs.logging.io.LoadSwitchesPanel#execute()
	 */
	public void execute() {
		guiSwitches.execute();
	}
}
