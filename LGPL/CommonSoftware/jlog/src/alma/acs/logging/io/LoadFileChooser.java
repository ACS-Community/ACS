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
import javax.swing.JDialog;
import javax.swing.JComponent;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;

import java.awt.BorderLayout;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import alma.acs.logging.dialogs.LoadSwitchesPanel;

import com.cosylab.logging.LoggingClient;

import java.io.File;

/**
 * A FileChooser customized to show hidden files
 * 
 * @author acaproni
 *
 */
public class LoadFileChooser extends JDialog implements ActionListener {
	
	/**
	 * The filter of files shown by the chooser.
	 * <P>
	 * The filtering is based on the extension of the files (case insensitive)
	 * 
	 * @author acaproni
	 *
	 */
	public class CustomFilter extends FileFilter {
		
		/**
		 * The extensions used for filtering like for example ".gz" or ".xml"
		 */
		private String[] extensions;
		
		/**
		 * Constructor
		 * 
		 * @param extensions The extension used for filtering
		 */
		public CustomFilter(String[] extensions) {
			if (extensions==null || extensions.length==0) {
				throw new IllegalArgumentException("Invalid extensions");
			}
			this.extensions=extensions;
		}

		/**
		 * Check if the file is readable and if its extensions matches one
		 * of the available extensions.
		 * 
		 * @see javax.swing.filechooser.FileFilter#accept(java.io.File)
		 */
		@Override
		public boolean accept(File f) {
			if (!f.canRead()) {
				return false;
			}
			if (f.isDirectory()) {
				return true;
			}
			String fileName = f.getAbsolutePath().toLowerCase();
			for (String ext: extensions) {
				if (fileName.endsWith(ext.toLowerCase())) {
					return true;
				}
			}
			return false;
		}

		/**
		 * @see javax.swing.filechooser.FileFilter#getDescription()
		 */
		@Override
		public String getDescription() {
			StringBuilder ret = new StringBuilder("Filtering based on ");
			for (String str: extensions) {
				ret.append(str);
				ret.append(' ');
			}
			return null;
		}
	}
	
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
		super();
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		
		loggingClient=client;
		setTitle(title);
		setModal(true);
		initialize(currentDir);
		if (extensions!=null && extensions.length>0) {
			fc.setFileFilter(new CustomFilter(extensions));
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
				guiSwitches.execute();
			} else if (evt.getActionCommand().equals("CancelSelection")) {
				selectedFile=null;
				setVisible(false);
				dispose();
			}
		}
	}
}
