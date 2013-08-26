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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JDialog;
import javax.swing.JFileChooser;

/**
 * Base class for the file chooser dialogs to load and save logs
 * 
 * @author acaproni
 *
 */
public class LogsFileChooser extends JDialog implements ActionListener {
	
	/**
	 * The file chooser
	 */
	protected JFileChooser fileChooser;
	
	/**
	 * The file selected by the user
	 */
	private File selectedFile=null;
	
	/**
	 * Constructor
	 * 
	 * @param title The tile of the dialog
	 * @param currentDir The dir whose content is shown at startup
	 * @param load <code>true</code> if the file chooser is used to load,
	 * 			<code>false</code> otherwise
	 */
	public LogsFileChooser(String title, File currentDir, boolean load) {
		super();
		setTitle(title);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		fileChooser = new JFileChooser(currentDir);
		fileChooser.addActionListener(this);
		if (load) {
			fileChooser.setDialogType(JFileChooser.OPEN_DIALOG);
		} else {
			fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
		}
	}
	
	/**
	 * Return the file selected by the user.
	 * 
	 * @return the file selected by the user or <code>null</code> if the user cancelled
	 * 
	 */
	public File getSelectedFile() {
		return selectedFile;
	}
	
	/**
	 * @return The current directory
	 */
	public File getCurrentDirectory() {
		return fileChooser.getCurrentDirectory();
	}

	/**
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==fileChooser) {
			if (e.getActionCommand().equals("ApproveSelection")) {
				selectedFile=fileChooser.getSelectedFile();
				setVisible(false);
				dispose();
			} else if (e.getActionCommand().equals("CancelSelection")) {
				selectedFile=null;
				setVisible(false);
				dispose();
			}
		} else {
			System.out.println("Unknown source of events: "+e.getSource());
		}
	}
}
