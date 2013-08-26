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

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Dictionary;
import java.util.Hashtable;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;

import alma.acs.logging.engine.io.IOHelper;

/**
 * The file chooser for saving, supporting compression
 * 
 * @author acaproni
 *
 */
public class SaveFileChooser extends LogsFileChooser implements ActionListener {
	
	/**
	 * The check box to enable the compression
	 */
	private JCheckBox compressCB=new JCheckBox("Compress",false);
	
	/**
	 * The slider to set compression level
	 */
	private JSlider compressionLevelS = new JSlider(JSlider.HORIZONTAL,0,9,IOHelper.DEFAULT_COMPRESSION_LEVEL);
	
	
	
	/**
	 * The labels shown in the slider
	 */
	private Dictionary<Integer, JLabel> labels = new Hashtable<Integer, JLabel>();
	
	/**
	 * Constructor
	 * 
	 * @param title The title of the dialog
	 * @param currentDirectoryPath The path of the current folder
	 */
	public SaveFileChooser(String title, File currentDir) {
		super(title,currentDir,false);
		initialize(currentDir);
		setTitle(title);
		setModal(true);
		pack();
		setVisible(true);
	}

	/**
	 * Init the GUI
	 * 
	 * @param folderPath The path of the current directory
	 */
	private void initialize(File folder) {
		rootPane.setLayout(new BorderLayout());
		
		JPanel compressPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		compressPnl.setBorder(BorderFactory.createTitledBorder("Compression"));
		compressPnl.add(compressCB);
		compressCB.addActionListener(this);
		compressPnl.add(compressionLevelS);
		compressionLevelS.setMajorTickSpacing(10);
		compressionLevelS.setMinorTickSpacing(1);
		compressionLevelS.setPaintTicks(true);
		compressionLevelS.setPaintLabels(true);
		compressionLevelS.setEnabled(compressCB.isSelected());
		for (int t=0; t<=9; t++) {
			labels.put(Integer.valueOf(t), new JLabel(""+t));
		}
		compressionLevelS.setLabelTable(labels);
		compressionLevelS.setSnapToTicks(true);
		
		rootPane.add(compressPnl,BorderLayout.NORTH);
		rootPane.add(fileChooser,BorderLayout.CENTER);
	}

	/**
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==compressCB) {
			compressionLevelS.setEnabled(compressCB.isSelected());
		} else {
			super.actionPerformed(e);
		}
	}
	
	/**
	 * 
	 * @return <code>true</code> if the file must be compressed
	 */
	public boolean mustBeCompressed() {
		return compressCB.isSelected();
	}
	
	/**
	 * 
	 * @return The compression level
	 */
	public int getCompressionLevel() {
		return compressionLevelS.getValue();
	}
}
