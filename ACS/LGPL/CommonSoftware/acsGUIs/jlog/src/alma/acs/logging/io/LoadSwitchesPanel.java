/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.io;

import java.awt.FlowLayout;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import com.cosylab.logging.LoggingClient;

/**
* The panel contains the checkboxes to clear the logs
* in the table and to disconnect.
* 
* It will be used in the panel to load logs (from URL,
* file or the database)
* 
* @author acaproni
*
*/
public class LoadSwitchesPanel extends JPanel {
	/**
	 * The check box to clear the logs before starting to load logs
	 * (enabled by default)
	 */
	private JCheckBox clearLogsCB = null;
	
	/**
	 * The checkbox to disconnect before starting to load logs
	 * (enabled by default)
	 */
	private JCheckBox disconnectCB = null;
	
	// The logging client
	private LoggingClient loggingClient=null;
	
	/**
	 * Constructor
	 *
	 */
	public LoadSwitchesPanel(LoggingClient client) {
		if (client==null) {
			throw new IllegalArgumentException("Invalid null LoggingClient!");
		}
		loggingClient=client;
		initGUI();
	}
	
	/**
	 * Initialize the panel
	 */
	private void initGUI() {
		clearLogsCB = new JCheckBox("Clear log table",true);
		disconnectCB = new JCheckBox("Disconnect",true);
		checkControlsState();

		setLayout(new FlowLayout(FlowLayout.LEFT));
		add(clearLogsCB);
		add(disconnectCB);
		
		// Add a border
		TitledBorder border = BorderFactory.createTitledBorder("GUI setup");
		this.setBorder(border);
	}
	
	/**
	 * Enable/disable the check boxes depending on the number
	 * of logs in the table and the connection status.
	 */
	public void checkControlsState() {
		// Clear and disable the disconnectCB if the engine is already
		// disconnected
		disconnectCB.setSelected(loggingClient.isConnected());
		disconnectCB.setEnabled(loggingClient.isConnected());
		
		// Clear and disable the clearLogsCB if there are no logs in the table
		clearLogsCB.setEnabled(loggingClient.getLCModel1().totalLogNumber()!=0);
		clearLogsCB.setSelected(loggingClient.getLCModel1().totalLogNumber()!=0);
	}
	
	/**
	 * 
	 * @return true if the logs has to be removed from the table
	 */
	public boolean clearLogs() {
		return clearLogsCB.isSelected();
	}
	
	/**
	 * 
	 * @return true if the engine must be disconnected
	 */
	public boolean disconnectEngine() {
		return disconnectCB.isSelected();
	}
	
	/**
	 * Remove the logs from the table and disconnect the engine
	 * (if the checkboxes are selected).
	 *
	 */
	public void execute() {
		if (disconnectCB.isSelected()) {
			loggingClient.connect(false);
		}
		if (clearLogsCB.isSelected()) {
			loggingClient.getLCModel1().clearAll();
		}
	}
}
