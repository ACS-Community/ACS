/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package cl.utfsm.samplingSystemUI;

import java.util.HashMap;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;

/**
 * The StatusIcon class is used througout the SSG to indicate the current status
 * of the program (Manager connection, sampling manager connection, etc).
 * It contains within it an icon, which is displayed to the user, and a
 * help string that is displayed when the mouse is over the icon
 * 
 * @author rtobar
 */
public class StatusIcon extends JLabel {

	private static final long serialVersionUID = 1L;
	
	private int _status;
	
	/**
	 * These constants are suposed to be OR'ed to check the actual status
	 */
	public static final int DISCONNECTED = 0x00;
	public static final int CONNECTED_TO_MANAGER = 0x01;
	public static final int CONNECTED_TO_SAMPMANAGER = 0x02;
	public static final int SAMPLING = 0x04;
	public static final int SAMPLING_WARNING = 0x08;
	
	private HashMap<Integer, StatusDisplay> mapping;
	
	/* These are the possible cases of the application status */
	{
		mapping = new HashMap<Integer, StatusDisplay>(4);
		mapping.put(DISCONNECTED, new StatusDisplay("Disconnected from ACS Manager and Sampling Manager",
				new ImageIcon(getClass().getClassLoader().getResource("console-disconnected.png"))));
		mapping.put(CONNECTED_TO_MANAGER, new StatusDisplay("<html>Connected to ACS Manager<br>Disconnected from Sampling Manager</html>",
				new ImageIcon(getClass().getClassLoader().getResource("console-suspended.png"))));
		mapping.put(CONNECTED_TO_SAMPMANAGER, new StatusDisplay("<html>Connected to ACS Manager<br>Connected to Sampling Manager</html>",
				new ImageIcon(getClass().getClassLoader().getResource("console-connected.png"))));
		mapping.put(SAMPLING, new StatusDisplay("Sampling process is running",
				new ImageIcon(getClass().getClassLoader().getResource("console-connected.png"))));
		mapping.put(SAMPLING_WARNING, new StatusDisplay("<html>Sampling process is running<br>but error were found</html>",
				new ImageIcon(getClass().getClassLoader().getResource("cl/utfsm/samplingSystemUI/img/console-warning.png"))));

	}
	
	public StatusIcon() {
		setStatus(CONNECTED_TO_MANAGER);
	}
	
	public StatusIcon(int status) {
		setStatus(status);
	}


	public void setStatus(int status) {
		if( status > 0x08 )
			throw new IllegalArgumentException();
		
		this.setIcon(mapping.get(Integer.valueOf(status)).getStatusIcon());
		this.setToolTipText(mapping.get(Integer.valueOf(status)).getHelpText());
		this._status = status;
	}

	public int getStatus() {
		return _status; 
	}

	private class StatusDisplay {
		private String helpText;
		private Icon statusIcon;
		
		public StatusDisplay(String str, Icon i) {
			this.helpText = str;
			this.statusIcon = i;
		}
		
		public String getHelpText() { return this.helpText; };
		public Icon getStatusIcon() { return this.statusIcon; };
	}
}
