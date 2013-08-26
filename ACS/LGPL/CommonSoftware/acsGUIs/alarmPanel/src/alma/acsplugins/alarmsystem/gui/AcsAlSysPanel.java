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
package alma.acsplugins.alarmsystem.gui;
import java.awt.FlowLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * 
 */

/**
 * The panel shown when the ACS alarm system is in use.
 * <P>
 * This panel contains a label to inform the user to open jlog instead
 * 
 * @author acaproni
 */
public class AcsAlSysPanel extends JPanel {
	/**
	 * The label shown by the panel
	 */
	private final JLabel lbl = new JLabel("<HTML><B>ACS alarm system in use!</B><BR>Use <I>jlog</I> to monitor alarms.</HTML>");
	
	/**
	 * Constructor
	 */
	public AcsAlSysPanel() {
		initialize();
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initialize() {
		setLayout(new FlowLayout(FlowLayout.LEFT));
		add(lbl);
	}
}
