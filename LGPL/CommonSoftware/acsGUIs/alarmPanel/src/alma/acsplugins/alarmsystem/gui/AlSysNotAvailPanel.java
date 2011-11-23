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

import java.awt.BorderLayout;
import java.awt.FlowLayout;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

/**
 * The panel shown when the AS is not available even if the CERN
 * AS is in use.
 * <P>
 * This panel is shown at startup until the client connects to the AS.
 * The purpose is to inform the user that the AS is not available but can signal 
 * an error if the AS did not start.
 * 
 * @author acaproni
 */
public class AlSysNotAvailPanel extends JPanel {

	/**
	 * The label shown by the panel
	 */
	private final JLabel lbl = new JLabel("<HTML>Alarm service <B>NOT</B> connected!</HTML>");
	
	private final JTextArea messages = new JTextArea();
	
	/**
	 * Constructor
	 */
	public AlSysNotAvailPanel() {
		initialize();
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initialize() {
		setLayout(new BorderLayout());
		
		// Add the label
		JPanel labelPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		labelPnl.add(lbl);
		
		add(labelPnl,BorderLayout.NORTH);
		
		// Add the message area
		messages.setEditable(false);
		add(messages,BorderLayout.CENTER);
	}
	
	/**
	 * Add a message in the message area
	 * 
	 * @param msg The message to add in the message area
	 */
	public synchronized void addMessage(final String msg) {
		if (msg==null || msg.isEmpty()) {
			// Do not add emty/null messages
			return;
		}
		
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				String txt = messages.getText();
				if (!txt.endsWith("\n")) {
					txt = txt +"\n"+msg;
				} else {
					txt = txt+msg;
				}
				messages.setText(txt);
			}
		});
	}
}
