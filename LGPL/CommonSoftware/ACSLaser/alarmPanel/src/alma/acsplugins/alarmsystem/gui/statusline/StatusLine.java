/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acsplugins.alarmsystem.gui.statusline;

import java.awt.FlowLayout;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JTextField;

import alma.acsplugins.alarmsystem.gui.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.AlarmTableModel;
import alma.acsplugins.alarmsystem.gui.AlarmsCounter;

/**
 * The status line showing info to the user
 * 
 * @author acaproni
 *
 */
public class StatusLine extends JPanel {
	
	// the widgets to show the numbers of alarms in the table 
	private JTextField[] valuesLbl = new JTextField[AlarmsCounter.values().length-1];

	/**
	 * Constructor
	 */
	public StatusLine() {
		initialize();
	}
	
	/**
	 * Init the status line
	 */
	private void initialize() {
		setBorder(BorderFactory.createLoweredBevelBorder());
		((FlowLayout)getLayout()).setAlignment(FlowLayout.LEFT);
		
		// The num of cols in each text field depends on the MAX_ALARMS that
		// the mode shows in the table
		int len = Integer.valueOf(AlarmTableModel.MAX_ALARMS).toString().length()+2;
		// Build the text fields
		
		for (int t=0; t<valuesLbl.length; t++) {
			valuesLbl[t]= new JTextField(len);
			valuesLbl[t].setHorizontalAlignment(JTextField.CENTER);
			valuesLbl[t].setForeground(AlarmGUIType.values()[t].foreg);
			valuesLbl[t].setBackground(AlarmGUIType.values()[t].backg);
			
			// Add the widget
			add(valuesLbl[t]);
		}
	}
}
