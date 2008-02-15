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

import java.awt.Component;
import java.awt.FlowLayout;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JTextField;

import alma.acsplugins.alarmsystem.gui.AlarmCounter;
import alma.acsplugins.alarmsystem.gui.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.AlarmTableModel;

/**
 * The status line showing info to the user
 * 
 * @author acaproni
 *
 */
public class StatusLine extends JPanel {
	
	/**
	 * The counter for each type of alarm
	 * 
	 * @author acaproni
	 *
	 */
	private class Counter {
		// The text field showing the value
		private final JTextField widget;
		
		// The type of alarm whose number is show by the widget
		private final AlarmGUIType alarmType;
		
		// The counter to read the number shown by the widget
		private final AlarmCounter counter=null;
		
		public Counter(AlarmGUIType type) {
			if (type==null) {
				throw new IllegalArgumentException("The type can't be null");
			}
			alarmType=type;
			// The number of cols in each text field depends on the MAX_ALARMS that
			// the mode shows in the table
			int len = Integer.valueOf(AlarmTableModel.MAX_ALARMS).toString().length()+2;
			
			widget= new JTextField(len);
			widget.setHorizontalAlignment(JTextField.CENTER);
			widget.setForeground(alarmType.foreg);
			widget.setBackground(alarmType.backg);
			widget.setEditable(false);
		}
		
		/**
		 * Return the component
		 * 
		 * @return the component
		 */
		public Component getComponent() {
			return widget;
		}
	}
	
	// The counters showing the number of alarms
	private Counter[] counters = new Counter[AlarmGUIType.values().length];
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
		
		
		// Build the text fields
		
		for (int t=0; t<counters.length; t++) {
			counters[t]=new Counter(AlarmGUIType.values()[t]);
			
			// Add the widget
			add(counters[t].getComponent());
		}
	}
}
