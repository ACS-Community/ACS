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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.Timer;

import alma.acsplugins.alarmsystem.gui.table.AlarmCounter;
import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;

/**
 * The status line showing info to the user
 * 
 * @author acaproni
 *
 */
public class StatusLine extends JPanel implements ActionListener {
	
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
		private final AlarmCounter counter;
		
		public Counter(AlarmGUIType type, AlarmCounter alarmCounter) {
			if (type==null) {
				throw new IllegalArgumentException("The type can't be null");
			}
			if (alarmCounter==null) {
				throw new IllegalArgumentException("The AlarmCountercan't be null");
			}
			alarmType=type;
			counter=alarmCounter;
			// The number of cols in each text field depends on the MAX_ALARMS that
			// the mode shows in the table
			int len = Integer.valueOf(AlarmTableModel.MAX_ALARMS).toString().length();
			
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
		
		/**
		 * Update the value shown in the widget
		 */
		public void update() {
			widget.setText(Integer.valueOf(counter.getCount()).toString());
		}
	}
	
	// The counters showing the number of alarms
	private Counter[] counters = new Counter[AlarmGUIType.values().length];
	
	// The table model
	private final AlarmTableModel tableModel;
	
	// The time to refresh the values shown by the StatusLine
	private Timer timer=null;
	private static final int TIMER_INTERVAL=2000;
	/**
	 * Constructor
	 */
	public StatusLine(AlarmTableModel model) {
		if (model==null) {
			throw new IllegalArgumentException("The AlarmTableModel can't be null");
		}
		tableModel=model;
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
			counters[t]=new Counter(AlarmGUIType.values()[t],tableModel.getAlarmCounter(AlarmGUIType.values()[t]));
			
			// Add the widget
			add(counters[t].getComponent());
		}
	}
	
	/** 
	 * Start the thread to update values
	 */
	public void start() {
		if (timer!=null) {
			throw new IllegalStateException("Already started");
		}
		timer = new Timer(TIMER_INTERVAL, this);
		timer.setRepeats(true);
		timer.addActionListener(this);
		timer.start();
	}
	
	/** 
	 * Start the thread to update values
	 */
	public void stop() {
		timer.stop();
		timer.removeActionListener(this);
		timer=null;
	}
	
	/** 
	 * Pause the thread to update values
	 */
	public void pause() {
		timer.stop();
	}
	
	/** 
	 * Resume the thread to update values
	 */
	public void resume() {
		timer.start();
	}

	/**
	 * @see ActionListener
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==timer) {
			for (Counter cnt: counters) {
				cnt.update();
			}
		}
	}
	
	
}
