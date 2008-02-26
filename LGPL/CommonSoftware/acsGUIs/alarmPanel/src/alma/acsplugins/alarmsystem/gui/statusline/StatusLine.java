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
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
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
	
	// The counters showing the number of alarms
	private CounterWidget[] counters = new CounterWidget[AlarmGUIType.values().length];
	
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
			counters[t]=new CounterWidget(
					AlarmGUIType.values()[t],
					tableModel.getAlarmCounter(AlarmGUIType.values()[t]),
					tableModel);
			
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
			for (CounterWidget cnt: counters) {
				cnt.update();
			}
		}
	}
	
	
}
