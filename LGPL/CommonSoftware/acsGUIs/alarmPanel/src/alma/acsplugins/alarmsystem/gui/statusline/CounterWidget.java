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
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import cern.laser.client.data.Alarm;

import alma.acsplugins.alarmsystem.gui.table.AlarmCounter;
import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.table.AlarmTable;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;

/**
 * The counter for each type of alarm
 * 
 * @author acaproni
 *
 */
public class CounterWidget implements ActionListener {
	
	/**
	 * The mouse adapter receiving mouse events generated
	 * over the table of the alarms
	 * 
	 * @author acaproni
	 *
	 */
	private class CounterWidgetMouseAdapter extends MouseAdapter {
				
		// The last selected alarm
		//
		// It is set when the user presses over a row (i,e. selects an alarm)
		public Alarm selectedAlarm;
		/**
		 * @see MouseListener
		 */
		public void mouseClicked(MouseEvent e) {
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mousePressed(MouseEvent e) {
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mouseReleased(MouseEvent e) {
			showPopup(e);
		}
		
		/**
		 * Show the popup menu
		 * 
		 * @param e The mouse event that triggered the pop
		 */
		private void showPopup(MouseEvent e) {
			if (!e.isPopupTrigger()) {
				return;
			}
			class ShowPopup extends Thread {
				MouseEvent e;
				public ShowPopup(MouseEvent e) {
					this.e=e;
				}
				public void run() {
					statusLinePM.show(CounterWidget.this.widget,e.getX(),e.getY());
				}
			}
			SwingUtilities.invokeLater(new ShowPopup(e));	
		}
	}
	
	
	// The text field showing the value
	private final JTextField widget;
	
	// The type of alarm whose number is show by the widget
	private final AlarmGUIType alarmType;
	
	// The counter to read the number shown by the widget
	private final AlarmCounter counter;
	
	// The table model to remove inactive alarms
	private AlarmTableModel alarmModel;
	
	// The popup menu to remove inactive alarm of a given type
	private JPopupMenu statusLinePM = new JPopupMenu();
	private JMenuItem popupMI; 
	
	public CounterWidget(AlarmGUIType type, AlarmCounter alarmCounter, AlarmTableModel model) {
		if (type==null) {
			throw new IllegalArgumentException("The type can't be null");
		}
		if (alarmCounter==null) {
			throw new IllegalArgumentException("The AlarmCounter can't be null");
		}
		if (model==null) {
			throw new IllegalArgumentException("The AlarmTableModel can't be null");
		}
		alarmType=type;
		counter=alarmCounter;
		alarmModel=model;
		
		// The number of cols in each text field depends on the MAX_ALARMS that
		// the mode shows in the table
		int len = Integer.valueOf(AlarmTableModel.MAX_ALARMS).toString().length();
		
		widget= new JTextField(len);
		widget.setHorizontalAlignment(JTextField.CENTER);
		widget.setForeground(alarmType.foreg);
		widget.setBackground(alarmType.backg);
		widget.setEditable(false);
		
		// Build the popup
		popupMI = new JMenuItem("Remove "+alarmType.tile+" alarms");
		statusLinePM.add(popupMI);
		widget.addMouseListener(new CounterWidgetMouseAdapter());
		popupMI.addActionListener(this);
		
		// Set the tooltip
		String tip = "Num. of "+type;
		if (type!=AlarmGUIType.INACTIVE) {
			tip=tip+" inactive";
		} else {
			
		}
		tip=tip+" alarms";
		widget.setToolTipText(tip);
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

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==popupMI) {
			alarmModel.removeInactiveAlarms(alarmType);
		}
	}
}
