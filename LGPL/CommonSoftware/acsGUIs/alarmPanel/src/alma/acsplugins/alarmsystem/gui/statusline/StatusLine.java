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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import alma.acsplugins.alarmsystem.gui.AlarmPanel;
import alma.acsplugins.alarmsystem.gui.CernSysPanel;
import alma.acsplugins.alarmsystem.gui.ConnectionListener;
import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;

/**
 * The status line showing info to the user
 * 
 * @author acaproni
 *
 */
public class StatusLine extends JPanel implements ActionListener, ConnectionListener {
	
	/**
	 * The counters showing the number of alarms
	 */
	private CounterWidget[] counters = new CounterWidget[AlarmGUIType.values().length];
	
	/**
	 * The table model
	 */
	private final AlarmTableModel tableModel;
	
	/**
	 * The alarm panel
	 */
	private final CernSysPanel alarmPanel;
	
	/**
	 * The time to refresh the values of the widgets shown by the StatusLine
	 */
	private final Timer timer;
	
	/**
	 * The time interval between 2 refreshes of the widgets
	 */
	private static final int TIMER_INTERVAL=2000;
	
	/**
	 * The widget showing the icon and the tooltip for the status of the connection
	 */
	private ConnectionWidget connectionWidget;
	
	/**
	 * The label to write messages to the user
	 */
	private final StatusMessageTF statusMessageLbl = new StatusMessageTF();

	/**
	 * Constructor
	 */
	public StatusLine(AlarmTableModel model, CernSysPanel panel) {
		if (model==null) {
			throw new IllegalArgumentException("The AlarmTableModel can't be null");
		}
		if (panel==null) {
			throw new IllegalArgumentException("The AlarmPanel can't be null");
		}
		alarmPanel=panel;
		tableModel=model;
		initialize();
		// Init the timer
		timer = new Timer(TIMER_INTERVAL, this);
		timer.setRepeats(true);
		timer.addActionListener(this);
		
		connectionWidget.setConnectionState(ConnectionWidget.ConnectionStatus.DISCONNECTED);
	}
	
	/**
	 * Init the status line
	 */
	private void initialize() {
		BoxLayout layout = new BoxLayout(this,BoxLayout.LINE_AXIS);
		setLayout(layout);
		
		// Add the panel with the widgets at the left side
		JPanel widgetsPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		widgetsPnl.setBorder(BorderFactory.createLoweredBevelBorder());
		
		// Build the text fields	
		for (int t=0; t<counters.length; t++) {
			counters[t]=new CounterWidget(
					AlarmGUIType.values()[t],
					tableModel.getAlarmCounter(AlarmGUIType.values()[t]),
					tableModel);
			
			// Add the widget
			widgetsPnl.add(counters[t].getComponent());
		}
		add(widgetsPnl);
		
		JPanel statusMsgPnl = new JPanel();
		statusMsgPnl.setBorder(BorderFactory.createLoweredBevelBorder());
		statusMsgPnl.add(statusMessageLbl);
		add(statusMsgPnl);
		
		// Add the label with the connection status to the right
		JPanel connectionPnl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		connectionPnl.setBorder(BorderFactory.createLoweredBevelBorder());
		connectionWidget = new ConnectionWidget(alarmPanel);
		connectionPnl.add(connectionWidget);
		add(connectionPnl);
	}
	
	/** 
	 * Start the thread to update values
	 */
	public void start() {
		timer.start();
	}
	
	/** 
	 * Stop the thread to update values
	 */
	public void stop() {
		timer.stop();
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

	/**
	 * Set the icon and tooltip for the connected state
	 * 
	 * @see alma.acsplugins.alarmsystem.gui.ConnectionListener#connected()
	 */
	@Override
	public void connected() {
		connectionWidget.setConnectionState(ConnectionWidget.ConnectionStatus.CONNECTED);
	}

	/**
	 * Set the icon and tooltip for the connecting state
	 * 
	 * @see alma.acsplugins.alarmsystem.gui.ConnectionListener#connecting()
	 */
	@Override
	public void connecting() {
		connectionWidget.setConnectionState(ConnectionWidget.ConnectionStatus.CONNECTING);
	}

	/**
	 * Set the icon and tooltip for the disconnected state
	 * 
	 * @see alma.acsplugins.alarmsystem.gui.ConnectionListener#disconnected()
	 */
	@Override
	public void disconnected() {
		connectionWidget.setConnectionState(ConnectionWidget.ConnectionStatus.DISCONNECTED);
	}
	
	/**
	 * The heartbeat from the alarm system component has been lost
	 * 
	 * @see alma.acsplugins.alarmsystem.gui.ConnectionListener#heartbeatLost()
	 */
	@Override
	public void heartbeatLost() {
		connectionWidget.setConnectionState(ConnectionWidget.ConnectionStatus.HEARTBEAT_LOST);
	}

	/***
	 * Show a string the status line
	 * 
	 * @param mesg
	 * @param red
	 * 
	 * @see StatusMessageTF
	 */
	public void showMessage(String mesg, boolean red) {
		statusMessageLbl.showMessage(mesg, red);
	}
	
}
