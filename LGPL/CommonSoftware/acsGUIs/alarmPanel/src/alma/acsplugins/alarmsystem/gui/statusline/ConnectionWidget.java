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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;

import alma.acsplugins.alarmsystem.gui.AlarmPanel;
import alma.acsplugins.alarmsystem.gui.CernSysPanel;

/**
 * The widget showing the status of the connection with an icon and
 * a tooltip.
 * 
 * The widget shows a popup menu to allow the reconnection
 * 
 * @author acaproni
 *
 */
public class ConnectionWidget extends JLabel implements ActionListener {
	/**
	 *  The possible states of the connection of the category client
	 *  
	 * @author acaproni
	 *
	 */
	public enum ConnectionStatus {
		CONNECTED("Connected","/console-connected.png"),
		CONNECTING("Connecting","/console-connecting.png"),
		DISCONNECTED("Disconnected","/console-disconnected.png"),
		HEARTBEAT_LOST("Alarm component down","/console-delay.png");
		
		// The icon for each connection state
		public final ImageIcon icon;
		
		// The tooltip for each connection state
		public String tooltip;
		
		/**
		 * The constructor that load the icon.
		 * 
		 * @param iconName The file containing the connection icon
		 */
		private ConnectionStatus(String tooltip, String iconName) {
			icon=new ImageIcon(this.getClass().getResource(iconName));
			this.tooltip=tooltip;
		}
		
	};
	
	/**
	 * The class receiving events from the mouse
	 *  
	 * @author acaproni
	 *
	 */
	public class StatusLineMouseAdapter extends MouseAdapter {
		/**
		 * @see MouseListener
		 */
		public void mousePressed(MouseEvent e) {
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
					reconnectMI.setEnabled(currentState!=ConnectionStatus.CONNECTED && !alarmPanel.isConnecting());
					this.e=e;
				}
				public void run() {
					popMenu.show(ConnectionWidget.this,e.getX(),e.getY());
				}
			}
			SwingUtilities.invokeLater(new ShowPopup(e));	
		}
	}
	
	/**
	 * The popmenu shown when the user presses the mouse button over the label
	 */
	private JPopupMenu popMenu;
	
	/**
	 *  The menu item to reconnect
	 */
	private JMenuItem reconnectMI;
	
	/**
	 *  The actual state of the connection
	 */
	private ConnectionStatus currentState;
	
	/**
	 * The panel for the CERN alarm system
	 */
	private CernSysPanel alarmPanel;
	
	/**
	 * Constructor
	 */
	public ConnectionWidget(CernSysPanel panel) {
		if (panel==null) {
			throw new IllegalArgumentException("The AlarmPanel can't be null");
		}
		alarmPanel=panel;
		initialize();
		addMouseListener(new StatusLineMouseAdapter());
	}
	
	/**
	 * Init the GUI
	 */
	private void initialize() {
		popMenu = new JPopupMenu();
		reconnectMI = new JMenuItem("Reconnect");
		popMenu.add(reconnectMI);
		reconnectMI.addActionListener(this);
	}
	
	/**
	 * Set the icon and the tooltip of the connection label.
	 * 
	 * @param state The state of the connection
	 */
	public void setConnectionState(ConnectionStatus state) {
		if (state==null) {
			throw new IllegalArgumentException("The state can't be null");
		}
		currentState=state;
		class SetConnState extends Thread {
			public ConnectionStatus status;
			public void run() {
				setIcon(status.icon);
				setToolTipText(status.tooltip);
			}
		}
		SetConnState thread = new SetConnState();
		thread.status=state;
		SwingUtilities.invokeLater(thread);
	}

	/* (non-Javadoc)
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if 	(e.getSource()==reconnectMI) {
			System.out.println("Reconnecting");
			class Reconnect extends Thread {
				public void run() {
					try {
						alarmPanel.disconnect();
					} catch (Throwable t) { }
					try {
						Thread.sleep(2500);
					} catch (Exception e) {}
					try {
						alarmPanel.connect();
					} catch (Throwable t) { 
						System.err.println("Error connecting: "+t.getMessage());
						t.printStackTrace(System.err);
					}
				}
			}
			Thread t = new Reconnect();
			t.setName("Reconnect");
			t.setDaemon(true);
			t.run();
		}
		
	}
	
}
