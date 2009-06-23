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

/** 
 * @author  acaproni   
 * @version $Id: AlarmPanel.java,v 1.24 2009/06/23 14:38:10 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import java.awt.BorderLayout;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;

import alma.acs.container.ContainerServices;
import alma.alarmsystem.clients.CategoryClient;

import alma.acs.gui.util.panel.IPanel;
import alma.acsplugins.alarmsystem.gui.statusline.StatusLine;
import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.table.AlarmTable;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;
import alma.acsplugins.alarmsystem.gui.toolbar.Toolbar;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;

/**
 * 
 * The panel showing alarms
 *
 */
public class AlarmPanel extends JPanel implements IPanel {
	
	/**
	 * The startup option for reduction rules
	 */
	public final boolean ACTIVATE_RDUCTION_RULES=true;
	
	/**
	 * The container services
	 */
    private ContainerServices contSvc=null;
	
	/**
	 * The model of the table of alarms
	 */
	private AlarmTableModel model;
	
	/**
	 * The table of alarms
	 */
	private AlarmTable alarmTable;
	
	/**
	 * The scroll pane of the table
	 */
	private JScrollPane tableScrollPane = new JScrollPane(
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	
	/**
	 * The window that shows this panel
	 */
    private JFrame frame=null;
    
    /**
     *  The client to listen alarms from categories
     */
    private CategoryClient categoryClient=null;
    
    /**
     * The toolbar
     */
    private Toolbar toolbar;
    
    /**
     * The status line
     */
    private StatusLine statusLine;
    
    /**
     * The listener of the connection
     */
    private ConnectionListener connectionListener;
    
    /**
     * Say if there is an attempt to connect
     */
    private volatile boolean connecting=false;
    
    /**
     * <code>true</code> if the panel has been closed.
     * It helps stopping the connection thread
     */
    private volatile boolean closed=false;
    
    /**
     * The thread to connect/disconnect
     */
    private Thread connectThread;
    
    /**
     * Signal the thread to teminate
     */
    private Thread disconnectThread;
	
	/**
	 * Constructor 
	 *
	 */
	public AlarmPanel() {
		super(true);
		initialize();
	}
	
	/**
	 * Constructor 
	 * 
	 * @param frame The window that owns this panel
	 */
	public AlarmPanel(JFrame frame) {
		super(true);
		if (frame==null) {
			throw new IllegalArgumentException("Invalid null frame in constructor");
		}
		this.frame=frame;
		this.frame.setIconImage(new ImageIcon(AlarmGUIType.class.getResource(AlarmGUIType.iconFolder+"flag_red.png")).getImage());
		initialize();
	}
	
	/**
	 * Init the GUI
	 *
	 */
	private void initialize() {
		setLayout(new BorderLayout());
		
		
		
		// Build GUI objects
		model = new AlarmTableModel(this,ACTIVATE_RDUCTION_RULES);
		alarmTable = new AlarmTable(model,this);
		statusLine = new StatusLine(model,this);
		connectionListener=statusLine;
		model.setConnectionListener(statusLine);
		
		// Add the table of alarms
		tableScrollPane.setViewportView(alarmTable);
		tableScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
		tableScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		add(tableScrollPane,BorderLayout.CENTER);
		
		// Add the toolbar
		toolbar=new Toolbar(alarmTable,model,ACTIVATE_RDUCTION_RULES,this);
		add(toolbar,BorderLayout.NORTH);
		
		// Add the status line
		add(statusLine,BorderLayout.SOUTH);
	}
	
	/**
	 * @see IpauseResume
	 */
	public void pause() throws Exception {
		model.pause(true);
		statusLine.pause();
		toolbar.updatePauseBtn(true);
	}
	
	/**
	 * @see IPauseResume
	 */
	public void resume() throws Exception {
		model.pause(false);
		statusLine.resume();
		toolbar.updatePauseBtn(false);
	}
	
	/**
	 * Connect the Client and listens to the categories.
	 * 
	 * The <code>CategoryClient</code> is built only if its reference is null.
	 * Otherwise it means that the client is still trying to connect and the user 
	 * restarted the plugin.
	 *  
	 * @see SubsystemPlugin
	 */
	public void start() throws Exception {
		// Check if the CS have been set
		if (contSvc==null) {
			throw new Exception("PluginContainerServices not set");
		}
		class StartAlarmPanel extends Thread {
			public void run() {
				AlarmPanel.this.connect();
			}
		}
		closed=false;
		// Connect the categoryClient only if it is null
		if (categoryClient==null) {
			connectThread = new StartAlarmPanel();
			connectThread.setName("StartAlarmPanel");
			connectThread.setDaemon(true);
			connectThread.start();
		}
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public void stop() throws Exception {
		class StopAlarmPanel extends Thread {
			public void run() {
				try {
					AlarmPanel.this.disconnect();
				} catch (Throwable t) {
					System.err.println("Ignored error while disconnecting category client: "+t.getMessage());
					t.printStackTrace(System.err);
				}
			}
		}
		closed=true;
		disconnectThread = new StopAlarmPanel();
		disconnectThread.setName("StopAlarmPanel");
		disconnectThread.start();
		model.close();
		alarmTable.close();
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public void setServices (ContainerServices ctrl) {
		contSvc=ctrl;
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public boolean runRestricted (boolean restricted) throws Exception {
		return restricted;
	}
	
	
	
	/**
	 * Set the ContainerServices
	 * 
	 * @see alma.acs.gui.util.panel.IPanel
	 */
	public void setACSContainerServices(ContainerServices cs) {
		if (cs==null) {
			throw new IllegalArgumentException("Invalid null ContainerServices");
		}
		contSvc=cs;	
	}
	
	/**
	 * Return true if the panel is running inside OMC
	 * 
	 * @see alma.acs.gui.util.panel.IPanel
	 */
	public boolean isOMCPlugin() {
		return frame==null;
	}
	
	/**
	 * Connect
	 */
	public void connect() {
		if (connecting || closed) {
			return;
		}
		connecting=true;
		connectionListener.connecting();
		try {
			categoryClient = new CategoryClient(contSvc);
		} catch (Throwable t) {
			System.err.println("Error instantiating the CategoryClient: "+t.getMessage());
			t.printStackTrace(System.err);
			connectionListener.disconnected();
			categoryClient=null;
			connecting=false;
			return;
		}
		/**
		 * Try to connect to the alarm service until it becomes available
		 */
		while (true && !closed) {
			try {
				categoryClient.connect((AlarmSelectionListener)model);
				// If the connection succeeded then exit the loop
				break;
			} catch (AcsJCannotGetComponentEx cgc) {
				// Wait 30 secs before retrying
				// but checks if it is closed every second.
				int t=0;
				while (t<30) {
					if (closed) {
						return;
					}
					try {
						Thread.sleep(1000);
					} catch (Exception e) {}
					t++;
				}
				continue; // Try again
			} catch (Throwable t) {
				System.err.println("Error connecting CategoryClient: "+t.getMessage()+", "+t.getClass().getName());
				t.printStackTrace(System.err);
				connectionListener.disconnected();
				connecting=false;
				return;
			}
		}
		if (closed) {
			model.setCategoryClient(null);
			return;
		}
		connecting=false;
		connectionListener.connected();
		statusLine.start();
		model.setCategoryClient(categoryClient);
	}
	
	/**
	 * Disconnect
	 */
	public void disconnect() {
		statusLine.stop();
		model.setCategoryClient(null);
		// wait until the connect thread terminates (if it is running)
		while (connectThread!=null && connectThread.isAlive()) {
			try {
				Thread.sleep(1500);
			} catch (Exception e) {}
		}
		try {
			categoryClient.close();
		} catch (Throwable t) {
			System.err.println("Error closinging CategoryClient: "+t.getMessage());
			t.printStackTrace(System.err);
		} finally {
			categoryClient=null;
			connectionListener.disconnected();
		}
	}
	
	/**
	 * @return <code>true</code> if an attempt to connect is running
	 */
	public boolean isConencting() {
		return connecting;
	}

	/**
	 * @return the categoryClient
	 */
	public CategoryClient getCategoryClient() {
		return categoryClient;
	}
	
	/**
	 * A method to send alarms to the GUI outside of the alarm service.
	 * <P>
	 * At the present it is used by the OMC GUI to send alarms before the alarm
	 * service is started.
	 * 
	 * @deprecated this method will be deleted when the alarm system will run as a daemon 
	 * 				or as an ACS service.
	 * 
	 * @param alarm The alarm to show in the table (can't be <code>null</code>)
	 * @throws Exception In case the alarm is not well formed
	 * 
	 * 
	 */
	public synchronized void addSpecialAlarm(Alarm alarm) throws Exception {
		if (alarm==null || alarm.getAlarmId()==null || alarm.getAlarmId().isEmpty()) {
			throw new Exception("The alarm cant'be null and must have a valid ID!");
		}
		model.onAlarm(alarm);
	}

	/**
	 * Show a message in the status line
	 * 
	 * @param mesg
	 * @param red
	 * 
	 * @see StatusLine
	 */
	public void showMessage(String mesg, boolean red) {
		statusLine.showMessage(mesg, red);
	}
}
