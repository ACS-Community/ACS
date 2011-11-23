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
import java.awt.Dimension;
import java.awt.FlowLayout;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;

import org.omg.CORBA.ORB;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;

import alma.acs.logging.AcsLogger;
import alma.acsplugins.alarmsystem.gui.detail.AlarmDetailTable;
import alma.acsplugins.alarmsystem.gui.sound.AlarmSound;
import alma.acsplugins.alarmsystem.gui.statusline.StatusLine;
import alma.acsplugins.alarmsystem.gui.table.AlarmTable;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;
import alma.acsplugins.alarmsystem.gui.toolbar.Toolbar;
import alma.acsplugins.alarmsystem.gui.undocumented.table.UndocAlarmTableModel;
import alma.acsplugins.alarmsystem.gui.undocumented.table.UndocumentedAlarmTable;
import alma.alarmsystem.clients.CategoryClient;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;

/**
 * The panel shown while the CERN AS is in use and the 
 * alarm client is connected.
 * <P>
 * @author acaproni
 *
 */
public class CernSysPanel extends JPanel {
	
	/**
     * The toolbar
     */
    private Toolbar toolbar;
    
    /**
     * The status line
     */
    private StatusLine statusLine;
	
	/**
	 * The startup option for reduction rules
	 */
	public final boolean ACTIVATE_RDUCTION_RULES=true;
	
	/**
	 * The model of the table of alarms
	 */
	private AlarmTableModel model;
	
	/**
	 * The table of alarms
	 */
	private AlarmTable alarmTable;
	
	/**
	 * The table with the details of an alarm
	 */
	private AlarmDetailTable detailTable;
	
	/**
	 * The object to play sounds for alarms
	 */
	private AlarmSound alarmSound;
	
	/**
	 * The scroll pane of the table
	 */
	private JScrollPane tableScrollPane = new JScrollPane(
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	
	/**
	 * The scroll pane of the details table
	 */
	private JScrollPane detailsScrollPane = new JScrollPane(
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

	/**
     * The split pane dividing the table of alarms and the detail view
     */
    private JSplitPane splitPane;
    
    /**
     * The panel showing this container
     */
    private final AlarmPanel alarmPanel;
    
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
     * The listener of the connection
     */
    private ConnectionListener connectionListener;
    
    /**
     * The thread to connect/disconnect
     */
    private Thread connectThread;
    
    /**
     * Signal the thread to terminate
     */
    private Thread disconnectThread;
    
    /**
     *  The client to listen alarms from categories
     */
    private CategoryClient categoryClient=null;

    /**
     * The ORB
     */
    private ORB orb;
    
    /**
     * The logger
     */
    private AcsLogger logger;
    
    /**
     * The panel to show messages while connecting
     */
    private final AlSysNotAvailPanel notAvaiPnl;
       
    /**
     * Constructor
     * 
     * @param owner The panel showing this container
     * @param notAvaiPnl The panel when the AS is not available
     */
    public CernSysPanel(AlarmPanel owner, AlSysNotAvailPanel notAvaiPnl, UndocAlarmTableModel undocModel) {
    	if (notAvaiPnl==null) {
    		throw new IllegalArgumentException("AlSysNotAvailPanel can't be null");
    	}
    	alarmPanel=owner;
    	this.notAvaiPnl=notAvaiPnl;
    	initialize(undocModel);
    }
    
	/**
	 * Init the GUI
	 *
	 */
	private void initialize(UndocAlarmTableModel undocModel) {
		setLayout(new BorderLayout());
		
		// Build GUI objects
		model = new AlarmTableModel(this,ACTIVATE_RDUCTION_RULES,false,undocModel);
		alarmSound= new AlarmSound(model);
		alarmTable = new AlarmTable(model,this,undocModel);
		statusLine = new StatusLine(model,this);
		connectionListener=statusLine;
		model.setConnectionListener(statusLine);
		detailTable = new AlarmDetailTable();
		
		// The table of alarms
		tableScrollPane.setViewportView(alarmTable);
		Dimension minimumSize = new Dimension(300, 150);
		tableScrollPane.setMinimumSize(minimumSize);
		tableScrollPane.setPreferredSize(minimumSize);
		
		// The details table
		detailsScrollPane.setViewportView(detailTable);
		
		// The panel with the details
		JPanel detailsPanel = new JPanel();
		BoxLayout layout = new BoxLayout(detailsPanel,BoxLayout.Y_AXIS);
		detailsPanel.setLayout(new BorderLayout());
		
		JPanel lblPnl = new JPanel(new FlowLayout(FlowLayout.CENTER));
		lblPnl.add(new JLabel("Alarm details"));
		detailsPanel.add(lblPnl,BorderLayout.PAGE_START);
		detailsPanel.add(detailsScrollPane,BorderLayout.CENTER);
		minimumSize = new Dimension(120, 150);
		detailsPanel.setMinimumSize(minimumSize);
		detailsPanel.setPreferredSize(minimumSize);
		
		splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,tableScrollPane,detailsPanel);
		splitPane.setOneTouchExpandable(true);
		splitPane.setResizeWeight(1);
		//splitPane.setDividerLocation(tableScrollPane.getMinimumSize().width);
		add(splitPane,BorderLayout.CENTER);
		
		// Add the toolbar
		toolbar=new Toolbar(alarmTable,model,alarmSound,ACTIVATE_RDUCTION_RULES,this);
		add(toolbar,BorderLayout.NORTH);
		
		// Add the status line
		add(statusLine,BorderLayout.SOUTH);
	}
	
	/**
	 * Closes the panel
	 */
	public void close() {
		alarmSound.close();
		model.close();
		alarmTable.close();
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
	
	/**
	 * Show the alarm in the details table
	 *  
	 * @param alarm The alarm to show in the details panel;
	 * 				if <code>null</code> the details table is cleared.
	 */
	public void showAlarmDetails(Alarm alarm) {
		detailTable.showAlarmDetails(alarm);
	}

	public void setModel(AlarmTableModel model) {
		this.model = model;
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
		notAvaiPnl.addMessage("Connecting to the alarm service");
		notAvaiPnl.addMessage("Instantiating the category client");
		try {
			categoryClient = new CategoryClient(orb,logger);
		} catch (Throwable t) {
			System.err.println("Error instantiating the CategoryClient: "+t.getMessage());
			notAvaiPnl.addMessage("Error instantiating the CategoryClient: "+t.getMessage());
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
			notAvaiPnl.addMessage("Connecting to the categories");
			try {
				categoryClient.connect((AlarmSelectionListener)model);
				notAvaiPnl.addMessage("CategoryClient connected");
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
				cgc.printStackTrace();
				
				continue; // Try again
			} catch (Throwable t) {
				System.err.println("Error connecting CategoryClient: "+t.getMessage()+", "+t.getClass().getName());
				notAvaiPnl.addMessage("Error connecting CategoryClient: "+t.getMessage()+", "+t.getClass().getName());
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
		notAvaiPnl.addMessage("Connected to the alarm service");
		connecting=false;
		connectionListener.connected();
		statusLine.start();
		model.setCategoryClient(categoryClient);
		alarmPanel.showPanel(AlarmPanel.cernSysName);
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
	public boolean isConnecting() {
		return connecting;
	}
	
	public void setServices(ORB orb, AcsLogger logger) {
		if (orb==null) {
			throw new IllegalArgumentException("The ORB can't be null");
		}
		this.orb=orb;
		if (logger==null) {
			throw new IllegalArgumentException("The Logger can't be null");
		}
		this.logger=logger;
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
		if (orb==null || logger==null) {
			throw new IllegalStateException("ORB?Logger not set");
		}
		class StartAlarmPanel extends Thread {
			public void run() {
				CernSysPanel.this.connect();
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
					CernSysPanel.this.disconnect();
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
		close();
	}
}
