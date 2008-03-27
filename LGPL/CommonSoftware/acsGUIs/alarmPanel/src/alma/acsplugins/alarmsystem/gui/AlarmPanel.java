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
 * @version $Id: AlarmPanel.java,v 1.13 2008/03/27 14:14:25 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import java.awt.BorderLayout;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import cern.laser.client.services.selection.AlarmSelectionListener;

import alma.acs.container.ContainerServices;
import alma.alarmsystem.clients.CategoryClient;

import alma.acs.gui.util.panel.IPanel;
import alma.acsplugins.alarmsystem.gui.statusline.StatusLine;
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
	
	// The container services
    private ContainerServices contSvc=null;
	
	// The table of logs and its model
	private AlarmTableModel model;
	private AlarmTable alarmTable;
	
	private JScrollPane tableScrollPane = new JScrollPane(
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	
	// The window that shows this panel
    private JFrame frame=null;
    
    // The client to listen alarms from categories
    private CategoryClient categoryClient=null;
    
    // The toolbar
    private Toolbar toolbar;
    
    // The status line
    private StatusLine statusLine;
	
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
		initialize();
	}
	
	/**
	 * Init the GUI
	 *
	 */
	private void initialize() {
		// Build GUI objects
		model = new AlarmTableModel(this);
		alarmTable = new AlarmTable(model);
		statusLine = new StatusLine(model);
		model.setConnectionListener(statusLine);
		
		setLayout(new BorderLayout());
		// Add the toolbar
		toolbar=new Toolbar(model);
		add(toolbar,BorderLayout.NORTH);
		
		// Add the table of alarms
		tableScrollPane.setViewportView(alarmTable);
		tableScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
		tableScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		add(tableScrollPane,BorderLayout.CENTER);
		
		// Add the status line
		add(statusLine,BorderLayout.SOUTH);
	}
	
	/**
	 * @see IpauseResume
	 */
	public void pause() throws Exception {
		statusLine.pause();
	}
	
	/**
	 * @see IPauseResume
	 */
	public void resume() throws Exception {
		statusLine.resume();
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
			
			/**
			 * Constructor
			 * 
			 * @param listener The listener of the state of the connection
			 */
			public StartAlarmPanel( ConnectionListener listener) {
				if (listener==null) {
					throw new IllegalArgumentException("The ConnectionListener can't be null");
				}
				connListener=listener;
			}
			
			// The listener of the state of the connection
			private ConnectionListener connListener;
			
			public void run() {
				connListener.connecting();
				try {
					categoryClient = new CategoryClient(contSvc);
				} catch (Throwable t) {
					System.err.println("Error instantiating the CategoryClient: "+t.getMessage());
					t.printStackTrace(System.err);
					connListener.disconnected();
					categoryClient=null;
					return;
				}
				/**
				 * Try to connect to the alarm service until it becomes available
				 */
				while (true) {
					try {
						categoryClient.connect((AlarmSelectionListener)model);
						System.out.println("ASC connected");
						// If the connection succeded then exit the loop
						break;
					} catch (AcsJCannotGetComponentEx cgc) {
						System.out.println("Error getting the alarm service "+cgc.getMessage());
						// Wait 30 secs before retrying
						try {
							Thread.sleep(30000);
						} catch (Exception e) {}
						continue; // Try again
					} catch (Throwable t) {
						System.err.println("Error connecting CategoryClient: "+t.getMessage());
						t.printStackTrace(System.err);
						connListener.disconnected();
						return;
					}
				}
				System.out.println("Connected");
				statusLine.start();
				connListener.connected();
			}
		}
		// Connect the categoryClient only if it is null
		if (categoryClient==null) {
			Thread t = new StartAlarmPanel(statusLine);
			t.setName("StartAlarmPanel");
			t.setDaemon(true);
			t.start();
		}
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public void stop() throws Exception {
		class StopAlarmPanel extends Thread {
			public void run() {
				try {
					statusLine.stop();
					categoryClient.close();
					categoryClient=null;
				} catch (Throwable t) {
					System.err.println("Error closinging CategoryClient: "+t.getMessage());
					t.printStackTrace(System.err);
				}
			}
		}
		Thread t = new StopAlarmPanel();
		t.setName("StopAlarmPanel");
		t.setDaemon(true);
		t.start();	
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
}
