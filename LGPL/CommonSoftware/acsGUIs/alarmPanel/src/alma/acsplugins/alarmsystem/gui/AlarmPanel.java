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
 * @version $Id: AlarmPanel.java,v 1.6 2008/02/14 17:28:28 acaproni Exp $
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
import alma.acsplugins.alarmsystem.gui.toolbar.Toolbar;

/**
 * 
 * The panel showing alarms
 *
 */
public class AlarmPanel extends JPanel implements IPanel {
	
	// The container services
    private ContainerServices contSvc=null;
	
	// The table of logs and its model
	private AlarmTableModel model = new AlarmTableModel();
	private AlarmTable alarmTable = new AlarmTable(model);
	
	private JScrollPane tableScrollPane = new JScrollPane(
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	
	// The window that shows this panel
    private JFrame frame=null;
    
    // The client to listen alarms from categories
    private CategoryClient categoryClient=null;
    
    // The toolbar
    private Toolbar toolbar=new Toolbar();
    
    // The status line
    private StatusLine statusLine = new StatusLine();
	
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
		setLayout(new BorderLayout());
		// Add the toolbr
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
		
	}
	
	/**
	 * @see IPauseResume
	 */
	public void resume() throws Exception {
		
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public void start() throws Exception {
		// Check if the CS have been set
		if (contSvc==null) {
			throw new Exception("PluginContainerServices not set");
		}
		categoryClient = new CategoryClient(contSvc);
		categoryClient.connect((AlarmSelectionListener)model);
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public void stop() throws Exception {
		categoryClient.close();
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
