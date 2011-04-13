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
 * @version $Id: AlarmPanel.java,v 1.28 2011/04/13 15:48:21 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import java.awt.BorderLayout;
import java.awt.CardLayout;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.omg.CORBA.ORB;

import cern.laser.client.data.Alarm;

import alma.acs.alarmsystem.corbaservice.AlarmServiceUtils;

import alma.acs.logging.AcsLogger;
import alma.acsplugins.alarmsystem.gui.statusline.StatusLine;
import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;

/**
 * 
 * The panel showing alarms
 * <P>
 * The panel has a different content in the following situations:
 * <UL>
 * 	<LI>The ACS AS is in use
 * 	<LI>The CERN AS is use but the alarm client is not connected to the alarm service
 * 	<LI>The CERN AS is use and the alarm client is connected to the alarm service
 * </UL>
 * <P>
 * The panel runs in stand alone and inside the OMC. Its interface is not
 * the standard SubsystemPlugin but the more generic OmcPlugin. For that reason
 * we can't use  {@link alma.acs.gui.util.panel.Frame}.
 */
public class AlarmPanel extends JPanel {

	/**
	 * The ORB
	 */
    private ORB orb=null;
    
    /**
	 * The logger
	 */
    private AcsLogger logger=null;
	
	/**
	 * The window that shows this panel
	 */
    private JFrame frame=null;
    
    /**
     * The panel shown when the CERN alarm system is in use
     * and the client is connected to the AS
     */
    private final CernSysPanel cernSysPnl;
    
    /**
     * The panel shown when the ACS alarm system is in use.
     * <P>
     * This panel contains a label to inform the user to open jlog instead
     */
    private AcsAlSysPanel acsASPnl=new AcsAlSysPanel();
    
    /**
     * The panel shown when the AS is not available even if the CERN
     * AS is in use.
     * <P>
     * This panel is shown at startup until the client connects to the AS.
     * The purpose is to inform the user that the AS is not available but can signal 
     * an error if the AS did not start.
     */
    private AlSysNotAvailPanel alSysNotAvailPnl=new AlSysNotAvailPanel();
    
    /**
     * The layout to choose which panel the GUI shows
     */
    private final CardLayout layout = new CardLayout();
    
    /**
     * The panel showing the a different container for each situation
     */
    private final JPanel panel = new JPanel();
    
    /**
     * The name (in the layout) of <code>alSysNotAvailPnl</code>
     */
    public static final String alSysNotAvailName="CERN_N/A_pnl";
    
    /**
     * The name (in the layout) of <code>cernSysPnl</code>
     */
    public static final String cernSysName="CERN_pnl";
    
    /**
     * The name (in the layout) of <code>acsASPnl</code>
     */
    public static final String acsASName="ACS_pnl";
    
    /**
     * <code>true</code> if the alarm system in use is the ACS implementation
     */
    private boolean isAcsAs;
	
	/**
	 * Constructor 
	 *
	 */
	public AlarmPanel() {
		super(true);
		cernSysPnl=new CernSysPanel(this,alSysNotAvailPnl);
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
		cernSysPnl=new CernSysPanel(this,alSysNotAvailPnl);
		initialize();
	}
	
	/**
	 * Init the GUI
	 *
	 */
	private void initialize() {
		panel.setLayout(layout);
		panel.add(alSysNotAvailPnl, alSysNotAvailName);
		panel.add(cernSysPnl, cernSysName);
		panel.add(acsASPnl, acsASName);

		// At this stage the alarm system is unavailable  but we do not know yet
		// which is the type of alarm system in use
		panel.setLayout(layout);
		
		setLayout(new BorderLayout());
		add(panel,BorderLayout.CENTER);
		
		// This method is executed before setting the ContainerService
		// and so at this stage we do not know if the AS is CERN or ACS
		// but for sure we are not connected to the alarm service
		showPanel(alSysNotAvailName);
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
			throw new Exception("Services not set!");
		}
		cernSysPnl.setServices(orb, logger);
		cernSysPnl.start();
	}
	
	/**
	 * @see SubsystemPlugin
	 */
	public void stop() throws Exception {
		cernSysPnl.stop();
	}
	/**
	 * Set the services
	 * 
	 * @param orb The ORB
	 * @param logger The logger
	 */
	public void setServices(ORB orb, AcsLogger logger) {
		this.orb=orb;
		this.logger=logger;
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
	 * @return <code>true</code> if an attempt to connect is running
	 */
	public boolean isConnecting() {
		return cernSysPnl.isConnecting();
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
		cernSysPnl.addSpecialAlarm(alarm);
	}

	/**
	 * Show a message in the status line
	 * 
	 * @see StatusLine
	 */
	public void showMessage(String mesg, boolean red) {
		cernSysPnl.showMessage(mesg, red);
	}
	
	/**
	 * Show the alarm in the details table
	 *  
	 * @param alarm The alarm to show in the details panel;
	 * 				if <code>null</code> the details table is cleared.
	 */
	public void showAlarmDetails(Alarm alarm) {
		cernSysPnl.showAlarmDetails(alarm);
	}
	
	/**
	 * Read the type of the alarm system in use.
	 * <P> 
	 * If it is not possible to get the type, then we set the alarm type as <code>false</code> because
	 * in that case an error panel is shown.
	 * <P>
	 * If the AS is ACS, the ACS panel is shown.
	 */
	private void initAlarmServiceType() {
		if (orb==null || logger==null) {
			throw new IllegalStateException("The ORB/logger are still null!");
		}
		System.out.println("getAlarmServiceType");
		AlarmServiceUtils utils = new AlarmServiceUtils(orb,logger);
		boolean ret=false;
		try {
			ret=utils.getAlarmServiceType();
		} catch (Throwable t) {
			System.err.println("Unable to get the alarm system type: "+t.getMessage());
			t.printStackTrace(System.err);
		}
		isAcsAs=ret;
		// Show the ACS panel
		if (isAcsAs) {
			showPanel(acsASName);
		}
	}
	
	/**
	 * Show the panel with the given name
	 * 
	 * @param panelName The not <code>null</code> and not empty
	 * 					name of the panel to show
	 * 
	 * @throws IllegalArgumentException panelName is not a valid panel name
	 */
	public void showPanel(final String panelName) {
		if (panelName==null || panelName.isEmpty()) {
			throw new IllegalArgumentException("Invalid name of panel (null or empty name)");
		}
		boolean found=false;
		if (panelName.equals(cernSysName) || panelName.equals(acsASName) || panelName.equals(alSysNotAvailName)) {
			found=true;
		}
		if (!found) {
			throw new IllegalArgumentException("Panel name "+panelName+" NOT defined");
		}
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				layout.show(panel, panelName);
			}
		});
	}
}

