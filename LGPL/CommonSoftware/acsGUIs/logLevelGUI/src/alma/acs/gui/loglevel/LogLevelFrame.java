/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.gui.loglevel;

import java.awt.BorderLayout;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;

import java.util.logging.Logger;

/**
 * The frame of the application
 * 
 * @author acaproni
 *
 */
public class LogLevelFrame extends JFrame {
	
	// The content of the frame
	private LogLevelPanel panel=null;
	
	// The ACS component client
    private AdvancedComponentClient client=null;
    
    
    
    // The container services (from the ComponentClient)
    private ContainerServices contSvc=null;

    // The logger
    private Logger logger=null;
	
	/**
	 * Constructor
	 *
	 */
	public LogLevelFrame() {
		super("Log level explorer");
		initialize();
		try {
			connectACSComponentClient();
		} catch (Exception e) {
			e.printStackTrace(System.err);
			JOptionPane.showMessageDialog(
					this,
					"Error connecting to ACS:\n"+e.getMessage(), 
					"Error", 
					JOptionPane.ERROR_MESSAGE);
		}
		panel.setACSContainerServices(contSvc);
		try {
			panel.start();
		} catch (Exception e) {
			e.printStackTrace(System.err);
			JOptionPane.showMessageDialog(
					this,
					"Error starting the application:\n"+e.getMessage(), 
					"Error", 
					JOptionPane.ERROR_MESSAGE);
			System.exit(-1);
		}
	}
	
	/**
	 * Initialize the application
	 *
	 */
	private void initialize() {
		LogLevelWinAdapter winAdapter = new LogLevelWinAdapter(this);
		addWindowListener(winAdapter);
		
		panel = new LogLevelPanel();
		add(panel,BorderLayout.CENTER);
		setBounds(50,50,225,150);
		setVisible(true);
	}

	/**
	 * The starting point of the application when executed
	 * as stand alone
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		new LogLevelFrame();
	}
	
	 /**
     * Connect to ACS as component client.
     * It connects the client and the logger.
     * 
     * @throws Exception In case of failure connecting to ACS
     */
    private void connectACSComponentClient() throws Exception {
        String clientName="LogLevel GUI";
	    String managerLoc = System.getProperty("ACS.manager");
	    if (managerLoc!=null) {
	    	managerLoc=managerLoc.trim();
	    } else {
	    	throw new IllegalStateException("ACS.magager property not set!");
	    }
	
	    try {
            client = new AdvancedComponentClient(null, managerLoc, clientName);
            contSvc=client.getContainerServices();
            logger = contSvc.getLogger();
            logger.log(AcsLogLevel.INFO,"Connected to ACS");
	    } catch (Exception e) {
            if (logger!=null) {
                logger.log(AcsLogLevel.ERROR,"Error connecting the simple client: "+e.getMessage());
            } else {
                System.err.println("Error connecting the simple client: "+e.getMessage());
            }
            client=null;
            logger=null;
            contSvc=null;
            throw e;
	    }
	    
	    
    }
    
    /**
     * Stop the panel (same operation
     * done by the OMC when closing the plugin)
     *
     */
    public void stopPanel() {
    	if (panel!=null) {
    		try {
    			panel.stop();
    		} catch (Throwable t) {} // Ignored.. we are closing!
    	}
    }

    /**
     * Disconnect from ACS
     */
    public void disconnectACSComponentClient() {
	    if (logger!=null) {
            logger.log(AcsLogLevel.INFO,"Exiting from ACS");
	    }
	    try {
            if (client!=null) {
            	client.tearDown();
            }
        } catch (Throwable e) {
            System.err.println("Exception caught while releasing ACS: "+e.getMessage());
            e.printStackTrace(System.err);
	    } finally {
	    	logger=null;
	    	client=null;
	    	contSvc=null;
	    }
    }
}
