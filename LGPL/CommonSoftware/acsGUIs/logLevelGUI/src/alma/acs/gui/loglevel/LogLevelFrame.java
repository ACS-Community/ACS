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
import java.util.logging.Logger;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingWorker;

import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;

/**
 * The frame of the application
 * 
 * @author acaproni
 *
 */
public class LogLevelFrame extends JFrame {

	private static final long serialVersionUID = -3602101707686435260L;

	// The content of the frame
	private LogLevelPanel panel=null;
	
	// The ACS component client
    private ComponentClient client=null;
    
    
    
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
        final String clientName = "LogLevel GUI";
	    final String managerLoc = (System.getProperty("ACS.manager") == null ? "" : System.getProperty("ACS.manager").trim() );

	    if( managerLoc.equals("") )
	    	throw new IllegalStateException("ACS.magager property not set!");

	    SwingWorker<ComponentClient, Void> worker = new SwingWorker<ComponentClient, Void>() {

			protected ComponentClient doInBackground() throws Exception {
				ComponentClient tmp;
			    try {
		            tmp = new ComponentClient(null, managerLoc, clientName);
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
				return tmp;
			}
	    	
	    };
	    worker.execute();

	    client = worker.get();
        contSvc=client.getContainerServices();
        logger = contSvc.getLogger();
        logger.log(AcsLogLevel.INFO,"Connected to ACS");
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

	    SwingWorker<Void, Void> worker = new SwingWorker<Void, Void>() {
			protected Void doInBackground() throws Exception {
				try {
		            if (client!=null)
		            	client.tearDown();
		        } catch (Exception e) {
		            throw e;
			    } finally {
			    	logger=null;
			    	client=null;
			    	contSvc=null;
			    }
			    return null;
			}
	    };
	    worker.execute();

	    try {
	    	worker.get(); 
	    } catch(Exception e) {
            System.err.println("Exception caught while releasing ACS: "+e.getMessage());
            e.printStackTrace(System.err);
	    }
	    
    }
}
