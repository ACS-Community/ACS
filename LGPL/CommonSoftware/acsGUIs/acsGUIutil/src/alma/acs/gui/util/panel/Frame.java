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
package alma.acs.gui.util.panel;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.lang.reflect.Constructor;
import java.util.logging.Logger;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JRootPane;
import javax.swing.SwingUtilities;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;

/**
 * The main frame of the application.
 * It is the main window showing the panel inside.
 * 
 * The panel must implement the IPanel interface and extend JComponent.
 * It can also be a JRootPane in order to define its own menu bar.
 * 
 * @author acaproni
 *
 */
public class Frame extends JFrame {
	
	public class FrameWindowListener extends WindowAdapter {
		
		/**
		 * Signal the panel that the application is going to terminate
		 * 
		 * @see WindowAdapter
		 */
		public void windowClosing(WindowEvent e) {
			close();
		}

	    /**
	     * Release the panel and terminate the application.
	     * The application has to terminate cleany without calling a 
	     * System.exit. If it is not the case it mean that the panel still 
	     * has some resource to release like a non daemon Thread or an open dialog
	     * or....
	     * 
	     * @see WindowAdapter
	     */
	    public void windowClosed(WindowEvent e) {
	    	panel=null;
	    	client=null;
	    	contSvc=null;
	    	logger=null;
	    	windowListener=null;
	    }
	}
	
	// The panel shown in the window
	private IPanel panel;
	
	// The ACS client
	private AdvancedComponentClient client=null;
	
	// The logger
	private Logger logger=null;
	
	// The ContainerServices
	private ContainerServices contSvc=null;
	
	private FrameWindowListener windowListener = new FrameWindowListener();
	
	/**
	 * Build the main window with the component inside.
	 * 
	 * The main window catches window events in order to be able to
	 * close the panel before closing.
	 *  
	 * @param params A string of paramters.
	 *               The first parameter is the component to show in the window.
	 * 
	 */
	public Frame(String[] params) throws PanelException {
		if (params==null || params.length<0) {
			throw new PanelException("Wrong params in constructor");
		}
		try {
			connectACSComponentClient(params[0]);
			panel = loadPanel(params[0]);
			initialize(panel);
		} catch (Throwable t) {
			throw new PanelException("Error connecting the ACS component client",t);
		}
		addWindowListener(windowListener);
		// Stert the panel
		try {
			panel.setACSContainerServices(contSvc);
			panel.start();
		} catch (Throwable t) {
			throw new PanelException("Error starting IPanel "+params[0],t);
		}
	}
	
	/**
	 * Init the GUI
	 * 
	 * @param pnl The IPanel to show in the GUI
	 */
	private void initialize(IPanel pnl) throws PanelException {
		if (pnl==null) {
			throw new IllegalArgumentException("Invalid null IPanel content");
		}
		if (!(pnl instanceof JComponent)) {
			throw new PanelException("The panel is not a JComponent");
		}
		if (pnl instanceof JRootPane) {
			setContentPane((JRootPane)pnl);
		} else {
			add((JComponent)pnl, BorderLayout.CENTER);
		}
		pack();
		setVisible(true);
	}
	
	/**
	 * Load the panel whose class name is in the parameter
	 * 
	 * @param className The class name of the poanel to be shown
	 *                  in the main window
	 * @return A reference to the panel
	 */
	private IPanel loadPanel(String className) throws PanelException {
		Thread t = Thread.currentThread();
		ClassLoader loader = t.getContextClassLoader();
		try {
			Class cl =loader.loadClass(className);
			Class[] classes = { JFrame.class };
			Constructor constructor = cl.getConstructor(classes);
			return (IPanel)constructor.newInstance((JFrame)this);
		} catch (Throwable throwable) {
			throwable.printStackTrace();
			throw new PanelException("Error building "+className,throwable);
		}
	}
	
	/**
	 * Print the usage message on the stdout
	 *
	 *@param cmd The name of the command
	 */
	public static void printUsage(String cmd) {
		System.out.println("USAGE:");
		System.out.println(cmd+" PanelClassName [params]");
		System.out.println("Show a window with the panel inside.");
		System.out.println("PanelClassName the name of the class implementing the panel");
		System.out.println("               to show in the window");
		System.out.println("params: a list of params to send to the panel, if present\n");
	}
	
	/**
	 * The starting point of the application: build the window
	 * and load the panel.
	 * 
	 * @param args A list of params.
	 *             The first parameter is the name of the panel
	 *             to load.
	 */
	public static void main(String[] args) {
		if (args.length==0) {
			throw new IllegalArgumentException("Panel class name missing in args");
		}
		class LaunchThread extends Thread {
			private String[] commandLineArgs;
			public LaunchThread(String[] arguments) {
				commandLineArgs=arguments;
			}
			public void run() {
				try {
					new Frame(commandLineArgs);
				} catch (Throwable t) {
					System.err.println(t.getMessage());
					t.printStackTrace(System.err);
					printUsage("PanelFrame");
					String msg = "<HTML><BODY>Error opening panel: <I>"+t.getMessage();
					msg+="</I><P>Is ACS running?";
					JOptionPane.showMessageDialog(null, msg, "Error opening panel", JOptionPane.ERROR_MESSAGE);
					System.exit(-1);
				}
			}
		};
		try {
			SwingUtilities.invokeAndWait(new LaunchThread(args));
		} catch (Throwable t) {
			System.err.println(t.getMessage());
			t.printStackTrace(System.err);
			printUsage("PanelFrame");
			System.exit(-1);
		}
	}
	
	/**
     * Connect to ACS as component client.
     * It connects the client and the logger.
     * 
     * @param className The name of the class of the panel to show in the frame
     * 
     * @throws Exception In case of failure connecting to ACS
     */
    private void connectACSComponentClient(String className) throws PanelException {
    	String[] tmp = className.split("\\.");
        String clientName=tmp[tmp.length-1];
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
            logger.log(AcsLogLevel.DEBUG,"Connected to ACS");
            setTitle(clientName);
	    } catch (Throwable t) {
            if (logger!=null) {
                logger.log(AcsLogLevel.ERROR,"Error connecting the simple client: "+t.getMessage());
            } else {
                System.err.println("Error connecting the simple client: "+t.getMessage());
            }
            client=null;
            logger=null;
            contSvc=null;
            throw new PanelException("Error connetting ACS client",t);
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
    
    /**
     * Release all the resources 
     * This is the last operation before closing
     */
    private void close() {
    	Thread t = new Thread(new Runnable() {
			public void run() {
				if (logger!=null) {
					logger.log(AcsLogLevel.DEBUG, "Closing");
				}
				try {
					panel.stop();
				} catch (Throwable t) {
					// Ignore exceptions while closing
					System.err.println("Exception caught while closing: "+t.getMessage());
					t.printStackTrace(System.err);
				}
				if (logger!=null) {
					logger.log(AcsLogLevel.DEBUG, "Releasing ACS client");
				}
				disconnectACSComponentClient();
				System.out.println("Done.");
				setVisible(false);
				setRootPane(null);
				removeWindowListener(windowListener);
				dispose();
			}
		});
		t.setDaemon(true);
		t.setName("acsGUIUtil.Frame.close");
		SwingUtilities.invokeLater(t);
    }
}
