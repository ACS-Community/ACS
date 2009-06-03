/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.alarmsystem.test.manager;

import java.util.logging.Logger;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.source.alarmsysteminterface.FaultState;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.clients.SourceClient;
import alma.alarmsystem.clients.source.SourceListener;

/**
 * Listen to alarms from sources and the alarm component.
 * The alarms are written to the stdout to be checked by tat.
 * 
 * @author acaproni
 *
 */
public class ManagerTest extends Thread implements SourceListener, AlarmSelectionListener {
	
	// ACS component client
	private AdvancedComponentClient client;
	
	// The client to receive alarms from categories
	private CategoryClient categoryClient;
	
	// The client to receive alarms from the sources
	private SourceClient sourceClient;
	
	// ContainerServices
	private ContainerServices containerServices;
	
	// The millisecs to wait for alarms
	private static final int WAIT_TIME=90000;
	
	/**
	 * Constructor.
	 * 
	 * Connect as a component client.
	 * 
	 *  @param pidFileName The name of the file containing the PID of the
	 *                     container to kill
	 */
	public ManagerTest() {
		// Get the logger
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("ManagerTest",true);
		if (logger==null) {
			System.out.println("The logger is null");
		}
		// Get the manager CORBA loc
		String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
        	System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
        	System.exit(-1);
        }
        // Connect as ACS client
        try {
        	client = new AdvancedComponentClient(logger,managerLoc,"ManagerTest");
        	logger.log(AcsLogLevel.DEBUG,"ACS client connected");
        } catch (Exception e) {
        	System.out.println("Error creating the AdvancedComponentClient: "+e.getMessage());
        	e.printStackTrace();
        	System.exit(-1);
        }
        // Get ContainerServices
        containerServices=client.getContainerServices();
        if (containerServices==null) {
        	System.out.println("ContainerServices is null!");
        	System.exit(-1);
        }
        // Connect the source client
        try {
        	sourceClient=new SourceClient(containerServices);
        	logger.log(AcsLogLevel.INFO,"Source client connected");
        } catch (Exception e) {
        	System.out.println("Exception caught while instantiating the source client: "+e.getMessage());
        	e.printStackTrace();
        	System.exit(-1);
        }
        try {
        	sourceClient.connect();
        } catch (Exception e) {
        	System.err.println("Error connecting sources: "+e.getMessage());
        	logger.log(AcsLogLevel.ERROR,"Source client connected");
        	return;
        }
        // Connect the category client
        try {
        	categoryClient=new CategoryClient(containerServices);
        	logger.fine("Category client connected");
        } catch (Exception e) {
        	System.out.println("Exception caught while instantiating the category client: "+e.getMessage());
        	e.printStackTrace();
        	System.exit(-1);
        }
        try {
        	categoryClient.connect(this);
        } catch (Exception e) {
        	System.err.println("Error connecting the CategoryClient: "+e.getMessage());
        	logger.log(AcsLogLevel.ERROR,"Source client connected",e);
        	System.exit(-1);
        }
        // Connect the source listeners
        sourceClient.addAlarmListener(this);
        System.out.println("ManagerTest ready to receive alarms");
	}
	
	public void close() {
		sourceClient.close();
		try {
			categoryClient.close();
		} catch (Exception e) {
			System.out.println("Exception while closing: "+e.getMessage());
			e.printStackTrace();
		}
	}
	
	/**
	 * @see AlarmSelectionListener
	 */
	@Override
	public void onAlarm(Alarm alarm) {
		if (alarm==null) {
			System.out.println("ERROR: received a null alarm from category!");
			return;
		}
		StringBuffer str = new StringBuffer("Alarm from category received: <");
		str.append(alarm.getAlarmId());
		str.append("> priority: ");
		str.append(alarm.getPriority());
		str.append(" active: ");
		str.append(alarm.getStatus().isActive());
		System.out.println(str.toString());
	}

	/**
	 * @see AlarmSelectionListener
	 */
	@Override
	public void onException(LaserSelectionException e) {
		System.out.println("Exception got from CategoryClient: "+e.getMessage());
		e.printStackTrace();
	}

	/**
	 * @see alma.alarmsystem.clients.SourceClient
	 */
	public void faultStateReceived(FaultState faultState) {
		if (faultState==null) {
			System.out.println("ERROR: received a null FaultState from source!");
			return;
		}
		StringBuffer str = new StringBuffer("Alarm from source received: <");
		str.append(faultState.getFamily());
		str.append(", ");
		str.append(faultState.getMember());
		str.append(", ");
		str.append(faultState.getCode());
		str.append("> ");
		str.append(faultState.getDescriptor());
		System.out.println(str.toString());
	}

	/**
	 * Start the program and wait to leave the manager enough time to detect
	 * the crash of the container and send the alarm.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		
		ManagerTest managertest=new ManagerTest();
		try {
			Thread.sleep(3000);
		} catch (InterruptedException e) {}
		// Wait for alarms
		System.out.println("Waiting");
		try {
			Thread.sleep(WAIT_TIME);
		} catch (InterruptedException e) {}
		System.out.println("Closing");
		managertest.close();
		System.out.println("Done");
	}

	/**
	 * @see alma.alarmsystem.clients.source.SourceListener#sourceXMLMsgReceived(java.lang.String)
	 */
	@Override
	public void sourceXMLMsgReceived(String asiMessage) {}
}
