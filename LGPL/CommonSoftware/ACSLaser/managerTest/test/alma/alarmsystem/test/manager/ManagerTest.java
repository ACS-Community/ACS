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
import alma.alarmsystem.clients.source.SourceListener;

/**
 * Listen to alarms from sources and the alarm component.
 * The alarms are written to the stdout to be checked by tat.
 * 
 * @author acaproni
 *
 */
public class ManagerTest implements AlarmSelectionListener {
	
	/**
	 * ACS component client
	 */
	private AdvancedComponentClient client;
	
	/**
	 * The client to receive alarms from categories
	 */
	private CategoryClient categoryClient;
	
	/**
	 * ContainerServices
	 */
	private ContainerServices containerServices;
	
	/**
	 * The number of alarms received from the alarm service
	 */
	private volatile int alarmsReceived=0;
	
	/**
	 * The millisecs to wait for alarms
	 */
	private static final int WAIT_TIME=120000;
	
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
        System.out.println("ManagerTest ready to receive alarms");
	}
	
	public void close() {
		try {
			categoryClient.close();
		} catch (Exception e) {
			System.out.println("Exception while closing CategoryClient: "+e.getMessage());
			e.printStackTrace();
		}
		try {
			client.tearDown();
		} catch (Exception e) {
			System.out.println("Exception while closing ComponentClient: "+e.getMessage());
			e.printStackTrace();
		}
		System.out.println("ManagerTest closed");
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
		if (alarm.getAlarmId().equals("Manager:bilboContainer:1")) {
			alarmsReceived++;
		} else {
			System.out.println("Unknown alarm received: "+alarm.getAlarmId());
		}
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
		managertest.test();
	}
	
	public void test() {
		try {
			Thread.sleep(3000);
		} catch (InterruptedException e) {}
		// Wait for alarms
		System.out.println("Waiting");
		long timeout=System.currentTimeMillis()+WAIT_TIME;
		while (alarmsReceived<2 && System.currentTimeMillis()<=timeout) {
			try {
				Thread.sleep(250);
			} catch (InterruptedException ie) {
				continue;
			}
		}
		System.out.println("Closing; num of received alarms="+alarmsReceived);
		close();
		System.out.println("Done");
	}
}
