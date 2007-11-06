package alma.alarmsystem.test.manager;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.logging.Logger;

import cern.laser.source.alarmsysteminterface.FaultState;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.clients.SourceClient;
import alma.alarmsystem.clients.category.AlarmView;
import alma.alarmsystem.clients.category.CategoryListener;
import alma.alarmsystem.clients.source.SourceListener;

/**
 * Listen to alarms from sources and the alarm component.
 * The alarms are written to the stdout to be checked by tat.
 * 
 * The container is killed here by a separate thread. 
 * The file containing the PID of the container is passed in the command line
 * 
 * @author acaproni
 *
 */
public class ManagerTest extends Thread implements SourceListener, CategoryListener {
	
	// ACS component client
	private AdvancedComponentClient client;
	
	// The client to receive alarms from categories
	private CategoryClient categoryClient;
	
	// The client to receive alarms from the sources
	private SourceClient sourceClient;
	
	// ContainerServices
	private ContainerServices containerServices;
	
	// The millisecs to wait for alarms
	private static final int WAIT_TIME=120000;
	
	// The name of the file containing the PID number of the
	// container to kill to trigger the alarm from the Manager
	private String containerPIDFileName;
	
	/**
	 * Constructor.
	 * 
	 * Connect as a component client.
	 * 
	 *  @param pidFileName The name of the file containing the PID of the
	 *                     container to kill
	 */
	public ManagerTest(String pidFileName) {
		if (pidFileName==null || pidFileName.length()==0) {
			throw new IllegalArgumentException("Invalid PID file name");
		}
		containerPIDFileName=pidFileName;
		// Get the logger
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("Manager",true);
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
        	categoryClient.connect();
        } catch (Exception e) {
        	System.err.println("Error connecting categoies: "+e.getMessage());
        	logger.log(AcsLogLevel.ERROR,"Source client connected");
        	return;
        }
        // Connect the listeners
        sourceClient.addAlarmListener(this);
        categoryClient.addAlarmListener(this);
        System.out.println("ManagerTest ready to receive alarms");
	}
	
	/**
	 * @see alma.alarmsystem.clients.CategoryClient
	 */
	public void alarmReceived(AlarmView alarm) {
		if (alarm==null) {
			System.out.println("ERROR: received a null alarm from category!");
			return;
		}
		StringBuffer str = new StringBuffer("Alarm from category received: <");
		str.append(alarm.alarmID);
		str.append("> priority: ");
		str.append(alarm.priority);
		System.out.println(str.toString());
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
		str.append(">");
		System.out.println(str.toString());
	}
	
	/**
	 * Kill the container whose PID number is written in the passed file
	 * 
	 * @param pidFile The name of the file containing the PID of the
	 *                container to kill
	 */
	public void killContainer() {
		if (containerPIDFileName==null || containerPIDFileName.length()==0) {
			throw new IllegalStateException("No PID file name availbale");
		}
		File f = new File(containerPIDFileName);
		if (!f.canRead()) {
			System.out.println("Impossible to read "+containerPIDFileName);
			return;
		}
		FileInputStream inF;
		try {
			inF=new FileInputStream(f);
		} catch (Exception e) {
			System.out.println("Error opening "+containerPIDFileName+": "+e.getMessage());
			return;
		}
		byte[] buffer = new byte[128];
		try {
			inF.read(buffer);
		} catch (IOException ioe) {
			System.out.println("Error reading "+containerPIDFileName+": "+ioe.getMessage());
		}
		String str = new String(buffer);
		int pid;
		
		try {
			pid = Integer.parseInt(str.trim());
		} catch (Exception e) {
			System.out.println("Error parsing the pid file "+containerPIDFileName+": "+e.getMessage());
			return;
		}
		String cmd = "kill -9 "+pid;
		Runtime runTime = Runtime.getRuntime();
		try {
			Process proc=runTime.exec(cmd);
			int t= proc.waitFor();
			if (t!=0) {
				throw new Exception("kill returned error "+t);
			}
		} catch (Exception e) {
			System.out.println("Error executing command '"+cmd+"': "+e.getMessage());
		}
	}

	/**
	 * Start the program and wait to leave the manager enough time to detect
	 * the crash of the container and send the alarm.
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		if (args==null || args.length!=1) {
			System.out.println("Pass the file with the PID of the container to kill in the command line");
		}
		ManagerTest managertest=new ManagerTest(args[0]);
		try {
			Thread.sleep(3000);
		} catch (InterruptedException e) {}
		managertest.killContainer();
		// Wait for alarms
		System.out.println("Waiting");
		try {
			Thread.sleep(WAIT_TIME);
		} catch (InterruptedException e) {}
	}
}
