package alma.demo.test;

import java.util.logging.Logger;
import java.util.Collection;
import java.util.Iterator;

import com.cosylab.acs.jms.ACSJMSMessageEntity;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerException;
import alma.acs.container.ContainerServices;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.Consumer;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.AlarmServiceHelper;
import alma.alarmsystemdemo.Antenna;
import alma.alarmsystemdemo.AntennaHelper;
import alma.alarmsystemdemo.AntennaPOA;
import alma.alarmsystemdemo.Mount;
import alma.alarmsystemdemo.MountHelper;
import alma.alarmsystemdemo.PS;
import alma.alarmsystemdemo.PSHelper;
import alma.acs.exceptions.AcsJException;

import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;

import alma.acs.alarmsystem.binding.ACSLaserFaultStateImpl;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;


/**
 * Check the sending of the messages done by the java and C++ components.
 * The check is done getting the components and invoking the fault and terminate_fault 
 * method.
 * The generated output is checked for correctness by TAT
 * 
 * NOTE: the reduction operated by the AS is also checked because it consists of a couple
 * of booleans in the messages published by the service.
 * 
 * @author acaproni
 *
 */
public class DemoTest {
	/** 
	 * Actually there is only one channel used by all sources
	 * to publish alarms
	 **/ 
	private static final String srcChName = "CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES";
	
	Logger logger;
	private AdvancedComponentClient m_client;
	private ContainerServices m_contSvcs;
	private Consumer m_consumer = null;
	/**
	 * Constructor
	 *
	 */
	public DemoTest() {
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("DemoTest",true);
		String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
                System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                System.exit(-1);
        }
        try {
        	m_client = new AdvancedComponentClient(logger,managerLoc,"TestSendLaserSources");
        } catch (Exception e) {
        	System.out.println("Error creating the AdvancedComponentClient: "+e.getMessage());
        	e.printStackTrace();
        	System.exit(-1);
        }
        m_contSvcs=m_client.getContainerServices();
	}
	
	/**
	 * 1. Get the java components
	 *    - AlarmSystem (not needed but in this way we are sure it is running)
	 *    - PS
	 *    - Mount
	 *    - Antenna
	 *
	 * Call the fult method of the PS (it will generate a chain of failures
	 * up to the Antenna)
	 * Then it call the terminate_fault method of the PS (that will generate
	 * a second chain of failures)
	 */
	public void testJavaComponents() {
		// Get the AlarmService
		AlarmService alarmSvc;
		try {
			alarmSvc = AlarmServiceHelper.narrow(m_contSvcs.getComponent("AlarmService"));
		} catch (ContainerException ce) {
			System.out.println("Error getting AlarmService: "+ce.getMessage());
			ce.printStackTrace();
			return;
		}
		if (alarmSvc==null) {
			System.out.println("The AlarmService component is null!");
			return;
		}
		// Get PS
		PS ps;
		try {
			ps = PSHelper.narrow(m_contSvcs.getComponent("ALARM_SOURCE_PS"));
		} catch (ContainerException ce) {
			System.out.println("Error getting PS: "+ce.getMessage());
			ce.printStackTrace();
			return;
		}
		if (ps==null) {
			System.out.println("The PS component is null!");
			m_contSvcs.releaseComponent("AlarmService");
			return;
		}
		// Get Mount
		Mount mount;
		try {
			mount = MountHelper.narrow(m_contSvcs.getComponent("ALARM_SOURCE_MOUNT"));
		} catch (ContainerException ce) {
			System.out.println("Error getting Mount: "+ce.getMessage());
			ce.printStackTrace();
			return;
		}
		if (mount==null) {
			System.out.println("The MOUNT component is null!");
			m_contSvcs.releaseComponent("AlarmService");
			m_contSvcs.releaseComponent("ALARM_SOURCE_PS");
			return;
		}
		// Get the Antenna
		Antenna antenna;
		try {
			antenna = AntennaHelper.narrow(m_contSvcs.getComponent("ALARM_SOURCE_ANTENNA"));
		} catch (ContainerException ce) {
			System.out.println("Error getting Antenna: "+ce.getMessage());
			ce.printStackTrace();
			return;
		}
		if (antenna==null) {
			System.out.println("The ANTENNA component is null!");
			m_contSvcs.releaseComponent("AlarmService");
			m_contSvcs.releaseComponent("ALARM_SOURCE_PS");
			m_contSvcs.releaseComponent("ALARM_SOURCE_MOUNT");
			return;
		}
 		// Call the fault of the PS
		ps.faultPS();
		try {
			Thread.sleep(30000);
		} catch(InterruptedException ie) {}
		// Call the terminate_fault
		ps.terminate_faultPS();
		try {
			Thread.sleep(30000);
		} catch(InterruptedException ie) {}
		// Release the components
		m_contSvcs.releaseComponent("ALARM_SOURCE_ANTENNA");
		m_contSvcs.releaseComponent("ALARM_SOURCE_MOUNT");
		m_contSvcs.releaseComponent("ALARM_SOURCE_PS");
		m_contSvcs.releaseComponent("AlarmService");
		try {
			Thread.sleep(30000);
		} catch(InterruptedException ie) {}
	}
	
	/**
	 * Get the C++ Mount component and invoke the fault and terminate fault method
	 *
	 */
	public void testCppComponents() {
		// Get Mount
		Mount mount;
		try {
			mount = MountHelper.narrow(m_contSvcs.getComponent("ALARM_SOURCE_MOUNTCPP"));
		} catch (ContainerException ce) {
			System.out.println("Error getting Mount: "+ce.getMessage());
			ce.printStackTrace();
			return;
		}
		if (mount==null) {
			System.out.println("Got a null C++ MOUNT component!");
			return;
		}
		// Call the fault of the Mount
		mount.faultMount();
		try {
			Thread.sleep(30000);
		} catch(InterruptedException ie) {}
		// Call the terminate_fault of the Mount
		mount.terminate_faultMount();
		try {
			Thread.sleep(30000);
		} catch(InterruptedException ie) {}
		// Release the component
		m_contSvcs.releaseComponent("ALARM_SOURCE_MOUNTCPP");
		try {
			Thread.sleep(30000);
		} catch(InterruptedException ie) {}
	}
	
	/**
	 * Setup the listener for the sources
	 *
	 */
	private void setupSourceListener() {
		// Connect to the NC used by the sources
	    try {
	    	m_consumer = new Consumer(srcChName,m_contSvcs);
	    } catch (AcsJException e) {
	    	logger.severe("Error instantiating the consumer: "+e.getMessage());
	    	e.printStackTrace();
	    	System.exit(-1);
	    }
	    try {
	    	m_consumer.addSubscription(com.cosylab.acs.jms.ACSJMSMessageEntity.class,this);
	    } catch (Exception e) {
	    	logger.severe("Error subscribing: "+e.getMessage());
	    	e.printStackTrace();
	    	System.exit(-1);
	    }
		try {
			m_consumer.consumerReady();
		} catch (Exception e) {
	    	logger.severe("Error in consumerReady: "+e.getMessage());
	    	e.printStackTrace();
	    	System.exit(-1);
	    }
	}
	
	/**
	 * The method recives all the messages published in the NC by the sources
	 * 
	 *  
	 * @param msg The message received from the NC
	 * @see alma.acs.nc.Consumer
	 */
	public synchronized void receive(ACSJMSMessageEntity msg) {
		logger.fine("Msg received");
		ASIMessage asiMsg;
		Collection faultStates;
		try {
			asiMsg = XMLMessageHelper.unmarshal(msg.text);
			faultStates = ASIMessageHelper.unmarshal(asiMsg);
		} catch (Exception e) {
			System.out.println("Exception caught while unmarshalling the msg "+e.getMessage());
			e.printStackTrace();
			return;
		}
		Iterator iter = faultStates.iterator();
		while (iter.hasNext()) {
			FaultStateImpl fs = (FaultStateImpl)iter.next();
			StringBuilder str = new StringBuilder("Alarm message received from source: <");
			str.append(fs.getFamily());
			str.append(",");
			str.append(fs.getMember());
			str.append(",");
			str.append(fs.getCode());
			str.append(">");
			str.append(" Status: ");
			str.append(fs.getDescriptor());
			System.out.println(str.toString());
		}
	}
	
	/**
	 * The starting point of the test
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		DemoTest test = new DemoTest();
		System.out.println("Invoking methods on java components");
		test.testJavaComponents();
		System.out.println("Invoking methods on C++ components");
		test.testCppComponents();
	}
}
