/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.demo.test;

import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;

import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;
import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;

import com.cosylab.acs.jms.ACSJMSMessageEntity;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.AcsEventSubscriber;
import alma.acsnc.EventDescription;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.corbaservice.CernAlarmServiceUtils;
import alma.alarmsystemdemo.Antenna;
import alma.alarmsystemdemo.AntennaHelper;
import alma.alarmsystemdemo.MF;
import alma.alarmsystemdemo.MFHelper;
import alma.alarmsystemdemo.Mount;
import alma.alarmsystemdemo.MountHelper;
import alma.alarmsystemdemo.PS;
import alma.alarmsystemdemo.PSHelper;


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
	
	private final Logger logger;
	private final AdvancedComponentClient m_client;
	private final ContainerServices m_contSvcs;
	private  AcsEventSubscriber<ACSJMSMessageEntity> subscriber;
	private final MyReceiver myReceiver;
	
	
	// A reference to the ASC (used only to be sure it is running)
	AlarmService alarmSvc=null;
	
	/**
	 * Constructor
	 */
	public DemoTest() throws Exception {
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("DemoTest",true);
		String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
                System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                System.exit(-1);
        }
       	m_client = new AdvancedComponentClient(logger, managerLoc,"TestSendLaserSources");
        m_contSvcs = m_client.getContainerServices();
        myReceiver = new MyReceiver();
        connectSrcChannel();
	}
	
	public void disconnect() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		subscriber.disconnect();
		subscriber=null;
		try {
			m_client.tearDown();
		} catch (Throwable t) {
			System.out.println("!!!! Error shutting down the ComponentClient: "+t.getMessage());
		}
	}
	
	/**
	 * Connect to the NC of the sources
	 *
	 */
	private void connectSrcChannel() {
		// Connect to the NC used by the sources
        try {
        	subscriber = m_contSvcs.createNotificationChannelSubscriber(
        							srcChName, alma.acsnc.ALARMSYSTEM_DOMAIN_NAME.value, ACSJMSMessageEntity.class);
        } catch (Exception e) {
        	logger.severe("Error instantiating the consumer: "+e.getMessage());
        	e.printStackTrace();
        	System.exit(-1);
        }
        try {
        	subscriber.addSubscription(myReceiver);
        } catch (Exception e) {
        	logger.severe("Error subscribing: "+e.getMessage());
        	e.printStackTrace();
        	System.exit(-1);
        }
		try {
			subscriber.startReceivingEvents();
		} catch (Exception e) {
        	logger.severe("Error in consumerReady: "+e.getMessage());
        	e.printStackTrace();
        	System.exit(-1);
        }
		System.out.println("Connected to source channel");
	}
	
	/**
	 * Get the AS component.
	 * The test does not need such component but this forces the AS to run.
	 * 
	 * @return If the reference to the AS is valid
	 */
	public boolean getAlarmServiceComponent() {
		if (alarmSvc!=null) {
			return true;
		}
		CernAlarmServiceUtils alarmUtils = new CernAlarmServiceUtils(m_contSvcs);
		try {
			alarmSvc = alarmUtils.getAlarmService();
		} catch (Exception ce) {
			System.out.println("Error getting AlarmService: "+ce.getMessage());
			ce.printStackTrace();
			return false;
		}
		if (alarmSvc==null) {
			System.out.println("The AlarmService component is null!");
			return false;
		}
		return true;
	}
	
	/**
	 * 1. Get the java components for the NODE reduction
	 *    - AlarmSystem (not needed but in this way we are sure it is running)
	 *    - PS
	 *    - Mount
	 *    - Antenna
	 *
	 * 2. Call the fult method of the PS (it will generate a chain of failures
	 *    up to the Antenna)
	 * 3. Call the terminate_fault method of the PS (that will generate
	 *    a second chain of failures)
	 */
	public void testJavaNR() {
		
		// Get PS
		PS ps;
		try {
			ps = PSHelper.narrow(m_contSvcs.getComponent("ALARM_SOURCE_PS"));
		} catch (Exception ce) {
			System.out.println("Error getting PS: "+ce.getMessage());
			ce.printStackTrace();
			return;
		}
		if (ps==null) {
			System.out.println("The PS component is null!");
			return;
		}
		// Get Mount
		Mount mount;
		try {
			mount = MountHelper.narrow(m_contSvcs.getComponent("ALARM_SOURCE_MOUNT"));
		} catch (Exception ce) {
			System.out.println("Error getting Mount: "+ce.getMessage());
			ce.printStackTrace();
			return;
		}
		if (mount==null) {
			System.out.println("The MOUNT component is null!");
			m_contSvcs.releaseComponent("ALARM_SOURCE_PS");
			return;
		}
		// Get the Antenna
		Antenna antenna;
		try {
			antenna = AntennaHelper.narrow(m_contSvcs.getComponent("ALARM_SOURCE_ANTENNA"));
		} catch (Exception ce) {
			System.out.println("Error getting Antenna: "+ce.getMessage());
			ce.printStackTrace();
			return;
		}
		if (antenna==null) {
			System.out.println("The ANTENNA component is null!");
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
		try {
			Thread.sleep(30000);
		} catch(InterruptedException ie) {}
	}
	
	/**
	 * 1. Get the component to test the Multiplicity reduction
	 * 2. call the fault and the terminate methods to activate the reduction
	 *    (the MR causes the generation of another alarm when the threshold is 
	 *    passed)
	 */
	public void testJavaMR() {
		// Get MF
		MF mf;
		try {
			mf = MFHelper.narrow(m_contSvcs.getComponent("MULTIPLE_FAILURES"));
		} catch (Exception ce) {
			System.out.println("Error getting MF: "+ce.getMessage());
			ce.printStackTrace();
			return;
		}
		if (mf==null) {
			System.out.println("The MF component is null!");
			return;
		}
 		// Call the fault of the MF
		mf.multiFault();
		
		try {
			Thread.sleep(30000);
		} catch(InterruptedException ie) {}
		// Call the terminate_fault
		mf.terminate_multiFault();
		try {
			Thread.sleep(30000);
		} catch(InterruptedException ie) {}
		// Release the components
		m_contSvcs.releaseComponent("MULTIPLE_FAILURES");
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
		} catch (Exception ce) {
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

// HSO: This method is not used and can probably be removed.
//	/**
//	 * Setup the listener for the sources
//	 *
//	 */
//	private void setupSourceListener() {
//		// Connect to the NC used by the sources
//	    try {
//	    	m_consumer = new Consumer(srcChName,alma.acsnc.ALARMSYSTEM_DOMAIN_NAME.value,m_contSvcs);
//	    } catch (AcsJException e) {
//	    	logger.severe("Error instantiating the consumer: "+e.getMessage());
//	    	e.printStackTrace();
//	    	System.exit(-1);
//	    }
//	    try {
//	    	m_consumer.addSubscription(com.cosylab.acs.jms.ACSJMSMessageEntity.class,this);
//	    } catch (Exception e) {
//	    	logger.severe("Error subscribing: "+e.getMessage());
//	    	e.printStackTrace();
//	    	System.exit(-1);
//	    }
//		try {
//			m_consumer.consumerReady();
//		} catch (Exception e) {
//	    	logger.severe("Error in consumerReady: "+e.getMessage());
//	    	e.printStackTrace();
//	    	System.exit(-1);
//	    }
//	}
	
	private static class MyReceiver implements AcsEventSubscriber.Callback<ACSJMSMessageEntity> {

		/**
		 * The method receives all the messages published in the NC by the sources
		 *  
		 * @param msg The message received from the NC
		 * @see alma.acs.nc.Consumer
		 */
		@Override
		public void receive(ACSJMSMessageEntity msg, EventDescription eventDescrip) {
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

		@Override
		public Class<ACSJMSMessageEntity> getEventType() {
			return ACSJMSMessageEntity.class;
		}
		
	}
	
	/**
	 * The starting point of the test
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			DemoTest test = new DemoTest();
			// Check if the ASC is running
			boolean alSvcRunning=test.getAlarmServiceComponent();
			if (!alSvcRunning) {
				System.out.println("The alarm service component is not running!");
				System.out.println("Test aborted.");
				return;
			}
			System.out.println("Invoking methods on java components (NODE REDUCTION)");
			test.testJavaNR();
			System.out.println("Invoking methods on java components (MULTIPLICITY REDUCTION)");
			test.testJavaMR();
			System.out.println("Invoking methods on C++ components");
			test.testCppComponents();
			// Disconnect from the source NC
			test.disconnect();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
}
