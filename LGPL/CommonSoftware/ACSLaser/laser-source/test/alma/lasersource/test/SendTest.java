/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
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
 * @author  almadev   
 * @version $Id: SendTest.java,v 1.5 2007/11/07 10:24:56 acaproni Exp $
 * @since    
 */

package alma.lasersource.test;

import org.omg.CORBA.ORB;

import alma.acs.alarmsystem.binding.ACSLaserFaultStateImpl;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.source.ACSAlarmSystemInterface;

import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;


import alma.acs.logging.ClientLogManager;

import java.util.Properties;
import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;

import java.sql.Timestamp;

import alma.acs.nc.Consumer;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;

import cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl;

import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;

/**
 * A class to test the sources.
 * It is composed of 2 tests:
 *  1- send an alarm
 *  2- stress test that sends several alarms
 *  
 * Each message is checked for integrity
 *
 */
public class SendTest extends ComponentClientTestCase {
	
	public SendTest() throws Exception {
		super("SendTest");
	}
	
	private Consumer m_consumer = null;
	private String m_channelName = "CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES";
	private ContainerServices m_contSvcs;
	
	private final int ITERATIONS=20000;
	
	// The number of the message received
	// Each FS has the FM and FM containing that number and the FC=num of msg
	// In this way we can check the integrity
	private int nMsgReceived;
	
	// The FM and FF of a FaultState are composed of the following String
	// with the num of the message appended
	private String faultFamily="AlarmSource";
	private String faultMember ="ALARM_SOURCE_ANTENNA";
	
	
	public void setUp() throws Exception {
		super.setUp();
		nMsgReceived=0;

        m_contSvcs=super.getContainerServices();
        assertNotNull("Error getting the ContainerServices",m_contSvcs);
        ACSAlarmSystemInterfaceFactory.init(m_contSvcs);
        
        // Check if the CRN AS is in use
		assertFalse("Using ACS implementation instead of CERN",ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());
		
		m_consumer = new Consumer(m_channelName,m_contSvcs);
		assertNotNull("Error instantiating the Consumer",m_consumer);
		m_consumer.addSubscription(com.cosylab.acs.jms.ACSJMSMessageEntity.class,this);
		m_consumer.consumerReady();
	}
	
	public void tearDown() throws Exception {
		m_consumer.disconnect();
		m_consumer=null;
		ACSAlarmSystemInterfaceFactory.done();
		super.tearDown();
		
	}
	
	// Send one message to check if the message received 
	// from the NC is coherent
	public void testSend() throws Exception {
		
		int faultCode=0;
		String descriptor=ACSFaultState.ACTIVE;
		ACSAlarmSystemInterface alarmSource;
		try {
			alarmSource = ACSAlarmSystemInterfaceFactory.createSource();
			assertNotNull("Error instantiating the source",alarmSource);
			ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(
					faultFamily+"0", faultMember+"0", faultCode);
			assertNotNull("Error instantiating the FS",fs);
			fs.setDescriptor(descriptor);
			fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

			Properties props = new Properties();
			props.setProperty(ACSFaultState.ASI_PREFIX_PROPERTY, "prefix");
			props.setProperty(ACSFaultState.ASI_SUFFIX_PROPERTY, "suffix");
			fs.setUserProperties(props);
			alarmSource.push(fs);
			try {
				Thread.sleep(10000);
			} catch (Exception e) {}
		} catch (Exception e) {
			
			System.out.println("Exception caught while pushing"+e.getMessage());
			System.out.println("Cause: "+e.getCause());
			e.printStackTrace();
			throw e;
		}
	}
	
	// Send 20K alarms
	public void testStress() throws Exception {
		ACSAlarmSystemInterface alarmSource;
		try {
			alarmSource = ACSAlarmSystemInterfaceFactory.createSource();
			assertNotNull("Error instantiating the source",alarmSource);
			ACSFaultState[] faultStates = new ACSFaultState[ITERATIONS];
			for (int t=0; t<ITERATIONS; t++) {
				faultStates[t]= ACSAlarmSystemInterfaceFactory.createFaultState(
						faultFamily+t, faultMember+t, t);
				assertNotNull("Error instantiating a fault state",faultStates[t]);
				if (t%2==0) {
					faultStates[t].setDescriptor(ACSFaultState.ACTIVE);
				} else {
					faultStates[t].setDescriptor(ACSFaultState.TERMINATE);
				}
				Properties props = new Properties();
				props.setProperty(ACSFaultState.ASI_PREFIX_PROPERTY, "prefix");
				props.setProperty(ACSFaultState.ASI_SUFFIX_PROPERTY, "suffix");
				faultStates[t].setUserProperties(props);
				faultStates[t].setUserTimestamp(new Timestamp(System.currentTimeMillis()));
			}
			
			for (int t=0; t<ITERATIONS; t++) {
				alarmSource.push(faultStates[t]);
				try {
					Thread.sleep(10);
				} catch (Exception e) {}
			}
			
			try {
				Thread.sleep(10000);
			} catch (Exception e) {}
		} catch (Exception e) {
			
			System.out.println("Exception caught while pushing"+e.getMessage());
			System.out.println("Cause: "+e.getCause());
			e.printStackTrace();
			throw e;
		}
	}
	
	/**
	 * The method recives all the messages published in the NC
	 * For each message recived it check if its content is right
	 * i.e. the name of the class, the member and the code contains the 
	 * number of the message in the sequence. In this way it also checks
	 * if the messages are recived in the same order they were sent.
	 * The method also checks if all the messages have been received
	 * and prints a message if receives more messages then the messages 
	 * pushed
	 *  
	 * @param msg The message received from the NC
	 * @see alma.acs.nc.Consumer
	 */
	public void receive(com.cosylab.acs.jms.ACSJMSMessageEntity msg) {
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
			assertTrue("The message received is not coherent",validateMsg(fs,nMsgReceived));
			nMsgReceived++;
			if (nMsgReceived==ITERATIONS) {
				System.out.println("All alarms sent and received");
			} else if (nMsgReceived>ITERATIONS) {
				System.out.println("Received an alarm that has never been sent");
			}
		}
	}
	
	/**
	 * Check if the message is coherent with the number
	 * 
	 * @param fs The FS to check
	 * @param num The number of the message
	 * 
	 * @return true if the message is right
	 */
	private boolean validateMsg(FaultStateImpl fs, int num) {
		boolean res = fs.getFamily().equals(faultFamily+num);
		res = res && fs.getMember().equals(faultMember+num);
		res = res && fs.getCode()==num;
		return res;
	}

}
