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
 * @version $Id: SendTest.java,v 1.6 2007/11/09 18:44:38 hsommer Exp $
 * @since    
 */

package alma.lasersource.test;

import java.sql.Timestamp;
import java.util.Collection;
import java.util.Iterator;
import java.util.Properties;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Level;

import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;
import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.Consumer;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

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
	
	private volatile Consumer m_consumer;
	private static final String m_channelName = "CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES";
//	private ContainerServices m_contSvcs;
	
	private final int ITERATIONS = 10;
	
	// The number of the message received
	// Each FS has the FM and FM containing that number and the FC=num of msg
	// In this way we can check the integrity
	private int nMsgReceived;
	
	/**
	 * Alarm NC events are received in different threads than the tests run in.
	 * In order to let the test fail when there are receiver exceptions or validation errors, we can 
	 * communicate them in this variable.
	 */
	private volatile Object receiverError;
	
	// The FM and FF of a FaultState are composed of the following String
	// with the num of the message appended
	private String faultFamily="AlarmSource";
	private String faultMember ="ALARM_SOURCE_ANTENNA";
	
	
	public void setUp() throws Exception {
		super.setUp();
		nMsgReceived=0;

		try {
			final ContainerServices contSvcs = getContainerServices();
			assertNotNull("Error getting the ContainerServices",contSvcs);
			
			ACSAlarmSystemInterfaceFactory.init(contSvcs);
			
			// Check if the CERN AS is in use
			assertFalse("Using ACS implementation instead of CERN", ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem());

			m_logger.info("alarm system initialized.");

			// Due to some problem with NC libs, the Consumer creation hangs
			// when the second test gets executed.
			// Going back to a simple TestCase that uses a ComponentClient (instead of writing a ComponentClientTestCase)
			// would resolve this issue. 
			// In order to fix it, we just provoke a timeout so that the other tests can run afterwards.
			Future<Consumer> consumerCtorFuture = Executors.newSingleThreadExecutor(getContainerServices().getThreadFactory()).submit(
					new Callable<Consumer>() {
						public Consumer call() throws Exception {
							// Register this object as a consumer of ACSJMSMessageEntity events.
							return new Consumer(m_channelName, contSvcs);
						}
					});
			m_consumer = consumerCtorFuture.get(10, TimeUnit.SECONDS); // will throw a TimeoutException if the call hangs
			
			m_consumer.addSubscription(com.cosylab.acs.jms.ACSJMSMessageEntity.class, this);
			m_consumer.consumerReady();
			m_logger.info("NC consumer installed on channel " + m_channelName);
		} catch (Exception ex) {
			super.tearDown();
			throw ex;
		}
	}
	
	
	public void tearDown() throws Exception {
		m_logger.info("tearDown called.");
		m_consumer.disconnect();
//		m_consumer=null;
		ACSAlarmSystemInterfaceFactory.done();
		m_logger.info("tearDown: done clearing NC consumer and alarm factory.");
		super.tearDown();		
	}
	
	
	// Send one message to check if the message received 
	// from the NC is coherent
	public void testSend() throws Exception {
		m_logger.info("testSend() will send a single alarm message.");
		
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

			m_logger.info("alarm message is prepared and will be sent now. Then test will wait for 10 seconds.");

			alarmSource.push(fs);
			try {
				Thread.sleep(10000);
			} catch (Exception e) {}
		} catch (Exception e) {
			m_logger.log(Level.INFO, "Exception caught while pushing the alarm", e);
			throw e;
		}
		finally {
			assertNull(receiverError);
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
			m_logger.info("all alarm messages are prepared.");
			
			for (int t=0; t<ITERATIONS; t++) {
				m_logger.info("alarm message #" + t + " will be sent now.");
				alarmSource.push(faultStates[t]);
				try {
					Thread.sleep(10); // HSO: if alarm system needs 10 ms sleep, then this should be enforced in the alarm classes, not in the test!
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
		finally {
			assertNull(receiverError);
		}
	}
	
	/**
	 * The method receives all the messages published in the NC
	 * For each message received it checks if its content is right
	 * i.e. the name of the class, the member and the code contains the 
	 * number of the message in the sequence. In this way it also checks
	 * if the messages are received in the same order they were sent.
	 * The method also checks if all the messages have been received
	 * and prints a message if receives more messages then the messages 
	 * pushed
	 *  
	 * @param msg The message received from the NC
	 * @see alma.acs.nc.Consumer
	 */
	public void receive(com.cosylab.acs.jms.ACSJMSMessageEntity msg) {
		Collection faultStates;
		try {
			ASIMessage asiMsg = XMLMessageHelper.unmarshal(msg.text);
			faultStates = ASIMessageHelper.unmarshal(asiMsg);
		} catch (Exception e) {
			System.out.println("Exception caught while unmarshalling the msg "+e.getMessage());
			e.printStackTrace();
			receiverError = e;
			return;
		}
		Iterator iter = faultStates.iterator();
		while (iter.hasNext()) {
			FaultStateImpl fs = (FaultStateImpl)iter.next();
			if (!isValidFSMessage(fs,nMsgReceived)) {
				receiverError = "Invalid FaultState received as #" + nMsgReceived;
			}
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
	private boolean isValidFSMessage(FaultStateImpl fs, int num) {
		boolean res = fs.getFamily().equals(faultFamily+num);
		res = res && fs.getMember().equals(faultMember+num);
		res = res && fs.getCode()==num;
		return res;
	}

}
