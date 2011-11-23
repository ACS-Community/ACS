/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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
package alma.alarmsystem.clients.test;

import java.sql.Timestamp;
import java.util.Properties;
import java.util.Set;
import java.util.Vector;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.source.alarmsysteminterface.FaultState;
import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.clients.SourceClient;
import alma.alarmsystem.clients.source.SourceListener;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * Test transmission of user defined properties from a fault state to
 * clients.
 * <P>
 * The test is done by a couple of alarms with user properties and listening
 * from both the sources and categories notification channels.
 * <BR>
 * The checking is almost entirely delegated to tat by printing messages on the
 * stdout.
 * 
 * @author acaproni
 *
 */
public class UserPropsTest extends ComponentClientTestCase implements AlarmSelectionListener, SourceListener {
	
	/**
	 *  The categoryClient to test
	 */
	private CategoryClient categoryClient;
	
	/**
	 *  The source client
	 */
	private SourceClient sourceClient;
	
	/**
	 * The vector with the alarms received
	 */
	private Vector<Alarm> alarmsReceived;
	
	/**
	 *  Max number of seconds to wait for the messages
	 */
	private static final int MAX_TIMEOUT = 120;

	/**
	 * Constructor
	 */
	public UserPropsTest() throws Exception {
		super(UserPropsTest.class.getName());
	}
	
	/**
	 * @see extends ComponentClientTestCase
	 */
	protected void setUp() throws Exception {
		super.setUp();
		categoryClient = new CategoryClient(getContainerServices());
		assertNotNull(categoryClient);
		categoryClient.connect(this);
		// Instantiate and connect the source client
		sourceClient = new SourceClient(getContainerServices());
		assertNotNull(sourceClient);
		sourceClient.addAlarmListener(this);
		sourceClient.connect();
		alarmsReceived=new Vector<Alarm>();
	}
	
	/**
	 * @see extends ComponentClientTestCase
	 */
	protected void tearDown() throws Exception {
		// Close the source client
		if (sourceClient!=null) {
			sourceClient.close();
			sourceClient=null;
		}
		// Close the category client
		if (categoryClient!=null) {
			categoryClient.close();
			categoryClient=null;
		}
		alarmsReceived.clear();
		super.tearDown();
	}
	
	/**
	 * @see AlarmSelectionListener
	 */
	public void onAlarm(Alarm alarm) {
		synchronized (alarmsReceived) {
			alarmsReceived.add(alarm);
			
			if (alarm.getStatus().isActive()) {
				Properties props = alarm.getStatus().getUserProperties();
				StringBuilder tripletStr = new StringBuilder();
				tripletStr.append('<');
				tripletStr.append(alarm.getTriplet().getFaultFamily());
				tripletStr.append(", ");
				tripletStr.append(alarm.getTriplet().getFaultMember());
				tripletStr.append(", ");
				tripletStr.append(alarm.getTriplet().getFaultCode());
				tripletStr.append("> ");
				if (props!=null) {
					System.out.println(tripletStr.toString()+" size="+props.size());
					Set keys=props.keySet();
					for (Object key: keys) {
						System.out.println("\t"+tripletStr.toString()+" user property ["+(String)key+", "+props.getProperty((String)key)+"]");	
					}
				}
			}
			if (alarm.getTriplet().getFaultCode()==1 && alarm.getStatus().isActive()) {
				assertNotNull(alarm.getStatus().getUserProperties());
				assertEquals(2, alarm.getStatus().getUserProperties().size());
			} else if (alarm.getTriplet().getFaultCode()==2 && alarm.getStatus().isActive()) {
				assertNotNull(alarm.getStatus().getUserProperties());
				assertEquals(1, alarm.getStatus().getUserProperties().size());
			}
		}
	}
	
	/**
	 * @see AlarmSelectionListener
	 */
	public void onException(LaserSelectionException e) {
		System.err.println("onException: "+e.getMessage());
		e.printStackTrace(System.err);
	}
	
	/**
	 * Wait for the messages from the alarm system.
	 * 
	 * @param numOfMessages The number of messages to wait for
	 * @return true if all the messages are received
	 *         false in case of timeout (i.e. not all the messages received
	 *               in MAX_TIMEOUT seconds)
	 */
	private int waitForMessages(int numOfMessages) {
		long startTime = System.currentTimeMillis();
		long endTime = startTime+MAX_TIMEOUT*1000;
		while (alarmsReceived.size()<numOfMessages && System.currentTimeMillis()<=endTime) {
			try {
				Thread.sleep(250);
			} catch (Exception e) {}
		}
		return alarmsReceived.size();
	}
	
	/**
	 * Push an alarm
	 * 
	 * @param active If true the alarm is active
	 */
	private void send_alarm(String family, String member, int code, boolean active, Properties props) throws Exception {
		ACSAlarmSystemInterface alarmSource;
		alarmSource = ACSAlarmSystemInterfaceFactory.createSource(member);
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(family, member, code);
		if (active) {
			fs.setDescriptor(FaultState.ACTIVE);
		} else {
			fs.setDescriptor(FaultState.TERMINATE);
		}
		fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
		if (props!=null && props.size()>0) {
			fs.setUserProperties((Properties)props.clone());
		}
		alarmSource.push(fs);
	}
	
	/**
	 * Test if the properties set while sending an alarm arrive
	 * to the <code>CategoryClient</code>.
	 * 
	 * <P>
	 * The test is done by sending 2 active alarms with 2 properties and one
	 * properties respectively.
	 * The triplets of the two alarms differ only for the FC that will be
	 * used to check the correctness of the received properties.
	 * <BR>Checking is done by listening to sources and to source NCs.
	 * 
	 * @throws Exception
	 */
	public void testProperties() throws Exception {
		
		Properties userProperties = new Properties();
		userProperties.setProperty("TestProp1", "Value of test property 1");
		userProperties.setProperty("TestProp2", "Another property");
		
		send_alarm("TEST", "TEST_MEMBER1", 1, true, userProperties);
		
		userProperties.clear();
		userProperties.setProperty("PropertyKey", "One property only");
		send_alarm("TEST", "TEST_MEMBER1", 2, true, userProperties);
		
		userProperties.clear();
		send_alarm("TEST", "TEST_MEMBER1", 1, false, userProperties);
		send_alarm("TEST", "TEST_MEMBER1", 2, false, userProperties);
		
		waitForMessages(4);
	}

	/**
	 * @see alma.alarmsystem.clients.source.SourceListener#faultStateReceived(cern.laser.source.alarmsysteminterface.FaultState)
	 */
	@Override
	public void faultStateReceived(FaultState faultState) {
		if (faultState.getDescriptor().equals(FaultState.ACTIVE)) {
			StringBuilder str = new StringBuilder('<');
			str.append(faultState.getFamily());
			str.append(", ");
			str.append(faultState.getMember());
			str.append(", ");
			str.append(faultState.getCode());
			str.append("> ");
			System.out.println("Source FaultState <"+str.toString());
			if (faultState.getUserProperties()!=null) {
				System.out.println("\tSize of props of "+str.toString()+"= "+faultState.getUserProperties().size());
				Set<Object> keys = faultState.getUserProperties().keySet();
				for (Object key: keys) {
					System.out.println("\t"+str.toString()+" ["+key+", "+faultState.getUserProperties().getProperty((String)key)+"]");
				}
			}
			if (faultState.getCode()==1) {
				assertNotNull(faultState.getUserProperties());
				assertEquals(2, faultState.getUserProperties().size());
			} else if (faultState.getCode()==2) {
				assertNotNull(faultState.getUserProperties());
				assertEquals(1, faultState.getUserProperties().size());
			}
		}
		
	}

	/*
	 * @see alma.alarmsystem.clients.source.SourceListener#sourceXMLMsgReceived(java.lang.String)
	 */
	@Override
	public void sourceXMLMsgReceived(String asiMessage) {
		// Nothing to do here
	}
}
