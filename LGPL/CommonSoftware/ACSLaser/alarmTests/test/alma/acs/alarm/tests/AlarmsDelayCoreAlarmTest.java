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
package alma.acs.alarm.tests;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.sql.Timestamp;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import junit.framework.JUnit4TestAdapter;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import cern.laser.business.pojo.AlarmMessageProcessorImpl;
import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;
import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;

import com.cosylab.acs.jms.ACSJMSMessageEntity;
import com.cosylab.acs.jms.ACSJMSTextMessage;

import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.util.AcsLocations;
import alma.acscommon.ACS_NC_DOMAIN_ALARMSYSTEM;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.core.alarms.LaserCoreFaultState;
import alma.alarmsystem.core.alarms.LaserCoreFaultState.LaserCoreFaultCodes;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;

/**
 * Test if the alarm server send/clear an alarm when the difference between the actual
 * time and the timestamp of the alarms received by the sources differs more then a threashold.
 * <BR>
 * The alarm this test want to check is {@link LaserCoreFaultCodes#ALARMS_TOO_OLD}.
 * <P>
 * All other alarms received by {@link #onAlarm(Alarm)} that do not match with {@link LaserCoreFaultCodes#ALARMS_TOO_OLD}
 * are discarded. 
 * <P>
 * The test {@link #testAlarmsTooOld()} waits for a signal when the alarms are received in {@link #onAlarm(Alarm)}.
 * If the alarms are not received a timeout occurs and the relative assert fails.
 * 
 * @author acaproni
 * @since ACS 11.1
 */
public class AlarmsDelayCoreAlarmTest  extends ComponentClient implements AlarmSelectionListener {
	
	/**
	 * TODO: Check if this rule and the getMethodName() call in setUp() can be moved up to ComponentClient,
	 *      if that adds a runtime dependency on junit, and how bad that would be.
	 *      Probably we should add a class ComponentClientTestCaseJUnit4 that extends ComponentClient
	 *      and only adds this testname business.
	 */
	@Rule 
	public TestName testName = new TestName();
	
	/**
	 *  The category client
	 */
	private CategoryClient categoryClient;
	
	/**
	 *  Container services
	 */
	private ContainerServices contSvcs;
	
	/**
	 * The logger
	 */
	private Logger logger;
	
	/**
	 * The name of the source NCs
	 */
	private final String sourceChannelName="CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES";
	
	/**
	 * The channel to publish alarms
	 */
	private AcsEventPublisher<ACSJMSMessageEntity> sourceNC;
	
	/**
	 * The ID of the ALARMS_TOO_OLD alarm that we want to receive
	 */
	private final String tooOldAlarmId=LaserCoreFaultState.FaultFamily+":"+LaserCoreFaultState.FaultMember+":"+LaserCoreFaultCodes.ALARMS_TOO_OLD.faultCode;
	
	/** 
	 * The countdown to wait for the arrival of the active core alarm
	 */
	private CountDownLatch activeAlarmCL = new CountDownLatch(1);
	
	/** 
	 * The countdown to wait for the arrival of the active core alarm
	 */
	private CountDownLatch terminateAlarmCL = new CountDownLatch(1);
	
	/**
	 * Monitor if the activate core alarm has been received more then once
	 */
	private volatile boolean coreActivatedReceived=false;
	
	/**
	 * Monitor if the activate core alarm has been received more then once
	 */
	private volatile boolean coreTerminatedReceived=false;
	
	/**
	 * The source to send alarms
	 */
	private ACSAlarmSystemInterface alarmSource;
	
	/**
	 * For compatibility with JUnit3 based TATJUnitRunner
	 */
	public static junit.framework.Test suite() {
		return new JUnit4TestAdapter(AlarmsDelayCoreAlarmTest.class);
	}
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public AlarmsDelayCoreAlarmTest() throws Exception {
		super(null, AcsLocations.figureOutManagerLocation(), AlarmsDelayCoreAlarmTest.class.getSimpleName());
	}
	
	@After
	public void tearDown() throws Exception {
		categoryClient.close();
		super.tearDown();
	}
	
	@Before
	public void setUp() throws Exception {
		contSvcs = getContainerServices();
		assertThat("Invalid null container services",contSvcs,notNullValue());
		logger=contSvcs.getLogger();
		assertThat("Invalid null logger",logger,notNullValue());
		
		// Connect the categories
		categoryClient= new CategoryClient(contSvcs);
		assertThat("Invalid null CategoryClient",categoryClient,notNullValue());
		categoryClient.connect(this);
		
		alarmSource = ACSAlarmSystemInterfaceFactory.createSource();
		assertThat("THE source NC is  null!",alarmSource,notNullValue());
		
		// Connect to the source NC
		sourceNC=contSvcs.createNotificationChannelPublisher(sourceChannelName, ACS_NC_DOMAIN_ALARMSYSTEM.value, ACSJMSMessageEntity.class);
		assertThat("THE source NC is  null!",sourceNC,notNullValue());
		
	}
	
	/**
	 * Create a ASI message with a old timestamp and checks if the alarm server detects the error and publishes the alarm.
	 * <P>
	 * If the previous part passed, then generate another alarm with the correct timestamp to check if the server clears the alarm
	 * 
	 * @throws Exception
	 */
	@Test
	public void testAlarmsTooOld() throws Exception {
		// Set a alarm with a old time stamp
		long timestamp = System.currentTimeMillis()-AlarmMessageProcessorImpl.delayThreashold-5000;
		sendAlarm("TheFamily", "TheMember", 1, true, timestamp);
		// Leave the alarm server enough time to detect the problem and send the alarm
		logger.log(AcsLogLevel.DEBUG,"Waiting for the alarm server to set the core alarm");
		boolean timedOut=activeAlarmCL.await(AlarmMessageProcessorImpl.alarmTimestampsCheckInterval+60000, TimeUnit.MILLISECONDS);
		assertThat("Active core alarm NOT received", timedOut, is(true));
		// Set another alarm with a correct time stamp
		timestamp = System.currentTimeMillis();
		logger.log(AcsLogLevel.DEBUG,"Waiting for the alarm server to clear the core alarm");
		sendAlarm("TheFamily", "TheMember", 1, false, timestamp);
		// Leave the alarm server enough time to detect that the problem has been solved and clear the alarm
		timedOut=terminateAlarmCL.await(AlarmMessageProcessorImpl.alarmTimestampsCheckInterval+60000, TimeUnit.MILLISECONDS);
		assertThat("Terminate core alarm NOT received", timedOut, is(true));
	}
	
	/**
	 * Send a alarm to the alarm NC bypassing the ACS API because we want to simulate a delay!
	 * 
	 * @param faultFamily
	 * @param faultMember
	 * @param code
	 * @param active
	 * @param timeStamp
	 * @throws Exception
	 */
	private void sendAlarm(String faultFamily, String faultMember, int code, boolean active, long timeStamp) throws Exception {
		FaultState fs = new FaultStateImpl(faultFamily, faultMember, code);
		if (active) {
			fs.setDescriptor(FaultState.ACTIVE);
		} else {
			fs.setDescriptor(FaultState.TERMINATE);
		}
		Timestamp timestamp = new Timestamp(timeStamp);
		fs.setUserTimestamp(timestamp);
		
		Vector<FaultState> states = new Vector<FaultState>();
		states.add(fs);
		ASIMessage asiMessage = ASIMessageHelper.marshal(states);
		// Set the timestamp
		cern.laser.source.alarmsysteminterface.impl.message.Timestamp sourceTimestamp=new cern.laser.source.alarmsysteminterface.impl.message.Timestamp();
		sourceTimestamp.setSeconds(timeStamp/1000);
		sourceTimestamp.setMicroseconds(0);
		asiMessage.setSourceTimestamp(sourceTimestamp);
		// Set a fake hostname
		asiMessage.setSourceHostname("alma");
		asiMessage.setSourceName("ALARM_SYSTEM_SOURCES");
		ACSJMSTextMessage textMessage = new ACSJMSTextMessage(contSvcs);
		textMessage.setText(XMLMessageHelper.marshal(asiMessage));
		
		// Finally send the alarm
		sourceNC.publishEvent(textMessage.getEntity());
	}
	
	/**
	 * 
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onAlarm(cern.laser.client.data.Alarm)
	 */
	@Override
	public void onAlarm(Alarm alarm) {
		if (!tooOldAlarmId.equals(alarm.getAlarmId())) {
			// Only want to check for tooOldAlarmId
			return;
		}
		if (alarm.getStatus().isActive()) {
			if (!coreActivatedReceived) {
				activeAlarmCL.countDown();
				coreActivatedReceived=true;
			} else {
				logger.log(AcsLogLevel.ERROR,alarm.getAlarmId()+" already received");
			}	
		} else {
			if (!coreTerminatedReceived) {
				terminateAlarmCL.countDown();
				coreTerminatedReceived=true;
			} else {
				logger.log(AcsLogLevel.ERROR,alarm.getAlarmId()+" already received");
			}
		}
	}

	/**
	 * @see cern.laser.client.services.selection.AlarmSelectionListener#onException(cern.laser.client.services.selection.LaserSelectionException)
	 */
	@Override
	public void onException(LaserSelectionException e) {
		super.getContainerServices().getLogger().log(AcsLogLevel.ERROR, "Error from the alarm server", e);
	}
}
