package alma.acs.lasercore.test.reductions;

import java.sql.Timestamp;

import cern.laser.source.alarmsysteminterface.FaultState;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.lasercore.test.stress.CategoryClient;
import alma.acs.lasercore.test.stress.category.AlarmView;
import alma.acs.lasercore.test.stress.category.CategoryListener;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * Check the functioning of RR with regular expressions / wildcards
 * 
 * @author acaproni
 *
 */
public class RRWithRegExp extends ComponentClientTestCase implements CategoryListener{
	
	/**
	 * The category client to listen to alarms
	 */
	private CategoryClient categoryClient;
	
	/**
	 * The alarm source
	 */
	private ACSAlarmSystemInterface alarmSource;
	
	/**
	 * The number of received alarms
	 */
	private int receivedAlarms;

	/**
	 * Constructor 
	 * 
	 * @param name The name of the test
	 * @throws Exception
	 */
	public RRWithRegExp() throws Exception {
		super(RRWithRegExp.class.getName());
		System.out.println("Test built");
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		receivedAlarms=0;
		categoryClient = new CategoryClient(this.getContainerServices());
		assertNotNull(categoryClient);
		categoryClient.connect();
		categoryClient.addAlarmListener(this);
		alarmSource = ACSAlarmSystemInterfaceFactory.createSource();
		assertNotNull(alarmSource);
		System.out.println("setUp done");
	}

	@Override
	protected void tearDown() throws Exception {
		ACSAlarmSystemInterfaceFactory.done();
		categoryClient.close();
		super.tearDown();
		System.out.println("tearDown done");
	}

	/**
	 * @param alarm
	 * @see {@link AlarmSelectionListener}
	 */
	@Override
	public void alarmReceived(AlarmView alarm) {
		System.out.print("Alarm received: "+alarm.alarmID+" active="+alarm.active);
		System.out.print(", nodeC="+alarm.nodeChild);
		System.out.print(", nodeP="+alarm.nodeParent);
		System.out.print(", multiC="+alarm.multiplicityChild);
		System.out.println(", multiP="+alarm.multiplicityParent);
		if (alarm.alarmID.startsWith("MF_REGEXP") || alarm.alarmID.startsWith("NODE_REGEXP")) {
			receivedAlarms++;
		}
	}

	/**
	 * Test the MULTIPLICITY and NODE reduction 
	 * 
	 * <B>Note</B>: initially I wrote 2 test for distinguishing between
	 * 				NODe and MULTILPICITY but there is something going wrong 
	 * 				when the alarms for the second tests are published: SimpleSupplier.publishEvent 
	 * 				hangs probably due to some misuse of static variables.
	 * 				I have decided to condesate the 2 tests in only one.
	 * 
	 * @TODO Split this test in two test when the problem described upon has been fixed.
	 * 
	 * @throws Exception
	 */
	public void testReductions() throws Exception {
		System.out.println("testMultiplicity");
		// Send the alarms to trigger the reduction
		sendAlarm("MF_REGEXP", "REGEXP1", 0, true);
		sendAlarm("MF_REGEXP", "REGEXP2", 0, true);
		sendAlarm("MF_REGEXP", "REGEXP3", 0, true);
		sendAlarm("MF_REGEXP", "REGEXP4", 0, true);
		sendAlarm("MF_REGEXP", "REGEXP5", 0, true);
		// Give time for the reduced alarm to arrive
		try {
			Thread.sleep(10000);
		} catch (InterruptedException i) {}
		// Clear the alarms
		sendAlarm("MF_REGEXP", "REGEXP1", 0, false);
		sendAlarm("MF_REGEXP", "REGEXP2", 0, false);
		sendAlarm("MF_REGEXP", "REGEXP3", 0, false);
		sendAlarm("MF_REGEXP", "REGEXP4", 0, false);
		sendAlarm("MF_REGEXP", "REGEXP5", 0, false);
		// Give time for the alarms to arrive
		try {
			Thread.sleep(10000);
		} catch (InterruptedException i) {}
		
		////////////////////////////////////////////////////////////////////
		System.out.println("testNode");
		sendAlarm("NODE_REGEXP", "REGEXP_NODE1", 1, true);
		sendAlarm("NODE_REGEXP", "REGEXP_NODE_2", 1, true);
		sendAlarm("NODE_REGEXP", "REGEXP_NODE_3", 1, true);
		sendAlarm("NODE_REGEXP", "REGEXP_NODE_4", 1, true);
		// Give time for the reduced alarm to arrive
		try {
			Thread.sleep(10000);
		} catch (InterruptedException i) {}
		// Clear the alarms
		sendAlarm("NODE_REGEXP", "REGEXP_NODE1",  1, false);
		sendAlarm("NODE_REGEXP", "REGEXP_NODE_2", 1, false);
		sendAlarm("NODE_REGEXP", "REGEXP_NODE_3", 1, false);
		sendAlarm("NODE_REGEXP", "REGEXP_NODE_4", 1, false);
		// Give time for the alarms to arrive
		try {
			Thread.sleep(10000);
		} catch (InterruptedException i) {}
	}

	/**
	 * Send an alarm
	 * 
	 * @param FF The fault family
	 * @param FM The fault Member
	 * @param FC The fault code
	 * @param active The state active/terminate
	 */
	private void sendAlarm(String FF, String FM, int FC, boolean active) throws Exception {
		ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(FF, FM, FC);
		assertNotNull(fs);
		if (active) {
			fs.setDescriptor(FaultState.ACTIVE);
		} else {
			fs.setDescriptor(FaultState.TERMINATE);
		}
		fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));

		alarmSource.push(fs);
		m_logger.info("Alarm sent <"+FF+", "+FM+", "+FC+"> "+fs.getDescriptor());
	}
	
}
