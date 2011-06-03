/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
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
package alma.acs.alarmsystem.test.alarmsource;

import java.util.concurrent.TimeUnit;

import alma.acs.alarmsystem.source.AlarmSource;
import alma.acs.alarmsystem.source.AlarmSourceImpl;
import alma.acs.component.client.ComponentClientTestCase;

/**
 * The test for the {@link AlarmSourceImpl}.
 * <P>
 * The test is done by junit and tat. tat is used for checking
 * if the alarms are published as expected.
 * <P>
 * <EM>Note</em>: in this module the only availabale alarm system
 *                is the ACS implementation so the published alarms 
 *                are identified by logs.
 *                
 * @author acaproni
 *
 */
public class AlarmSourceTest extends ComponentClientTestCase {
	
	/**
	 * The source to test.
	 */
	private AlarmSource alarmSource;
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public AlarmSourceTest() throws Exception {
		super(AlarmSourceTest.class.getName());
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		alarmSource= new AlarmSourceImpl(this.getContainerServices());
		assertNotNull(alarmSource);
		alarmSource.start();
	}

	@Override
	protected void tearDown() throws Exception {
		alarmSource.tearDown();
		super.tearDown();
	}
	
	/**
	 * Check the multiple activation of the same alarm 
	 * 
	 * @throws Exception
	 */
	public void testMultipleRaise() throws Exception {
		// Raise several times the same alarm to
		// check if it is sent only once
		for (int t=0; t<5; t++) {
			alarmSource.raiseAlarm("RaiseMultipleSend", "RaiseMultipleSendMember", 1);
		}
	}
	
	/**
	 * Check the multiple clearing of the same alarm
	 *  
	 * @throws Exception
	 */
	public void testMultipleClear() throws Exception {
		// Clear several times the same alarm to
		// check if it is sent only once
		for (int t=0; t<5; t++) {
			alarmSource.clearAlarm("ClearMultipleSend", "ClearMultipleSendMember", 1);
		}
		// Give the oscillation thread enough time to clear the alarm
		Thread.sleep(TimeUnit.SECONDS.toMillis(AlarmSourceImpl.ALARM_OSCILLATION_TIME*2));
	}
	
	/**
	 * Test if the source is limiting the oscillation of alarms
	 * <P>
	 * The test is done by activating and clearing the alarm several time inside 
	 * the {@link AlarmSourceImpl#ALARM_OSCILLATION_TIME} time interval.
	 * In this case, the activation of an alarm should inhibit its termination.
	 * <P>
	 * If this test is working well, there are only 2 alarms: activation
	 * at the beginning and the final clearing.
	 * 
	 * @throws Exception
	 */
	public void testOscillation() throws Exception {
		// Activate the alarm
		alarmSource.raiseAlarm("OscillationFF", "OscillationFM", 1);
		// now a series of activation and deactivation that must not
		// produce any alarm to be sent to the alarm service
		for (int t=0; t<10; t++) {
			alarmSource.clearAlarm("OscillationFF", "OscillationFM", 1);
			Thread.sleep(250);
			alarmSource.raiseAlarm("OscillationFF", "OscillationFM", 1);
		}
		// As the last operation clear the alarm
		alarmSource.clearAlarm("OscillationFF", "OscillationFM", 1);
		// Give the oscillation thread enough time to clear the alarm
		Thread.sleep(TimeUnit.SECONDS.toMillis(AlarmSourceImpl.ALARM_OSCILLATION_TIME*2));
	}
	
	/**
	 * Check that no alarms are published when the source is disabled
	 */
	public void testDisable() throws Exception {
		// An alarm sent before, it must be published
		alarmSource.raiseAlarm("DisableFF", "DisableFM", 15);
		alarmSource.disableAlarms();
		for (int t=0; t<10; t++) {
			alarmSource.clearAlarm("DisableFF", "DisableFM", t);
			Thread.sleep(250);
			alarmSource.raiseAlarm("DisableFF", "DisableFM"+t, 20);
		}
		alarmSource.enableAlarms();
		// An alarm sent after, it must be published
		alarmSource.clearAlarm("DisableFF", "DisableFM", 15);
		// Give the oscillation thread enough time to clear the alarm
		Thread.sleep(TimeUnit.SECONDS.toMillis(AlarmSourceImpl.ALARM_OSCILLATION_TIME*2));
	}
	
	/**
	 * The alarm are queued and sent all together when queuing is disbaled
	 * 
	 * @throws Exception
	 */
	public void testQueueing() throws Exception {
		// An alarm sent before, it must be published
		alarmSource.raiseAlarm("QueueFF", "QueueFM", 15);
		System.out.println("Queueing alarms");
		alarmSource.queueAlarms();
		// The following alarms will be published after queueing
		for (int t=0; t<10; t++) {
			alarmSource.clearAlarm("QueueFF", "QueueFM", t);
			Thread.sleep(250);
			alarmSource.raiseAlarm("QueueFF", "QueueFM"+t, 20);
		}
		Thread.sleep(10000);
		System.out.println("Flushing alarms");
		alarmSource.flushAlarms();
		// An alarm sent after, it must be published
		alarmSource.clearAlarm("QueueFF", "QueueFM", 15);
		// Give the oscillation thread enough time to clear the alarm
		Thread.sleep(TimeUnit.SECONDS.toMillis(AlarmSourceImpl.ALARM_OSCILLATION_TIME*2));
	}
	
	/**
	 * The alarm are queued for 30 seconds 
	 * and sent all together when queuing is disbaled
	 * 
	 * @throws Exception
	 */
	public void testQueueingTimer() throws Exception {
		// An alarm sent before, it must be published
		alarmSource.raiseAlarm("QueueTimerFF", "QueueTimerFM", 15);
		System.out.println("Queueing alarms with timer");
		alarmSource.queueAlarms(30,TimeUnit.SECONDS);
		// The following alarms will be published after queueing
		for (int t=0; t<10; t++) {
			alarmSource.clearAlarm("QueueTimerFF", "QueueTimerFM", t);
			Thread.sleep(250);
			alarmSource.raiseAlarm("QueueTimerFF", "QueueTimerFM"+t, 20);
		}
		// An alarm sent after, it must be published
		alarmSource.clearAlarm("QueueTimerFF", "QueueTimerFM", 15);
		// Give the oscillation thread enough time to clear the alarm
		Thread.sleep(TimeUnit.SECONDS.toMillis(AlarmSourceImpl.ALARM_OSCILLATION_TIME*2));
		// Wait 30 seconds until the flush terminates
		System.out.println("Waiting for the flush to happen");
		Thread.sleep(TimeUnit.SECONDS.toMillis(30));
	}
	
	/**
	 * When queuing is activated we expect that an alarm that has been
	 * raised and cleared is not published at all
	 */
	public void testQueuingSingleAlarm() throws Exception {
		// An alarm sent before queuing: it must be published
		alarmSource.raiseAlarm("QueueFFSA", "QueueFMSA", 15);
		System.out.println("Queueing alarms");
		alarmSource.queueAlarms();
		// The following alarms will be published after queueing
		for (int t=0; t<10; t++) {
			alarmSource.raiseAlarm("QueueFFSA", "QueueFMSA", t);
			Thread.sleep(250);
		}
		for (int t=0; t<10; t++) {
			alarmSource.clearAlarm("QueueFFSA", "QueueFMSA", t);
			Thread.sleep(250);
		}
		Thread.sleep(10000);
		System.out.println("Flushing alarms");
		alarmSource.flushAlarms();
		// An alarm sent after, it must be published
		alarmSource.clearAlarm("QueueFFSA", "QueueFMSA", 15);
		// Give the oscillation thread enough time to clear the alarm
		Thread.sleep(TimeUnit.SECONDS.toMillis(AlarmSourceImpl.ALARM_OSCILLATION_TIME*2));
	}
}
