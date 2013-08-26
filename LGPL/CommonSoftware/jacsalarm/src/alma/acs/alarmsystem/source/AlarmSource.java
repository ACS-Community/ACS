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
package alma.acs.alarmsystem.source;

import java.util.Properties;
import java.util.concurrent.TimeUnit;

/**
 * The AlarmSource interface offers a set of useful methods
 * to handle alarms from a source.
 * <P>
 * This class, on one side, allows the sending of alarms with
 * a shorter syntax compared to the original API and, 
 * on the other side, offers some common functionalities 
 * useful while sending alarms in ACS.
 * <P>
 * This class implements a guard in the sense that a active (terminated)
 * alarm is not sent to the alarm service if it has been already
 * activated (terminated).
 * <P>
 * To damp the effect of oscillation an active alarm is immediately forwarded
 * to the alarm service but the sending of a terminated alarm is delayed
 * of approximately one second to catch a reactivation in that period
 * of time. 
 * <P>
 * AlarmSource allows to queue the alarms instead of sending them immediately
 * by calling <code>queueAlarms(...)</code>. To stop queuing the alarms
 * and send them to the alarm service the method <code>flushAlarms()</code> 
 * must be executed.
 * The queuing of the alarms has the advantage that if an alarm is activated
 * and then deactivated during the queuing time, it is not sent to the
 * alarm service.
 * This functionalities can be useful for example when starting up a piece
 * of software connected to an hardware device. In that case it is quite 
 * common that before initialization some of the values returned by the 
 * device are wrong and can trigger the sending of alarms that will be cleared
 * only when the device is fully operative. 
 * With the queuing the operator does not receive this kind of (false)
 * alarms. It is left to developer the responsibility to enable/disable
 * the queuing at the right moment.
 * <BR>
 * If {@link AlarmSource#queueAlarms(long, TimeUnit)} is called twice,
 * the the new time interval is used to flush the queue and the old one is discarded.
 * <P>
 * Alarm sending can be inhibited by calling {@link AlarmSource#disableAlarms()}:
 * all the alarm events submitted after calling this method are discarded. 
 * To revert just call {@link AlarmSource#enableAlarms()}.
 * The inhibition of the alarms does not interfere with the queuing: alarms queued
 * before inhibit alarms remain queued and will be sent when flushing.
 * 
 * <P>
 * Life cycle:
 * {@link AlarmSource#start()} must be called before using methods of this
 * class.  {@link AlarmSource#tearDown()} must be called when finished 
 * using this class. 
 * 
 * @author acaproni
 */
public interface AlarmSource {

	/**
	 * Raise an alarm with the given triplet.
	 * <P>
	 * If the alarm is already active, it is not sent again 
	 * to the alarm service.
	 * 
	 * @param faultFamily The FaultFamily
	 * @param faultMember The FaultMember
	 * @param faultCode The FaultCode
	 */
	public void raiseAlarm(
			String faultFamily, 
			String faultMember, 
			int faultCode);
	
	/**
	 * Raise an alarm with the given triplet and a set of properties.
	 * <P>
	 * If the alarm is already active, it is not sent again 
	 * to the alarm service.
	 * 
	 * @param faultFamily The FaultFamily
	 * @param faultMember The FaultMember
	 * @param faultCode The FaultCode
	 * @param properties The user properties. 
	 *                   It can be <code>null</code> or empty.
	 */
	public void raiseAlarm(
			String faultFamily, 
			String faultMember, 
			int faultCode,
			Properties properties);
	
	/**
	 * Clear the alarm with the passed triplet.
	 * <P>
	 * If the alarm is already terminated, it is not sent again 
	 * to the alarm service.
	 * 	
	 * @param faultFamily The FaultFamily
	 * @param faultMember The FaultMember
	 * @param faultCode The FaultCode
	 */
	public void clearAlarm(
			String faultFamily, 
			String faultMember, 
			int faultCode);
	
	/**
	 * Send the alarm with the passed triplet and properties to the
	 * alarm service. The state activate/terminate of the alarm
	 * depends on the passed boolean.
	 * <P>
	 * This method is the most complete way to send an alarm.
	 * <P>
	 * If the alarm is already active, it is not sent again 
	 * to the alarm service.
	 * If the alarm is already terminated, it is not sent again 
	 * to the alarm service.
	 * 
	 * @param faultFamily The FaultFamily
	 * @param faultMember The FaultMember
	 * @param faultCode The FaultCode
	 * @param alarmProps The user properties. 
	 *                   It can be <code>null</code> or empty.
	 * @param active if <code>true</code> the alarm is activated
	 * 				 otherwise it is terminated.
	 */
	public void setAlarm(
			String faultFamily,
			String faultMember,
			int faultCode,
			Properties alarmProps,
			boolean active);
	
	/**
	 * Send the alarm with the passed triplet to the
	 * alarm service. The state activate/terminate of the alarm
	 * depends on the passed boolean.
	 * <P>
	 * This method is the most complete way to send an alarm.
	 * <P>
	 * If the alarm is already active, it is not sent again 
	 * to the alarm service.
	 * If the alarm is already terminated, it is not sent again 
	 * to the alarm service.
	 * 
	 * @param faultFamily The FaultFamily
	 * @param faultMember The FaultMember
	 * @param faultCode The FaultCode
	 * @param active if <code>true</code> the alarm is activated
	 * 				 otherwise it is terminated.
	 */
	public void setAlarm(
			String faultFamily,
			String faultMember,
			int faultCode,
			boolean active);
	
	/**
	 * Terminate all the active alarms.
	 */
	public void terminateAllAlarms();
	
	/**
	 * Start queuing the alarms.
	 * <P>
	 * The alarms are flushed when the passed time expires or flush gets called.
	 * <P>
	 * The purpose of the queuing is to avoid alarms flickering for example 
	 * during the initialization phase of a device where spurious states 
	 * could trigger the event of false alarms
	 * 
	 * @param delayTime The time to queue alarms
	 * @param unit The unit of the delay
	 */
	public void queueAlarms(long delayTime, TimeUnit unit);
	
	/**
	 * Start queuing the alarms.
	 * <P>
	 * The alarms are queued until <code>flushAlarms()</code> is executed.
	 * <P>
	 * The purpose of the queuing is to avoid alarms flickering for example 
	 * during the initialization phase of a device where spurious states 
	 * could trigger the event of false alarms.
	 * 
	 * @see AlarmSource#flushAlarms()
	 */
	public void queueAlarms();
	
	/**
	 * Flush the alarms queued and stop queuing.
	 * 
	 * @see {@link AlarmSource#queueAlarms()}
	 */
	public void flushAlarms();
	
	/**
	 * Disable the sending of alarms.
	 * <P>
	 * When the alarms are disabled, all the alarms submitted with
	 * raise, clear and set are discarded. This means that those alarms
	 * are immediately discarded. They are not queued, and will never arrive at the alarm service. 
	 * 
	 * @see AlarmSource#enableAlarms()
	 */
	public void disableAlarms();
	
	/**
	 * Enable the sending of alarms.
	 * <P>
	 * Better just "This method reverts the effect of a previous call to disableAlarms(),
	 * so that alarms will get processed again.
	 * <P>
	 * Alarms are enabled by default.
	 * 
	 * @see AlarmSource#disableAlarms()
	 */
	public void enableAlarms();
	
	/**
	 * Life-cycle: this method must be called before using this class.
	 */
	public void start();
	
	/**
	 * Life-cycle: tearDown must be called when terminated using this class
	 */
	public void tearDown();
}
