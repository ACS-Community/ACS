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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.TimeUnit;

import alma.acs.alarmsystem.source.AlarmQueue.AlarmToQueue;
import alma.acs.container.ContainerServicesBase;

/**
 * The implementation of {@link AlarmSource}.
 * <P>
 * 
 * 
 * @author acaproni
 *
 */
public class AlarmSourceImpl implements AlarmSource {
	
	/**
	 * The TimerTask to flush the queue of alarms 
	 * 
	 * @author acaproni
	 *
	 */
	private class QueueFlusher extends TimerTask {
		@Override
		public void run() {
			Thread d = new Thread(new Runnable() {
				public void run() {
					queuing=false;
					flushQueue();
				}
			},"AlarmSourceImpl.flushQueue");
			d.setDaemon(true);
			d.start();
		}
	}
	
	/**
	 * To limit the effect of oscillations, publishing of inactive alarms 
	 * is delayed of <code>ALARM_OSCILLATION_TIME</code> (in msecs)
	 * <P>
	 * If during this time interval, the alarm is activated again then 
	 * it will not be deactivated.
	 */
	private static final int ALARM_OSCILLATION_TIME=1000;
	
	/**
	 * The object to publish alarms
	 */
	private final AlarmSender alarmSender;
	
	/**
	 * The container services
	 */
	private final ContainerServicesBase containerServices;
	
	/**
	 * The alarms sent or terminated
	 */
	private final AlarmsMap alarms;

	/**
	 * The queue of alarms to be sent when the queuing will be disabled
	 */
	private final AlarmQueue queue = new AlarmQueue();
	
	/**
	 * Enable/disable the sending of alarms.
	 * <P>
	 * When the sending is disabled, new alarms are discarded.
	 */
	private volatile boolean enabled=true;
	
	/**
	 * When <code>true</code> the alarm are queued instead of being sent 
	 * immediately.
	 */
	private volatile boolean queuing=false;
	
	/**
	 * The timer to flush the queue of alarms
	 */
	private Timer timer=null;
	
	/**
	 * The alarms to clean are initially stored in alarmToClean.
	 * <P> In fact the deletion of alarms is delayed of {@link AlarmSourceImpl#ALARM_OSCILLATION_TIME}
	 * to limit the effect of oscillation.
	 * <P> 
	 * The key is the ID of the alarm; the value is the instant when the alarm
	 * has been sent.
	 */
	private final Map<String, Long>alarmToClean=Collections.synchronizedMap(new HashMap<String, Long>());
	
	/**
	 * The {@link TimerTask} to flush the queue
	 */
	private QueueFlusher queueFlusher=null;
	
	/**
	 * Constructor 
	 * 
	 * @param containerServices The container services
	 */
	public AlarmSourceImpl(ContainerServicesBase containerServices) {
		if (containerServices==null) {
			throw new IllegalArgumentException("Invalid null ContainerServicesBase");
		}
		this.containerServices=containerServices;
		alarms=new AlarmsMap(containerServices.getThreadFactory(),containerServices.getLogger());
		alarms.start();
		alarmSender=new AlarmSender(containerServices);
		
	}

	@Override
	public void raiseAlarm(String faultFamily, String faultMember, int faultCode) {
		raiseAlarm(faultFamily,faultMember,faultCode,null);
	}

	@Override
	public void raiseAlarm(String faultFamily, String faultMember,
			int faultCode, Properties properties) {
		if (!enabled) {
			return;
		}
		if (queuing) {
			queue.add(faultFamily, faultMember, faultCode, properties, true);
			return;
		}
		String id= buildAlarmID(faultFamily, faultMember, faultCode);
		if (!alarms.raise(id)) {
			alarmSender.sendAlarm(faultFamily, faultMember, faultCode, properties, true);
		}
	}

	@Override
	public void clearAlarm(String faultFamily, String faultMember, int faultCode) {
		if (!enabled) {
			return;
		}
		if (queuing) {
			queue.add(faultFamily, faultMember, faultCode, null, false);
			return;
		}
		String id= buildAlarmID(faultFamily, faultMember, faultCode);
		if (!alarms.clear(id)) {
			alarmSender.sendAlarm(faultFamily, faultMember, faultCode, false);
		}
	}

	@Override
	public void setAlarm(String faultFamily, String faultMember, int faultCode,
			Properties alarmProps, boolean active) {
		if (active) {
			raiseAlarm(faultFamily,faultMember,faultCode,alarmProps);
		} else {
			clearAlarm(faultFamily,faultMember,faultCode);
		}
	}
	
	@Override
	public void setAlarm(String faultFamily, String faultMember, int faultCode,
			boolean active) {
		setAlarm(faultFamily, faultMember,faultCode,null,active);
	}

	@Override
	public void terminateAllAlarms() {
		Collection<String> alarmsToTerminate = alarms.getActiveAlarms();
		for (String ID: alarmsToTerminate) {
			String[] strs=ID.split(":");
			clearAlarm(strs[0], strs[1], Integer.parseInt(strs[2]));
		}
	}

	@Override
	public void queueAlarms(long delayTime) {
		if (timer==null) {
			timer = new Timer("AlarmSourceImpl.Timer", true);
			queueFlusher= new QueueFlusher();
		}
		timer.schedule(queueFlusher, delayTime);
	}

	@Override
	public void queueAlarms(long delayTime, TimeUnit unit) {
		queueAlarms(unit.toMillis(delayTime));
	}

	@Override
	public void queueAlarms() {
		queuing=true;
	}

	@Override
	public void flushAlarms() {
		queuing=false;
		if (timer!=null) {
			timer.purge();
		}
		flushQueue();
	}

	@Override
	public void disableAlarms() {
		enabled=false;
	}

	@Override
	public void enableAlarms() {
		enabled=true;
	}
	
	/**
	 * Build the alarm ID from the triplet.
	 * <P>
	 * This method ensures that all the triplet are built in the right and consistent
	 * way between the different calls. This is particularly important given
	 * that the ID is the key used in the map.
	 * 
	 * @param faultFamily The fault family
	 * @param faultMember The fault member
	 * @param faultCode The fault code
	 * @return The ID of the alarm
	 */
	private String buildAlarmID(String faultFamily, String faultMember, int faultCode) {
		return faultFamily+":"+faultMember+":"+faultCode;
	}
	
	/**
	 * Send the alarms in the queue
	 */
	private void flushQueue() {
		AlarmToQueue[] temp = new AlarmToQueue[queue.size()];
		queue.toArray(temp);
		for (AlarmToQueue alarm: temp) {
			setAlarm(alarm.faultFamily, alarm.faultMember, alarm.faultCode, alarm.getProperties(), alarm.active);
		}
		queue.clear();
	}
	
	@Override
	public void tearDown() {
		alarmSender.close();
		if (timer!=null) {
			timer.cancel();
		}
		alarms.shutdown();
		queue.clear();
	}
}
