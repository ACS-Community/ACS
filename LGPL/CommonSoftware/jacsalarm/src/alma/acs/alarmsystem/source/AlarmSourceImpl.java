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
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import alma.acs.alarmsystem.source.AlarmQueue.AlarmToQueue;
import alma.acs.concurrent.NamedThreadFactory;
import alma.acs.concurrent.ThreadLoopRunner;
import alma.acs.concurrent.ThreadLoopRunner.CancelableRunnable;
import alma.acs.concurrent.ThreadLoopRunner.ScheduleDelayMode;
import alma.acs.container.ContainerServicesBase;

/**
 * The implementation of {@link AlarmSource}.
 * <P>
 * Flushing of queued alarms is done by a thread to avoid blocking 
 * the caller if there are too many queued alarms.
 * There is possible critical race if the user executes a sequence
 * of queueAlarms()/flushAlarms(), it happens if the queueAlarms()
 * is called when the thread has not yet finished flushing
 * the alarms.
 * This is avoided by copying the alarms in {@link AlarmSourceImpl#queue}
 * in a temporary immutable vector. 
 * 
 * @author acaproni
 *
 */
public class AlarmSourceImpl implements AlarmSource {
	
	/**
	 * The task to flush the queue of alarms.
	 * @see AlarmSourceImpl#queuing
	 * @author acaproni
	 */
	private class QueueFlusherTask implements Runnable {
		// The local copy of the alarms to flush
		private final AlarmToQueue[] alarmsToFlush;
		
		/**
		 * Constructor
		 * 
		 * @param alarmsToFlush The alarms to flush
		 */
		public QueueFlusherTask(AlarmToQueue[] alarmsToFlush) {
			this.alarmsToFlush=alarmsToFlush;
		}
		
		@Override
		public void run() {
			for (AlarmToQueue alarm: alarmsToFlush) {
				setAlarm(alarm.faultFamily, alarm.faultMember, alarm.faultCode, alarm.getProperties(), alarm.active);
			}
		}
	}
	
	/**
	 * The loop to clear alarms on the alarm service, run every second.
	 * <P>
	 * The alarms to clear are inserted in {@link AlarmSourceImpl#alarmsToClean} and must be
	 * cleared if they are in the queue for more then {@link AlarmSourceImpl#ALARM_OSCILLATION_TIME}
	 * seconds.
	 * 
	 * @author acaproni
	 *
	 */
	private class OscillationTask extends CancelableRunnable {

		@Override
		public void run() {
			for (String key : alarmsToClean.keySet()) {
				if (shouldTerminate) {
					return;
				}
				Long timestamp=alarmsToClean.get(key);
				if (timestamp==null) {
					// The entry has been removed by another task
					continue;
				}
				if (System.currentTimeMillis()-TimeUnit.SECONDS.toMillis(ALARM_OSCILLATION_TIME)>timestamp) {
					internalAlarmClear(key);
				}
			}
		}
		
	}
	
	/**
	 * To limit the effect of oscillations, publishing of inactive alarms 
	 * is delayed by <code>ALARM_OSCILLATION_TIME</code> (in seconds)
	 * <P>
	 * If during this time interval the alarm is activated again then 
	 * its temporary deactivation will be skipped.
	 * <P>
	 * Visibility is public for testing purposes.
	 */
	public static final int ALARM_OSCILLATION_TIME=1;
	
	/**
	 * The object to publish alarms
	 */
	private final AlarmSender alarmSender;
	
	/**
	 * The container services
	 */
	private final ContainerServicesBase containerServices;
	
	/**
	 * The alarms sent or terminated.
	 * Using this queue should avoid sending the same information (raise or clear) several times in a row.
	 * Note that this is different from the purpose of {@link #alarmsToClean} which limits the alternation
	 * between raise and clear.
	 */
	private final AlarmsMap alarms;

	/**
	 * The queue of alarms to be sent when the queuing will be disabled`.
	 * @see #queuing
	 */
	private final AlarmQueue queue = new AlarmQueue();
	
	/**
	 * When <code>true</code> the alarms are queued instead of being sent 
	 * immediately.
	 */
	private volatile boolean queuing=false;
	
	/**
	 * Enable/disable the sending of alarms.
	 * <P>
	 * When the sending is disabled, new alarms are discarded.
	 */
	private volatile boolean enabled=true;
	
	/**
	 * The loop to limit clear-raise oscillations, by only clearing alarms that have been inactive for a certain time.
	 * @see #alarmsToClean
	 */
	private final ThreadLoopRunner oscillationLoop;
	
	/**
	 * The alarms to clean are initially stored in alarmsToClean.
	 * <P> In fact the deletion of alarms is delayed by {@link AlarmSourceImpl#ALARM_OSCILLATION_TIME}
	 * to limit the effect of oscillation.
	 * <P> 
	 * The key is the ID of the alarm; the value is the system time at which the alarm
	 * was cleared by the user.
	 */
	private final ConcurrentHashMap<String, Long> alarmsToClean = new ConcurrentHashMap<String, Long>();
	
	/**
	 * The executor to schedule the threads for flushing the queue
	 * after a given time interval.
	 *
	 * @see AlarmSource#queueAlarms(long, TimeUnit)
	 */
	private final ScheduledExecutorService scheduledExecutor;
	
	/**
	 * The future for the thread to flush the queue of alarms
	 * after a given time interval.
	 * <P>
	 * Only one thread can be scheduled by AlarmSource#queueAlarms(long, TimeUnit) 
	 */
	private ScheduledFuture<?> flusherFuture;
	
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
		alarmSender=new AlarmSender(containerServices);
		
		// Allocating this optional executor here is cheap enough, because no thread is created, see ThreadPoolExecutor#prestartAllCoreThreads()
		// The pool size is 1 because we can have only one thread at a time
		scheduledExecutor = Executors.newScheduledThreadPool(1, containerServices.getThreadFactory());
		
		oscillationLoop = new ThreadLoopRunner(
				new OscillationTask(), 
				ALARM_OSCILLATION_TIME, 
				TimeUnit.SECONDS, 
				containerServices.getThreadFactory(), 
				containerServices.getLogger(),
				"alarm_osci");
		
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
			synchronized (queue) {
				queue.add(faultFamily, faultMember, faultCode, properties, true);
			}
			return;
		}
		String id= buildAlarmID(faultFamily, faultMember, faultCode);
		alarmsToClean.remove(id);
		if (!alarms.raise(id)) {
			alarmSender.sendAlarm(faultFamily, faultMember, faultCode, properties, true);
		}
	}

	/**
	 * The alarm to clear is not sent directly to the alarm service.
	 * It is instead queued in {@link AlarmSourceImpl#alarmsToClean} and 
	 * will be cleared by the oscillation loop.
	 */
	@Override
	public void clearAlarm(String faultFamily, String faultMember, int faultCode) {
		if (!enabled) {
			return;
		}
		if (queuing) {
			synchronized (queue) {
				queue.add(faultFamily, faultMember, faultCode, null, false);
			}
			return;
		}
		String id= buildAlarmID(faultFamily, faultMember, faultCode);
		alarmsToClean.putIfAbsent(id, System.currentTimeMillis());
	}
	
	/**
	 * Clear an alarm by sending to the alarm service.
	 *  
	 * @param id The id of the alarm
	 */
	private void internalAlarmClear(String id) {
		if (id==null || id.isEmpty()) {
			throw new IllegalArgumentException("Invalid alarm ID received "+id);
		}
		
		String[] alarmMembers=id.split(":");
		if (alarmMembers.length!=3) {
			containerServices.getLogger().warning("Invalid alarm ID received "+id);
		}
		String faultFamily=alarmMembers[0];
		String faultMember=alarmMembers[1];
		String faultCode=alarmMembers[2];
		if (!alarms.clear(id)) {
			alarmSender.sendAlarm(faultFamily, faultMember, Integer.parseInt(faultCode), false);
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
	public synchronized void queueAlarms(long delayTime, TimeUnit unit) {
		queuing=true;
		
		// try to change the target run time, if the task hasn't been started.
		if (flusherFuture!=null && !flusherFuture.isDone()) {
			flusherFuture.cancel(false);
		}
		flusherFuture = scheduledExecutor.schedule(new Runnable() {
			public void run() {
				flushAlarms();
			}
		}, delayTime, unit);
	}

	@Override
	public void queueAlarms() {
		queuing = true;
	}

	/**
	 * {@inheritDoc}
	 * <p>
	 * This method gets called both directly for flushing alarms immediately, and also 
	 * delayed (new thread from {@link #queueAlarms(long, TimeUnit)}).
	 * <p>
	 * Implementation note: this method is synchronized and calls {@link #setAlarm(String, String, int, Properties, boolean)}
	 * asynchronously in order to not block access to this class for too long.
	 * 
	 * @see alma.acs.alarmsystem.source.AlarmSource#flushAlarms()
	 */
	@Override
	public synchronized void flushAlarms() {
		queuing=false;
		if (flusherFuture!=null) {
			flusherFuture.cancel(false);
		}
		AlarmToQueue[] temp;
		synchronized (queue) {
			if (queue.isEmpty()) {
				return;
			}
			temp = new AlarmToQueue[queue.size()];
			queue.values().toArray(temp);
			queue.clear();
		}
		
		ThreadFactory tf = new NamedThreadFactory(containerServices.getThreadFactory(), "AlarmQueueFlusher");
		tf.newThread(new QueueFlusherTask(temp)).start();
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
	 * Start the threads.
	 * @see #tearDown()
	 */
	public void start() {
		oscillationLoop.setDelayMode(ScheduleDelayMode.FIXED_DELAY);
		oscillationLoop.runLoop();
		alarms.start();
	}
	
	@Override
	public void tearDown() {
		enabled = false;
		
		// take care of alarms that the user queued on purpose
		scheduledExecutor.shutdown();
		flushAlarms();
		
		// take care of alarms that should be cleared but whose clearing got delayed
		for (String key : alarmsToClean.keySet()) {
			internalAlarmClear(key);
		}
		alarmsToClean.clear();

		boolean oscillationLoopShutdownOK;
		try {
			oscillationLoopShutdownOK = oscillationLoop.shutdown(1, TimeUnit.SECONDS);
		} catch (InterruptedException ie) {
			oscillationLoopShutdownOK = false;
		}
		if (!oscillationLoopShutdownOK) {
			System.err.println("Error shutting down the oscillation timer task with a 2 s timeout.");
		}
		
		alarmSender.close();
		alarms.shutdown();
	}
}
