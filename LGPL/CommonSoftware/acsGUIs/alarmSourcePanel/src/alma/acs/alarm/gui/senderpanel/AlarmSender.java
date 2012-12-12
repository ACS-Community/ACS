/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2011
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
package alma.acs.alarm.gui.senderpanel;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Logger;

import alma.acs.alarm.gui.senderpanel.ParallelAlarmSender.AlarmToSend;
import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.AlarmDescriptorType;
import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.Triplet;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.FaultStateCreationErrorEx;
import alma.acsErrTypeAlarmSourceFactory.SourceCreationErrorEx;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * The class to send alarms to the alarm service.
 * <P>
 * The sending is done by a dedicated thread to avoid blocking the caller.
 * Alarms are queued and published in a FIFO order.
 * The size of the queue is limited to {@value #maxAlarmsToQueue} that is enough for testing 
 * purposes of this class. 
 * If the queues is full, the caller methods waits until there is enough room in the queue.
 * <P>
 * AlarmSender objects notify the listener whenever an alarm has been published.
 * <P>
 * Life cycle:
 *  
 * @author acaproni
 *
 */
public class AlarmSender implements Runnable {
	
	/**
	 * The max number of alarms that can stored in the queue for sending.
	 */
	private static final int maxAlarmsToQueue=50000;
	
	/**
	 * The number of msecs between one alarm sending and another.
	 * This avoid flooding the system. 
	 */
	private long TIME_BETWEEN_SENDINGS=100;
	
	/**
	 * The time (msec) between two log messages showing the number 
	 * of alarms sent by the class 
	 */
	private final long TIME_BETWEEN_LOGS=60000;
	
	/**
	 * / Log a message every minute reporting the number of alarms published
	 */
	private long lastLogMessageTime=System.currentTimeMillis();
	
	/**
	 * The total number of alarms sent by this sender
	 */
	private int numOfAlarmsSent=0;
	
	/**
	 * Signal if the sender is closed;
	 */
	private volatile boolean closed=false;
	
	/**
	 * The queue of alarms to send.
	 * <P>
	 * Alarms to be sent are stored in this queue. 
	 * The thread gets alarms from this queue and publishes them in the source NC. 
	 */
	private final BlockingQueue<AlarmToSend> alarmsToSend = new ArrayBlockingQueue<AlarmToSend>(maxAlarmsToQueue);
	
	/**
	 * The listeners to notify when alarms have been sent to the alarm system.
	 */
	private final Set<AlarmSentListener> listeners = Collections.synchronizedSet(new HashSet<AlarmSentListener>());

	/**
	 * The source to send alarms
	 */
	private final ACSAlarmSystemInterface alarmSource;
	
	/**
	 * The logger
	 */
	private final Logger logger;
	
	/**
	 * The ContainerServices
	 */
	private final ContainerServices contSvcs;
	
	/**
	 * The thread to send alarms 
	 * @see #run()
	 */
	private Thread thread=null;
	
	/**
	 * The name of the thread
	 */
	private final String threadName;
	
	/**
	 * Constructor
	 * 
	 * @param svcs The {@link ContainerServices}
	 * @param name The name of the sender
	 * @throws SourceCreationErrorEx 
	 * @throws ACSASFactoryNotInitedEx 
	 */
	public AlarmSender(ContainerServices svcs, String name) throws ACSASFactoryNotInitedEx, SourceCreationErrorEx {
		if (svcs==null) {
			throw new IllegalArgumentException("The ContainerServices can't be null");
		}
		if (name==null || name.isEmpty()) {
			throw new IllegalArgumentException("Invalid name of sender");
		}
		alarmSource=ACSAlarmSystemInterfaceFactory.createSource(this.getClass().getName());
		this.contSvcs=svcs;
		this.threadName=name;
		this.logger=svcs.getLogger();
	}
	
	/**
	 * Constructor
	 * 
	 * @param svcs The {@link ContainerServices}
	 * @param name The name of the sender
	 * @param The time between sending 2 alarms
	 * @throws SourceCreationErrorEx 
	 * @throws ACSASFactoryNotInitedEx 
	 */
	public AlarmSender(ContainerServices svcs, String name, long timeBetweenAlarms) 
			throws ACSASFactoryNotInitedEx, SourceCreationErrorEx {
		this(svcs,name);
		if (timeBetweenAlarms<0) {
			throw new IllegalArgumentException("Invalid time between two alarms sending: "+timeBetweenAlarms);
		}
		TIME_BETWEEN_SENDINGS=timeBetweenAlarms;
	}
	
	/**
	 * Synchronously publishes the specified alarm in the source NC.
	 * <P>
	 * Sending alarms through this method should be generally avoided, preferring
	 * {@link #send(Triplet, AlarmDescriptorType, Properties)} instead.
	 * 
	 * @param triplet The triplet in the form FF,FM,FC
	 * @param The descriptor
	 * @param props The user properties
	 * @throws FaultStateCreationErrorEx 
	 * @throws ACSASFactoryNotInitedEx 
	 */
	protected void sendSynch(Triplet triplet, AlarmDescriptorType descriptor, Properties props) {
		if (triplet==null) {
			throw new IllegalArgumentException("The Triplet can't be null");
		}
		if (descriptor==null) {
			throw new IllegalArgumentException("The descriptor can't be null");
		}
		if (closed) {
			return;
		}
		logger.log(AcsLogLevel.DELOUSE, "Sending alarm "+triplet.toString()+" with descriptor "+descriptor);
		ACSFaultState state =null;
		try {
			state=ACSAlarmSystemInterfaceFactory.createFaultState(triplet.faultFamily,triplet.faultMember,triplet.faultCode);
		} catch (Throwable t) {
			notifyListeners(triplet, descriptor,false);
			logger.log(AcsLogLevel.ERROR, "Alarm "+triplet.toString()+" sent with descriptor "+descriptor,t);
			return;
		}
		state.setDescriptor(descriptor.descriptor);
		state.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
		if (props!=null && !props.isEmpty()) {
			state.setUserProperties(props);
		}
		alarmSource.push(state);
		notifyListeners(triplet, descriptor,true);
		logger.log(AcsLogLevel.DELOUSE, "Alarm "+triplet.toString()+" sent");
	}
	
	/**
	 * Send the specified alarm to the alarm service.
	 * <P>
	 * This method return immediately: each alarm is queued and sent later on by the thread.
	 * <P>If the queue of alarms contains more then {@value #maxAlarmsToQueue}, the caller waits until
	 * the thread frees the queue.
	 * 
	 * @param alarmToSend The alarm to send to the alarm server
	 * @throws InterruptedException If interrupted while awaiting to put the alarm in the queue
	 */
	public void send(AlarmToSend alarmToSend) throws InterruptedException {
		if (alarmToSend==null) {
			throw new IllegalArgumentException("The alarm to end can't be null");
		}
		if (!closed) {
			alarmsToSend.put(alarmToSend);
		}
	}
	
	/**
	 * Add a listener
	 * 
	 * @param listener The listener to notify when an alarm has been sent
	 * @return <code>true</code> if this set did not already contain the specified element
	 */
	public boolean addListener(AlarmSentListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can'þ be null");
		}
		return listeners.add(listener);
	}
	
	/**
	 * Remove a listener
	 * 
	 * @param listener The listener to notify when an alarm has been sent
	 * @return <code>true</code> if this set contained the specified element
	 */
	public boolean removeListener(AlarmSentListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		return listeners.remove(listener);
	}
	
	/**
	 * Notify the listeners that an alarm has been sent to the alarm service
	 * 
	 * @param triplet The triplet of the alarm
	 * @param descriptor The descriptor (i.e. activation type
	 * @param success is <code>true</code> if the alarm has been successfully published;
	 * 				  <code>false</code> otherwise
	 */
	private void notifyListeners(Triplet triplet, AlarmDescriptorType descriptor, boolean success) {
		if (triplet==null || descriptor==null) {
			throw new IllegalArgumentException("Invalid null triplet and/or descriptor");
		}
		synchronized (listeners) {
			for (AlarmSentListener listener: listeners) {
				listener.alarmSent(triplet, descriptor,success);
			}	
		}
	}
	
	/**
	 * Start the thread.
	 */
	public synchronized void start() {
		ThreadFactory threadFactory=contSvcs.getThreadFactory();
		thread = threadFactory.newThread(this);
		thread.setName(threadName);
		thread.start();
	}
	
	/**
	 * Close the sender and the thread
	 */
	public synchronized void close() {
		closed=true;
		alarmsToSend.clear();
		if (thread!=null) {
			thread.interrupt();
		}
		alarmSource.close();
	}
	
	/**
	 * Return the number of alars queued and waiting to be sent
	 * @return
	 */
	public int alarmsWaiting() {
		return alarmsToSend.size();
	}

	/**
	 * The thread to get alarms from queue and publish them into the NC
	 */
	@Override
	public void run() {
		logger.log(AcsLogLevel.DEBUG,"Thread "+threadName+" started");
		while (!closed) {
			AlarmToSend alarm = null;
			try {
				alarm=alarmsToSend.take();
				if (TIME_BETWEEN_SENDINGS>0) {
					Thread.sleep(TIME_BETWEEN_SENDINGS);
				}
			} catch (InterruptedException ie) {
				continue;
			}
			try {
				sendSynch(alarm.triplet, alarm.descriptor, alarm.userProperties);
				numOfAlarmsSent++;
			} catch (Throwable t) {
				logger.log(AcsLogLevel.ERROR,threadName+": Error sending alarm",t);
			}
			
			if (System.currentTimeMillis()-lastLogMessageTime>TIME_BETWEEN_LOGS) {
				lastLogMessageTime=System.currentTimeMillis();
				logger.log(AcsLogLevel.DEBUG, threadName+": alarms sent: "+numOfAlarmsSent+", alarms waiting to be sent: "+alarmsToSend.size());
			}
		}
		logger.log(AcsLogLevel.DEBUG,"Thread "+threadName+" terminated");
	}
}
