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
	 * An alarm to send.
	 * <P>
	 * {@link AlarmToSend} objects are put in a queue to be retrieved
	 * and published asynchronously by the thread.
	 * 
	 * @author acaproni
	 *
	 */
	private class AlarmToSend {
		/**
		 * The triplet
		 */
		public final Triplet triplet;
		
		/**
		 * The user properties
		 */
		public final Properties userProperties;
		
		/**
		 * The alarm descriptor
		 */
		public final AlarmDescriptorType descriptor;
		
		/**
		 * Constructor
		 * 
		 * @param triplet The triplet 
		 * @param userProperties The user properties
		 * @param descriptor The descriptor
		 */
		public AlarmToSend(Triplet triplet,Properties userProperties,AlarmDescriptorType descriptor) {
			if (triplet==null) {
				throw new IllegalArgumentException("The Triplet can't be null");
			}
			if (descriptor==null) {
				throw new IllegalArgumentException("The descriptor can't be null");
			}
			this.triplet=triplet;
			this.userProperties=userProperties;
			this.descriptor=descriptor;
		}
	}
	
	/**
	 * The max number of alarms that can stored in the queue for sending.
	 */
	private static final int maxAlarmsToQueue=50000;
	
	/**
	 * The number of msecs between one alarm sending and another.
	 * This avoid flooding the system. 
	 */
	private final long TIME_BETWEEN_SENDINGS=100;
	
	/**
	 * The queu of alarms to send.
	 * <P>
	 * Alarms to be sent are stored in this queue. 
	 * The thread gets alarms from this queue and publishes them in the source NC. 
	 */
	private final BlockingQueue<AlarmToSend> alarmsToSend = new ArrayBlockingQueue<AlarmSender.AlarmToSend>(maxAlarmsToQueue);
	
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
	 * Constructor
	 * 
	 * @throws SourceCreationErrorEx 
	 * @throws ACSASFactoryNotInitedEx 
	 */
	public AlarmSender(ContainerServices svcs) throws ACSASFactoryNotInitedEx, SourceCreationErrorEx {
		if (svcs==null) {
			throw new IllegalArgumentException("The ContainerServices can't be null");
		}
		alarmSource=ACSAlarmSystemInterfaceFactory.createSource(this.getClass().getName());
		this.contSvcs=svcs;
		this.logger=svcs.getLogger();
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
		logger.finer("Sending alarm "+triplet.toString()+" with descriptor "+descriptor);
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
		logger.finer("Alarm "+triplet.toString()+" sent");
	}
	
	/**
	 * Send the specified alarm to the alarm service.
	 * <P>
	 * This method return immediately: each alarm is queued and sent later on by the thread.
	 * <P>If the queue of alarms contains more then {@value #maxAlarmsToQueue}, the caller waits until
	 * the thread frees the queue.
	 * 
	 * @param triplet The triplet in the form FF,FM,FC
	 * @param The descriptor
	 * @param props The user properties
	 * @throws InterruptedException If interrupted while awaiting to put the alarm in the queue
	 */
	public void send(Triplet triplet, AlarmDescriptorType descriptor, Properties props) throws InterruptedException {
		if (triplet==null) {
			throw new IllegalArgumentException("The Triplet can't be null");
		}
		if (descriptor==null) {
			throw new IllegalArgumentException("The descriptor can't be null");
		}
		alarmsToSend.put(new AlarmToSend(triplet, props, descriptor));
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
		thread.start();
	}
	
	/**
	 * Close the sender and the thread
	 */
	public synchronized void close() {
		if (thread!=null) {
			thread.interrupt();
		}
		alarmSource.close();
	}

	/**
	 * The thread to get alarms from queue and publish them into the NC
	 */
	@Override
	public void run() {
		while (!Thread.currentThread().isInterrupted()) {
			AlarmToSend alarm = null;
			try {
				alarm=alarmsToSend.take();
				Thread.sleep(TIME_BETWEEN_SENDINGS);
			} catch (InterruptedException ie) {
				return;
			}
			sendSynch(alarm.triplet, alarm.descriptor, alarm.userProperties);
		}
	}
}
