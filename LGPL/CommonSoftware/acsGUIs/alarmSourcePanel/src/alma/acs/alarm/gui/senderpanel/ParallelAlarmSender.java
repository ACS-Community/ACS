/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.alarm.gui.senderpanel;

import java.util.ArrayList;
import java.util.Properties;
import java.util.concurrent.ArrayBlockingQueue;

import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.AlarmDescriptorType;
import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.Triplet;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.SourceCreationErrorEx;


/** 
 * Sends alarms in parallel by sending each alarm to a different sender.
 * <P>
 * There is upper limit in the number of alarms sent each second in {@link #maxNumberOfAlarmsPerSecond}
 * and implemented by a {@link ArrayBlockingQueue} bounded to {@link #maxNumberOfAlarmsPerSecond}.
 * <P>
 * The alarms are sent by the thread that loops once per second (aprox) and empties the queue.
 * <P>
 * The {@link ArrayBlockingQueue} control the max number of alarms queued and ready
 * to be sent in the next second. However it does not prevent to add new alarms while
 * the thread is sending alarms (i.e. the thread gets a alarm out of the queue so 
 * a new room is available and the send pushes a new alarm). <BR>
 * To be sure that the number of alarms sent each second is at most {@link #maxNumberOfAlarmsPerSecond},
 * the thread counts the number of alarms it get out of the queue at each iteration.
 * 
 * @author  acaproni
 * @version $Id: ParallelAlarmSender.java,v 1.2 2012/12/14 18:10:48 acaproni Exp $
 * @since ACS 11.0
 */
public class ParallelAlarmSender implements Runnable {
	
	/**
	 * An alarm to send.
	 * <P>
	 * {@link AlarmToSend} objects are put in a queue to be retrieved
	 * and published asynchronously by the thread.
	 * 
	 * @author acaproni
	 *
	 */
	public static class AlarmToSend {
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
	 * The senders of alarms
	 */
	private final ArrayList<AlarmSender> senders;
	
	/**
	 * <code>numOfSender</code> select the sender to send an alarm
	 * by cycling through all the senders.
	 */
	private int numOfSender=0;
	
	/**
	 * The ContainerServices
	 */
	private final ContainerServices contSvcs;
	
	/**
	 * The time (msec) between two log messages showing the number 
	 * of alarms sent by the class 
	 */
	private final long TIME_BETWEEN_LOGS=60000;
	
	/**
	 * The max number of alarms that can be sent each minute.
	 * From the last log I read from the AOS I got 3133 logs/minute.
	 * (3500 alarms per minute is around 60 alarms per second) 
	 */
	private final int maxNumberOfAlarmsPerSecond=60;
	
	/**
	 * / Log a message every minute reporting the number of alarms published
	 */
	private long lastLogMessageTime=System.currentTimeMillis();
	
	/**
	 * The total number of alarms sent by this sender
	 */
	private int numOfAlarmsSent=0;
	
	/**
	 * The total number of alarms sent by this sender in the last minute
	 */
	private int numOfAlarmsSentInLastMinute=0;
	
	/**
	 * The thread that runs this Runnable
	 */
	private final Thread thread;
	
	/**
	 * Signal the thread to terminate
	 */
	private volatile boolean terminateThread=false;
	
	/**
	 * The alarms to send in a second.
	 * <P>
	 * The array blocking queue ensures the maximum number of logs sent per second
	 * because it blocks if a ut is called when the queue is full.
	 * 
	 */
	private final ArrayBlockingQueue<AlarmToSend> alarmsToSend;
	
	
	
	/**
	 * Constructor
	 * 
	 * @param svcs The {@link ContainerServices}
	 * @param numOfSenders The number of {@link AlarmSender} to run in parallel
	 * @param timeBetweenAlarms The time between 2 send by each {@link AlarmSender} 
	 * 
	 * @throws SourceCreationErrorEx 
	 * @throws ACSASFactoryNotInitedEx
	 */
	public ParallelAlarmSender(ContainerServices svcs, int numOfSenders, long timeBetweenAlarms) 
			throws ACSASFactoryNotInitedEx, SourceCreationErrorEx {
		if (svcs==null) {
			throw new IllegalArgumentException("The ContainerServices can't be null");
		}
		if (numOfSenders<=1) {
			throw new IllegalArgumentException("The number of thread must be greater the 1");
		}
		this.contSvcs=svcs;
		this.alarmsToSend= new ArrayBlockingQueue<AlarmToSend>(maxNumberOfAlarmsPerSecond);
		senders = new ArrayList<AlarmSender>(numOfSenders);
		for (int t=0; t<numOfSenders; t++) {
			senders.add(new AlarmSender(svcs, "AlarmSender_"+t,timeBetweenAlarms));
		}
		thread=new Thread(this, this.getClass().getName());
		thread.setDaemon(true);
	}
	
	
	/**
	 * Start the threads.
	 */
	public void start() {
		thread.start();
		for (AlarmSender sender: senders) {
			sender.start();
		}
	}
	
	/**
	 * Close the sender and the threads
	 */
	public void close() {
		terminateThread=true;
		thread.interrupt();
		for (AlarmSender sender: senders) {
			sender.close();
		}
	}
	
	/**
	 * Add a listener
	 * 
	 * @param listener The listener to notify when an alarm has been sent
	 */
	public void addListener(AlarmSentListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		for (AlarmSender sender: senders) {
			sender.addListener(listener);
		}
	}
	
	/**
	 * Remove a listener
	 * 
	 * @param listener The listener to notify when an alarm has been sent
	 * @return <code>true</code> if this set contained the specified element
	 */
	public void removeListener(AlarmSentListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		for (AlarmSender sender: senders) {
			sender.removeListener(listener);
		}
	}
	
	/**
	 * Send the specified alarm to on of the {@link AlarmSender} to be sent to the service.
	 * <P>
	 * This method return immediately: each alarm is queued and sent later on by the thread.
	 * <P>If the queue of alarms contains more then {@value #maxAlarmsToQueue}, the caller waits until
	 * the thread frees the queue.
	 * 
	 * @param triplet The triplet in the form FF,FM,FC
	 * @param The descriptor
	 * @param props The user properties
	 * @throws InterruptedException If interrupted while awaiting to put the alarm in the queue
	 * @see AlarmSender#send(alma.acs.alarm.gui.senderpanel.SenderPanelUtils.Triplet, alma.acs.alarm.gui.senderpanel.SenderPanelUtils.AlarmDescriptorType, java.util.Properties)
	 */
	public void send(Triplet triplet, AlarmDescriptorType descriptor, Properties props) throws InterruptedException {
		if (terminateThread) {
			return;
		}
		alarmsToSend.put(new AlarmToSend(triplet, props, descriptor));
	}


	@Override
	public void run() {
		while (!terminateThread) {
			int count=0; // number of alarms sent in this iteration
			while (count<maxNumberOfAlarmsPerSecond && !terminateThread) {
				AlarmToSend alarm = alarmsToSend.poll();
				if (alarm==null) {
					// No more alarms in queue;
					break;
				}
				try {
					senders.get(numOfSender).send(alarm);
					numOfSender = (numOfSender + 1) % senders.size();
					numOfAlarmsSent++;
					numOfAlarmsSentInLastMinute++;
					count++;
				} catch (InterruptedException ie) {
					continue;
				}
			}
			long now=System.currentTimeMillis();
			
			if (now-lastLogMessageTime>TIME_BETWEEN_LOGS) {
				int alarmsQueued=0;
				for (AlarmSender sender: senders) {
					alarmsQueued+=sender.alarmsWaiting();
				}
				lastLogMessageTime=now;
				contSvcs.getLogger().log(AcsLogLevel.INFO, "Tot. alarms sent: "+numOfAlarmsSent+", Tot. alarms waiting to be sent: "+alarmsQueued+", in the last minute: "+numOfAlarmsSentInLastMinute);
				numOfAlarmsSentInLastMinute=0;
			}
			try {
				Thread.sleep(1000);
			} catch (InterruptedException ie) {}
		}
	}
}
