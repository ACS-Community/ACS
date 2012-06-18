/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2012
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

import java.util.Collections;
import java.util.HashSet;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.ThreadFactory;

import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.AlarmDescriptorType;
import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.Triplet;
import alma.acs.container.ContainerServices;

/**
 * The base class for alarms senders.
 * <P>
 * {@link BaseAlarmsSender} starts a thread, {@link #alarmsSenderThread}, to randomly set/clear the alarms whose definition 
 * is read from the file.<BR>
 * There is only one thread alive at a given time. So if the user want to start a task,
 * he must wait the termination of the current task or stop it.
 * <P>
 * Default fault members have a '*' in the FM of the triplet. Default fault members are used only while cycling 
 * i.e. when activating or terminating all the alarms they are ignored.
 * The fault member name of a default is built by appending a random number to the prefix name, {@link #defaultFMNamePrefix}.
 * This means that the user must clean default fault members by using the table of alarms.
 * 
 * 
 * 
 * @author acaproni
 *
 */
public class BaseAlarmsSender {
	
	/**
	 * An alarm read from the file or the TM/CDB.
	 * <P>
	 * The descriptor for the activation is not present because
	 * it is generated depending on the  button the user pressed.
	 * 
	 * {@link AlarmRead} is immutable.
	 * 
	 * @author acaproni
	 *
	 */
	protected final class AlarmRead {
		
		/**
		 * The triplet "FF,FM,FC"
		 */
		public final String tripletStr; 
		
		/** 
		 * The alarm triplet
		 */
		public final Triplet triplet;
		
		/**
		 * The properties "key=val1, key2=val2..."
		 */
		public final String props;
		
		/**
		 * The properties built parsing {@link #props}.
		 */
		public final Properties properties;
		
		/**
		 * Constructor.
		 * <P>
		 * Build the {@link AlarmRead} object form the strings.
		 * None of the parameters can contain a comment (i.e. "#...").
		 * 
		 * 
		 * @param triplet The triplet
		 * @param props The user properties
		 * @throws Exception if the triplet is invalid
		 */
		public AlarmRead(String triplet,String props) throws Exception{
			if (triplet==null || triplet.isEmpty() || !SenderPanelUtils.isATriplet(triplet)) {
				throw new Exception("Invalid triplet "+triplet);
			}
			this.tripletStr=triplet.trim();
			this.triplet=SenderPanelUtils.tripletFromString(tripletStr);
			this.props=props;
			
			Properties temp=null;
			if (SenderPanelUtils.isAStringOfProperties(props)) {
				temp=SenderPanelUtils.propertiesFromString(props.trim());
			}
			properties=temp;
		}
		
		/**
		 * @return <code>true</code> if there is a default fault member
		 */
		public boolean isDefaultFM() {
			return triplet.faultMember.equals("*");
		}
		
		/**
		 * Print an alarm
		 */
		@Override
		public String toString() {
			StringBuilder str = new StringBuilder();
			str.append('<');
			str.append(tripletStr);
			str.append("> Properties=[");
			str.append(props);
			str.append("]");
			if (properties!=null && !properties.isEmpty()) {
				str.append(" ("+properties.size()+" properties)");
			}
			return str.toString();
		}
	}
	
	/**
	 * The thread factory to create threads for sending alarms.
	 * <P>
	 * The purpose of this class is to create the threads with the same settings.
	 * 
	 * @author acaproni
	 */
	protected class AlarmSenderThreadFactory implements ThreadFactory {
		
		/**
		 * The number of the thread used to build its name
		 */
		private int threadNumber=0;
		
		/**
		 * The prefix of the name of the thread
		 */
		private final String threadNamePrefix;
		
		/**
		 * 
		 * @param threadNamePrefix The prefix of the name of the thread
		 */
		public AlarmSenderThreadFactory(String threadNamePrefix) {
			this.threadNamePrefix=threadNamePrefix;
		}

		/**
		 * @see ThreadFactory#newThread(Runnable)
		 */
		@Override
		public synchronized Thread newThread(Runnable r) {
			if (r==null) {
				throw new IllegalArgumentException("Impossible to create a thread from a null Runnable");
			}
			threadNumber++;
			Thread t = new Thread(r,threadNamePrefix+threadNumber);
			t.setDaemon(true);
			return t;
		}
		
	}
	
	/**
	 * The prefix of a fault member generated from a default 
	 */
	protected final String defaultFMNamePrefix="DefaultFM_";
	
	/**
	 * The max number appended to the prefix to build a fault member with a default
	 * <P>
	 * This is also the max number of alarms that can be generated for a default fault member
	 * i.e. an alarm like FF,*,1 can be translated into at most {@link #maxDefaultFMSuffix} alarms 
	 */
	protected final int maxDefaultFMSuffix=1000;
	
	/**
	 * ContainerServices
	 */
	protected final ContainerServices contSvcs;
	
	/**
	 * The object to send alarms
	 */
	protected final AlarmSender alarmsSender;
	
	/**
	 * The parent component to show the dialog
	 */
	protected final SenderPanel panel;
	
	/**
	 * The factory to create threads
	 */
	protected final AlarmSenderThreadFactory threadFactory;
	
	/**
	 * The thread to send alarms.
	 */
	protected volatile Thread alarmsSenderThread=null;
	
	/**
	 * The alarms read from the file.
	 */
	protected final Vector<AlarmRead> alarms=new Vector<FileSender.AlarmRead>();
	
	/**
	 * The listeners to notify when alarms have been sent to the alarm system.
	 */
	protected final Set<SlowTaskListener> listeners = Collections.synchronizedSet(new HashSet<SlowTaskListener>());
	
	/**
	 * Constructor
	 * 
	 * @param parent the parent component of the dialog
	 * @param contSvcs The ContainerServices
	 * @param sender The object to send alarms
	 * @param threadPrefixName The prefix of the name of the thread
	 */
	public BaseAlarmsSender(SenderPanel parent,ContainerServices contSvcs, AlarmSender sender, String threadPrefixName) {
		if (parent==null) {
			throw new IllegalArgumentException("The parent component can't be null");
		}
		if (contSvcs==null) {
			throw new IllegalArgumentException("The ContainerServices can't be null");
		}
		if (sender==null) {
			throw new IllegalArgumentException("The AlarmSender can't be null");
		}
		this.panel=parent;
		this.contSvcs=contSvcs;
		this.alarmsSender=sender;
		threadFactory = new AlarmSenderThreadFactory(threadPrefixName);
	}
	
	/**
	 * Stop the thread to send alarms
	 */
	public synchronized void stopThread() {
		if (alarmsSenderThread==null) {
			return;
		}
		alarmsSenderThread.interrupt();
	}
	
	/**
	 * Add a not <code>null</code> listener.
	 * 
	 * @param listener The listener to be notified
	 * @return <code>true</code> if this set did not already contain the specified element
	 */
	public boolean addSlowTaskListener(SlowTaskListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null!");
		}
		return listeners.add(listener);
	}
	
	/**
	 * Remove a listener.
	 * 
	 * @param listener The not <code>null</code> listener to remove
	 * @return <code>true</code> if this set contained the specified element
	 */
	public boolean removeSlowTaskListener(SlowTaskListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null!");
		}
		return listeners.remove(listener);
	}
	
	
	/**
	 * Stop the thread and frees the resources
	 */
	public synchronized void close() {
		stopThread();
	}
	
	/**
	 * Notify the listeners that a slow task started.
	 * 
	 * @param nSteps The number of steps or <code>null</code> if undefined.
	 */
	protected void notifyStartTask(Integer nSteps) {
		synchronized (listeners) {
			for (SlowTaskListener listener: listeners) {
				listener.slowTaskStarted(this,nSteps);
			}
		}
	}
	
	/**
	 * Notify the listeners that a slow task terminated.
	 */
	protected void notifyStopTask() {
		synchronized (listeners) {
			for (SlowTaskListener listener: listeners) {
				listener.slowTaskFinished(this);
			}
		}
	}
	
	/**
	 * Notify the listeners that a slow task terminated.
	 * 
	 * @param currentStep The actual step
	 */
	protected void notifyTaskProgress(int currentStep) {
		synchronized (listeners) {
			for (SlowTaskListener listener: listeners) {
				listener.slowTaskProgress(this,currentStep);
			}
		}
	}
	
	/**
	 * Notify the listeners that the alarms have been read from the file 
	 * or the TM/CDB 
	 */
	protected void notifyAlarmsRead() {
		synchronized (listeners) {
			for (SlowTaskListener listener: listeners) {
				listener.alarmsRead(this, alarms.size());
			}
		}
	}
	
	/**
	 * @return The number of alarms in memory i.e. the number of alarms
	 * 		   read from the file
	 * @return
	 */
	public int size() {
		return alarms.size();
	}
	
	/**
	 * Print on the stdout the alarms {@link InterruptedException} {@link #alarms}
	 */
	protected void dumpAlarms() {
		System.out.println("Alarms in memory: "+alarms.size());
		for (AlarmRead alarm: alarms) {
			System.out.println("\t"+alarm.toString());
		}
	}
	
	/**
	 * Send the alarms read from the file.
	 * 
	 * @param active if <code>true</code> the alarms are activated,
	 * 				 otherwise terminated
	 */
	public synchronized void sendAlarms(final boolean active) {
		alarmsSenderThread = threadFactory.newThread(new Runnable() {
			@Override
			public void run() {
				AlarmDescriptorType type = (active)?AlarmDescriptorType.ACTIVE:AlarmDescriptorType.TERMINATE;
				int nAlarmsSent=0;
				synchronized (alarms) {
					notifyStartTask(alarms.size());
					for (AlarmRead alarm: alarms) {
						if (Thread.currentThread().isInterrupted()) {
							break;
						}
						if (alarm.isDefaultFM()) {
							// Defaults are discarded when raising/clearing all the alarms
							continue;
						}
						try {
							alarmsSender.send(alarm.triplet, type, alarm.properties);
							notifyTaskProgress(++nAlarmsSent);
						} catch (InterruptedException ie) {
							// If interrupted, terminates.
							break;
						}
					}
				}
				notifyStopTask();
			}
		});
		alarmsSenderThread.start();
	}
	
	/**
	 * Start sending alarms randomly.
	 * <P>
	 * The triplet of alarms are those set in the file but they are picked up randomly
	 * in a loop. 
	 * The activation state (ACTIVE, TERMINATE) is also chosen randomly
	 * 
	 */
	public synchronized void startSendingRandomly() {
		alarmsSenderThread = threadFactory.newThread(new Runnable() {
			@Override
			public void run() {
				notifyStartTask(null);
				Random rnd = new Random(System.currentTimeMillis());
				synchronized (alarms) {
					while (!Thread.currentThread().isInterrupted()) {
						AlarmDescriptorType type = (rnd.nextBoolean())?AlarmDescriptorType.ACTIVE:AlarmDescriptorType.TERMINATE;
						int alarmIdx=rnd.nextInt(alarms.size());
						AlarmRead alarm=alarms.get(alarmIdx);
						AlarmRead alarmToSend=alarm;
						if (alarm.isDefaultFM()) {
							int postFix=rnd.nextInt(maxDefaultFMSuffix);
							String tripletStr=alarm.triplet.faultFamily+","+defaultFMNamePrefix+postFix+","+alarm.triplet.faultCode;
							try {
								alarmToSend=new AlarmRead(tripletStr, alarm.props);
							} catch (Throwable t) {
								System.out.println("Exception building an alarm from a default: "+t.getMessage());
								t.printStackTrace();
								continue;
							}
						}
						try {
							alarmsSender.send(alarmToSend.triplet, type, alarmToSend.properties);
							Thread.sleep(100);
						} catch (InterruptedException ie) {
							// If interrupted, terminates.
							break;
						}
					}
				}
				notifyStopTask();
			}
		});
		alarmsSenderThread.start();
	}
}
