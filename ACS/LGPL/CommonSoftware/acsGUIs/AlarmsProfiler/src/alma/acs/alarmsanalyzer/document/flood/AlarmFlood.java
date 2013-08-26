/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
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
package alma.acs.alarmsanalyzer.document.flood;

import java.util.Vector;

import cern.laser.client.data.Alarm;
import alma.acs.alarmsanalyzer.engine.AlarmCategoryListener;

/**
 * An alarm flood begins when the flow of annunciated alarms if greater then 10 alarms in 
 * 10 minutes and finish when the flow is less then 5 alarm in 10 minutes. 
 * <P>
 * Objects of this class records a flood.
 * <P>
 * When the object is created, it starts a Thread to count each minute and for each
 * minute the important value is the number of received alarms to recognize if 
 * a flood starts or finish.  
 * <P>
 * The alarm counted in this context are the annunciated alarms i.e. 
 * <UL>
 * 	<LI>alarms active
 * 	<LI>alarms not reduced
 * </UL>
 * @author acaproni
 *
 */
public class AlarmFlood implements Runnable,  AlarmCategoryListener {
	
	/**
	 * The alarm registered every minute
	 * @author acaproni
	 *
	 */
	private class AlarmPerMinute {
		public final long startTime;
		public long numOfAlarms;
		
		/**
		 * Contructor
		 * 
		 * @param time The start time
		 */
		public AlarmPerMinute(long time) {
			this.startTime=time;
			numOfAlarms=0;
		}
	}
	
	/**
	 * The counter with the alarms received in the last 10 minute
	 * <P>
	 * The alarm of the oldest minute are int head, the newest in the tail. 
	 */
	private final Vector<AlarmPerMinute> counter = new Vector<AlarmFlood.AlarmPerMinute>(10);
	
	/**
	 * An alarm flood starts when the number of alarms per 10 minutes
	 * is greater or equal to this value
	 */
	private static final int STARTALARMFLOOD=10;
	
	/**
	 * An alarm flood finishes when the number of alarms per 10 minutes
	 * is less then this value
	 */
	private static final int ENDALARMFLOOD=5;
	
	/**
	 * The time (in millisecond) when the flood started.
	 * <P>
	 * This is the time of the beginning of the first minute when the flood started.
	 * <code>startTimeOfFlood</code> signals if a flood started.
	 */
	private long startTimeOfFlood=0;
	
	/**
	 * The time (in millisec) when the flood finished
	 * <P>
	 * This is the time of the beginning of the last minute when the flood finished.
	 */
	private long endTimeOfFlood=0;
	
	/**
	 * The number of alarms in flood
	 */
	private int alarmsInFlood=0;
	
	/**
	 * The thread
	 */
	private final Thread thread;
	
	/**
	 * Signal the thread to terminate
	 */
	private boolean stopped=false;
	
	/**
	 * The container that owns this object
	 * (used to refresh the content of the table)
	 */
	private FloodContainer container;
	
	/**
	 * Constructor
	 * @param container The container that owns this object
	 */
	public AlarmFlood(FloodContainer container) {
		this.container=container;
		for (int t=0; t<10; t++) {
			counter.add(t, null);
		}
		thread=new Thread(this,"AlarmFlood");
		thread.setDaemon(true);
		thread.start();
	}
	
	/**
	 * The counter of the actual minute
	 */
	private AlarmPerMinute actualCounter;
	
	/**
	 * The length of a this flood in msec.
	 * 
	 * @return The length of the flood. 
	 * 			It is a negative number, if a flood started but it is not yet finished
	 */
	public long lengthOfFlood() {
		if (endTimeOfFlood==0) {
			return System.currentTimeMillis()-startTimeOfFlood;
		} 
		return endTimeOfFlood-startTimeOfFlood;
	}
	
	/**
	 * Stop counting
	 */
	public void stop() {
		stopped=true;
		thread.interrupt();
	}
	
	/**
	 * The thread check every minute the state of the flood and
	 * terminates when the flood finishes.
	 */
	@Override
	public void run() {
		while (!stopped) {
			synchronized (this) {
				actualCounter=new AlarmPerMinute(System.currentTimeMillis());
				while (counter.size()>10) {
					counter.remove(0);
				}
				counter.add(actualCounter);
			}
			try {
				Thread.sleep(60*1000);
			} catch (InterruptedException ie) {
				continue;
			}
			// Check flood
			if (checkFlood()) {
				// The flood has finished: exit
				break;
			}
		}
		container.doneFlood();
		System.out.println("Thread terminated");
	}
	
	/**
	 * Check the state of the flood
	 * 
	 * @return <code>true</code> if the flood finished
	 */
	private synchronized boolean checkFlood() {
		// The alarms in the past 10 minutes
		int alarms=0;
		for (int t=0; t<10; t++) {
			if (counter.get(t)!=null) {
				System.out.println("Alarm at min "+t+": "+counter.get(t).numOfAlarms);
				alarms+=counter.get(t).numOfAlarms;
			}
		}
		System.out.println("Alarms in the last 10 minutes: "+alarms);
		// Check if a new flood started
		if (startTimeOfFlood==0) {
			// Flood not started
			if (alarms<STARTALARMFLOOD) {
				System.out.println("No flood started yet");
				return false;
			}
			// The flood started right now
			//
			// The start time is in the first element in counter
			// that is not null
			int idx=-1;
			for (int t=0; t<10; t++) {
				if (counter.get(t)!=null) {
					idx=t;
					break;
				}
			}
			if (idx==-1) {
				// Impossible!
				throw new IllegalStateException("Index is -1!");
			}
			startTimeOfFlood=counter.get(idx).startTime;
			endTimeOfFlood=0;
			alarmsInFlood=alarms;
			System.out.println("Flood started NOW");
			return false;
		}
		// Check if the current flood just finished
		if (endTimeOfFlood!=0) {
			// The flood has already finished
			return false;
		}
		if (alarms>ENDALARMFLOOD) {
			System.out.println("No flood ended yet");
			// Not yet finished
			return false;
		}
		// flood finished now
		// Get the time of the last sample
		AlarmPerMinute temp=null;
		for (int t=9; t>=0 && temp==null; t++) {
			temp=counter.get(t);
		}
		if (temp==null) {
			throw new IllegalStateException("This point should never be null!!!");
		}
		endTimeOfFlood=temp.startTime;
		System.out.println("Flood finished NOW");
		return true;
	}

	/**
	 * A new alarm has been received.
	 * 
	 * @param alarm The alarm
	 */
	@Override
	public synchronized void onAlarm(Alarm alarm) {
		if (!alarm.getStatus().isActive()) {
			return;
		} 
		if (alarm.getStatus().isReduced()) {
			return;
		}
		if (!alarm.getStatus().isReduced()) {
			actualCounter.numOfAlarms++;
		}
		if (startTimeOfFlood>0 && endTimeOfFlood==0) {
			alarmsInFlood++;
		}
	}

	/**
	 * Getter
	 */
	public long getStartTimeOfFlood() {
		return startTimeOfFlood;
	}

	/**
	 * Getter
	 */
	public long getEndTimeOfFlood() {
		return endTimeOfFlood;
	}

	/**
	 * Getter
	 */
	public int getAlarmsInFlood() {
		return alarmsInFlood;
	}
	
	/**
	 * 
	 * @return <code>true</code> if a flood started
	 */
	public boolean isFloodStarted() {
		return startTimeOfFlood!=0;
	}
	
	/**
	 * 
	 * @return <code>true</code> if the flood finished
	 * 		A value of <code>false</code> means that a flood has not finished yet
	 * 		but it could as well be that it never started.
	 */
	public boolean isFloodFinished() {
		return endTimeOfFlood!=0;
	}

}


