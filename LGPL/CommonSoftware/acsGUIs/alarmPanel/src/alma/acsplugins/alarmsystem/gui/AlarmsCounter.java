/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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
package alma.acsplugins.alarmsystem.gui;

/**
 * The number of alarms for each type in the table
 * 
 * Priorities contains only active alarms
 * 
 * @author acaproni
 *
 */
public enum AlarmsCounter {
	
	PRI0,
	PRI1,
	PRI2,
	PRI3,
	INACTIVE,
	AUTOACKNOWLEDGED;
	
	private volatile long count=0;
	
	/**
	 * Increase the counter
	 */
	public void incCounter() {
		if (count==Long.MAX_VALUE) {
			count=1;
			return;
		}
		count++;
	}
	
	/**
	 * Decrease the counter
	 */
	public void decCounter() {
		if (count<=0) {
			throw new IllegalStateException("Error managing counters");
		}
		count--;
	}
	
	/**
	 * 
	 * @return The value of the counter
	 */
	public long getCount() {
		return count;
	}
	
	/**
	 * Print the values of all the counters in the stdout.
	 * 
	 * It is a debugging helper
	 */
	public static void dumpCounters() {
		System.out.println("Counters values");
		for (AlarmsCounter counter: AlarmsCounter.values()) {
			System.out.println("\t"+counter+" = "+counter.count);
		}
	}
	
	/**
	 * Increase the counter of the alarm of the given priority
	 * 
	 * @param priority The priority of the given alarm
	 */
	public static void incActiveAlarm(int priority) {
		if (priority<PRI0.ordinal() || priority>PRI3.ordinal()) {
			throw new IllegalArgumentException("Priority out of range");
		}
		AlarmsCounter.values()[priority-PRI0.ordinal()].incCounter();
	}
	
	/**
	 * Decrease the counter of the alarm of the given priority
	 * 
	 * @param priority The priority of the given alarm
	 */
	public static void decActiveAlarm(int priority) {
		if (priority<PRI0.ordinal() || priority>PRI3.ordinal()) {
			throw new IllegalArgumentException("Priority out of range");
		}
		AlarmsCounter.values()[priority-PRI0.ordinal()].decCounter();
	}
}

