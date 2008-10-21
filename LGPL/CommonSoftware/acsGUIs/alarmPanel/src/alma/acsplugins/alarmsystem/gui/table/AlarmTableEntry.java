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
package alma.acsplugins.alarmsystem.gui.table;

import com.sun.mail.handlers.text_html;

import cern.laser.client.data.Alarm;

/**
 * An entry in the alarm table i.e. a row in the table
 * 
 * It consists of the alarm plus other information to represent the
 * alarm in the table. 
 * The AlarmTablemodel finds here all it needs to show an alarm
 * in a row.
 * 
 * @author acaproni
 *
 */
public class AlarmTableEntry {
	
	/**
	 *  An entry is new till the user look at it by pressing one mouse
	 *  button over the row of the alarm
	 */
	private volatile boolean isNew;
	
	/**
	 * The alarm received by the alarm system
	 */
	private Alarm alarm;
	
	/**
	 * The type of the alarm
	 */
	private AlarmGUIType alarmType;
	
	/** 
	 * Build an AlarmTableEntry from the given alarm
	 * 
	 * @param alarm The alarm in the entry
	 */
	public AlarmTableEntry(Alarm alarm) {
		if (alarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		this.alarm=alarm;
		isNew=true;
		alarmType=AlarmGUIType.fromAlarm(alarm);
	}
	
	/**
	 * Update the alarm with the new one.
	 * 
	 * Replace the alarm with the one but only if they have
	 * the same ID
	 * 
	 * @param newAlarm The not null new alarm
	 */
	public void updateAlarm(Alarm newAlarm) {
		if (newAlarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		if (!newAlarm.getAlarmId().equals(alarm.getAlarmId())) {
			throw new IllegalArgumentException("The ID of the new alarm is not equals to the old one");
		}
		alarm=newAlarm;
		alarmType=AlarmGUIType.fromAlarm(alarm);
		if (newAlarm.getStatus().isActive()) {
			isNew=true;
		}
	}

	/**
	 * 
	 * @return The type of the alarm
	 */
	public AlarmGUIType getAlarmType() {
		return alarmType;
	}

	/**
	 * 
	 * @return The alarm
	 */
	public Alarm getAlarm() {
		return alarm;
	}
	
	/**
	 * Return true if the alarm is new
	 * 
	 * @return true if the alarm is new
	 */
	public boolean isNew() {
		return isNew;
	}
	
	/**
	 * The user saw the alarm and press one mouse button over its line
	 * so the alarm is not new anymore
	 */
	public void alarmSeen() {
		isNew=false;
	}
	
	/**
	 * @return <code>true</code> if the alarm is reduced
	 */
	public boolean isReduced() {
		return alarm.getStatus().isReduced();
	}
	
	/**
	 * 
	 * @return <code>true</code> if the alarm is a node or multiplicity child
	 */
	public boolean isChild() {
		return alarm.isNodeChild() || alarm.isMultiplicityChild();
	}
	
	/**
	 * 
	 * @return <code>true</code> if the alarm is a node or multiplicity parent
	 */
	public boolean isParent() {
		return alarm.isNodeParent() || alarm.isMultiplicityParent();
	}
}
