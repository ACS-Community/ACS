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

import java.net.URL;
import java.util.Collection;

import cern.laser.client.data.Alarm;
import cern.laser.client.data.Location;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;
import cern.laser.client.data.Status;
import cern.laser.client.data.Triplet;

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
public class AlarmTableEntry implements Alarm {
	
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
		alarmType=AlarmGUIType.fromAlarm(this);
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
		alarmType=AlarmGUIType.fromAlarm(this);
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

	/**
	 * @return
	 * @throws CloneNotSupportedException
	 * @see cern.laser.client.data.Alarm#clone()
	 */
	public Object clone() throws CloneNotSupportedException {
		return alarm.clone();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getAction()
	 */
	public String getAction() {
		return alarm.getAction();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getAlarmId()
	 */
	public String getAlarmId() {
		return alarm.getAlarmId();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getCategories()
	 */
	public Collection getCategories() {
		return alarm.getCategories();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getCause()
	 */
	public String getCause() {
		return alarm.getCause();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getConsequence()
	 */
	public String getConsequence() {
		return alarm.getConsequence();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getHelpURL()
	 */
	public URL getHelpURL() {
		return alarm.getHelpURL();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getIdentifier()
	 */
	public String getIdentifier() {
		return alarm.getIdentifier();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getLocation()
	 */
	public Location getLocation() {
		return alarm.getLocation();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getPiquetEmail()
	 */
	public String getPiquetEmail() {
		return alarm.getPiquetEmail();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getPiquetGSM()
	 */
	public String getPiquetGSM() {
		return alarm.getPiquetGSM();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getPriority()
	 */
	public Integer getPriority() {
		return alarm.getPriority();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getProblemDescription()
	 */
	public String getProblemDescription() {
		return alarm.getProblemDescription();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getResponsiblePerson()
	 */
	public ResponsiblePerson getResponsiblePerson() {
		return alarm.getResponsiblePerson();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getSource()
	 */
	public Source getSource() {
		return alarm.getSource();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getStatus()
	 */
	public Status getStatus() {
		return alarm.getStatus();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getSystemName()
	 */
	public String getSystemName() {
		return alarm.getSystemName();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#getTriplet()
	 */
	public Triplet getTriplet() {
		return alarm.getTriplet();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#isInstant()
	 */
	public boolean isInstant() {
		return alarm.isInstant();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#isMultiplicityChild()
	 */
	public boolean isMultiplicityChild() {
		return alarm.isMultiplicityChild();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#isMultiplicityParent()
	 */
	public boolean isMultiplicityParent() {
		return alarm.isMultiplicityParent();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#isNodeChild()
	 */
	public boolean isNodeChild() {
		return alarm.isNodeChild();
	}

	/**
	 * @return
	 * @see cern.laser.client.data.Alarm#isNodeParent()
	 */
	public boolean isNodeParent() {
		return alarm.isNodeParent();
	}
}
