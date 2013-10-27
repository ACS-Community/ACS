/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
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
package alma.alarmsystem.clients.alarm;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import cern.laser.client.data.Alarm;

/**
 * The statistics for the alarms processed to be sent to clients.
 * 
 * @author acaproni
 * @since ACS-12.2
 */
public class AlarmStatistics {
	
	/**
	 * The holder for each category
	 * 
	 * @author acaproni
	 * @since ACS-12.2
	 */
	public enum AlarmStatField {
		PRI1,
		PRI2,
		PRI3,
		PRI4,
		ACTIVE,
		REDUCED, 
		MASKED,
		MULTIPLICITY_PARENT,
		NODE_PARENT,
		MULTIPLICITY_CHILD,
		NODE_CHILD;
	}
	
	/**
	 * Statistics are stored in a map having the field as key
	 */
	private final Map<AlarmStatField ,Integer> values = Collections.synchronizedMap(new HashMap<AlarmStatField ,Integer>());
	
	/**
	 * Constructor
	 * 
	 */
	public AlarmStatistics() {
		// Initialize the map
		for (AlarmStatField field: AlarmStatField.values()) {
			values.put(field, Integer.valueOf(0));
		}
	}
	
	/**
	 * Update the passed field depending on the activation
	 * 
	 * @param field The field to update
	 * @param active <code>true</code> if the alarm is active
	 */
	public void updateField(AlarmStatField field, boolean active) {
		if (field==null) {
			throw new IllegalArgumentException("The field can't be null");
		}
		synchronized (values) {
			int value = values.get(field).intValue();
			value=active?++value:--value;
			if (value<0) value=0;
			values.put(field, Integer.valueOf(value));
		}
	}
	
	/**
	 * Update the statistics with the passed alarm
	 * @param alarm The alarm received
	 */
	public void update(Alarm alarm) {
		if (alarm==null) {
			throw new IllegalArgumentException("Can't calculate stats over a null alarm");
		}
		boolean active=alarm.getStatus().isActive();
		updateField(AlarmStatField.ACTIVE,active);
		
		switch (alarm.getPriority()) {
		case 1: {
			updateField(AlarmStatField.PRI1,active);
			break;
		}
		case 2: {
			updateField(AlarmStatField.PRI2,active);
			break;
		}
		case 3: {
			updateField(AlarmStatField.PRI3,active);
			break;
		}
		case 4: {
			updateField(AlarmStatField.PRI4,active);
			break;
		}
		}
		
		if (alarm.getStatus().isReduced()) {
			updateField(AlarmStatField.REDUCED,active);
		}
		if (alarm.getStatus().isMasked()) {
			updateField(AlarmStatField.MASKED,active);
		}
		if (alarm.isMultiplicityParent()) {
			updateField(AlarmStatField.MULTIPLICITY_PARENT,active);
		}
		if (alarm.isNodeParent()) {
			updateField(AlarmStatField.NODE_PARENT,active);
		}
		if (alarm.isMultiplicityChild()) {
			updateField(AlarmStatField.MULTIPLICITY_CHILD,active);
		}
		if (alarm.isNodeChild()) {
			updateField(AlarmStatField.NODE_CHILD,active);
		}
	}
	
	/**
	 * Return the accumulated statistic value for the passed field
	 * 
	 * @param field The field to get the statistic
	 * @return The value of the statistic field or
	 * 			<code>null</code> if no statistics is available for the passed field
	 */
	public Integer getStatValue(AlarmStatField field) {
		if (field==null) {
			throw new IllegalArgumentException("The field can't be null");
		}
		return values.get(field);
	}
	
}
