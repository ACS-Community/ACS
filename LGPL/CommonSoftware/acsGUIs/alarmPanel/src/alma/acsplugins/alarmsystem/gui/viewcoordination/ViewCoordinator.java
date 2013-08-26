/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2013
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acsplugins.alarmsystem.gui.viewcoordination;

import alma.acsplugins.alarmsystem.gui.table.AlarmTable;

import cern.laser.client.data.Alarm;

/**
 * Adapter that allows the alarm panel to coordinate table filters and selection notification
 * with external software such as the OMC.
 * See http://ictjira.alma.cl/browse/ICT-114
 * 
 * @author hsommer
 */
public class ViewCoordinator
{
	private final AlarmTable alarmTable;

	public ViewCoordinator(AlarmTable alarmTable) {
		this.alarmTable = alarmTable;
	}
	
	/**
	 * The table will show only alarms that contain this string.
	 * 
	 * Currently filtering works only for a single search string, 
	 * regardless of the field it occurs in.
	 * @param filterString
	 */
	public void setTextFilter(String filterString) {
		alarmTable.filter(filterString, false);
	}

	public void removeTextFilter() {
		alarmTable.filter(null, false);
	}
	
	
	/**
	 * The selection callback interface, to be provided by external code.
	 * Note that there is no deselection possible once an alarm row is selected.
	 * <p>
	 * We transmit an Alarm class and not just the Alarm ID that would be sufficient
	 * for extracting antenna names. This is to allow extraction of other information 
	 * in the future, e.g. timestamps. 
	 * The Alarm class comes from the CERN AS. Ideally we'd hide it inside some
	 * ACS class, but that would have to be done in the future, along with the rest of the alarm code.
	 */
	public static interface AlarmSelectionListener {
		
		public void notifyAlarmSelected(Alarm alarm);
	}
	
	/**
	 * Allows the external code to be notified when an alarm gets selected in the table.
	 * <p>
	 * If called multiple times, the provided listener will supersede previously registered listeners. 
	 */
	public void setAlarmSelectionListener(AlarmSelectionListener listener) {
		alarmTable.setAlarmSelectionListener(listener);
	}
}
