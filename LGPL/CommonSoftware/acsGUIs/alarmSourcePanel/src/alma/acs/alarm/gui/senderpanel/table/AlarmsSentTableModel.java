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
package alma.acs.alarm.gui.senderpanel.table;

import java.util.Collection;
import java.util.Collections;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;

/**
 * Th emodel for the table
 * 
 * @author acaproni
 *
 */
public class AlarmsSentTableModel extends AbstractTableModel {
	
	/**
	 * The alarms shown in the table
	 */
	private final Vector<String> alarms=new Vector<String>();

	@Override
	public int getRowCount() {
		return alarms.size();
	}

	@Override
	public int getColumnCount() {
		return 1;
	}

	@Override
	public Object getValueAt(int rowIndex, int columnIndex) {
		return alarms.get(rowIndex);
	}
	
	/**
	 * Active alarms must be added to the table;
	 * terminate alarm must be removed.
	 * 
	 * @param triplet The triplet of the alrm
	 * @param active The state of the alarm
	 * @return The number of alarms in the table
	 */
	public int alarmSent(String triplet, boolean active) {
		if (!active && alarms.contains(triplet)) {
			alarms.remove(triplet);
		} else if (active && !alarms.contains(triplet)) {
			alarms.add(triplet);
		}
		Collections.sort(alarms);
		fireTableDataChanged();
		return alarms.size();
	}
	
	/**
	 * @return The alarms in the table
	 */
	public Collection<String> getAlarms() {
		Vector<String> ret = new Vector<String>(alarms.size());
		for (String alarm: alarms) {
			ret.add(alarm);
		}
		return ret;
	}

}
