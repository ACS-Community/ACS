/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
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
 package alma.acs.alarm.source.gui;

import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;

import cern.laser.source.alarmsysteminterface.FaultState;

public class SourceTableModel extends AbstractTableModel {
	
	/**
	 * The max number of fault states to show in the table
	 */
	private static final int MAX_FAULTSTATES = 10000;
	
	/**
	 * The fault states received from the sources
	 */
	private List<FaultState> faultStates = Collections.synchronizedList(new Vector<FaultState>());	
	
	/**
	 * The names of the headers for the standard view
	 */
	private static final String[] headerNames = {
		"Timestamp",
		"Family",
		"Member",
		"Code",
		"Activator"
	};
	
	/**
	 * If <code>true</code> the table appear in the compact form
	 */
	private volatile boolean isCompact=false; 
	
	/**
	 * The name of the headers for the compact view
	 */
	private static final String[] compactHdrNames = {
		"Timestamp",
		"Triplet",
		"Activator"
	};
	
	public String getColumnName(int column) {
		if (!isCompact) {
			return headerNames[column];
		} else {
			return compactHdrNames[column]; 
		}
	}

	
	public int getColumnCount() {
		if (!isCompact) {
			return headerNames.length;
		} else {
			return compactHdrNames.length;
		}
	}

	public int getRowCount() {
		return faultStates.size();
	}

	public Object getValueAt(int rowIndex, int columnIndex) {
		FaultState state = faultStates.get(rowIndex);
		if (!isCompact) {
			switch (columnIndex) {
			case 0: return state.getUserTimestamp().toString();
			case 1: return state.getFamily();
			case 2: return state.getMember();
			case 3: return Integer.toString(state.getCode());
			case 4: return state.getDescriptor();
			default: return "?";
			} 
		} else {
			switch (columnIndex) {
				case 0: return buildCompactTimestamp(state.getUserTimestamp());
				case 1: return "<"+state.getFamily()+", "+state.getMember()+", "+Integer.toString(state.getCode())+">";
				case 2: return state.getDescriptor();
				default:return "?";
			}
		}
	}
	
	private String buildCompactTimestamp(Timestamp ts) {
		if (ts==null) {
			throw new IllegalArgumentException("The Timestamp  can't be null");
		}
		Calendar cal=Calendar.getInstance();
		cal.setTimeInMillis(ts.getTime());
		StringBuilder str = new StringBuilder();
		if (cal.get(Calendar.HOUR_OF_DAY)<10) {
			str.append('0');
		}
		str.append(cal.get(Calendar.HOUR_OF_DAY));
		str.append(':');
		if (cal.get(Calendar.MINUTE)<10) {
			str.append('0');
		}
		str.append(cal.get(Calendar.MINUTE));
		str.append(':');
		if (cal.get(Calendar.SECOND)<10) {
			str.append('0');
		}
		str.append(cal.get(Calendar.SECOND));
		str.append('.');
		str.append(cal.get(Calendar.MILLISECOND));
		return str.toString();
	}

	/**
	 * Add an alarm to the model.
	 * 
	 * Add an alarm to the vector of fault states.
	 * If the alarms in memory are more then the limit, an alarm is deleted
	 * from the vector.
	 * 
	 * @param faultState The alarm received
	 * 
	 * @see MAX_FAULTSTATES
	 */
	public synchronized void addFS(FaultState faultState) {
		if (faultState==null) {
			throw new IllegalArgumentException("The FaultState can't be null!");
		}
		while (faultStates.size()>MAX_FAULTSTATES) {
			faultStates.remove(faultStates.size()-1);
		}
		faultStates.add(0, faultState);
		fireTableDataChanged();
	}
	
	/**
	 * Clear the list of fault states shown in the table
	 */
	public synchronized void clear() {
		faultStates.clear();
		fireTableDataChanged();
	}
	
	/**
	 * Show the table as compact
	 * 
	 * @param compact if <code>true</code> the table is compactedS
	 */
	public void showCompact(boolean compact) {
		isCompact=compact;
		fireTableStructureChanged();
	}
}
