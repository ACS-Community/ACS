package alma.acs.alarm.source.gui;

import java.util.Vector;

import javax.swing.table.AbstractTableModel;

import cern.laser.source.alarmsysteminterface.FaultState;

public class SourceTableModel extends AbstractTableModel {
	
	// The max number of fault states to show in the table
	private static final int MAX_FAULTSTATES = 10000;
	
	// The fault states received from the sources
	private Vector<FaultState> faultStates = new Vector<FaultState>();	
	
	// The names of the headers
	private static final String[] headerNames = {
		"Family",
		"Member",
		"Code",
		"Activator"
	};
	
	public String getColumnName(int column) {
		return headerNames[column];
	}

	
	public int getColumnCount() {
		return headerNames.length;
	}

	
	public int getRowCount() {
		return faultStates.size();
	}

	
	public Object getValueAt(int rowIndex, int columnIndex) {
		FaultState state = faultStates.get(rowIndex);
		switch (columnIndex) {
		case 0: return state.getFamily();
		case 1: return state.getMember();
		case 2: return Integer.toString(state.getCode());
		case 3: return state.getDescriptor();
		default: return "?";
		}
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
	public void addFS(FaultState faultState) {
		if (faultState==null) {
			throw new IllegalArgumentException("The FaultState can't be null!");
		}
		synchronized (faultStates) {
			if (faultStates.size()>MAX_FAULTSTATES) {
				faultStates.remove(faultStates.size()-1);
			}
			faultStates.add(0, faultState);
			fireTableDataChanged();
		}
	}
}
