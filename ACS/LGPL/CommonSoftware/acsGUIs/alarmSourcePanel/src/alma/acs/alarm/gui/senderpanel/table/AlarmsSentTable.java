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
import java.util.Vector;

import javax.swing.JList;
import javax.swing.JTable;

import alma.acs.alarm.gui.senderpanel.SenderPanelUtils.Triplet;

/**
 * The table with the alarms sent
 * <P>
 * The easiest way is to use a simple {@link JList} but it is very 
 * ugly when initialized empty
 * 
 * @author acaproni
 */
public class AlarmsSentTable extends JTable {
	
	/**
	 * The table model
	 */
	private final AlarmsSentTableModel model = new AlarmsSentTableModel();

	/**
	 * Constructor
	 */
	public AlarmsSentTable() {
		super();
		super.setModel(model);
		getColumnModel().getColumn(0).setHeaderValue("Alarms sent");
		model.start();
	}
	
	/**
	 * Active alarms must be added to the table;
	 * terminate alarm must be removed.
	 * 
	 * @param triplet The triplet of the alrm
	 * @param active The state of the alarm
	 * @see AlarmsSentTableModel#alarmSent(String, boolean)
	 */
	public void alarmSent(Triplet triplet, boolean active) {
		model.alarmSent(triplet, active);
	}
	
	/**
	 * @return the alarms in the table
	 */
	public Collection<Triplet> getAlarms() {
		return model.getAlarms();
	}
	
	/**
	 * @return the alarms in the table
	 */
	public Collection<Triplet> getSelectedAlarms() {
		Vector<Triplet> ret= new Vector<Triplet>();
		int selectedRows[] =getSelectedRows();
		for (int idx: selectedRows) {
			ret.add((Triplet)model.getValueAt(idx, 0));
		}
		return ret;
	}
}
