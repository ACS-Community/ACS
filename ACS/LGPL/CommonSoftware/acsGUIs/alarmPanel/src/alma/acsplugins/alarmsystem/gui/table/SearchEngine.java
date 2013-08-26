/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2009
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

import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

/**
 * Object of this class perform the search function on the alarm table.
 * 
 * @author acaproni
 *
 */
public class SearchEngine {
	
	/**
	 * The table of alarms
	 */
	private final AlarmTable table;
	
	/**
	 * The model of the table of alarms
	 */
	private final AlarmTableModel model;
	
	/**
	 * Constructor
	 * 
	 * @param table The table of logs
	 * @param model The table model
	 */
	public SearchEngine(AlarmTable table, AlarmTableModel model) {
		if (table==null) {
			throw new IllegalArgumentException("The AlarmTable can't be null");
		}
		this.table=table;
		if (model==null) {
			throw new IllegalArgumentException("The AlarmTableModel can't be null");
		}
		this.model=model;
	}
	
	/**
	 * Search for a string in the table
	 * <P>
	 * The method search for the string only in the visible columns.
	 * It succeeds is a column contains the passed string.
	 * <BR>
	 * The returned values tell if the methods reached the end (or the beginning)
	 * of the table without finding a matching row.
	 * <P> 
	 * @param string The string to search in the table
	 * @param next if <code>true</code> search the next entry, 
	 * 				otherwise the previous 
	 * @return the table index of the entry or -1 if no
	 * 			matching entry has been found
	 */
	public int search(String string, boolean next) {
		// Is the table empty?
		if (table.getRowCount()==0) {
			return -1;
		}
		int row=table.getSelectedRow();
		if (row==-1) {
			if (next) {
				row=0;
			} else {
				row=table.getRowCount()-1;
			}
		} else if (next) {
			row++;
		} else {
			row--;
		}
		while ((next && row<table.getRowCount()) || (!next && row>=0)) {
			TableColumnModel colModel = table.getColumnModel();
			// Remove all the columns
			for (int t=0; t< colModel.getColumnCount(); t++) {
				TableColumn tc=colModel.getColumn(t);
				int idx=tc.getModelIndex();
				Object obj = model.getValueAt(table.convertRowIndexToModel(row),idx);
				if (!(obj instanceof String)) {
					continue;
				}
				if (((String)obj).contains(string)) {
					// found
					return row;
				}
			}
			row=next?row+1:row-1;
		}
		return -1;
	}
}
