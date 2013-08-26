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
package alma.acsplugins.alarmsystem.gui.undocumented.table;

import java.awt.Color;
import java.awt.Component;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableRowSorter;

import alma.acsplugins.alarmsystem.gui.undocumented.table.UndocAlarmTableModel.AlarmData;
import alma.acsplugins.alarmsystem.gui.undocumented.table.UndocAlarmTableModel.ColumnTitles;

/**
 * The table of undocumented alarms
 * 
 * @author acaproni
 * @since ACS 10.0.0
 */
public class UndocumentedAlarmTable extends JTable {
	
	/**
	 * The model of the table
	 */
	private final UndocAlarmTableModel model; 
	
	/**
	 * The sorter of the table
	 */
	private final TableRowSorter<UndocAlarmTableModel> sorter;
	
	/**
	 * The background color of an active alarm
	 */
	private final Color activeBk=Color.yellow;
	
	/**
	 * The foreground color of an active alarm
	 */
	private final Color activeFg=Color.black;
	
	/**
	 * The background color of an inactive alarm
	 */
	private final Color inactiveBk=new Color(188,255,188);
	
	/**
	 * The foreground color of an inactive alarm
	 */
	private final Color inactiveFg=Color.black;

	/**
	 * Constructor
	 */
	public UndocumentedAlarmTable(UndocAlarmTableModel model) {
		super(model);
		this.model=model;
		sorter = new TableRowSorter<UndocAlarmTableModel>(model);
		this.setOpaque(false);
		sorter.setRowFilter(null);
		setRowSorter(sorter);
		List<RowSorter.SortKey> sortKeys = new ArrayList<RowSorter.SortKey>();
		sortKeys.add(new RowSorter.SortKey(ColumnTitles.TIME.ordinal(), SortOrder.DESCENDING));
		sortKeys.add(new RowSorter.SortKey(ColumnTitles.FAMILY.ordinal(), SortOrder.DESCENDING));
		sorter.setSortKeys(sortKeys); 
		sorter.setMaxSortKeys(2);
		sorter.setSortsOnUpdates(true);
	}
	
	/**
	 * @see JTable
	 */
	public Component prepareRenderer(TableCellRenderer renderer, int rowIndex, int vColIndex) {
		
		TableColumn col = getColumnModel().getColumn(vColIndex);
		AlarmData entry=null; 
		try {
			entry = model.getRowEntry(sorter.convertRowIndexToModel(rowIndex));
		} catch (Throwable t) {
			// This can happen if the entry has been removed by the thread while
			// this method runs.
			entry=null;
		}
		if (entry==null) {
			return new JLabel();
		}
		
		Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);
		colorizeCell(c, entry);
	
		if (c instanceof JComponent) {
			JComponent jc = (JComponent) c;
			if (((UndocAlarmTableModel)model).getValueAt(sorter.convertRowIndexToModel(rowIndex), convertColumnIndexToModel(vColIndex))==null) {
				jc.setToolTipText(null);
			} else { 
				jc.setToolTipText("<HTML>"+((UndocAlarmTableModel)model).getValueAt(sorter.convertRowIndexToModel(rowIndex), convertColumnIndexToModel(vColIndex)));
			}
		}
		return c;
	}
	
	/**
	 * Set the background and the foreground of the component depending
	 * on the priority and the state of the passed alarm
	 * 
	 * @param c The component to color
	 * @param priority The alarm to set the color
	 */
	private void colorizeCell(Component c, AlarmData alarm) {
		if (alarm.active) {
			c.setForeground(activeFg);
			c.setBackground(activeBk);
		} else {
			c.setForeground(inactiveFg);
			c.setBackground(inactiveBk);
		}
	}
}
