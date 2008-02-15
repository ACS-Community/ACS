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

/** 
 * @author  aaproni
 * @version $Id: AlarmTable.java,v 1.7 2008/02/15 17:38:14 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.JComponent;
import javax.swing.event.RowSorterListener;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableRowSorter;

import alma.acsplugins.alarmsystem.gui.AlarmTableModel.AlarmTableColumn;

import cern.laser.client.data.Alarm;

/**
 * 
 * The table of alarms
 *
 */
public class AlarmTable extends JTable {
	// The model of the table
	private AlarmTableModel model;
	
	// The sorter for sorting the rows of the table
	private TableRowSorter<TableModel> sorter;
	
	private TableColumn[] columns;
	
	/**
	 * Constructor 
	 * @param model The model for this table
	 */
	public AlarmTable(AlarmTableModel model) {
		super(model);
		if (model==null) {
			throw new IllegalArgumentException("Invalid null model in constructor");
		}
		this.model=model;
		this.setCellSelectionEnabled(false);
		this.setOpaque(false);
		sorter = new TableRowSorter<TableModel>(model);
		this.setRowSorter(sorter);
		sorter.setMaxSortKeys(2);
		sorter.setSortsOnUpdates(true);
		
		// Remove all the columns that are not visible at startup
		TableColumnModel colModel = getColumnModel();
		columns = new TableColumn[colModel.getColumnCount()];
		for (int t=0; t<columns.length; t++) {
			columns[t]=colModel.getColumn(t);
		}
		for (AlarmTableColumn col: AlarmTableColumn.values()) {
			if (!col.visible) {
				colModel.removeColumn(columns[col.ordinal()]);
			} 
		}
	}
	
	/**
	 * @see JTable
	 */
	public Component prepareRenderer(TableCellRenderer renderer, int rowIndex,
			int vColIndex) {
		Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);
		Alarm alarm = model.getRowAlarm(sorter.convertRowIndexToModel(rowIndex));
		colorizeCell(c, alarm);
	
		if (c instanceof JComponent) {
			JComponent jc = (JComponent) c;
			jc.setToolTipText("<HTML>"+((AlarmTableModel)model).getCellContent(rowIndex, vColIndex));
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
	private void colorizeCell(Component c, Alarm alarm ) {
		AlarmGUIType alarmType = AlarmGUIType.fromAlarm(alarm);
		c.setForeground(alarmType.foreg);
		c.setBackground(alarmType.backg);
	}
}