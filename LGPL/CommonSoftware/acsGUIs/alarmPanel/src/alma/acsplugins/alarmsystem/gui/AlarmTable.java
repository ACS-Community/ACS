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
 * @version $Id: AlarmTable.java,v 1.5 2008/02/14 01:19:09 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.JComponent;
import javax.swing.event.RowSorterListener;
import javax.swing.table.TableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableRowSorter;

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
		// Initially sorts by time
	}
	
	/**
	 * @see JTable
	 */
	public Component prepareRenderer(TableCellRenderer renderer, int rowIndex,
			int vColIndex) {
		Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);
		Alarm alarm = model.getRowAlarm(sorter.convertRowIndexToModel(rowIndex));
		colorizeCell(c, alarm.getPriority(), alarm.getStatus().isActive());
	
		if (c instanceof JComponent) {
			JComponent jc = (JComponent) c;
			jc.setToolTipText("<HTML>"+((AlarmTableModel)model).getCellContent(rowIndex, vColIndex));
		}
		return c;
	}
	
	/**
	 * Set the background and the foreground of the component depending
	 * on the priority and the state
	 * 
	 * @param c The component to color
	 * @param priority The priority to set the color
	 * @param active The state active/inactive to set the color
	 */
	private void colorizeCell(Component c, int priority, boolean active) {
		if (active) {
			CellColor cellColor = CellColor.fromPriority(priority);
			c.setForeground(cellColor.foreg);
			c.setBackground(cellColor.backg);
		} else {
			c.setForeground(CellColor.INACTIVE.foreg);
			c.setBackground(CellColor.INACTIVE.backg);
		}
	}
}