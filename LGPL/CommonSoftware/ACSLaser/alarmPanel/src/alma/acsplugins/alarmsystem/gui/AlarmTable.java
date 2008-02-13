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
 * @version $Id: AlarmTable.java,v 1.3 2008/02/13 21:39:40 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.JComponent;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.table.TableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableRowSorter;

/**
 * 
 * The table of alarms
 *
 */
public class AlarmTable extends JTable {
	// The model of the table
	private TableModel model;
	
	// The sorter for sorting the rows of the table
	private TableRowSorter<TableModel> sorter;
	
	/**
	 * Constructor 
	 * @param model The model for this table
	 */
	public AlarmTable(TableModel model) {
		super(model);
		if (model==null) {
			throw new IllegalArgumentException("Invalid null model in constructor");
		}
		this.model=model;
		this.setCellSelectionEnabled(false);
		sorter = new TableRowSorter<TableModel>(model);
		this.setRowSorter(sorter);
		sorter.setMaxSortKeys(2);
		sorter.setSortsOnUpdates(true);
		// Initially sorts by time
	}
	
	public Component prepareRenderer(TableCellRenderer renderer, int rowIndex,
			int vColIndex) {
		Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);
		if (c instanceof JComponent) {
			JComponent jc = (JComponent) c;
			jc.setToolTipText("<HTML>"+((AlarmTableModel)model).getCellContent(rowIndex, vColIndex));
		}
		return c;
	}

}