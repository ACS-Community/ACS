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
 * @version $Id: AlarmTable.java,v 1.2 2007/09/28 12:56:04 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui;

import java.awt.Component;

import javax.swing.JTable;
import javax.swing.JComponent;
import javax.swing.table.TableModel;
import javax.swing.table.TableCellRenderer;

/**
 * 
 * The table of alarms
 *
 */
public class AlarmTable extends JTable {
	// The model of the table
	private TableModel model;
	
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