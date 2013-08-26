/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.demo.dyncomp;

import java.awt.Component;
import javax.swing.JButton;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

/** The class used to show the button inside the table
*
*  @author Alessandro Caproni, 2003, Nov 7 
*/
class  MyCellRendererr extends JButton implements TableCellRenderer {

	/** The constructor
	*/
	public  MyCellRendererr() {
	}

	/** Return the button to show into the cell when the user is not editing the cell
	*
	* @param table The JTable that own the cell
	* @param value The value of the cell
	* @param True if the cell is selected
	* @param hasFocus True if the cell has the focus
	* @param row The row of the cell
	* @param column The column of the cell
	*
	* @return The button to show into the cell
	*/
	public Component getTableCellRendererComponent(
			JTable table, 
			Object value, 
			boolean isSelected, 
			boolean hasFocus, 
			int row, 
			int column) {
		if (table.getValueAt(row,column)!=null) {
			JButton btn = (JButton)value;
			btn.setVisible(true);
			return btn;
		} else return null;
	}
	
}



