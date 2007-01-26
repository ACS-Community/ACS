/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package com.cosylab.logging;

import javax.swing.border.*;

import alma.acs.logging.dialogs.main.LogEntryTable;
/**
 * This class implements the method required by any object 
 * that would like to be a renderer for cells in a JTable.
 * Used in the LogEntryTable to allow rendering headers. 
 * <p>
 * Creation date: (1/6/2002 16:22:33)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class SortableHeaderRenderer extends javax.swing.JButton implements javax.swing.table.TableCellRenderer {
	private javax.swing.ImageIcon[] icons = new javax.swing.ImageIcon[3];

//	private static final Insets noBorder = new Insets(0, 0, 0, 0);
//	private static final Insets defaultBorder = new Insets(1, 1, 1, 1);

	private static final Border noBorder = new EmptyBorder(0, 0, 0, 0);
	private static final Border defaultBorder = new BevelBorder(BevelBorder.RAISED);
	
/**
 * SortableHeaderRenderer constructor comment.
 */
public SortableHeaderRenderer() {
	super();

	this.setPreferredSize(new java.awt.Dimension(50, 16));
	
	icons[0] = null;
	try {
		icons[1] = new javax.swing.ImageIcon(getClass().getResource("/arrup.gif"));
		icons[2] = new javax.swing.ImageIcon(getClass().getResource("/arrdown.gif"));
	} catch (Exception e) {
		System.out.println("Error loading icons");
	}
}
public final java.awt.Component getTableCellRendererComponent(
	javax.swing.JTable table,
	Object value,
	boolean isSelected,
	boolean hasFocus,
	int row,
	int column) {

	LogEntryTable let = (LogEntryTable) table;

	setFont(table.getFont());

	if (value == null)
		return this;

	LogTableDataModel ltdm = let.getLCModel();

	if (ltdm == null)
		return this;
	
	if (table.convertColumnIndexToModel(column) == 0 || table.convertColumnIndexToModel(column) == 1 ) {
		setIcon(null);
		setBorder(noBorder);
	} else {
		setBorder(defaultBorder);
		if (let.getSortIndex() + 2 == table.convertColumnIndexToModel(column)) {
			if (let.isSortAscending())
				setIcon(icons[1]);
			else
				setIcon(icons[2]);
		} else
			setIcon(icons[0]);
	}

	setText(value.toString());

	return this;
}
}
