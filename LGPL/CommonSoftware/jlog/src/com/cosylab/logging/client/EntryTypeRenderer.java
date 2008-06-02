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
package com.cosylab.logging.client;

import java.awt.Color;

import javax.swing.JLabel;
import javax.swing.table.TableCellRenderer;

import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * A specialized renderer to display both icon and text for each type of the
 * log. Creation date: (12/4/2001 12:18:00)
 * 
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class EntryTypeRenderer extends JLabel implements TableCellRenderer {
	
	/**
	 * Background color
	 */
	private Color bColor;
	
	/**
	 * Foreground color
	 */
	private Color fColor;
	
	public EntryTypeRenderer() {
		super();
	}

	/**
	 * This method is sent to the renderer by the drawing table to configure the
	 * renderer appropriately before drawing. Return the Component used for
	 * drawing.
	 * 
	 * @param table
	 *            the JTable that is asking the renderer to draw. This parameter
	 *            can be null.
	 * @param value
	 *            the value of the cell to be rendered. It is up to the specific
	 *            renderer to interpret and draw the value. eg. if value is the
	 *            String "true", it could be rendered as a string or it could be
	 *            rendered as a check box that is checked. null is a valid
	 *            value.
	 * @param isSelected
	 *            true is the cell is to be renderer with selection highlighting
	 * @param row
	 *            the row index of the cell being drawn. When drawing the header
	 *            the rowIndex is -1.
	 * @param column
	 *            the column index of the cell being drawn
	 */
	public java.awt.Component getTableCellRendererComponent(
			javax.swing.JTable table, Object value, boolean isSelected,
			boolean hasFocus, int row, int column) {

		if (isSelected) {
		    fColor = table.getSelectionForeground();
		    bColor = table.getSelectionBackground();
		} else {
		    fColor = table.getForeground();
		    bColor = table.getBackground();
		}
		setForeground(fColor);
		setBackground(bColor);
		
		setFont(table.getFont());
		if (value == null) {
			return this;
		}

		if (value instanceof LogTypeHelper) {
			setIcon(EntryTypeIcon.getIcon((LogTypeHelper)value));
			setFont(table.getFont());
			setText(((LogTypeHelper)value).logEntryType);
		} 
		return this;

	}
}
