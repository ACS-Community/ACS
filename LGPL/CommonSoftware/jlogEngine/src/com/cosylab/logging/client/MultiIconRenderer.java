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
import java.awt.Graphics;

import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;
import javax.swing.table.TableCellRenderer;

import com.cosylab.gui.components.r2.JMultiIconLabel;
/**
 * TableCellRenderer that allows rendering of icons. Objects selects the
 * icon to display based on the value object passed by the TableDataModel.
 * The parameter must be a integer Number object (Short, Integer, Byte).
 * The icons are loaded from the projects resource during the construction.
 * Creation date: (12/1/2001 16:37:14)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class MultiIconRenderer
	extends JMultiIconLabel
	implements TableCellRenderer {

	protected static javax.swing.border.Border noFocusBorder;
	private boolean isSelected;
	private boolean hasFocus;
	private Color bColor;
	private Color fColor;

/**
 * Generic constructor
 */
public MultiIconRenderer() {
	super();
	setPreferredSize(new java.awt.Dimension(18, 18));
	setOpaque(true);
	noFocusBorder = new EmptyBorder(1, 2, 1, 2);
	setBorder(noFocusBorder);

}
/**
 * MultiIconRenderer constructor comment.
 * @param resourceNames java.lang.String[]
 */
public MultiIconRenderer(java.lang.String[] resourceNames) {
	super(resourceNames);
}
/**
 * MultiIconRenderer constructor comment.
 * @param resourceNames java.lang.String[]
 * @param horizontalAlignement int
 */
public MultiIconRenderer(java.lang.String[] resourceNames, int horizontalAlignement) {
	super(resourceNames, horizontalAlignement);
}
/**
 * MultiIconRenderer constructor comment.
 * @param icons javax.swing.Icon[]
 */
public MultiIconRenderer(javax.swing.Icon[] icons) {
	super(icons);
}
/**
 * MultiIconRenderer constructor comment.
 * @param icons javax.swing.Icon[]
 * @param horizontalAlignment int
 */
public MultiIconRenderer(javax.swing.Icon[] icons, int horizontalAlignment) {
	super(icons, horizontalAlignment);
}
/**
 *  This method is sent to the renderer by the drawing table to
 *  configure the renderer appropriately before drawing.  Return
 *  the Component used for drawing.
 *
 * @param	table		the JTable that is asking the renderer to draw.
 *				This parameter can be null.
 * @param	value		the value of the cell to be rendered.  It is
 *				up to the specific renderer to interpret
 *				and draw the value.  eg. if value is the
 *				String "true", it could be rendered as a
 *				string or it could be rendered as a check
 *				box that is checked.  null is a valid value.
 * @param	isSelected	true is the cell is to be renderer with
 *				selection highlighting
 * @param	row	        the row index of the cell being drawn.  When
 *				drawing the header the rowIndex is -1.
 * @param	column	        the column index of the cell being drawn
 */
public java.awt.Component getTableCellRendererComponent(
	javax.swing.JTable table,
	java.lang.Object value,
	boolean isSelected,
	boolean hasFocus,
	int row,
	int column) {

	this.isSelected = isSelected;
	this.hasFocus = hasFocus;

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
	
	if (hasFocus) {
		setBorder(UIManager.getBorder("Table.focusCellHighlightBorder"));
		if (table.isCellEditable(row, column)) {
			setForeground(UIManager.getColor("Table.focusCellForeground"));
			setBackground(UIManager.getColor("Table.focusCellBackground"));
		}
	} else {
		setBorder(noFocusBorder);
	}
	return this;
}
public void paint( Graphics g )
	{
		g.setColor( bColor );

		g.fillRect( 0, 0, getWidth() - 1, getHeight() - 1 );

		super.paint( g );
	}
}
