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
package alma.acs.logging.table.renderer;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;

import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.table.TableCellRenderer;

/**
 * Renders a button used to expand/collapse groups.
 * Creation date: (12/4/2001 12:12:52)
 * @author: Ales Pucelj (ales.pucelj@kgb.ijs.si)
 */
public class InfoRenderer implements TableCellRenderer {
	
	/**
	 * The icon showed when the log has additional data
	 */
	private static ImageIcon infoIcon=null;
	
	/**
	 * The label with the info icon
	 */
	private static final JLabel infoLabel=new JLabel();
	
	/**
	 * The label with no icon
	 */
	private static final JLabel emptyLabel = new JLabel();
		
	/**
	 * ExpandButtonRenderer constructor comment.
	 */
	public InfoRenderer() {
		super();
		if (infoIcon==null) {
			infoIcon=new ImageIcon(this.getClass().getResource("/info.gif"));
			infoLabel.setIcon(infoIcon);
		}
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
	public Component getTableCellRendererComponent(
			JTable table, Object value, boolean isSelected,
			boolean hasFocus, int row, int column) {
		
		if (value == null) {
			return emptyLabel;
		}

		if (value instanceof Boolean && (Boolean)value) {
			Boolean b = (Boolean)value;
			if (b.booleanValue()) {
				return infoLabel;
			} else {
				return emptyLabel;
			}
		} 
		return emptyLabel;
	}
	
}
