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

/**
 * This class implements a listener to the changes in TableColumnModel
 * which is used in the LogEntryTable.
 * Creation date: (1/24/02 4:48:28 PM)
 * @author: 
 */
public class ColumnListener implements javax.swing.event.TableColumnModelListener {
/**
 * ColumnListener constructor comment.
 */
public ColumnListener() {
	super();
}
	/** Tells listeners that a column was added to the model. */
public void columnAdded(javax.swing.event.TableColumnModelEvent e) {}
	/** Tells listeners that a column was moved due to a margin change. */
public void columnMarginChanged(javax.swing.event.ChangeEvent e) {}
	/** Tells listeners that a column was repositioned. */
public void columnMoved(javax.swing.event.TableColumnModelEvent e) {}
	/** Tells listeners that a column was removed from the model. */
public void columnRemoved(javax.swing.event.TableColumnModelEvent e) {}
	/**
	 * Tells listeners that the selection model of the
	 * TableColumnModel changed.
	 */
public void columnSelectionChanged(javax.swing.event.ListSelectionEvent e) {}
}
