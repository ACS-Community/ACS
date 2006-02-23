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
package com.cosylab.logging.settings;

/**
 * Subclasses JDialog having Ok and Cancel modalities. Used by FilterParameterDialog.
 * Creation date: (2/7/02 11:26:03 AM)
 * @author: 
 */
public class JModalDialog extends javax.swing.JDialog {
	public static final int MODAL_CANCEL = 0;
	public static final int MODAL_OK = 1;

	private int modalResult = MODAL_CANCEL;
	
/**
 * JModalDialog constructor comment.
 */
public JModalDialog() {
	super();
	setModal(true);
}
/**
 * JModalDialog constructor comment.
 * @param owner java.awt.Dialog
 */
public JModalDialog(java.awt.Dialog owner) {
	super(owner);
}
/**
 * JModalDialog constructor comment.
 * @param owner java.awt.Dialog
 * @param title java.lang.String
 */
public JModalDialog(java.awt.Dialog owner, String title) {
	super(owner, title);
}
/**
 * JModalDialog constructor comment.
 * @param owner java.awt.Dialog
 * @param title java.lang.String
 * @param modal boolean
 */
public JModalDialog(java.awt.Dialog owner, String title, boolean modal) {
	super(owner, title, modal);
}
/**
 * JModalDialog constructor comment.
 * @param owner java.awt.Dialog
 * @param modal boolean
 */
public JModalDialog(java.awt.Dialog owner, boolean modal) {
	super(owner, modal);
}
/**
 * JModalDialog constructor comment.
 * @param owner java.awt.Frame
 */
public JModalDialog(java.awt.Frame owner) {
	super(owner);
}
/**
 * JModalDialog constructor comment.
 * @param owner java.awt.Frame
 * @param title java.lang.String
 */
public JModalDialog(java.awt.Frame owner, String title) {
	super(owner, title);
}
/**
 * JModalDialog constructor comment.
 * @param owner java.awt.Frame
 * @param title java.lang.String
 * @param modal boolean
 */
public JModalDialog(java.awt.Frame owner, String title, boolean modal) {
	super(owner, title, modal);
}
/**
 * JModalDialog constructor comment.
 * @param owner java.awt.Frame
 * @param modal boolean
 */
public JModalDialog(java.awt.Frame owner, boolean modal) {
	super(owner, modal);
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:28:09 AM)
 */
public void returnModalCancel() {
	modalResult = MODAL_CANCEL;
	hide();	
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:27:36 AM)
 */
public void returnModalOK() {
	modalResult = MODAL_OK;
	hide();	
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:26:30 AM)
 * @return int
 */
public int showModal() {
	setModal(true);
	show();
	return modalResult;
}
}
