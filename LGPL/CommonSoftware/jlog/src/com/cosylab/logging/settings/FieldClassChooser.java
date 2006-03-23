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

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.Filter;
import javax.swing.*;
import java.awt.event.*;

/**
 * Specific helper class used to allow selection of different editors for different
 * classes of parameters.
 * Creation date: (2/4/02 3:41:28 PM)
 * @author: 
 */
public class FieldClassChooser extends javax.swing.JComboBox {
	private FilterParameterPanel intEditor = null;
	private FilterParameterPanel doubleEditor = null;
	private FilterParameterPanel stringEditor = null;
	private FilterParameterPanel dateEditor = null;
	private FilterParameterPanel typeEditor = null;

	private FilterParameterPanel currentEditor = null;

	private JPanel mainPanel = null;

	private class ChangeListener implements ItemListener {
		public void itemStateChanged(ItemEvent e) {
			if (e.getStateChange() == ItemEvent.SELECTED) {
				FieldClassChooser.this.updateEditor();
			}
		}
	}
	
/**
 * FieldClassChooser constructor comment.
 */
public FieldClassChooser() {
	super();

	for (int i=0; i < ILogEntry.NUMBER_OF_FIELDS; i++) {
		addItem(ILogEntry.fieldNames[i]);
	}

	addItemListener(new ChangeListener());
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @return javax.swing.JPanel
 */
public FilterParameterPanel getDateEditor() {
	return dateEditor;
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @return javax.swing.JPanel
 */
public FilterParameterPanel getDoubleEditor() {
	return doubleEditor;
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 1:57:55 PM)
 * @return com.cosylab.logging.engine.Filter
 */
public Filter getFilter() throws FilterParameterException {
	if (currentEditor != null) 
		return currentEditor.getFilter();
	throw new FilterParameterException("No filter specified");
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @return javax.swing.JPanel
 */
public FilterParameterPanel getIntEditor() {
	return intEditor;
}

public FilterParameterPanel getTypeEditor() {
	return typeEditor;
}

/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @return javax.swing.JPanel
 */
public javax.swing.JPanel getMainPanel() {
	return mainPanel;
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:52:03 PM)
 * @return java.lang.Class
 */
public Class getSelectedClass() {
	return ILogEntry.fieldClasses[getSelectedIndex()];
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @return javax.swing.JPanel
 */
public FilterParameterPanel getStringEditor() {
	return stringEditor;
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:47:52 PM)
 * @param newEditor javax.swing.JPanel
 */
private void replaceEditor(FilterParameterPanel newEditor) {
	if (mainPanel != null) {
		mainPanel.removeAll();
		currentEditor = newEditor;
		if (newEditor != null) {
			java.awt.GridBagConstraints c = new java.awt.GridBagConstraints();

			c.fill = java.awt.GridBagConstraints.BOTH;
			c.gridx = 0;
			c.gridy = 0;
			c.weightx = 1.0;
			c.weighty = 1.0;

			newEditor.setFieldIndex(getSelectedIndex());
			newEditor.setLethal(false);
			
//			mainPanel.add(new JLabel("Test"), c);
			mainPanel.add(newEditor, c);
			mainPanel.doLayout();
			mainPanel.setVisible(false);
			mainPanel.setVisible(true);;
			invalidate();
			mainPanel.invalidate();
			invalidate();
			mainPanel.repaint();
		}
	}	
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @param newDateEditor javax.swing.JPanel
 */
public void setDateEditor(FilterParameterPanel newDateEditor) {
	dateEditor = newDateEditor;
	updateEditor();
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @param newDoubleEditor javax.swing.JPanel
 */
public void setDoubleEditor(FilterParameterPanel newDoubleEditor) {
	doubleEditor = newDoubleEditor;
	updateEditor();
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:46:51 PM)
 * @param c java.lang.Class
 */
public void setEditor(Class c) {
	if (c == String.class) {
		replaceEditor(stringEditor);
	} if (c == Integer.class) {
		if (getSelectedIndex()==1) {
			replaceEditor(typeEditor);
		} else { 
			replaceEditor(intEditor);
		}
	} if (c == java.util.Date.class)
		replaceEditor(dateEditor);
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 12:59:39 PM)
 * @param f com.cosylab.logging.engine.Filter
 */
public void setFilter(Filter f) {
	if (f == null)
		return;

	setSelectedIndex(f.field);
	if (currentEditor != null) {
		currentEditor.setFilter(f);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @param newIntEditor javax.swing.JPanel
 */
public void setIntEditor(FilterParameterPanel newIntEditor) {
	intEditor = newIntEditor;
	updateEditor();
}

public void setTypeEditor(FilterParameterPanel newTypeEditor) {
	typeEditor = newTypeEditor;
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @param newMainPanel javax.swing.JPanel
 */
public void setMainPanel(javax.swing.JPanel newMainPanel) {
	mainPanel = newMainPanel;
	updateEditor();
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:44:39 PM)
 * @param newStringEditor javax.swing.JPanel
 */
public void setStringEditor(FilterParameterPanel newStringEditor) {
	stringEditor = newStringEditor;
	updateEditor();
}
/**
 * Insert the method's description here.
 * Creation date: (2/4/02 3:52:53 PM)
 */
protected void updateEditor() {
	setEditor(getSelectedClass());	
}
}
