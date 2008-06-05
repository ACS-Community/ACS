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

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JCheckBox;
import javax.swing.JTextField;

import com.cosylab.logging.engine.Filter;
/**
 * Serves the purpose of filtering according to a string.  
 * Creation date: (2/7/02 11:29:37 AM)
 * @author: 
 */
public class FilterStringPanel extends FilterParameterPanel {
	private JTextField exact;
	private JTextField regexp;

	private JCheckBox exactCheck;
	private JCheckBox regexpCheck;
	
	private JCheckBox notCheck; // NOT policy

	private class SelectionChecked implements ItemListener {
		public void itemStateChanged(ItemEvent e) {
			if (e.getStateChange() == ItemEvent.SELECTED) {
				if (e.getSource() == exactCheck)
					FilterStringPanel.this.regexpCheck.setSelected(false);
				else
					FilterStringPanel.this.exactCheck.setSelected(false);
			}
		}
	}
/**
 * FilterStringPanel constructor comment.
 */
public FilterStringPanel() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:41:10 AM)
 */
protected void createComponents() {
	SelectionChecked sc = new SelectionChecked();
	
	notCheck = new JCheckBox("Discard entries matching this filter");
	notCheck.setToolTipText("Keep/discard entries matching this filter");
	add(notCheck,newConstraints(0,4,4,4,4));
	
	exactCheck = new JCheckBox("Exact value");
	exactCheck.addItemListener(sc);
	add(exactCheck, newConstraints(1, 4, 4, 0, 4));

	exact = new JTextField();
	add(exact, newConstraints(2, 0, 4, 4, 4));

	regexpCheck = new JCheckBox("Regular expression pattern");
	regexpCheck.addItemListener(sc);
	add(regexpCheck, newConstraints(3, 4, 4, 0, 4));

	regexp = new JTextField();
	add(regexp, newConstraints(4, 0, 4, 4, 4));
	
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:41:10 AM)
 * @return com.cosylab.logging.engine.Filter
 * @exception com.cosylab.logging.engine.InvalidFilterConstraintException The exception description.
 */
public Filter getFilter() throws FilterParameterException {

	if (exactCheck.isSelected()) {
	    if (exact.getText().length() == 0)
		    throw new FilterParameterException("Cannot use empty string");
		try {
			return new com.cosylab.logging.engine.Filter(
				getFieldIndex(),
				isLethal(),
				(Object) (exact.getText()),
				notCheck.isSelected());
		} catch (Exception e) {
			throw new FilterParameterException(e.getMessage());
		}
	}
	if (regexpCheck.isSelected()) {
	    if (regexp.getText().length() == 0)
		    throw new FilterParameterException("Cannot use empty string");
		try {
			return new com.cosylab.logging.engine.Filter(
				getFieldIndex(),
				isLethal(),
				regexp.getText(),
				notCheck.isSelected());
		} catch (Exception e) {
			throw new FilterParameterException(e.getMessage());
		}
	}
	throw new FilterParameterException("Select at least one constraint");
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:41:10 AM)
 * @param f com.cosylab.logging.engine.Filter
 */
public void setFilter(com.cosylab.logging.engine.Filter f) {
	if (f == null)
		return;
		
	switch (f.constraint) {
		case EXACT :
			exactCheck.setSelected(true);
			exact.setText((String)f.exact);
			break;
		case STRING_WILDCHAR :
			regexpCheck.setSelected(true);
			regexp.setText((String)f.regularExpression);
			break;
	}
	notCheck.setSelected(f.notPolicyApplyed());
}
}
