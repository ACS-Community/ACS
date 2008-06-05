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

import java.util.Date;

import javax.swing.JCheckBox;

import com.cosylab.gui.components.r2.DateTimeChooser;
import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.InvalidFilterConstraintException;
/**
 * Serves the purpose of filtering according to the date. Used by FilterParameterDialog. 
 * Creation date: (2/7/02 11:59:47 AM)
 * @author: 
 */
public class FilterDatePanel extends FilterParameterPanel {
	private DateTimeChooser minimum;
	private DateTimeChooser maximum;

	private JCheckBox minimumCheck;
	private JCheckBox maximumCheck;
	
	private JCheckBox notCheck; // NOT policy
	
/**
 * FilterDatePanel constructor comment.
 */
public FilterDatePanel() {
	super();
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:59:47 AM)
 */
protected void createComponents() {
	
	notCheck = new JCheckBox("Discard entries matching this filter");
	notCheck.setToolTipText("Keep/discard entries matching this filter");
	add(notCheck,newConstraints(0,4,4,4,4));
	
	minimumCheck = new JCheckBox("From");
	add(minimumCheck, newConstraints(1, 4, 4, 0, 4));
		
	minimum = new DateTimeChooser();
	add(minimum, newConstraints(2, 0, 4, 4, 4));

	maximumCheck = new JCheckBox("To");
	add(maximumCheck, newConstraints(3, 4, 4, 0, 4));

	maximum = new DateTimeChooser();
	add(maximum, newConstraints(4, 0, 4,4,4));
	
	
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:59:47 AM)
 * @return com.cosylab.logging.engine.Filter
 * @exception com.cosylab.logging.engine.InvalidFilterConstraintException The exception description.
 */
public Filter getFilter() throws FilterParameterException {
	Date min = null;
	Date max = null;

	if (minimumCheck.isSelected()) {
		min = minimum.getDate();
//		System.out.println(min.toString());
	}

	if (maximumCheck.isSelected()) {
		max = maximum.getDate();		
//		System.out.println(max.toString());
	}

	if ((min == null) && (max == null)) {
		throw new FilterParameterException("Select at least one constraint");
	}
	if ((min != null) && (max != null)) {
		if (min.compareTo(max) > -1)
			throw new FilterParameterException("From must be less than To");
	}

	try {
		return new Filter(getFieldIndex(), isLethal(), min, max,notCheck.isSelected());
	} catch (InvalidFilterConstraintException e) {
		throw new FilterParameterException(e.getMessage());
	}
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:59:47 AM)
 * @param f com.cosylab.logging.engine.Filter
 */
public void setFilter(com.cosylab.logging.engine.Filter f) {
	if (f == null)
		return;
		
	if ((f.constraint == Filter.Constraint.MINMAX) || (f.constraint == Filter.Constraint.MINIMUM)) {
		minimum.setDate((Date)f.minimum);
		minimumCheck.setSelected(true);
	} else {
		minimumCheck.setSelected(false);
	}
	if ((f.constraint == Filter.Constraint.MINMAX) || (f.constraint == Filter.Constraint.MAXIMUM)) {
		maximum.setDate((Date)f.maximum);
		maximumCheck.setSelected(true);
	} else {
		maximumCheck.setSelected(false);
	}
	notCheck.setSelected(f.notPolicyApplyed());
}
}
