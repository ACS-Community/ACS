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

import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JCheckBox;
import javax.swing.JPanel;

import com.cosylab.gui.components.r2.JIntegerTextField;
import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.InvalidFilterConstraintException;

/**
 * Serves the purpose of filtering according to the integer value. Used by FilterParameterDialog. 
 * Creation date: (2/7/02 10:42:17 AM)
 * @author: 
 */
public class FilterIntegerPanel extends FilterParameterPanel {
	private JIntegerTextField minimum;
	private JIntegerTextField maximum;
	private JIntegerTextField exact;
	protected JCheckBox minimumCheck;
	protected JCheckBox maximumCheck;
	protected JCheckBox exactCheck;
	
	private JCheckBox notCheck; // NOT policy


	private class MinmaxChecked implements ItemListener {
		public void itemStateChanged(ItemEvent e) {
			if (e.getStateChange() == ItemEvent.SELECTED)
				FilterIntegerPanel.this.exactCheck.setSelected(false);
		}
	}

	private class ExactChecked implements ItemListener {
		public void itemStateChanged(ItemEvent e) {
			if (e.getStateChange() == ItemEvent.SELECTED) {
				FilterIntegerPanel.this.minimumCheck.setSelected(false);
				FilterIntegerPanel.this.maximumCheck.setSelected(false);
			}
		}
	}
/**
 * FilterIntegerPanel constructor comment.
 */
public FilterIntegerPanel() {
	super();

}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 10:44:28 AM)
 */
protected void createComponents() {
	MinmaxChecked mmc = new MinmaxChecked();
	
	JPanel panelTop = new JPanel(new GridBagLayout());
	add(panelTop, newConstraints(0, 4, 4, 4, 4));
	
	notCheck = new JCheckBox("Discard entries matching this filter");
	notCheck.setToolTipText("Keep/discard entries matching this filter");
	panelTop.add(notCheck,newConstraints(0,4,4,4,4));

	minimumCheck = new JCheckBox("Minimum value");
	minimumCheck.addItemListener(mmc);
	panelTop.add(minimumCheck, newConstraints(1, 4, 0, 0, 0));
	
	minimum = new JIntegerTextField();
	panelTop.add(minimum, newConstraints(2, 0, 0, 4, 0));

	maximumCheck = new JCheckBox("Maximum value");
	maximumCheck.addItemListener(mmc);
	panelTop.add(maximumCheck, newConstraints(3, 4, 0, 0, 0));

	maximum = new JIntegerTextField();
	panelTop.add(maximum, newConstraints(4, 0, 0, 4, 0));

	JPanel panelBottom = new JPanel(new GridBagLayout());
	add(panelBottom, newConstraints(1, 4, 4, 4, 4));

	exactCheck = new JCheckBox("Exact value");
	exactCheck.addItemListener(new ExactChecked());
	panelBottom.add(exactCheck, newConstraints(0, 4, 0, 0, 0));

	exact = new JIntegerTextField();
	panelBottom.add(exact, newConstraints(1, 0, 0, 4, 0));
	
	
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:08:31 AM)
 * @return com.cosylab.logging.engine.Filter
 * @exception com.cosylab.logging.settings.FilterParameterException The exception description.
 */
public Filter getFilter() throws FilterParameterException {
	boolean bmin = minimumCheck.isSelected();
	boolean bmax = maximumCheck.isSelected();
	boolean bexact = exactCheck.isSelected();

	Integer min = null;
	Integer max = null;
	
	if (bexact) {
		try {
			return new Filter(
					getFieldIndex(), 
					isLethal(), 
					new Integer(exact.getIntegerValue()),
					notCheck.isSelected());
		} catch (InvalidFilterConstraintException e) {
			throw new FilterParameterException(e.getMessage());
		}
	}

	if (bmin) {
		min = new Integer(minimum.getIntegerValue());
	}
	if (bmax) {
		max = new Integer(maximum.getIntegerValue());
	}

	if ((min != null) && (max != null)) {
		if (min.compareTo(max) > -1) {
			throw new FilterParameterException("Minimum must be less than maximum");
		}
	}

	try {
		return new Filter(getFieldIndex(), isLethal(), min, max,notCheck.isSelected());
	} catch (InvalidFilterConstraintException e) {
		throw new FilterParameterException(e.getMessage());
	}
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:16:32 AM)
 * @param min int
 * @param max int
 */
public void setBounds(int min, int max) {
	minimum.setMinimum(min);
	minimum.setMaximum(max);

	maximum.setMinimum(min);
	maximum.setMaximum(max);

	exact.setMinimum(min);
	exact.setMaximum(max);
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:00:09 AM)
 * @param f com.cosylab.logging.engine.Filter
 */
public void setFilter(Filter f) {
	if (f == null)
		return;

	switch (f.constraint) {
		case EXACT:
			exactCheck.setSelected(true);
			exact.setIntegerValue(((Number) f.exact).intValue());
			break;
		case MINIMUM :
			setMinimum(f);
			break;
		case MAXIMUM :
			setMaximum(f);
			break;
		case MINMAX :
			setMinimum(f);
			setMaximum(f);
			break;
	}
	notCheck.setSelected(f.notPolicyApplyed());
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:06:35 AM)
 * @param f com.cosylab.logging.engine.Filter
 */
private void setMaximum(Filter f) {
	maximumCheck.setSelected(true);
	maximum.setIntegerValue(((Number) f.maximum).intValue());
}
/**
 * Insert the method's description here.
 * Creation date: (2/7/02 11:06:06 AM)
 * @param f com.cosylab.logging.engine.Filter
 */
private void setMinimum(Filter f) {
	minimumCheck.setSelected(true);
	minimum.setIntegerValue(((Number) f.minimum).intValue());
}
}
