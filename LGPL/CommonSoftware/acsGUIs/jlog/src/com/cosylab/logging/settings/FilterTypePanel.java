/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/*
 * Created on Dec 20, 2004
 *
 */
package com.cosylab.logging.settings;

import java.awt.GridBagLayout;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JPanel;

import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.ExactFilter;
import com.cosylab.logging.engine.MinMaxFilter;
import com.cosylab.logging.engine.InvalidFilterConstraintException;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * A class to edit a filter based on the type of the log
 * (the type is an Integer but we wish to show the type as
 * a string with an icon to avoid confusion while
 * selecting)
 * 
 * @author acaproni
 *
 */
public class FilterTypePanel extends FilterParameterPanel implements ItemListener {
	
	protected JCheckBox minimumCheck;
	protected JCheckBox maximumCheck;
	protected JCheckBox exactCheck;
	
	private JCheckBox notCheck; // NOT policy
	
	private JComboBox minimum;
	private JComboBox maximum;
	private JComboBox exact;
	
	// The renderer for the combo boxes
	public LogTypeRenderer rendererMin;
	private LogTypeRenderer rendererMax;
	private LogTypeRenderer rendererExact;
	
	// The icons
	ImageIcon[] icons;
	
	/**
	 * Constructor
	 */
	public FilterTypePanel() {
		super();
		
	}

	/* (non-Javadoc)
	 * @see com.cosylab.logging.settings.FilterParameterPanel#createComponents()
	 */
	public void createComponents() {
		
		// Build the renderer for the combo boxes
		rendererMin = new LogTypeRenderer();
		rendererMax = new LogTypeRenderer();
		rendererExact = new LogTypeRenderer();
		
		JPanel panelTop = new JPanel(new GridBagLayout());
		add(panelTop, newConstraints(0, 4, 4, 4, 4));
		
		notCheck = new JCheckBox("Discard entries matching this filter");
		notCheck.setToolTipText("Keep/discard entries matching this filter");
		panelTop.add(notCheck,newConstraints(0,4,4,4,4));

		minimumCheck = new JCheckBox("Minimum value");
		minimumCheck.addItemListener(this);
		panelTop.add(minimumCheck, newConstraints(1, 4, 0, 0, 0));
		
		LogTypeHelper[] logTypes = LogTypeHelper.values();
		minimum = new JComboBox(logTypes);
		minimum.setSelectedIndex(0);
		minimum.setEditable(false);
		minimum.setMaximumRowCount(logTypes.length);
		minimum.setRenderer(rendererMin);
		panelTop.add(minimum, newConstraints(2, 0, 0, 4, 0));

		maximumCheck = new JCheckBox("Maximum value");
		maximumCheck.addItemListener(this);
		panelTop.add(maximumCheck, newConstraints(3, 4, 0, 0, 0));

		maximum = new JComboBox(logTypes);
		maximum.setSelectedIndex(0);
		maximum.setEditable(false);
		maximum.setMaximumRowCount(logTypes.length);
		maximum.setRenderer(rendererMax);
		panelTop.add(maximum, newConstraints(4, 0, 0, 4, 0));

		JPanel panelBottom = new JPanel(new GridBagLayout());
		add(panelBottom, newConstraints(1, 4, 4, 4, 4));

		exactCheck = new JCheckBox("Exact value");
		exactCheck.addItemListener(this);
		panelBottom.add(exactCheck, newConstraints(0, 4, 0, 0, 0));

		exact = new JComboBox(logTypes);
		exact.setSelectedIndex(0);
		exact.setEditable(false);
		exact.setMaximumRowCount(logTypes.length);
		exact.setRenderer(rendererExact);
		panelBottom.add(exact, newConstraints(1, 0, 0, 4, 0));
	}

	/* (non-Javadoc)
	 * @see com.cosylab.logging.settings.FilterParameterPanel#getFilter()
	 */
	public Filter getFilter() throws FilterParameterException {
		boolean bmin = minimumCheck.isSelected();
		boolean bmax = maximumCheck.isSelected();
		boolean bexact = exactCheck.isSelected();

		LogTypeHelper min = null;
		LogTypeHelper max = null;
		
		if (bexact) {
			try {
				return new ExactFilter(
						getFieldIndex(), 
						isLethal(), 
						(LogTypeHelper)exact.getSelectedItem(),
						notCheck.isSelected());
			} catch (InvalidFilterConstraintException e) {
				e.printStackTrace();
				throw new FilterParameterException(e.getMessage());
			}
		}

		if (bmin) {
			min = (LogTypeHelper)minimum.getSelectedItem();
		}
		if (bmax) {
			max = (LogTypeHelper)maximum.getSelectedItem();
		}

		if ((min != null) && (max != null)) {
			if (min.compareTo(max) > -1) {
				throw new FilterParameterException("Minimum must be less than maximum");
			}
		}

		try {
			return new MinMaxFilter(getFieldIndex(), isLethal(), min, max,notCheck.isSelected());
		} catch (InvalidFilterConstraintException e) {
			throw new FilterParameterException(e.getMessage());
		}
	}

	/* (non-Javadoc)
	 * @see com.cosylab.logging.settings.FilterParameterPanel#setFilter(com.cosylab.logging.engine.Filter)
	 */
	public void setFilter(Filter f) {
		if (f == null)
			return;

		switch (f.getConstraint()) {
			case EXACT :
				exactCheck.setSelected(true);
				exact.setSelectedIndex(((LogTypeHelper)((ExactFilter)f).getExact()).ordinal());
				break;
			case MINIMUM :
				minimumCheck.setSelected(true);
				minimum.setSelectedIndex(((LogTypeHelper)((MinMaxFilter)f).getMinimum()).ordinal());
				break;
			case MAXIMUM :
				maximumCheck.setSelected(true);
				maximum.setSelectedIndex(((LogTypeHelper)((MinMaxFilter)f).getMaximum()).ordinal());
				break;
			case MINMAX :
				minimumCheck.setSelected(true);
				minimum.setSelectedIndex(((LogTypeHelper)((MinMaxFilter)f).getMinimum()).ordinal());
				maximumCheck.setSelected(true);
				maximum.setSelectedIndex(((LogTypeHelper)((MinMaxFilter)f).getMaximum()).ordinal());
				break;
		}
		notCheck.setSelected(f.notPolicyApplyed());
	}
	
	public void itemStateChanged(ItemEvent e) {
		if (e.getStateChange()==ItemEvent.SELECTED) {
			if (e.getSource()==exactCheck) {
				maximumCheck.setSelected(false);
				minimumCheck.setSelected(false);
			} else if (e.getSource()==minimumCheck || e.getSource()==maximumCheck) {
				exactCheck.setSelected(false);
			}
		}
	}
	

}
