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
		
		String[] Descriptions = new String[LogTypeHelper.values().length];
		int t=0;
		for (LogTypeHelper log: LogTypeHelper.values()) {
			Descriptions[t++]=log.logEntryType;
		}
		minimum = new JComboBox(Descriptions);
		minimum.setSelectedIndex(0);
		minimum.setEditable(false);
		minimum.setMaximumRowCount(Descriptions.length);
		minimum.setRenderer(rendererMin);
		panelTop.add(minimum, newConstraints(2, 0, 0, 4, 0));

		maximumCheck = new JCheckBox("Maximum value");
		maximumCheck.addItemListener(this);
		panelTop.add(maximumCheck, newConstraints(3, 4, 0, 0, 0));

		maximum = new JComboBox(Descriptions);
		maximum.setSelectedIndex(0);
		maximum.setEditable(false);
		maximum.setMaximumRowCount(Descriptions.length);
		maximum.setRenderer(rendererMax);
		panelTop.add(maximum, newConstraints(4, 0, 0, 4, 0));

		JPanel panelBottom = new JPanel(new GridBagLayout());
		add(panelBottom, newConstraints(1, 4, 4, 4, 4));

		exactCheck = new JCheckBox("Exact value");
		exactCheck.addItemListener(this);
		panelBottom.add(exactCheck, newConstraints(0, 4, 0, 0, 0));

		exact = new JComboBox(Descriptions);
		exact.setSelectedIndex(0);
		exact.setEditable(false);
		exact.setMaximumRowCount(Descriptions.length);
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

		Integer min = null;
		Integer max = null;
		
		if (bexact) {
			try {
				return new Filter(
						getFieldIndex(), 
						isLethal(), 
						new Integer(exact.getSelectedIndex()),
						notCheck.isSelected());
			} catch (InvalidFilterConstraintException e) {
				throw new FilterParameterException(e.getMessage());
			}
		}

		if (bmin) {
			min = new Integer(minimum.getSelectedIndex());
		}
		if (bmax) {
			max = new Integer(maximum.getSelectedIndex());
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

	/* (non-Javadoc)
	 * @see com.cosylab.logging.settings.FilterParameterPanel#setFilter(com.cosylab.logging.engine.Filter)
	 */
	public void setFilter(Filter f) {
		if (f == null)
			return;

		switch (f.constraint) {
			case EXACT :
				exactCheck.setSelected(true);
				exact.setSelectedIndex(((Number)f.exact).intValue());
				break;
			case MINIMUM :
				minimumCheck.setSelected(true);
				minimum.setSelectedIndex(((Number)f.minimum).intValue());
				break;
			case MAXIMUM :
				maximumCheck.setSelected(true);
				maximum.setSelectedIndex(((Number)f.maximum).intValue());
				break;
			case MINMAX :
				minimumCheck.setSelected(true);
				minimum.setSelectedIndex(((Number)f.minimum).intValue());
				maximumCheck.setSelected(true);
				maximum.setSelectedIndex(((Number)f.maximum).intValue());
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
