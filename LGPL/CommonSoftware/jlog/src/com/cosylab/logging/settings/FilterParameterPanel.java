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

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.log.LogField;

/**
 * Serves the purpose of filtering according to the parameter. Used by
 * FieldClassChoser, FilterDataPanel, FilterIntegerPanel, FilterParameterPanel
 * and FilterStringPanel. Creation date: (2/7/02 11:35:17 AM)
 * 
 * @author:
 */
public abstract class FilterParameterPanel extends javax.swing.JPanel {
	/**
	 * The field filtered
	 */
	private LogField field;

	private boolean lethal;

	/**
	 * FilterParameterPanel constructor comment.
	 */
	public FilterParameterPanel() {
		super(new GridBagLayout());
		createComponents();
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 11:39:51 AM)
	 */
	protected abstract void createComponents();

	/**
	 * Insert the method's description here. Creation date: (2/7/02 11:53:41 AM)
	 * 
	 * @return int
	 */
	public LogField getFieldIndex() {
		return field;
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 11:38:47 AM)
	 * 
	 * @return com.cosylab.logging.engine.Filter
	 * @exception com.cosylab.logging.engine.InvalidFilterConstraintException
	 *                The exception description.
	 */
	public abstract Filter getFilter() throws FilterParameterException;

	/**
	 * Insert the method's description here. Creation date: (2/7/02 11:53:41 AM)
	 * 
	 * @return boolean
	 */
	public boolean isLethal() {
		return lethal;
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 10:45:18 AM)
	 * 
	 * @param y
	 *            int
	 * @param insetsTop
	 *            int
	 * @param insetsBottom
	 *            int
	 */
	protected GridBagConstraints newConstraints(int y, int insetsTop,
			int insetsLeft, int insetsBottom, int insetsRight) {
		GridBagConstraints g = new GridBagConstraints();
		g.gridx = 0;
		g.gridy = y;
		g.insets = new Insets(insetsTop, insetsLeft, insetsBottom, insetsRight);
		g.fill = GridBagConstraints.HORIZONTAL;
		g.weightx = 1.0;
		g.weighty = 0.0;
		return g;
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 11:53:41 AM)
	 * 
	 * @param newFieldIndex
	 *            int
	 */
	public void setFieldIndex(LogField newField) {
		field = newField;
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 11:37:19 AM)
	 * 
	 * @param f
	 *            com.cosylab.logging.engine.Filter
	 */
	public abstract void setFilter(Filter f);

	/**
	 * Insert the method's description here. Creation date: (2/7/02 11:53:41 AM)
	 * 
	 * @param newLethal
	 *            boolean
	 */
	public void setLethal(boolean newLethal) {
		lethal = newLethal;
	}
}
