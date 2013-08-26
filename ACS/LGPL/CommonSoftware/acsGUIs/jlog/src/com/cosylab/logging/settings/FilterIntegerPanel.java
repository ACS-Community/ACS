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
import java.util.regex.Pattern;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;

import alma.acs.gui.util.threadsupport.EDTExecutor;

import com.cosylab.logging.engine.ExactFilter;
import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.InvalidFilterConstraintException;
import com.cosylab.logging.engine.MinMaxFilter;

/**
 * Serves the purpose of filtering according to the integer value. Used by
 * FilterParameterDialog. Creation date: (2/7/02 10:42:17 AM)
 * 
 * @author:
 */
public class FilterIntegerPanel extends FilterParameterPanel {
	
	/**
	 * The filter to 
	 * <UL>
	 * 	<LI>accept only digits ({@link #digitsRegExp}
	 *  <LI>Limit the size of the input string to 9 digits
	 * </UL>
	 * 
	 * @author acaproni
	 * @since ACS 12.1
	 */
	private class FormattedIntDocFilter extends DocumentFilter {
		
		/**
		 * The regular expression used to check if the user input
		 *  contains only digits
		 */
		private final Pattern digitsRegExp = Pattern.compile("[0-9]*");
		
		/**
		 * The max number of digits that the user can insert in the text field
		 */
		public final int maxNumOfDigits;

		/**
		 * Constructor
		 * 
		 * @param size
		 *            The max number of digits that the user can insert in the
		 *            text field
		 */
		public FormattedIntDocFilter(int size) {
			this.maxNumOfDigits = size;
		}

		@Override
		public void remove(FilterBypass fb, int offset, int length)
				throws BadLocationException {
			// Removing is always allowed
			super.remove(fb, offset, length);
		}

		/**
		 * According to {@link DocumentFilter} documentation this method is called prior of
		 * insertion of a string. 
		 * <P>
		 * We override to allow the insertion only at the condition described in the
		 * documentation of this class
		 * @see FormattedIntDocFilter
		 */
		@Override
		public void insertString(FilterBypass fb, int offset, String string,
				AttributeSet attr) throws BadLocationException {
			if (string==null || string.isEmpty()) {
				return;
			}
			if (
					digitsRegExp.matcher(string).matches() &&
					fb.getDocument().getLength()+string.length()<=maxNumOfDigits) {
				
				super.insertString(fb, offset, string, attr);
			}
		}

		/**
		 * According to {@link DocumentFilter} documentation this method is called prior of
		 * replacing a string in the text field with a text. 
		 * The length of the string to replace is also passed as parameter.
		 * <P>
		 * We override to allow the insertion only at the condition described in the
		 * documentation of this class
		 * @see FormattedIntDocFilter
		 */
		@Override
		public void replace(FilterBypass fb, int offset, int length,
				String text, AttributeSet attrs) throws BadLocationException {
			if (text==null || text.isEmpty()) {
				return;
			}
			if (
					digitsRegExp.matcher(text).matches() &&
					fb.getDocument().getLength()+text.length()-length<=maxNumOfDigits) {
				
				super.replace(fb, offset, length, text, attrs);
			} 
		}
	}

	/**
	 * The text field to insert the minimum value
	 */
	private JTextField minimum;

	/**
	 * The text field to insert the maximum value
	 */
	private JTextField maximum;
	
	/**
	 * The text field to insert the exact value
	 */
	private JTextField exact;
	
	
	protected JCheckBox minimumCheck;
	protected JCheckBox maximumCheck;
	protected JCheckBox exactCheck;

	private JCheckBox notCheck; // NOT policy
	
	/**
	 * All the integer text fields have the same properties:
	 * this method avoid code repetition.
	 *   
	 * @param initialValue The initial value in the text field
	 * @return The JTextField
	 * 
	 */
	private JTextField buildIntegerTextField(int initialValue) {
		JTextField ret=new JTextField(""+initialValue);
		ret.setColumns(10);
		AbstractDocument document = (AbstractDocument) ret.getDocument();
		document.setDocumentFilter(new FormattedIntDocFilter(9));
		return ret;
	}

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
	 * Insert the method's description here. Creation date: (2/7/02 10:44:28 AM)
	 */
	protected void createComponents() {
		EDTExecutor.instance().execute(new Runnable() {
			@Override
			public void run() {
				MinmaxChecked mmc = new MinmaxChecked();

				JPanel panelTop = new JPanel(new GridBagLayout());
				add(panelTop, newConstraints(0, 4, 4, 4, 4));

				notCheck = new JCheckBox("Discard entries matching this filter");
				notCheck.setToolTipText("Keep/discard entries matching this filter");
				panelTop.add(notCheck, newConstraints(0, 4, 4, 4, 4));

				minimumCheck = new JCheckBox("Minimum value");
				minimumCheck.addItemListener(mmc);
				panelTop.add(minimumCheck, newConstraints(1, 4, 0, 0, 0));

				minimum = buildIntegerTextField(1);
				panelTop.add(minimum, newConstraints(2, 0, 0, 4, 0));

				maximumCheck = new JCheckBox("Maximum value");
				maximumCheck.addItemListener(mmc);
				panelTop.add(maximumCheck, newConstraints(3, 4, 0, 0, 0));

				maximum = buildIntegerTextField(2);
				panelTop.add(maximum, newConstraints(4, 0, 0, 4, 0));

				JPanel panelBottom = new JPanel(new GridBagLayout());
				add(panelBottom, newConstraints(1, 4, 4, 4, 4));

				exactCheck = new JCheckBox("Exact value");
				exactCheck.addItemListener(new ExactChecked());
				panelBottom.add(exactCheck, newConstraints(0, 4, 0, 0, 0));

				exact = buildIntegerTextField(1);
				panelBottom.add(exact, newConstraints(1, 0, 0, 4, 0));
			}
		});
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 11:08:31 AM)
	 * 
	 * @return com.cosylab.logging.engine.Filter
	 * @exception com.cosylab.logging.settings.FilterParameterException
	 *                The exception description.
	 */
	public Filter getFilter() throws FilterParameterException {
		boolean bmin = minimumCheck.isSelected();
		boolean bmax = maximumCheck.isSelected();
		boolean bexact = exactCheck.isSelected();

		Integer min = null;
		Integer max = null;

		if (bexact) {
			try {
				return new ExactFilter(getFieldIndex(), isLethal(),
						Integer.parseInt(exact.getText()),
						notCheck.isSelected());
			} catch (InvalidFilterConstraintException e) {
				throw new FilterParameterException(e.getMessage());
			}
		}

		if (bmin) {
			min = Integer.parseInt(minimum.getText());
		}
		if (bmax) {
			max = Integer.parseInt(maximum.getText());
		}

		if ((min != null) && (max != null)) {
			if (min.compareTo(max) > -1) {
				throw new FilterParameterException(
						"Minimum must be less than maximum");
			}
		}

		try {
			return new MinMaxFilter(getFieldIndex(), isLethal(), min, max,
					notCheck.isSelected());
		} catch (InvalidFilterConstraintException e) {
			throw new FilterParameterException(e.getMessage());
		}
	}

	/**
	 * Insert the method's description here. Creation date: (2/7/02 11:00:09 AM)
	 * 
	 * @param f
	 *            com.cosylab.logging.engine.Filter
	 */
	public void setFilter(Filter f) {
		if (f == null)
			return;

		switch (f.getConstraint()) {
		case EXACT:
			setField(exactCheck, ((Number)((ExactFilter)f).getExact()).intValue(),exact);
			break;
		case MINIMUM:
			setField(minimumCheck, ((Number)((MinMaxFilter)f).getMinimum()).intValue(),minimum);
			break;
		case MAXIMUM:
			setField(maximumCheck, ((Number)((MinMaxFilter)f).getMaximum()).intValue(),maximum);
			break;
		case MINMAX:
			setField(minimumCheck, ((Number)((MinMaxFilter)f).getMinimum()).intValue(),minimum);
			setField(maximumCheck, ((Number)((MinMaxFilter)f).getMaximum()).intValue(),maximum);
			break;
		}
		notCheck.setSelected(f.notPolicyApplyed());
	}
	
	/**
	 * Select the checkbox and set the value in the text field.
	 * <P>
	 * The same operation is repeated for the same check box and test field:
	 * this simple method avoids code repetition.
	 * 
	 * @param checkBox The check box to select
	 * @param val The value to set in the text field
	 * @param textField The text field to set the value into
	 */
	private void setField(JCheckBox checkBox, int val, JTextField textField) {
		if (checkBox==null) {
			throw new IllegalArgumentException("The check box can't be null");
		}
		if (textField==null) {
			throw new IllegalArgumentException("The text field can't be null");
		}
		if (val<0) {
			throw new IllegalArgumentException("The value can't be less then 0! Rejected value is "+val);
		}
		checkBox.setSelected(true);
		textField.setText(""+val);
	}
}
