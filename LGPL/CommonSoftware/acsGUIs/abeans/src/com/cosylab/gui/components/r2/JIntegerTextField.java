package com.cosylab.gui.components.r2;

import javax.swing.text.*;

/**
 * Modified JTextField that can only hold integers. Provides convinience methods 
 * for direct access to numeric value. Performs neccesary safety checks.<p>
 * Minimum and maximum values that can be entered into this field can also
 * be set. By default they are 0 and 100 respectively. If user attempts to
 * enter a number outside these bounds, the value will be set to the
 * appropriate limit.
 * Creation date: (26-Oct-01 11:24:17 AM)
 * @author: 
 */
public class JIntegerTextField extends javax.swing.JTextField {

	private int integerValue = 0;

	private int minValue = 0;
	private int maxValue = 100;

	private boolean minimumSet = false;
	private boolean maximumSet = false;

	protected class IntegerDocument extends PlainDocument {

		public void remove(int off, int len) throws BadLocationException {
			
			JIntegerTextField owner = JIntegerTextField.this;

			super.remove(off, len);
			
			String newText = getText(0, getLength());
			
			int newIntegerValue = 0;

			try {
				newIntegerValue = Integer.parseInt(newText);

			} catch (NumberFormatException e) {
			}

			owner.internalSet(newIntegerValue);
		}

		public void insertString(int offs, String str, AttributeSet a)
			throws BadLocationException {

			JIntegerTextField owner = JIntegerTextField.this;
				
			char[] source = str.toCharArray();
			char[] result = new char[source.length];
			int j = 0;

			int l = result.length;

			for (int i = 0; i < l; i++) {
				if (Character.isDigit(source[i]))
					result[j++] = source[i];
			}

			super.insertString(offs, new String(result, 0, j), a);

			int newIntegerValue = 0;

			if (j > 0) {
				String newText = getText(0, getLength());
				try {

					newIntegerValue = Integer.parseInt(newText);

				} catch (NumberFormatException e) {
//					System.err.println("JIntegerTextField invalid conversion: " + newText);
				}
			}

			if ((owner.maximumSet) && (newIntegerValue > owner.maxValue)) {
				owner.setIntegerValue(owner.maxValue);
				owner.setText(String.valueOf(owner.maxValue));
				return;
			}

			if ((owner.minimumSet) && (newIntegerValue < owner.minValue)) {
				owner.setIntegerValue(owner.minValue);
				owner.setText(String.valueOf(owner.minValue));
				return;
			}
			
			owner.internalSet(newIntegerValue);
		}
	}
/**
 * Creates new JIntegerTextField.
 */
public JIntegerTextField() {
	super();
	setText(String.valueOf(integerValue));
}
    protected Document createDefaultModel() {
        return new IntegerDocument();

    }
/**
 * Returns the integer value represented by this component.
 * Creation date: (29-Oct-01 1:54:44 PM)
 * @return int
 */
public int getIntegerValue() {
	return integerValue;
}
/**
 * Sets the integer value and fires the property change event.
 * Creation date: (2/4/02 3:00:32 PM)
 * @param value int
 */
protected void internalSet(int value) {
	if (value == integerValue)
		return;
		
	int oldValue = integerValue;
	
	integerValue = value;

	firePropertyChange("integerValue", oldValue, value);
	
}
/**
 * Returns if this component has maximum set.
 * <p>
 * Creation date: (2/10/2002 18:38:35)
 * @return boolean
 */
public boolean isMaximumSet() {
	return maximumSet;
}
/**
 * Returns if this component has minimum set.
 * <p>
 * Creation date: (2/10/2002 18:38:35)
 * @return boolean
 */
public boolean isMinimumSet() {
	return minimumSet;
}
/**
 * Sets the integer value of <code>JIntegerTextField</code> and fires the
 * propertyChange event.
 * Creation date: (29-Oct-01 1:54:44 PM)
 * @param newValue int
 */
public void setIntegerValue(int newValue) {
//	internalSet(newValue);
	if (newValue != integerValue)
		setText(String.valueOf(newValue));
}
/**
 * Sets the maximum value that can be entered into this component. Any
 * value larger than this will be set to this maximum.
 * Creation date: (2/4/02 2:44:24 PM)
 * @param value int
 */
public void setMaximum(int value) {
	maximumSet = true;
	maxValue = value;	
}
/**
 * Sets whether the maximum value should be checked.
 * <p>
 * Creation date: (2/10/2002 18:38:35)
 * @param newMaximumSet boolean
 */
public void setMaximumSet(boolean newMaximumSet) {
	maximumSet = newMaximumSet;
}
/**
 * Sets the minimal value that can be displayed by this component. Any
 * value lower than this value will be set to this minimum.
 * Creation date: (2/4/02 2:43:53 PM)
 * @param value int
 */
public void setMinimum(int value) {
	minimumSet = true;
	minValue = value;	
}
/**
 * Sets whether currently set minimum should be checked.
 * <p>
 * Creation date: (2/10/2002 18:38:35)
 * @param newMinimumSet boolean
 */
public void setMinimumSet(boolean newMinimumSet) {
	minimumSet = newMinimumSet;
}
}
