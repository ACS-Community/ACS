package com.cosylab.gui.components.r2;

import javax.swing.text.*;

/**
 * Descendant of JTextField which only allows input of real-typed values.
 * Stores value internally as double. Provides accessors and fires property
 * change event on each valid value change.
 * Creation date: (10/28/01 21:22:31)
 * @author: 
 */
public class JDoubleTextField extends javax.swing.JTextField {

	private double doubleValue = 0.0;
	private FloatingPointParser fpParser = new FloatingPointParser();

	protected class DoubleDocument extends PlainDocument {

		public void insertString(int offs, String str, AttributeSet a) throws BadLocationException {

			JDoubleTextField owner = JDoubleTextField.this;

			String currentText = getText(0, getLength());
			String beforeOffset = currentText.substring(0, offs);
			String afterOffset = currentText.substring(offs, currentText.length());

			String proposedResult = beforeOffset + str + afterOffset;

			//double oldValue = owner.getDoubleValue();
			double newValue = Double.NaN;

			try {
				newValue = owner.fpParser.parseDouble(proposedResult);

				super.insertString(offs, str, a);

				owner.internalSet(newValue);

			} catch (RuntimeException e) {
			} catch (BadLocationException e) {
			}

		}

		public void remove(int offs, int len) {
			JDoubleTextField owner = JDoubleTextField.this;

			try {

			       String currentText = getText(0, getLength());
			       String beforeOffset = currentText.substring(0, offs);
			       String afterOffset = currentText.substring(offs + len, currentText.length());
			
			       String proposedResult = beforeOffset + afterOffset;

			       //double oldValue = owner.getDoubleValue();
			       double newValue = 0.0;

				newValue = owner.fpParser.parseDouble(proposedResult);

				super.remove(offs, len);

				owner.internalSet(newValue);

			} catch (RuntimeException e) {
			} catch (BadLocationException e) {
			}
		}

	}

/**
 * Default constructor for JDoubleTextField.
 */
public JDoubleTextField() {
	super();
}
/**
 * Constructs the new JDoubleTextField and sets the default value.
 * @param text java.lang.String
 */
public JDoubleTextField(double value) {
	this();
	setDoubleValue(value);
}
/**
 * Constructs a new JDoubleTextField and sets the text property. If the text
 * does not specify a valid double value, nothing will happen.
 * @param text java.lang.String
 */
public JDoubleTextField(String text) {
	super(text);
}
/**
 * Overrides the default function to create DoubleDocument instead of the default one.
 */

protected Document createDefaultModel() {
    return new DoubleDocument();
}
/**
 * Returns value represented by this component or zero if not valid.
 * Creation date: (29-Oct-01 1:44:45 PM)
 * @return double
 */
public double getDoubleValue() {
	return doubleValue;
}
/**
 * Sets the value and triggers the property change event.
 * Creation date: (12/16/2001 17:57:14)
 * @param value double
 */
protected void internalSet(double value) {
    if (doubleValue != value) {
        double oldValue = doubleValue;
        doubleValue = value;
        firePropertyChange("doubleValue", oldValue, value);
    }
}
/**
 * Sets the double value of this component.
 * Creation date: (29-Oct-01 1:44:45 PM)
 * @param newDoubleValue double
 */
public void setDoubleValue(double newDoubleValue) {
	internalSet(newDoubleValue);
}
}
