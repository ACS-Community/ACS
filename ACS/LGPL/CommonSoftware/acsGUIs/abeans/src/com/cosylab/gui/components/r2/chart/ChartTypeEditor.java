package com.cosylab.gui.components.r2.chart;

/**
 * This type was created in VisualAge.
 */
public class ChartTypeEditor extends java.beans.PropertyEditorSupport {
	private int mode;
	private static final String[] tags = {"DOTS", "LINES"};
/**
 * This method was created in VisualAge.
 * @return java.lang.String
 */
public String getAsText() {
	return tags[mode+1];
}
/**
 * This method was created in VisualAge.
 * @return java.lang.String[]
 */
public String[] getTags() {
	return tags;
}
/**
 * This method was created in VisualAge.
 * @return java.lang.Object
 */
public Object getValue() {
	return new Integer(mode);
}
/**
 * This method was created in VisualAge.
 * @param text java.lang.String
 */
public void setAsText(String text) throws IllegalArgumentException {
	if (text == null) throw new IllegalArgumentException("Null string value for this property is not allowed.");
	for (int i = 0; i < tags.length; i++)
	{
		if (tags[i].equals(text))
		{
			mode = i-1;
			firePropertyChange();
			return;
		}
	}
	throw new IllegalArgumentException("Selector mode with this tag does not exist.");
}
/**
 * This method was created in VisualAge.
 * @param value java.lang.Object
 */
public void setValue(Object value) {
	if (!(value instanceof Integer)) throw new IllegalArgumentException("Value edited is of an invalid type.");
	mode = ((Integer)value).intValue();
}
}
