package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (12.1.2002 17:26:20)
 * @author: 
 */
public class DefaultTimeXAxis extends DefaultChartXAxis {
	protected java.text.SimpleDateFormat dateFormat = null;
/**
 * DefaultTimeXAxis constructor.
 */
public DefaultTimeXAxis() {
	super();
}
/**
 * This method return string that represent date, which is constructed from parameter specified number.
 * @return java.lang.String
 * @param number double
 */
public String format(double number) {
	return getDateFormat().format(new java.util.Date(((long)number)*1000L));
}
/**
 * This method return dateFormat. If it is not initialized return new <code>java.text.SimpleDateFormat</code>.
 * @return java.text.SimpleDateFormat
 */
public java.text.SimpleDateFormat getDateFormat() {
	if (dateFormat==null) dateFormat=new  java.text.SimpleDateFormat(format);
	return dateFormat;
}
/**
 * This method sets new format in <code>AbstractChartAxis</code>.
 * @param newFormat java.lang.String
 */
public void setFormat(java.lang.String newFormat) {
	format = newFormat;
	if (format==null) format="hh:mm:ss";
	getDateFormat().applyPattern(format);
}
}
