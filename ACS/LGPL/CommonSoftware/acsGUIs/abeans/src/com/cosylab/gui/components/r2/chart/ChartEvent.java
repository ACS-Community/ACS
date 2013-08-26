package com.cosylab.gui.components.r2.chart;

/**
 * This is event object, used in a chart.
 */
public class ChartEvent extends java.util.EventObject {
	protected int type;
	public final static int RESET = 1;
	public final static int RELOAD = RESET+1;
	private static ChartEvent defaultResetEvent = null;
	private static ChartEvent defaultReloadEvent = null;
/**
 * ChartEvent constructor.
 * @param <code>source</code> java.lang.Object is object that caused event
 * @param <code>type</code> is type of event
 */
public ChartEvent(Object source, int type) {
	super(source);
	this.type= type;
}
/**
 * This method return field <code>defaultResetEvent</code> and initialize field <code>defaultReloadEvent</code>.
 */
public static ChartEvent defaultReloadEvent() {
	if (defaultReloadEvent==null) {
		defaultReloadEvent= new ChartEvent(null,RELOAD);
	}
	return defaultResetEvent;
}
/**
 * This method return field <code>defaultResetEvent</code> and initialize field <code>defaultResetEvent</code>.
 */
public static ChartEvent defaultResetEvent() {
	if (defaultResetEvent==null) {
		defaultResetEvent= new ChartEvent(null,RESET);
	}
	return defaultResetEvent;
}
/**
 * This method return type of event.
 */
public int getType() {
	return type;
}
/**
 * Returns a String that represents the value of this object.
 * @return a string representation of the receiver
 */
public String toString() {
	// Insert code to print the receiver here.
	// This implementation forwards the message to super. You may replace or supplement this.
	return super.toString();
}
}
