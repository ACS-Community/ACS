package com.cosylab.gui.components.r2.chart;

/**
 * This class represent interval between two values and is usually used for
 * specification of axis scale. It can fire event to listeners, when is changed.
 */
public class Interval {
	private double min = 0.0;
	private double max = 1.0;
	private java.util.ArrayList list;
	private boolean logaritmic= false;
/**
 * Interval constructor.
 */
public Interval() {
	super();
}
/**
 * Interval constructor, which takes as an argument minimal and maximal value of interval.
 */
public Interval(double min, double max) {
	super();
	this.min=min;
	this.max=max;
}
/**
 * This method adds IntervalListener to this class oblect.
 */
public synchronized void addIntervalListener(IntervalListener l) {
	if (l==null) return;
	if (list==null) list= new java.util.ArrayList(10);
	list.add(l);
}
/**
 * This method tests if compared object is instance of Interval object and if is,
 * then tests is values of both intervals are the same.
 */
public boolean equals(Object o) {
	if (o==null) return false;
	if (o instanceof Interval) if ((((Interval)o).max==max)&&(((Interval)o).min==min)) return true;
	return false;
}
private void fireIntervalChange() {
	if (list==null) return;
	for (int i=0; i<list.size(); i++) 
		try {
			((IntervalListener)list.get(i)).intervalChange(this);
		} catch (Throwable t) {System.out.println("Error IntervalChange event dispatching:");t.printStackTrace();}
}
/**
 * This method return length of interval.
 */
public double getLength() {
	return max-min;
}
/**
 * Return <code>true</code> if this is logaritmic interval.
 * @return boolean <code>true</code> if this is logaritmic interval.
 */
public boolean isLogaritmic() {
	return logaritmic;
}
/**
 * This method return length of interval.
 */
public double length() {
	return max-min;
}
/**
 * This method returns value on the end of interval (maximal value).
 */
public double max() {
	return max;
}
/**
 * This method returns value at start of interval (minimal value).
 */
public double min() {
	return min;
}
/**
 * This method removes parameter specified <code>IntervalListner</code>.
 */
public synchronized void removeIntervalListener(IntervalListener l) {
	if (list!=null) list.remove(l);
}
/**
 * This method sets start and end of interval.
 */
public void set(double min, double max) {
	this.min=min;
	this.max=max;
	fireIntervalChange();
}
/**
 * This method change interval to parameter specified interval and fires event <code>IntervalChange</code>
 * to all listeners in list.
 */
public void set(Interval interval) {
	if (interval==null) return;
	this.min=interval.min();
	this.max=interval.max();
	fireIntervalChange();
}
/**
 * Sets this object to present logaritmic interval. Interval event is fired.
 * @param newLogaritmic boolean true to set logaritmic interval.
 */
public void setLogaritmic(boolean newLogaritmic) {
	logaritmic = newLogaritmic;
	fireIntervalChange();
}
/**
 * This method sets end of interval (maximal value) and fires event <code>IntervalChange</code>
 * to all listeners in list.
 */
public void setMax(double max) {
	this.max=max;
	fireIntervalChange();
}
/**
 * This method sets start of interval (minimal value) and fires event <code>IntervalChange</code>
 * to all listeners in list.
 */
public void setMin(double min) {
	this.min=min;
	fireIntervalChange();
}
/**
 * This method adds to start and end of interval parameter specified value and fires event <code>IntervalChange</code>
 * to all listeners in list.
 */
public void shift(double shift) {
	set(min+shift,max+shift);
	fireIntervalChange();
}
/**
 * This method return string that describe this class object.
 */
public String toString() {
	return "Interval["+min+","+max+"]";
}
/**
 * If union of parameter specified interval and this interval is not just this interval,
 * this method change this interval to union of both intervals and fires event <code>IntervalChange</code>
 * to all listeners in list.
 */
public void unionWith(Interval interval) {
	if (interval==null) return;
	boolean ch=false;
	if (interval.min()<min) {
		min=interval.min();
		ch=true;
	}
	if (interval.max()>max) {
		max=interval.max();
		ch=true;
	}
	if (ch) fireIntervalChange();
}
}
