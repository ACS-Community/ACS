package com.cosylab.gui.components.r2.chart;

/**
 * This type represent coordinates of one point (it practically represent point).
 */
public class Point implements Cloneable {
	public double x = 0.0;
	public double y = 0.0;
	public Point next;
/**
 * Point default constructor with no parameter.
 */
public Point() {
	super();
}
/**
 * Point constructor that takes coordinates as a parameters.
 */
public Point(double x, double y) {
	super();
	this.x=x;
	this.y=y;
}
/**
 * This method return clone of this element.
 */
public Object clone() {
	try {
		return super.clone();
	} catch (CloneNotSupportedException e) {
		return null;
	}
}
/**
 * This method returns string that describe this point.
 */
public String toString() {
	return "Point["+x+","+y+"]";
}
}
