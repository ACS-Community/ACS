package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/21/00 19:08:15)
 * @author: 
 */
public class IntPoint {
	public int x = 0;
	public int y = 0;
	public IntPoint next;
/**
 * Point constructor comment.
 */
public IntPoint() {
	super();
}
/**
 * Point constructor comment.
 */
public IntPoint(int x, int y) {
	super();
	this.x=x;
	this.y=y;
}
/**
 * Point constructor comment.
 */
public IntPoint(IntPoint p) {
	this(p.x,p.y);
}
/**
 * Insert the method's description here.
 * Creation date: (28.1.2002 21:16:17)
 * @return boolean
 * @param o java.lang.Object
 */
public boolean equals(Object o) {
	if (o==null) return false;
	if (! (o instanceof IntPoint)) return false;
	return ((IntPoint)o).x==x && ((IntPoint)o).y==y;
}
/**
 * Insert the method's description here.
 * Creation date: (3/2/01 23:22:27)
 * @return java.lang.String
 */
public String toString() {
	return "Point["+x+","+y+"]";
}
}
