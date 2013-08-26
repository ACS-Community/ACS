package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (31.03.2001 23:35:40)
 * @author: 
 */
public class PointCollector {
	private java.lang.ref.ReferenceQueue queue = new java.lang.ref.ReferenceQueue();
	private Point first= null;
	private Point last=null;
	private int recycled = 0;
	private int created = 0;
	private int size = 0;
	private int capacity = 10;
/**
 * PointCollector constructor with no arguments.
 */
public PointCollector() {
	super();
}
/**
 * PointCollector constructor with no arguments.
 */
public PointCollector(int capacity) {
	super();
	this.capacity=capacity;
}
public void finalize() {
	System.out.println(this+" r:"+recycled+" c:"+created);
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 23:38:04)
 * @return si.ijs.kgb.chart.Point
 */
public Point newPoint() {
	java.lang.ref.Reference r;
	while ((r=queue.poll())!=null) {
		if (r.get()!=null) {
			recycled++;
			return (Point)r.get();
		}
	}
	if (first!=null) {
		Point tmp= first;
		first=first.next;
		tmp.next=null;
		--size;
		recycled++;
		return tmp;
	}
	created++;
	Point tmp=new Point();
	new java.lang.ref.SoftReference(tmp,queue);
	return tmp;
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 23:38:04)
 * @return si.ijs.kgb.chart.Point
 */
public Point newPoint(double x, double y) {
	Point tmp=newPoint();
	tmp.x=x;
	tmp.y=y;
	return tmp;
}
/**
 * Insert the method's description here.
 * Creation date: (31.03.2001 23:46:49)
 * @param point si.ijs.kgb.chart.Point
 */
public void recyclePoint(Point point) {
	if (point==null) return;
	point.next=null;
	if (size>=capacity) return;

	if (first==null) {
		first=last=point;
	} else {
		last.next=point;
		last=point;
	}
	size++;
}
/**
 * Sets capacity of catch. If catch contains more point than new capacity, they will not be released until recycled.
 */
public void setCapacity(int capacity) {
	this.capacity= capacity;
}
}
