package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/24/00 11:27:34)
 * @author: 
 */
public class DefaultPointTransformer implements PointTransformerModel {
	protected double x0 =0;
	protected double x1 =0.0;
	protected double y0 =0;
	protected double y1 =0.0;
	protected java.awt.Rectangle chartRectangle= new java.awt.Rectangle(0,0,10,10);
	protected Interval xScale= new Interval(0.0,1.0);
	protected Interval yScale= new Interval(0.0,1.0);
	protected Point maxValue = new Point();
	protected Point minValue = new Point();
	protected IntPointCollector points = new IntPointCollector(300);
	protected IntPoint first;
	protected IntPoint last;
	protected int count = 0;
	protected Point lastPoint;
	protected IntervalListener listener= new IntervalListener() {
		public void intervalChange(Interval source) {
			updateTransformation();
		}
	};
/**
 * DefaultPointTransformer constructor comment.
 */
public DefaultPointTransformer() {
	super();
	updateTransformation();
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 13:58:11)
 * @param point si.ijs.kgb.chart.IntPoint
 */
protected IntPoint add(IntPoint point) {
	if (last!=null) last.next=point;
	last=point;
	if (first==null) first=point;
	count++;
	return point;
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 14:34:52)
 */
public void clear() {
	IntPointIterator it= pointIterator();
	while (it.hasNext()) it.next();
}
/**
 * getDrawSize method comment.
 */
public java.awt.Rectangle getChartRectangle() {
	return chartRectangle;
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 13:42:17)
 * @return int
 */
public int getCount() {
	return count;
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 14:58:48)
 * @return si.ijs.kgb.chart.IntPoint
 */
public IntPoint getFirstIntPoint() {
	return first;
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 14:58:48)
 * @return si.ijs.kgb.chart.IntPoint
 */
public IntPoint getLastIntPoint() {
	return last;
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 14:58:48)
 * @return si.ijs.kgb.chart.IntPoint
 */
public Point getLastPoint() {
	return lastPoint;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:46:12)
 * @return si.ijs.anka.databush.utilities.Point
 */
public Point getMaxValue() {
	return maxValue;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:47:00)
 * @return si.ijs.anka.databush.utilities.Point
 */
public Point getMinValue() {
	return minValue;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 12:21:42)
 * @return si.ijs.anka.databush.utilities.Interval
 */
public Interval getXScale() {
	return xScale;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 12:22:02)
 * @return si.ijs.anka.databush.utilities.Interval
 */
public Interval getYScale() {
	return yScale;
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 13:40:30)
 * @return si.ijs.kgb.chart.IntPointIterator
 */
public IntPointIterator pointIterator() {
	return new IntPointIterator() {
		IntPoint p=first;
		public boolean hasNext() {
			return p!=null;
		}
		public IntPoint next() {
			if (p==null) return null;
			p=p.next;
			return removeFirst();
		}
	};
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 13:58:11)
 * @param point si.ijs.kgb.chart.IntPoint
 */
protected IntPoint removeFirst() {
	if (first!=null) {
		IntPoint tmp=first;
		first=first.next;
		if (first==null) last=null;
		count--;
		points.recyclePoint(tmp);
		return tmp;
	} else return null;
}
/**
 * setDrawSize method comment.
 */
public void setChartRectangle(java.awt.Rectangle size) {
	chartRectangle= size;
	updateTransformation();
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 12:21:42)
 * @param newXScale si.ijs.anka.databush.utilities.Interval
 */
public void setXScale(Interval newXScale) {
	if (xScale!=null) xScale.removeIntervalListener(listener);
	xScale = newXScale;
	if (xScale!=null) xScale.addIntervalListener(listener);
	updateTransformation();
	
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 12:22:02)
 * @param newYScale si.ijs.anka.databush.utilities.Interval
 */
public void setYScale(Interval newYScale) {
	if (yScale!=null) yScale.removeIntervalListener(listener);
	yScale = newYScale;
	if (yScale!=null) yScale.addIntervalListener(listener);
	updateTransformation();
}
/**
 * transform method comment.
 */
public void transform(PointIterator points) {
	if (!points.hasNext()) return;
	updateTransformation();
	
	int x,y,xi=chartRectangle.x,yi=chartRectangle.y;
	
	maxValue.y=Double.MIN_VALUE;
	maxValue.x=0.0;
	minValue.y=Double.MAX_VALUE;
	minValue.x=0.0;

	Point p=null;
	while (points.hasNext()) {
		x= xi+(int)Math.rint(((p= points.next()).x-x0)*x1);
		y= yi+chartRectangle.height-1-(int)Math.rint((p.y-y0)*y1);
		if (Double.isInfinite(y)) {
			y= chartRectangle.height+1;
			System.out.println("Autocorrect: ("+x+",Infinite) -> ("+x+","+y+")");
		}
		if (Double.isNaN(y)) {
			y= 0;
			System.out.println("Autocorrect: ("+x+",NaN) -> ("+x+","+y+")");
		}
		add(this.points.newPoint(x,y));
		if (!(Double.isInfinite(p.y)||Double.isNaN(p.y))) {
			if (p.y>maxValue.y) {
				maxValue.y= p.y;
				maxValue.x= p.x;
			} else if (p.y<minValue.y) {
				minValue.y= p.y;
				minValue.x= p.x;
			}
		}
	}
	if (lastPoint==null) lastPoint= (Point)p.clone();
	else {
		lastPoint.x= p.x;
		lastPoint.y= p.y;
	}
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 11:32:57)
 */
protected void updateTransformation() {
	x0= xScale.min();
	x1= ((double)(chartRectangle.width-1))/(xScale.max()-xScale.min());
	y0= yScale.min();
	y1= ((double)(chartRectangle.height-1))/(yScale.max()-yScale.min());
}
}
