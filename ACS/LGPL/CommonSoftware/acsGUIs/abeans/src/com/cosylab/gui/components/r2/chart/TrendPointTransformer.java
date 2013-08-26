package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/24/00 11:27:34)
 * @author: 
 */
public class TrendPointTransformer extends DefaultPointTransformer {
	private double leftover = 0.0;
/**
 * DefaultPointTransformer constructor comment.
 */
public TrendPointTransformer() {
	super();
	updateTransformation();
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/01 14:35:59)
 */
public void clearLast() {
	lastPoint=null;
}
/**
 * transform method comment.
 */
public void transform(PointIterator points) {
	if (!points.hasNext()) return;

	int x,y,xi=chartRectangle.x,yi=chartRectangle.y;

	Point p=null;
	if (lastPoint!=null) {
		p=lastPoint;
		x= xi+(int)Math.rint((p.x-x0+leftover)*x1);
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
			maxValue.y= p.y;
			maxValue.x= p.x;
			minValue.y= p.y;
			minValue.x= p.x;
		} else {
			maxValue.y=Double.MIN_VALUE;
			maxValue.x=0.0;
			minValue.y=Double.MAX_VALUE;
			minValue.x=0.0;
		}
	} else {
		maxValue.y=Double.MIN_VALUE;
		maxValue.x=0.0;
		minValue.y=Double.MAX_VALUE;
		minValue.x=0.0;
	}
	
	while (points.hasNext()) {
		x= xi+(int)Math.rint(((p= points.next()).x-x0+leftover)*x1);
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
	if (lastPoint==null) {
		lastPoint= (Point)p.clone();
		leftover=0.0;
	}

/*	if (last.x-first.x>0) {
		leftover= p.x- ((double)last.x/x1+x0) +leftover;
	} else {
		leftover+=p.x-lastPoint.x;
	}*/

	lastPoint.x= p.x;
	lastPoint.y= p.y;

	if (xScale.max() < lastPoint.x) {
		xScale.set(lastPoint.x-xScale.length(),lastPoint.x);
		if (ChartProperties.getChartProperties().isDebugData()) 
			System.out.println("Scale Advance "+lastPoint+" -> "+last);
	}
}

}
