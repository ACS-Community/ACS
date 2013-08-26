package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/24/00 20:51:43)
 * @author: 
 */
public class ArrayDataModel extends AbstractDataModel {
	public double[] x;
	public double[] y;
	private int index=0;
/**
 * ArrayDataModel constructor comment.
 */
public ArrayDataModel(int points) {
	super();
	pointCount= points;
	x= new double[points];
	y= new double[points];
}
public PointIterator getPointIterator() {
	index=0;
	return this;
}
public boolean hasNext() {
	return index<pointCount;
}
/**
 * next method comment.
 */
public Point next() {
	point.x= x[index];
	point.y= y[index];
	index++;
	return point;
}
public void updateChart() {
	preferedXScale= new Interval(x[0]-0.1*(x[pointCount-1]-x[0]),x[pointCount-1]+0.1*(x[pointCount-1]-x[0]));
	double min=Double.MAX_VALUE;
	double max=Double.MIN_VALUE;
	for (int i=0; i<pointCount; i++) {
		if ((!Double.isInfinite(y[i]))&&(!Double.isNaN(y[i]))) {
			if(y[i]<min) min=y[i];
			if(y[i]>max) max=y[i];
		}
	}
	preferedYScale= new Interval(min-0.1*(max-min),max+0.1*(max-min));

	updateChartData();	
}
}
