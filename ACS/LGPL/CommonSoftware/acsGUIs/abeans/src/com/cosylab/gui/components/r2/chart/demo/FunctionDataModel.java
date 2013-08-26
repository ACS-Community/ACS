package com.cosylab.gui.components.r2.chart.demo;

/**
 * Insert the type's description here.
 * Creation date: (8.8.2002 1:48:27)
 * @author: 
 */
public class FunctionDataModel extends com.cosylab.gui.components.r2.chart.FunctionDataModel {
	public double step=0.1;
	double phase=0.0;
	double scale=1.0;
	private double offset = 0.0;
/**
 * FunctionDataModel constructor comment.
 */
public FunctionDataModel() {
	super();
}
/**
 * FunctionDataModel constructor comment.
 */
public FunctionDataModel(double step, double offset) {
	super();
	this.step=step;
	this.offset= offset;
}
/**
 * FunctionDataModel constructor comment.
 * @param f com.cosylab.gui.chart.Function
 */
public FunctionDataModel(com.cosylab.gui.components.r2.chart.Function f) {
	super(f);
}
/**
 * FunctionDataModel constructor comment.
 * @param f com.cosylab.gui.chart.Function
 */
public FunctionDataModel(com.cosylab.gui.components.r2.chart.Function f, double step, double offset) {
	super(f);
	this.step=step;
	this.offset= offset;
}
public com.cosylab.gui.components.r2.chart.PointIterator getPointIterator() {
	scale= Math.cos(phase);
	phase+=step;
	return super.getPointIterator();
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 1:59:27)
 * @return com.cosylab.gui.chart.Point
 */
public com.cosylab.gui.components.r2.chart.Point next() {
	com.cosylab.gui.components.r2.chart.Point p= super.next();
	p.y=offset+p.y*scale;
	return p;
}
}
