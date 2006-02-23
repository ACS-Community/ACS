package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (11/24/00 18:11:01)
 * @author: 
 */
public class FunctionDataModel extends AbstractDataModel {
	private Function function;
	private int index = 0;
	private Interval xScale;
	private Interval yScale;
/**
 * FunctionModel constructor comment.
 */
public FunctionDataModel() {
	super();
	chartStyle= new PlainChartStyle(PlainChartStyle.LINE_PLAIN,java.awt.Color.black,PlainChartStyle.SYMBOL_NONE,0,java.awt.Color.black);
}
/**
 * FunctionModel constructor comment.
 */
public FunctionDataModel(Function f) {
	this();
	function= f;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:11:44)
 * @return si.ijs.anka.databush.utilities.Function
 */
public Function getFunction() {
	return function;
}
/**
 * getPoinCount method comment.
 */
public int getPointCount() {
	return (function==null) ? 0 : (pointCount= chartService.getChartDisplayablePointCount());
}
public PointIterator getPointIterator() {
	index=0;
	xScale= chartService.getViewManager().getExtractedXScale(preferedXScale);
	yScale= chartService.getViewManager().getExtractedYScale(preferedYScale);
	return this;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 20:59:53)
 * @return boolean
 */
public boolean hasNext() {
	return index<pointCount;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:28:37)
 * @return si.ijs.anka.databush.utilities.Point
 */
public Point next() {
	if (function==null) return point;
	point.x= xScale.min()+((double)index)*(xScale.max()-xScale.min())/((double)(chartService.getChartDisplayablePointCount()-1));
	point.y= function.y(point.x);
	index++;
	return point;
}
/**
 * Insert the method's description here.
 * Creation date: (11/24/00 18:11:44)
 * @param newFunction si.ijs.anka.databush.utilities.Function
 */
public void setFunction(Function newFunction) {
	function = newFunction;
}
}
