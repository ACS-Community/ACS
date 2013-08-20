package com.cosylab.gui.components.r2.chart.demo;

import com.cosylab.gui.components.r2.chart.*;
/**
 * Insert the type's description here.
 * Creation date: (8.8.2002 17:02:45)
 * @author: 
 */
public class RandomTrendDataModel extends AbstractDataModel {
	private int pointsPerUpdate = 1;
	private double changePerPoint = 1.0;
	private int index = 0;
	private double startValue = 1.0;
	private double startPosition = 0.0;
	private double positionAdvance = 0.1;
	private double valueRange = 5.0;
/**
 * RandomTrendDataModel constructor comment.
 */
public RandomTrendDataModel() {
	super();
}
/**
 * RandomTrendDataModel constructor comment.
 */
public RandomTrendDataModel(double changePerPoint, int pointsPerUpdate, double positionAdvance, double startPosition, double startValue, double valueRange) {
	super();
	this.changePerPoint=changePerPoint;
	this.pointsPerUpdate=pointsPerUpdate;
	this.positionAdvance=positionAdvance;
	this.startPosition=startPosition;
	this.startValue=startValue;
	this.valueRange=valueRange;
	point.x=startPosition;
	point.y=startValue;
	pointCount=pointsPerUpdate;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:06:51)
 * @return double
 */
public double getChangePerPoint() {
	return changePerPoint;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:02:45)
 * @return si.ijs.anka.databush.utilities.PointIterator
 */
public PointIterator getPointIterator() {
	index=0;
	return this;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:06:51)
 * @return int
 */
public int getPointsPerUpdate() {
	return pointsPerUpdate;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:11:24)
 * @return double
 */
public double getPositionAdvance() {
	return positionAdvance;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:10:06)
 * @return double
 */
public double getStartPosition() {
	return startPosition;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:09:11)
 * @return double
 */
public double getStartValue() {
	return startValue;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:40:10)
 * @return double
 */
public double getValueRange() {
	return valueRange;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:02:45)
 * @return boolean
 */
public boolean hasNext() {
	return index<pointsPerUpdate;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:02:45)
 * @return si.ijs.anka.databush.utilities.Point
 */
public Point next() {
	point.x+=positionAdvance;

	double step= changePerPoint*(Math.random()-0.5)*2.0;
	double next= point.y+step;
	if (next>startValue+valueRange) point.y-=Math.abs(step);
	else if (next<startValue-valueRange)  point.y+=Math.abs(step);
	else point.y=next;
	index++;
	return point;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:06:51)
 * @param newChangePerPoint double
 */
public void setChangePerPoint(double newChangePerPoint) {
	changePerPoint = newChangePerPoint;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:06:51)
 * @param newPointsPerUpdate int
 */
public void setPointsPerUpdate(int newPointsPerUpdate) {
	pointsPerUpdate = newPointsPerUpdate;
	this.pointCount=pointsPerUpdate;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:11:24)
 * @param newPositionAdvance double
 */
public void setPositionAdvance(double newPositionAdvance) {
	positionAdvance = newPositionAdvance;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:10:06)
 * @param newStartPosition double
 */
public void setStartPosition(double newStartPosition) {
	startPosition = newStartPosition;
	point.x=startPosition;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:09:11)
 * @param newStartValue double
 */
public void setStartValue(double newStartValue) {
	startValue = newStartValue;
	point.y=startValue;
}
/**
 * Insert the method's description here.
 * Creation date: (8.8.2002 17:40:10)
 * @param newValueRange double
 */
public void setValueRange(double newValueRange) {
	valueRange = newValueRange;
}
}
