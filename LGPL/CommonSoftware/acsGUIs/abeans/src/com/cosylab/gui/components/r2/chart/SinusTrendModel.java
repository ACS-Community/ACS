package com.cosylab.gui.components.r2.chart;

/**
 * Insert the type's description here.
 * Creation date: (2/1/01 9:11:47 PM)
 * @author: 
 */
public class SinusTrendModel extends AbstractDataModel {
	protected int updateRate = 1000;
	protected int valuesPerUpdate = 1;
	protected double speed = 1.0;
	public SinusFunction f= new SinusFunction();
	public Point[] points;
	protected int index = 0;
	private long x = 0;
	protected boolean ready = true;
/**
 * SinusTrendModel constructor comment.
 */
public SinusTrendModel() {
	super();
//	chartStyle= BaseChart.STYLE_DOTS;
	setSpeed(0.1);
	setValuesPerUpdate(valuesPerUpdate);
	f.setAmplitude(2.0);
	f.setOffset(0.5);
	f.setPhase(0.0);
}
/**
 * Insert the method's description here.
 * Creation date: (2/1/01 9:11:47 PM)
 * @return si.ijs.anka.databush.utilities.PointIterator
 */
public PointIterator getPointIterator() {
	index=0;
	updateValues();
	return this;
}
/**
 * Insert the method's description here.
 * Creation date: (2/2/01 11:17:32 AM)
 * @return double
 */
public double getSpeed() {
	return speed;
}
/**
 * Insert the method's description here.
 * Creation date: (2/1/01 9:13:01 PM)
 * @return int
 */
public int getUpdateRate() {
	return updateRate;
}
/**
 * Insert the method's description here.
 * Creation date: (2/1/01 9:13:35 PM)
 * @return int
 */
public int getValuesPerUpdate() {
	return valuesPerUpdate;
}
/**
 * Insert the method's description here.
 * Creation date: (2/1/01 9:11:47 PM)
 * @return boolean
 */
public boolean hasNext() {
	return index<valuesPerUpdate;
}
/**
 * Insert the method's description here.
 * Creation date: (2/1/01 9:11:47 PM)
 * @return si.ijs.anka.databush.utilities.Point
 */
public Point next() {
	return points[index++];
}
/**
 * Insert the method's description here.
 * Creation date: (2/2/01 11:28:53 AM)
 */
protected synchronized void runIt() {
	System.out.println("Run It");
	try {
		wait(1000);
	} catch (InterruptedException e) {};

	while (true) {
		updateChartData();
		try {
			wait(updateRate);
		} catch (InterruptedException e) {};
	}
}
/**
 * Insert the method's description here.
 * Creation date: (2/2/01 11:17:32 AM)
 * @param newSpeed double
 */
public void setSpeed(double newSpeed) {
	speed = newSpeed;
	f.setFrequency(speed/1000.0);
}
/**
 * Insert the method's description here.
 * Creation date: (2/1/01 9:13:01 PM)
 * @param newUpdateRate int
 */
public void setUpdateRate(int newUpdateRate) {
	updateRate = newUpdateRate;
}
/**
 * Insert the method's description here.
 * Creation date: (2/1/01 9:13:35 PM)
 * @param newValuesPerUpdate int
 */
public void setValuesPerUpdate(int newValuesPerUpdate) {
	valuesPerUpdate = newValuesPerUpdate;
	points= new Point[valuesPerUpdate];
	for (int i=0; i<valuesPerUpdate; points[i++]=new Point());
	if (ready) pointCount= valuesPerUpdate;
}
/**
 * SinusTrendModel constructor comment.
 */
public void startAutomaticUpdate() {
	new Thread() {
		public void run() {
			runIt();
		}
	}.start();
}
/**
 * Insert the method's description here.
 * Creation date: (2/2/01 11:28:53 AM)
 */
protected void updateValues() {
	if (x>1000000000) x=0;
	for (int i=0; i<valuesPerUpdate; i++) {
		points[i].x= x+(i+1)*updateRate/valuesPerUpdate;
		points[i].y= f.y(points[i].x);
		points[i].x/=1000.0;
	}
	x+= updateRate;
//	System.out.println("Update: "+x);
//		for (int i=0; i<valuesPerUpdate; i++) System.out.println(points[i]);
	pointCount= valuesPerUpdate;
}
}
