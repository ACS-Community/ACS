/*
 * Created on Mar 1, 2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package antMountGUI;

/**
 * The arc stroke
 * 
 * @author acaproni
 *
 * @version 1.0
 */
public class ArcStroke extends LinearStroke {
	
	/**
	 * Build an arc stroke
	 * 
	 * @param time time
	 * @param orientation orientation
	 * @param x0 xArray[0]
	 * @param y0 yArray[0]
	 * @param x1 xArray[1]
	 * @param y1 yArray[1]
	 * @param longCtr Longitude of the center
	 * @param latCtr Latitude of the center
	 * @param polarOrient Polar orientation
	 */
	public ArcStroke(
			double time, 
			double orientation,
			double x0,
			double y0,
			double x1,
			double y1,
			double longCtr,
			double latCtr,
			double polarOrient) {
		super(time,orientation,x0,y0,x1,y1);
		this.longCenter=longCtr;
		this.latCenter=latCtr;
		this.polarOrientation=polarOrient;
	}
	
	/**
	 * Build zero-ed arc stroke
	 *
	 */
	public ArcStroke() {
		super();
		longCenter=latCenter=polarOrientation=0.0;
	}
	
	/**
	 * Return the long Center
	 * 
	 * @return
	 */
	public double getLongCenter() { return longCenter; }
	
	/**
	 * Return the lat Center
	 * 
	 * @return
	 */
	public double getLatCenter() { return latCenter; }
	
	/**
	 * Return the polar orientation
	 * 
	 * @return
	 */
	public double getPolarOrientation() { return polarOrientation; }
	
	/** 
	 * Set the long Center
	 * 
	 * @param longCtr
	 */
	public void setLongCenter(double longCtr ) { longCenter=longCtr; }
	
	/**
	 * Set the lat Center
	 * 
	 * @param latCtr
	 */
	public void setLatCenter(double latCtr ) { latCenter=latCtr; }
	
	/** 
	 * Set the polar orientation
	 * 
	 * @param polarOrient
	 */
	public void setPolarOrientation(double polarOrient ) { polarOrientation=polarOrient; }
	
	/**
	 * The longitude of the center
	 */
	private double longCenter;
	
	/**
	 * The latitude of the center
	 */
	private double latCenter;
	
	/**
	 * The polar orientation
	 */
	private double polarOrientation;
}
