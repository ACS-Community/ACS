/*
 * Created on Mar 1, 2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package antMountGUI;

/**
 * A linear stroke
 * 
 * @author acaproni
 *
 * @version 1.0
 */
public class LinearStroke extends Stroke {
	
	/**
	 * Build a linear stroke
	 * 
	 * @param time The time
	 * @param orientation The orientation
	 * @param x0 The xArray[0] coordinate
	 * @param y0 The yArray[0] coordinate
	 * @param x1 The xArray[1] coordinate
	 * @param y1 The yArray[1] coordinate
	 */
	public LinearStroke(double time, double orientation, double x0, double y0, double x1, double y1) {
		super(time, orientation);
		xArray0=x0;
		yArray0=y0;
		xArray1=x1;
		yArray1=y1;
	}
	
	/**
	 * Build a zero-ed linear stroke
	 *
	 */
	public LinearStroke() {
		super();
		xArray0=yArray0=xArray1=yArray1=0;
	}
	
	/**
	 * 
	 * @return The xArray[0] coordinate
	 */
	public double getX0() { return xArray0; }
	
	/**
	 * 
	 * @return The yArray[0] coordinate
	 */
	public double getY0() { return yArray0; }
	
	/**
	 * 
	 * @return The xArray[1] coordinate
	 */
	public double getX1() { return xArray1; }
	
	/**
	 * 
	 * @return The yArray[1] coordinate
	 */
	public double getY1() { return yArray1; }
	
	/**
	 * Set the xArray[0] coordinate
	 * 
	 * @param x0 New value
	 */
	public void setX0(double x0) { xArray0=x0; }
	
	/**
	 * Set the yArray[0] coordinate
	 * @param y0 New value
	 */
	public void setY0(double y0) { yArray0=y0; }
	
	/**
	 * Set the xArray[1] coordinate
	 * @param x1 New value
	 */
	public void setX1(double x1) { xArray1=x1; }
	
	/**
	 * Set the yArray[1] coordinate
	 * @param y1 New value
	 */
	public void setY1(double y1) { yArray1=y1; }
	
	/**
	 * The xArray[0] coordinate
	 */
	private double xArray0;
	
	/**
	 * The yArray[0] coordinate
	 */
	private double yArray0;
	
	/**
	 * The xArray[1] coordinate
	 */
	private double xArray1;
	
	/**
	 * The yArray[1] coordinate
	 */	
	private double yArray1;
}
