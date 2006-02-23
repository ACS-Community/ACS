/*
 * Stroke.java
 *
 * Created on January 28, 2004, 2:08 PM
 * 
 * 
 */

package antMountGUI;

/**
 * The superclass from wich all the strokes derive.
 * 
 * @author  acaproni
 * 
 * @version 1.0
 */
public class Stroke {
    
       
    /**
     * Creates a new instance of Stroke
     * 
     * @param time The time
     * @param orient The orientation
     */
    public Stroke(double time,double orient) {
        this.Time=time;
        this.Orientation=orient;
    }
    
    /** 
     * Create a zero-ed stroke
     *
     */
    public Stroke() {
        this.Time=0.0;
        this.Orientation=0.0;
    }
    
    /**
     * Return the time
     * 
     * @return
     */
    public double getTime() { return Time; }
    
    /**
     * Return the orientation
     * 
     * @return
     */
    public double getOrientation() { return Orientation; }
    
    /**
     * Set the time
     * 
     * @param time The new value
     */
    public void setTime(double time) { this.Time=time; }
    
    /**
     * Set the orientation
     * 
     * @param orientation The new value
     */
    public void setOrientation(double orientation) { this.Orientation=orientation; }
    
    /**
     * The time
     */
    private double Time;
    
    /**
     * The orientation
     */
    private double Orientation;
}
