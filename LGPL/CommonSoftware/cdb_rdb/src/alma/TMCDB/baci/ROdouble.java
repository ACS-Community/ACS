/*
 * ALMA - Atacama Large Millimeter Array
 * (c) European Southern Observatory, 2002
 * (c) Associated Universities Inc., 2002
 * Copyright by ESO (in the framework of the ALMA collaboration),
 * Copyright by AUI (in the framework of the ALMA collaboration),
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY, without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307  USA
 * File Component.java
 */
package alma.TMCDB.baci;


@SuppressWarnings("serial")
public class ROdouble extends Pdouble {
    static private final String newline = System.getProperty("line.separator");

    private double alarm_high_on = 0.0;
    private double alarm_low_on = 0.0;
    private double alarm_high_off = 0.0;
    private double alarm_low_off = 0.0;
    private double alarm_timer_trig = 1.0; // TMCDB default is 1.0, BACI is 0.0

    //@SuppressWarnings("unused")
	//private boolean ReadOnly;
    // TODO to be moved to RWclass
    private double min_value = -1.7976931348623157E+308;
    private double max_value = 1.7976931348623157E+308;
	
    public double getMin_value()
	{
		return min_value;
	}   
	
	public double getMax_value()
	{
		return max_value;
	}

	/**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public ROdouble () {
    	super();
    }

    public String toString() {
    	String s = "ROdouble:" + newline;

        s += super.toString() + newline;

        s += "\talarm_high_on: " + alarm_high_on + newline;

        s += "\talarm_low_on: " + alarm_low_on + newline;

        s += "\talarm_high_off: " + alarm_high_off + newline;

        s += "\talarm_low_off: " + alarm_low_off + newline;

        s += "\talarm_timer_trig: " + alarm_timer_trig + newline;

        return s;
    }

	/**
	 * @return the alarm_high_off
	 */
	public double getAlarm_high_off() {
		return alarm_high_off;
	}

	/**
	 * @param alarm_high_off the alarm_high_off to set
	 */
	public void setAlarm_high_off(double alarm_high_off) {
		this.alarm_high_off = alarm_high_off;
	}

	/**
	 * @return the alarm_high_on
	 */
	public double getAlarm_high_on() {
		return alarm_high_on;
	}

	/**
	 * @param alarm_high_on the alarm_high_on to set
	 */
	public void setAlarm_high_on(double alarm_high_on) {
		this.alarm_high_on = alarm_high_on;
	}

	/**
	 * @return the alarm_low_off
	 */
	public double getAlarm_low_off() {
		return alarm_low_off;
	}

	/**
	 * @param alarm_low_off the alarm_low_off to set
	 */
	public void setAlarm_low_off(double alarm_low_off) {
		this.alarm_low_off = alarm_low_off;
	}

	/**
	 * @return the alarm_low_on
	 */
	public double getAlarm_low_on() {
		return alarm_low_on;
	}

	/**
	 * @param alarm_low_on the alarm_low_on to set
	 */
	public void setAlarm_low_on(double alarm_low_on) {
		this.alarm_low_on = alarm_low_on;
	}

	/**
	 * @return the alarm_timer_trig
	 */
	public double getAlarm_timer_trig() {
		return alarm_timer_trig;
	}

	/**
	 * @param alarm_timer_trig the alarm_timer_trig to set
	 */
	public void setAlarm_timer_trig(double alarm_timer_trig) {
		this.alarm_timer_trig = alarm_timer_trig;
	}

    
}
