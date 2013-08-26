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
public class Pdouble extends TypelessProperty {
    static private final String newline = System.getProperty("line.separator");

    private double min_delta_trig = 1.0; // TMCDB default is 1.0, BACI is 0.0
    private double default_value = 0.0;
    private double graph_min = -1.7976931348623157E+308;
    private double graph_max = 1.7976931348623157E+308;
    private double min_step = 1.0; // TMCDB default is 1.0, BACI is 0.0
    private double archive_delta = 0.0;
	
    /**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public Pdouble () {
    	super();
    	setFormat("%9.4f");
    }

    public String toString() {
    	String s = "Pdouble:" + newline;

        s += super.toString() + newline;

        s += "\tmin_delta_trig: " + min_delta_trig + newline;

        s += "\tdefault_value: " + default_value + newline;

        s += "\tgraph_min: " + graph_min + newline;

        s += "\tgraph_max: " + graph_max + newline;

        s += "\tmin_step: " + min_step + newline;

        s += "\tarchive_delta: " + archive_delta + newline;

        return s;
    }

	/**
	 * @return the archive_delta
	 */
	public double getArchive_delta() {
		return archive_delta;
	}

	/**
	 * @param archive_delta the archive_delta to set
	 */
	public void setArchive_delta(double archive_delta) {
		this.archive_delta = archive_delta;
	}

	/**
	 * @return the default_value
	 */
	public double getDefault_value() {
		return default_value;
	}

	/**
	 * @param default_value the default_value to set
	 */
	public void setDefault_value(double default_value) {
		this.default_value = default_value;
	}

	/**
	 * @return the graph_max
	 */
	public double getGraph_max() {
		return graph_max;
	}

	/**
	 * @param graph_max the graph_max to set
	 */
	public void setGraph_max(double graph_max) {
		this.graph_max = graph_max;
	}

	/**
	 * @return the graph_min
	 */
	public double getGraph_min() {
		return graph_min;
	}

	/**
	 * @param graph_min the graph_min to set
	 */
	public void setGraph_min(double graph_min) {
		this.graph_min = graph_min;
	}

	/**
	 * @return the min_delta_trig
	 */
	public double getMin_delta_trig() {
		return min_delta_trig;
	}

	/**
	 * @param min_delta_trig the min_delta_trig to set
	 */
	public void setMin_delta_trig(double min_delta_trig) {
		this.min_delta_trig = min_delta_trig;
	}

	/**
	 * @return the min_step
	 */
	public double getMin_step() {
		return min_step;
	}

	/**
	 * @param min_step the min_step to set
	 */
	public void setMin_step(double min_step) {
		this.min_step = min_step;
	}


}