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
public class TypelessProperty {
    static private final String newline = System.getProperty("line.separator");

    @SuppressWarnings("unused")
	private int PropertyId;
    @SuppressWarnings("unused")
    private String AssemblyName;

    @SuppressWarnings("unused")
    // must be public to be accessible, but should not have getter to be come visible as node
    public String PropertyName;
    
    private String description = "-";
    private String format = "-";
    private String units = "-";
    private int resolution = 65535;

    private int archive_priority = 15; // TMCDB default is 15, BACI is 3
    private double archive_min_int = 0.0;
    private double archive_max_int = 0.0;
    private double default_timer_trig = 1.0;
    private double min_timer_trig = 0.048; // TMCDB default is 0.048, BACI is 0.001

    private boolean initialize_devio = false;

    /**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public TypelessProperty () {
    }

    public String toString() {
    	String s =  "TypelessProperty:" + newline;

        s += "\tdescription: " + description + newline;

        s += "\tformat: " + format + newline;

        s += "\tunits: " + units + newline;

        s += "\tresolution: " + resolution + newline;

        s += "\tarchive_priority: " + archive_priority + newline;

        s += "\tarchive_min_int: " + archive_min_int + newline;

        s += "\tarchive_max_int: " + archive_max_int + newline;

        s += "\tdefault_timer_trig: " + default_timer_trig + newline;

        s += "\tmin_timer_trig: " + min_timer_trig + newline;

        s += "\tinitialize_devio: " + initialize_devio + newline;

        return s;
    }

	/**
	 * @return the archive_max_int
	 */
	public double getArchive_max_int() {
		return archive_max_int;
	}

	/**
	 * @param archive_max_int the archive_max_int to set
	 */
	public void setArchive_max_int(double archive_max_int) {
		this.archive_max_int = archive_max_int;
	}

	/**
	 * @return the archive_min_int
	 */
	public double getArchive_min_int() {
		return archive_min_int;
	}

	/**
	 * @param archive_min_int the archive_min_int to set
	 */
	public void setArchive_min_int(double archive_min_int) {
		this.archive_min_int = archive_min_int;
	}

	/**
	 * @return the archive_priority
	 */
	public int getArchive_priority() {
		return archive_priority;
	}

	/**
	 * @param archive_priority the archive_priority to set
	 */
	public void setArchive_priority(int archive_priority) {
		this.archive_priority = archive_priority;
	}

	/**
	 * @return the default_timer_trig
	 */
	public double getDefault_timer_trig() {
		return default_timer_trig;
	}

	/**
	 * @param default_timer_trig the default_timer_trig to set
	 */
	public void setDefault_timer_trig(double default_timer_trig) {
		this.default_timer_trig = default_timer_trig;
	}

	/**
	 * @return the description
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * @param description the description to set
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * @return the format
	 */
	public String getFormat() {
		return format;
	}

	/**
	 * @param format the format to set
	 */
	public void setFormat(String format) {
		this.format = format;
	}

	/**
	 * @return the initialize_devio
	 */
	public boolean isInitialize_devio() {
		return initialize_devio;
	}

	/**
	 * @param initialize_devio the initialize_devio to set
	 */
	public void setInitialize_devio(boolean initialize_devio) {
		this.initialize_devio = initialize_devio;
	}

	/**
	 * @return the min_timer_trig
	 */
	public double getMin_timer_trig() {
		return min_timer_trig;
	}

	/**
	 * @param min_timer_trig the min_timer_trig to set
	 */
	public void setMin_timer_trig(double min_timer_trig) {
		this.min_timer_trig = min_timer_trig;
	}

	/**
	 * @return the resolution
	 */
	public int getResolution() {
		return resolution;
	}

	/**
	 * @param resolution the resolution to set
	 */
	public void setResolution(int resolution) {
		this.resolution = resolution;
	}

	/**
	 * @return the units
	 */
	public String getUnits() {
		return units;
	}

	/**
	 * @param units the units to set
	 */
	public void setUnits(String units) {
		this.units = units;
	}

    
}