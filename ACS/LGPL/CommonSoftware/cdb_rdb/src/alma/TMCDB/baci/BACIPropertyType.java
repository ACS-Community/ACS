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
 * 
 * /////////////////////////////////////////////////////////////////
 * // WARNING!  DO NOT MODIFY THIS FILE!                          //
 * //  ---------------------------------------------------------  //
 * // | This is generated code!  Do not modify this file.       | //
 * // | Any changes will be lost when the file is re-generated. | //
 * //  ---------------------------------------------------------  //
 * /////////////////////////////////////////////////////////////////
 *
 * File BACIPropertyType.java
 */
package alma.TMCDB.baci;

import org.w3c.dom.Element;

import com.cosylab.cdb.jdal.hibernate.ExtraDataFeatureUtil;


/**

   * Key: PropertyTypeId is automatically generated from:  PropertyName, AssemblyName

 *
 */
@SuppressWarnings("serial")
public class BACIPropertyType implements java.io.Serializable, BACIPropertyTypeIF {
    static private final String newline = System.getProperty("line.separator");

    @SuppressWarnings("unused")
	private int PropertyId;
    @SuppressWarnings("unused")
    private Integer ComponentId;

    @SuppressWarnings("unused")
    // must be public to be accessible, but should not have getter to be come visible as node
    public String PropertyName;

    // extra data support
    private String Data;
    //private Element extraData_;
    //private boolean extraDataParsed = false;
    
    /* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.ExtraDataFeature#getExtraData()
	 */
	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getExtraData()
	 */
	public Element getExtraData() {
		if (Data == null || Data.isEmpty())
			return null;
		//else if (!extraDataParsed)
		{
			try {
				/* extraData_ = */ return ExtraDataFeatureUtil.getExtraDataMap(Data);
			} catch (Throwable th) {
				System.err.println("Failed to parse extra data for property: " + PropertyName);
				th.printStackTrace();
				return null;
			}
			//extraDataParsed = true;
		}
		
		//return extraData_;
	}

	private String description;
    private String format;
    private String units;
    private String resolution;

    private int archive_priority;
    private double archive_min_int;
    private double archive_max_int;
    private boolean archive_suppress;
    private String archive_mechanism;
    private double default_timer_trig;
    private double min_timer_trig;

    private boolean initialize_devio;


    // P<type>
    private Double min_delta_trig;
    private String default_value;
    private Double graph_min;
    private Double graph_max;
    private Double min_step;
    private double archive_delta;
    private Double archive_delta_percent;

    // RO<type>
    private Double alarm_high_on;
    private Double alarm_low_on;
    private Double alarm_high_off;
    private Double alarm_low_off;
    private Double alarm_timer_trig;

    // RW<type>
    private Double min_value;
    private Double max_value;
    
    // ROpattern
    private String bitDescription;
    private String whenSet;
    private String whenCleared;
    
    // PEnum
    private String statesDescription;
    private String condition;
    private String alarm_on;
    private String alarm_off;    

    // alarms
    private String alarm_fault_family;
    private String alarm_fault_member;
    private Integer alarm_level;
    
    /**
     * Default Constructor for BACIPropertyType.  Setter methods must be used to insert data.
     */
    public BACIPropertyType () {
    }

    /**
     * Display the values of this object.
     */
    public String toString() {
    	String s =  "BACIPropertyType:" + newline;

        s += "\tPropertyId: " + PropertyId + newline;

        s += "\tPropertyName: " + PropertyName + newline;

        s += "\tComponentId: " + ComponentId + newline;

        s += "\tdescription: " + description + newline;

        s += "\tformat: " + format + newline;

        s += "\tunits: " + units + newline;

        s += "\tresolution: " + resolution + newline;

        s += "\tarchive_priority: " + archive_priority + newline;

        s += "\tarchive_min_int: " + archive_min_int + newline;

        s += "\tarchive_max_int: " + archive_max_int + newline;

        s += "\tarchive_suppress: " + archive_suppress + newline;

        s += "\tarchive_mechanism: " + archive_mechanism + newline;

        s += "\tdefault_timer_trig: " + default_timer_trig + newline;

        s += "\tmin_timer_trig: " + min_timer_trig + newline;

        s += "\tinitialize_devio: " + initialize_devio + newline;

        s += "\tmin_delta_trig: " + min_delta_trig + newline;

        s += "\tdefault_value: " + default_value + newline;

        s += "\tgraph_min: " + graph_min + newline;

        s += "\tgraph_max: " + graph_max + newline;

        s += "\tmin_step: " + min_step + newline;

        s += "\tarchive_delta: " + archive_delta + newline;

        s += "\tarchive_delta_percent: " + archive_delta_percent + newline;

        s += "\talarm_high_on: " + alarm_high_on + newline;

        s += "\talarm_low_on: " + alarm_low_on + newline;

        s += "\talarm_high_off: " + alarm_high_off + newline;

        s += "\talarm_low_off: " + alarm_low_off + newline;

        s += "\talarm_timer_trig: " + alarm_timer_trig + newline;

        s += "\tmin_value: " + min_value + newline;

        s += "\tmax_value: " + max_value + newline;

        s += "\tbitDescription: " + bitDescription + newline;

        s += "\twhenSet: " + whenSet + newline;

        s += "\twhenCleared: " + whenCleared + newline;

        s += "\tstatesDescription: " + statesDescription + newline;

        s += "\tcondition: " + condition + newline;

        s += "\talarm_on: " + alarm_on + newline;

        s += "\talarm_off: " + alarm_off + newline;

        s += "\talarm_fault_family" + alarm_fault_family + newline;

        s += "\talarm_fault_member" + alarm_fault_member + newline;

        s += "\talarm_level" + alarm_level + newline;

        return s;
    }

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_high_off()
	 */
	public Double getAlarm_high_off() {
		return alarm_high_off;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_high_off(java.lang.Double)
	 */
	public void setAlarm_high_off(Double alarm_high_off) {
		this.alarm_high_off = alarm_high_off;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_high_on()
	 */
	public Double getAlarm_high_on() {
		return alarm_high_on;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_high_on(java.lang.Double)
	 */
	public void setAlarm_high_on(Double alarm_high_on) {
		this.alarm_high_on = alarm_high_on;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_low_off()
	 */
	public Double getAlarm_low_off() {
		return alarm_low_off;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_low_off(java.lang.Double)
	 */
	public void setAlarm_low_off(Double alarm_low_off) {
		this.alarm_low_off = alarm_low_off;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_low_on()
	 */
	public Double getAlarm_low_on() {
		return alarm_low_on;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_low_on(java.lang.Double)
	 */
	public void setAlarm_low_on(Double alarm_low_on) {
		this.alarm_low_on = alarm_low_on;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_timer_trig()
	 */
	public Double getAlarm_timer_trig() {
		return alarm_timer_trig;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_timer_trig(java.lang.Double)
	 */
	public void setAlarm_timer_trig(Double alarm_timer_trig) {
		this.alarm_timer_trig = alarm_timer_trig;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_delta()
	 */
	public double getArchive_delta() {
		return archive_delta;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_delta(double)
	 */
	public void setArchive_delta(double archive_delta) {
		this.archive_delta = archive_delta;
	}

	public Double getArchive_delta_percent() {
		return archive_delta_percent;
	}

	public void setArchive_delta_percent(Double archive_delta_percent) {
		this.archive_delta_percent = archive_delta_percent;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_max_int()
	 */
	public double getArchive_max_int() {
		return archive_max_int;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_max_int(double)
	 */
	public void setArchive_max_int(double archive_max_int) {
		this.archive_max_int = archive_max_int;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_suppress()
	 */
	public boolean getArchive_suppress() {
		return archive_suppress;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_suppress(boolean)
	 */
	public void setArchive_suppress(boolean archive_suppress) {
		this.archive_suppress = archive_suppress;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_mechanism()
	 */
	public String getArchive_mechanism() {
		return archive_mechanism;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_mechanism(String)
	 */
	public void setArchive_mechanism(String archive_mechanism) {
		this.archive_mechanism = archive_mechanism;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_min_int()
	 */
	public double getArchive_min_int() {
		return archive_min_int;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_min_int(double)
	 */
	public void setArchive_min_int(double archive_min_int) {
		this.archive_min_int = archive_min_int;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_priority()
	 */
	public int getArchive_priority() {
		return archive_priority;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_priority(int)
	 */
	public void setArchive_priority(int archive_priority) {
		this.archive_priority = archive_priority;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getDefault_timer_trig()
	 */
	public double getDefault_timer_trig() {
		return default_timer_trig;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setDefault_timer_trig(double)
	 */
	public void setDefault_timer_trig(double default_timer_trig) {
		this.default_timer_trig = default_timer_trig;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getDefault_value()
	 */
	public String getDefault_value() {
		return default_value;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setDefault_value(java.lang.String)
	 */
	public void setDefault_value(String default_value) {
		this.default_value = default_value;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getDescription()
	 */
	public String getDescription() {
		return description;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setDescription(java.lang.String)
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getFormat()
	 */
	public String getFormat() {
		return format;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setFormat(java.lang.String)
	 */
	public void setFormat(String format) {
		this.format = format;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getGraph_max()
	 */
	public Double getGraph_max() {
		return graph_max;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setGraph_max(java.lang.Double)
	 */
	public void setGraph_max(Double graph_max) {
		this.graph_max = graph_max;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getGraph_min()
	 */
	public Double getGraph_min() {
		return graph_min;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setGraph_min(java.lang.Double)
	 */
	public void setGraph_min(Double graph_min) {
		this.graph_min = graph_min;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#isInitialize_devio()
	 */
	public boolean isInitialize_devio() {
		return initialize_devio;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setInitialize_devio(boolean)
	 */
	public void setInitialize_devio(boolean initialize_devio) {
		this.initialize_devio = initialize_devio;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMax_value()
	 */
	public Double getMax_value() {
		return max_value;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMax_value(java.lang.Double)
	 */
	public void setMax_value(Double max_value) {
		this.max_value = max_value;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMin_delta_trig()
	 */
	public Double getMin_delta_trig() {
		return min_delta_trig;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMin_delta_trig(java.lang.Double)
	 */
	public void setMin_delta_trig(Double min_delta_trig) {
		this.min_delta_trig = min_delta_trig;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMin_step()
	 */
	public Double getMin_step() {
		return min_step;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMin_step(java.lang.Double)
	 */
	public void setMin_step(Double min_step) {
		this.min_step = min_step;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMin_timer_trig()
	 */
	public double getMin_timer_trig() {
		return min_timer_trig;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMin_timer_trig(double)
	 */
	public void setMin_timer_trig(double min_timer_trig) {
		this.min_timer_trig = min_timer_trig;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMin_value()
	 */
	public Double getMin_value() {
		return min_value;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMin_value(java.lang.Double)
	 */
	public void setMin_value(Double min_value) {
		this.min_value = min_value;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getResolution()
	 */
	public String getResolution() {
		return resolution;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setResolution(java.lang.String)
	 */
	public void setResolution(String resolution) {
		this.resolution = resolution;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getUnits()
	 */
	public String getUnits() {
		return units;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setUnits(java.lang.String)
	 */
	public void setUnits(String units) {
		this.units = units;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getBitDescription()
	 */
	public String getBitDescription() {
		return bitDescription;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setBitDescription(java.lang.String)
	 */
	public void setBitDescription(String bitDescription) {
		this.bitDescription = bitDescription;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getWhenCleared()
	 */
	public String getWhenCleared() {
		return whenCleared;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setWhenCleared(java.lang.String)
	 */
	public void setWhenCleared(String whenCleared) {
		this.whenCleared = whenCleared;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getWhenSet()
	 */
	public String getWhenSet() {
		return whenSet;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setWhenSet(java.lang.String)
	 */
	public void setWhenSet(String whenSet) {
		this.whenSet = whenSet;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_off()
	 */
	public String getAlarm_off() {
		return alarm_off;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_off(java.lang.String)
	 */
	public void setAlarm_off(String alarm_off) {
		this.alarm_off = alarm_off;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_on()
	 */
	public String getAlarm_on() {
		return alarm_on;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_on(java.lang.String)
	 */
	public void setAlarm_on(String alarm_on) {
		this.alarm_on = alarm_on;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getCondition()
	 */
	public String getCondition() {
		return condition;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setCondition(java.lang.String)
	 */
	public void setCondition(String condition) {
		this.condition = condition;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getStatesDescription()
	 */
	public String getStatesDescription() {
		return statesDescription;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setStatesDescription(java.lang.String)
	 */
	public void setStatesDescription(String statesDescription) {
		this.statesDescription = statesDescription;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_fault_family()
	 */
	public String getAlarm_fault_family() {
		return alarm_fault_family;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_fault_family(java.lang.String)
	 */
	public void setAlarm_fault_family(String alarmFaultFamily) {
		alarm_fault_family = alarmFaultFamily;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_fault_member()
	 */
	public String getAlarm_fault_member() {
		return alarm_fault_member;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_fault_member(java.lang.String)
	 */
	public void setAlarm_fault_member(String alarmFaultMember) {
		alarm_fault_member = alarmFaultMember;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_level()
	 */
	public Integer getAlarm_level() {
		return alarm_level;
	}

	/* (non-Javadoc)
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_level(java.lang.Integer)
	 */
	public void setAlarm_level(Integer alarmLevel) {
		alarm_level = alarmLevel;
	}

}
