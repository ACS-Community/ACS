/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/**
 * 
 */
package alma.TMCDB.baci;

import org.w3c.dom.Element;

/**
 * @author msekoranja
 *
 */
public class EmptyStringHandlerBACIPropertyType implements BACIPropertyTypeIF {

	protected BACIPropertyTypeIF delegate;
	
	public final static String EMPTY_STRING = "";
	public final static String EMPTY_STRING_SUBSTITUTE = "!";
	
	public EmptyStringHandlerBACIPropertyType(BACIPropertyTypeIF delegate) {
		this.delegate = delegate;
	}

	public static final String handleInput(String value)
	{
		if (EMPTY_STRING.equals(value))
			return EMPTY_STRING_SUBSTITUTE;
		else
			return value;
	}

	public static final String handleOutput(String value)
	{
		if (EMPTY_STRING_SUBSTITUTE.equals(value))
			return EMPTY_STRING;
		else
			return value;
	}

	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_fault_family()
	 */
	public String getAlarm_fault_family() {
		return handleOutput(delegate.getAlarm_fault_family());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_fault_member()
	 */
	public String getAlarm_fault_member() {
		return handleOutput(delegate.getAlarm_fault_member());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_high_off()
	 */
	public Double getAlarm_high_off() {
		return delegate.getAlarm_high_off();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_high_on()
	 */
	public Double getAlarm_high_on() {
		return delegate.getAlarm_high_on();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_level()
	 */
	public Integer getAlarm_level() {
		return delegate.getAlarm_level();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_low_off()
	 */
	public Double getAlarm_low_off() {
		return delegate.getAlarm_low_off();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_low_on()
	 */
	public Double getAlarm_low_on() {
		return delegate.getAlarm_low_on();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_off()
	 */
	public String getAlarm_off() {
		return handleOutput(delegate.getAlarm_off());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_on()
	 */
	public String getAlarm_on() {
		return handleOutput(delegate.getAlarm_on());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getAlarm_timer_trig()
	 */
	public Double getAlarm_timer_trig() {
		return delegate.getAlarm_timer_trig();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_delta()
	 */
	public double getArchive_delta() {
		return delegate.getArchive_delta();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_delta_percent()
	 */
	public Double getArchive_delta_percent() {
		return delegate.getArchive_delta_percent();
	}

	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_max_int()
	 */
	public double getArchive_max_int() {
		return delegate.getArchive_max_int();
	}

	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_suppress()
	 */
	public boolean getArchive_suppress() {
		return delegate.getArchive_suppress();
	}

	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_mechanism()
	 */
	public String getArchive_mechanism() {
		return handleOutput(delegate.getArchive_mechanism());
	}

	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_min_int()
	 */
	public double getArchive_min_int() {
		return delegate.getArchive_min_int();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getArchive_priority()
	 */
	public int getArchive_priority() {
		return delegate.getArchive_priority();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getBitDescription()
	 */
	public String getBitDescription() {
		return handleOutput(delegate.getBitDescription());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getCondition()
	 */
	public String getCondition() {
		return handleOutput(delegate.getCondition());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getDefault_timer_trig()
	 */
	public double getDefault_timer_trig() {
		return delegate.getDefault_timer_trig();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getDefault_value()
	 */
	public String getDefault_value() {
		return handleOutput(delegate.getDefault_value());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getDescription()
	 */
	public String getDescription() {
		return handleOutput(delegate.getDescription());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getExtraData()
	 */
	public Element getExtraData() {
		return delegate.getExtraData();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getFormat()
	 */
	public String getFormat() {
		return handleOutput(delegate.getFormat());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getGraph_max()
	 */
	public Double getGraph_max() {
		return delegate.getGraph_max();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getGraph_min()
	 */
	public Double getGraph_min() {
		return delegate.getGraph_min();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMax_value()
	 */
	public Double getMax_value() {
		return delegate.getMax_value();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMin_delta_trig()
	 */
	public Double getMin_delta_trig() {
		return delegate.getMin_delta_trig();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMin_step()
	 */
	public Double getMin_step() {
		return delegate.getMin_step();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMin_timer_trig()
	 */
	public double getMin_timer_trig() {
		return delegate.getMin_timer_trig();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getMin_value()
	 */
	public Double getMin_value() {
		return delegate.getMin_value();
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getResolution()
	 */
	public String getResolution() {
		return handleOutput(delegate.getResolution());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getStatesDescription()
	 */
	public String getStatesDescription() {
		return handleOutput(delegate.getStatesDescription());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getUnits()
	 */
	public String getUnits() {
		return handleOutput(delegate.getUnits());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getWhenCleared()
	 */
	public String getWhenCleared() {
		return handleOutput(delegate.getWhenCleared());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#getWhenSet()
	 */
	public String getWhenSet() {
		return handleOutput(delegate.getWhenSet());
	}


	/**
	 * @return
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#isInitialize_devio()
	 */
	public boolean isInitialize_devio() {
		return delegate.isInitialize_devio();
	}


	/**
	 * @param alarmFaultFamily
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_fault_family(java.lang.String)
	 */
	public void setAlarm_fault_family(String alarmFaultFamily) {
		delegate.setAlarm_fault_family(handleInput(alarmFaultFamily));
	}


	/**
	 * @param alarmFaultMember
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_fault_member(java.lang.String)
	 */
	public void setAlarm_fault_member(String alarmFaultMember) {
		delegate.setAlarm_fault_member(handleInput(alarmFaultMember));
	}


	/**
	 * @param alarmHighOff
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_high_off(java.lang.Double)
	 */
	public void setAlarm_high_off(Double alarmHighOff) {
		delegate.setAlarm_high_off(alarmHighOff);
	}


	/**
	 * @param alarmHighOn
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_high_on(java.lang.Double)
	 */
	public void setAlarm_high_on(Double alarmHighOn) {
		delegate.setAlarm_high_on(alarmHighOn);
	}


	/**
	 * @param alarmLevel
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_level(java.lang.Integer)
	 */
	public void setAlarm_level(Integer alarmLevel) {
		delegate.setAlarm_level(alarmLevel);
	}


	/**
	 * @param alarmLowOff
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_low_off(java.lang.Double)
	 */
	public void setAlarm_low_off(Double alarmLowOff) {
		delegate.setAlarm_low_off(alarmLowOff);
	}


	/**
	 * @param alarmLowOn
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_low_on(java.lang.Double)
	 */
	public void setAlarm_low_on(Double alarmLowOn) {
		delegate.setAlarm_low_on(alarmLowOn);
	}


	/**
	 * @param alarmOff
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_off(java.lang.String)
	 */
	public void setAlarm_off(String alarmOff) {
		delegate.setAlarm_off(handleInput(alarmOff));
	}


	/**
	 * @param alarmOn
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_on(java.lang.String)
	 */
	public void setAlarm_on(String alarmOn) {
		delegate.setAlarm_on(handleInput(alarmOn));
	}


	/**
	 * @param alarmTimerTrig
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setAlarm_timer_trig(java.lang.Double)
	 */
	public void setAlarm_timer_trig(Double alarmTimerTrig) {
		delegate.setAlarm_timer_trig(alarmTimerTrig);
	}


	/**
	 * @param archiveDelta
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_delta(double)
	 */
	public void setArchive_delta(double archiveDelta) {
		delegate.setArchive_delta(archiveDelta);
	}


	/**
	 * @param archiveDeltaPercent
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_delta_percent(Double)
	 */
	public void setArchive_delta_percent(Double archiveDeltaPercent) {
		delegate.setArchive_delta_percent(archiveDeltaPercent);
	}

	/**
	 * @param archiveMaxInt
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_max_int(double)
	 */
	public void setArchive_max_int(double archiveMaxInt) {
		delegate.setArchive_max_int(archiveMaxInt);
	}


	/**
	 * @param archiveMinInt
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_min_int(double)
	 */
	public void setArchive_min_int(double archiveMinInt) {
		delegate.setArchive_min_int(archiveMinInt);
	}

	/**
	 * @param archiveSuppress
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_suppress(boolean)
	 */
	public void setArchive_suppress(boolean archiveSuppress) {
		delegate.setArchive_suppress(archiveSuppress);
	}

	/**
	 * @param archiveMechanism
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_mechanism(String)
	 */
	public void setArchive_mechanism(String archiveMechanism) {
		delegate.setArchive_mechanism(archiveMechanism);
	}

	/**
	 * @param archivePriority
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setArchive_priority(int)
	 */
	public void setArchive_priority(int archivePriority) {
		delegate.setArchive_priority(archivePriority);
	}


	/**
	 * @param bitDescription
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setBitDescription(java.lang.String)
	 */
	public void setBitDescription(String bitDescription) {
		delegate.setBitDescription(handleInput(bitDescription));
	}


	/**
	 * @param condition
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setCondition(java.lang.String)
	 */
	public void setCondition(String condition) {
		delegate.setCondition(handleInput(condition));
	}


	/**
	 * @param defaultTimerTrig
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setDefault_timer_trig(double)
	 */
	public void setDefault_timer_trig(double defaultTimerTrig) {
		delegate.setDefault_timer_trig(defaultTimerTrig);
	}


	/**
	 * @param defaultValue
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setDefault_value(java.lang.String)
	 */
	public void setDefault_value(String defaultValue) {
		delegate.setDefault_value(handleInput(defaultValue));
	}


	/**
	 * @param description
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setDescription(java.lang.String)
	 */
	public void setDescription(String description) {
		delegate.setDescription(handleInput(description));
	}


	/**
	 * @param format
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setFormat(java.lang.String)
	 */
	public void setFormat(String format) {
		delegate.setFormat(handleInput(format));
	}


	/**
	 * @param graphMax
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setGraph_max(java.lang.Double)
	 */
	public void setGraph_max(Double graphMax) {
		delegate.setGraph_max(graphMax);
	}


	/**
	 * @param graphMin
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setGraph_min(java.lang.Double)
	 */
	public void setGraph_min(Double graphMin) {
		delegate.setGraph_min(graphMin);
	}


	/**
	 * @param initializeDevio
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setInitialize_devio(boolean)
	 */
	public void setInitialize_devio(boolean initializeDevio) {
		delegate.setInitialize_devio(initializeDevio);
	}


	/**
	 * @param maxValue
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMax_value(java.lang.Double)
	 */
	public void setMax_value(Double maxValue) {
		delegate.setMax_value(maxValue);
	}


	/**
	 * @param minDeltaTrig
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMin_delta_trig(java.lang.Double)
	 */
	public void setMin_delta_trig(Double minDeltaTrig) {
		delegate.setMin_delta_trig(minDeltaTrig);
	}


	/**
	 * @param minStep
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMin_step(java.lang.Double)
	 */
	public void setMin_step(Double minStep) {
		delegate.setMin_step(minStep);
	}


	/**
	 * @param minTimerTrig
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMin_timer_trig(double)
	 */
	public void setMin_timer_trig(double minTimerTrig) {
		delegate.setMin_timer_trig(minTimerTrig);
	}


	/**
	 * @param minValue
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setMin_value(java.lang.Double)
	 */
	public void setMin_value(Double minValue) {
		delegate.setMin_value(minValue);
	}


	/**
	 * @param resolution
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setResolution(java.lang.String)
	 */
	public void setResolution(String resolution) {
		delegate.setResolution(handleInput(resolution));
	}


	/**
	 * @param statesDescription
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setStatesDescription(java.lang.String)
	 */
	public void setStatesDescription(String statesDescription) {
		delegate.setStatesDescription(handleInput(statesDescription));
	}


	/**
	 * @param units
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setUnits(java.lang.String)
	 */
	public void setUnits(String units) {
		delegate.setUnits(handleInput(units));
	}


	/**
	 * @param whenCleared
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setWhenCleared(java.lang.String)
	 */
	public void setWhenCleared(String whenCleared) {
		delegate.setWhenCleared(handleInput(whenCleared));
	}


	/**
	 * @param whenSet
	 * @see alma.TMCDB.baci.BACIPropertyTypeIF#setWhenSet(java.lang.String)
	 */
	public void setWhenSet(String whenSet) {
		delegate.setWhenSet(handleInput(whenSet));
	}

	
	// introspector introspects fields, so we must have it here...
	
	private String description;
    private String format;
    private String units;
    private String resolution;

    private int archive_priority;
    private double archive_min_int;
    private double archive_max_int;
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

    private boolean archive_suppress;
    private String archive_mechanism;

}
