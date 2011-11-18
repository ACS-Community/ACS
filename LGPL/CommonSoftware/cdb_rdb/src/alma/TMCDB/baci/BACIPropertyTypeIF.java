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
package alma.TMCDB.baci;

import com.cosylab.cdb.jdal.hibernate.ExtraDataFeature;

public interface BACIPropertyTypeIF extends ExtraDataFeature {

	/**
	 * @return the alarm_high_off
	 */
	public Double getAlarm_high_off();

	/**
	 * @param alarm_high_off the alarm_high_off to set
	 */
	public void setAlarm_high_off(Double alarm_high_off);

	/**
	 * @return the alarm_high_on
	 */
	public Double getAlarm_high_on();

	/**
	 * @param alarm_high_on the alarm_high_on to set
	 */
	public void setAlarm_high_on(Double alarm_high_on);

	/**
	 * @return the alarm_low_off
	 */
	public Double getAlarm_low_off();

	/**
	 * @param alarm_low_off the alarm_low_off to set
	 */
	public void setAlarm_low_off(Double alarm_low_off);

	/**
	 * @return the alarm_low_on
	 */
	public Double getAlarm_low_on();

	/**
	 * @param alarm_low_on the alarm_low_on to set
	 */
	public void setAlarm_low_on(Double alarm_low_on);

	/**
	 * @return the alarm_timer_trig
	 */
	public Double getAlarm_timer_trig();

	/**
	 * @param alarm_timer_trig the alarm_timer_trig to set
	 */
	public void setAlarm_timer_trig(Double alarm_timer_trig);

	/**
	 * @return the archive_delta
	 */
	public double getArchive_delta();

	/**
	 * @param archive_delta the archive_delta to set
	 */
	public void setArchive_delta(double archive_delta);

	/**
	 * @return the archive_delta_percent
	 */
	public Double getArchive_delta_percent();

	/**
	 * @param archive_delta the archive_delta to set
	 */
	public void setArchive_delta_percent(Double archive_delta_percent);

	/**
	 * @return the archive_max_int
	 */
	public double getArchive_max_int();

	/**
	 * @param archive_max_int the archive_max_int to set
	 */
	public void setArchive_max_int(double archive_max_int);

	/**
	 * @return the archive_suppress
	 */
	public boolean getArchive_suppress();

	/**
	 * @param archive_suppress the archive_suppress to set
	 */
	public void setArchive_suppress(boolean archive_suppress);

	/**
	 * @return the archive_mechanism
	 */
	public String getArchive_mechanism();

	/**
	 * @param archive_mechanism the archive_mechanism to set
	 */
	public void setArchive_mechanism(String archive_mechanism);

	/**
	 * @return the archive_min_int
	 */
	public double getArchive_min_int();

	/**
	 * @param archive_min_int the archive_min_int to set
	 */
	public void setArchive_min_int(double archive_min_int);

	/**
	 * @return the archive_priority
	 */
	public int getArchive_priority();

	/**
	 * @param archive_priority the archive_priority to set
	 */
	public void setArchive_priority(int archive_priority);

	/**
	 * @return the default_timer_trig
	 */
	public double getDefault_timer_trig();

	/**
	 * @param default_timer_trig the default_timer_trig to set
	 */
	public void setDefault_timer_trig(double default_timer_trig);

	/**
	 * @return the default_value
	 */
	public String getDefault_value();

	/**
	 * @param default_value the default_value to set
	 */
	public void setDefault_value(String default_value);

	/**
	 * @return the description
	 */
	public String getDescription();

	/**
	 * @param description the description to set
	 */
	public void setDescription(String description);

	/**
	 * @return the format
	 */
	public String getFormat();

	/**
	 * @param format the format to set
	 */
	public void setFormat(String format);

	/**
	 * @return the graph_max
	 */
	public Double getGraph_max();

	/**
	 * @param graph_max the graph_max to set
	 */
	public void setGraph_max(Double graph_max);

	/**
	 * @return the graph_min
	 */
	public Double getGraph_min();

	/**
	 * @param graph_min the graph_min to set
	 */
	public void setGraph_min(Double graph_min);

	/**
	 * @return the initialize_devio
	 */
	public boolean isInitialize_devio();

	/**
	 * @param initialize_devio the initialize_devio to set
	 */
	public void setInitialize_devio(boolean initialize_devio);

	/**
	 * @return the max_value
	 */
	public Double getMax_value();

	/**
	 * @param max_value the max_value to set
	 */
	public void setMax_value(Double max_value);

	/**
	 * @return the min_delta_trig
	 */
	public Double getMin_delta_trig();

	/**
	 * @param min_delta_trig the min_delta_trig to set
	 */
	public void setMin_delta_trig(Double min_delta_trig);

	/**
	 * @return the min_step
	 */
	public Double getMin_step();

	/**
	 * @param min_step the min_step to set
	 */
	public void setMin_step(Double min_step);

	/**
	 * @return the min_timer_trig
	 */
	public double getMin_timer_trig();

	/**
	 * @param min_timer_trig the min_timer_trig to set
	 */
	public void setMin_timer_trig(double min_timer_trig);

	/**
	 * @return the min_value
	 */
	public Double getMin_value();

	/**
	 * @param min_value the min_value to set
	 */
	public void setMin_value(Double min_value);

	/**
	 * @return the resolution
	 */
	public String getResolution();

	/**
	 * @param resolution the resolution to set
	 */
	public void setResolution(String resolution);

	/**
	 * @return the units
	 */
	public String getUnits();

	/**
	 * @param units the units to set
	 */
	public void setUnits(String units);

	/**
	 * @return the bitDescription
	 */
	public String getBitDescription();

	/**
	 * @param bitDescription the bitDescription to set
	 */
	public void setBitDescription(String bitDescription);

	/**
	 * @return the whenCleared
	 */
	public String getWhenCleared();

	/**
	 * @param whenCleared the whenCleared to set
	 */
	public void setWhenCleared(String whenCleared);

	/**
	 * @return the whenSet
	 */
	public String getWhenSet();

	/**
	 * @param whenSet the whenSet to set
	 */
	public void setWhenSet(String whenSet);

	/**
	 * @return the alarm_off
	 */
	public String getAlarm_off();

	/**
	 * @param alarm_off the alarm_off to set
	 */
	public void setAlarm_off(String alarm_off);

	/**
	 * @return the alarm_on
	 */
	public String getAlarm_on();

	/**
	 * @param alarm_on the alarm_on to set
	 */
	public void setAlarm_on(String alarm_on);

	/**
	 * @return the condition
	 */
	public String getCondition();

	/**
	 * @param condition the condition to set
	 */
	public void setCondition(String condition);

	/**
	 * @return the statesDescription
	 */
	public String getStatesDescription();

	/**
	 * @param statesDescription the statesDescription to set
	 */
	public void setStatesDescription(String statesDescription);

	/**
	 * @return the alarm_fault_family
	 */
	public String getAlarm_fault_family();

	/**
	 * @param alarmFaultFamily the alarm_fault_family to set
	 */
	public void setAlarm_fault_family(String alarmFaultFamily);

	/**
	 * @return the alarm_fault_member
	 */
	public String getAlarm_fault_member();

	/**
	 * @param alarmFaultMember the alarm_fault_member to set
	 */
	public void setAlarm_fault_member(String alarmFaultMember);

	/**
	 * @return the alarm_level
	 */
	public Integer getAlarm_level();

	/**
	 * @param alarmLevel the alarm_level to set
	 */
	public void setAlarm_level(Integer alarmLevel);

}
