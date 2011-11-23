/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
package cl.utfsm.samplingSystemUI;

import java.io.Serializable;

public class SerializableProperty implements Serializable {
	
	private static final long serialVersionUID = -850908451943987979L;
	
	private String component;
	private String property;
	private String samplingGroup;
	private String frequency;
	private String samplingTime;
	private String timeWindow;
	
	public double getFrequency() {
		return Double.parseDouble(frequency);
	}
	public void setFrequency(double d) {
		this.frequency = String.valueOf(d);
	}
	public int getSamplingTime() {
		return Integer.parseInt(samplingTime);
	}
	public void setSamplingTime(int samplingTime) {
		this.samplingTime = String.valueOf(samplingTime);
	}
	public int getTimeWindow() {
		return Integer.parseInt(timeWindow);
	}
	public void setTimeWindow(int timeWindow) {
		this.timeWindow = String.valueOf(timeWindow);
	}
	public String getComponent() {
		return component;
	}
	public void setComponent(String component) {
		this.component = component;
	}
	public String getProperty() {
		return property;
	}
	public void setProperty(String property) {
		this.property = property;
	}
	public String getSamplingGroup() {
		return samplingGroup;
	}
	public void setSamplingGroup(String samplingGroup) {
		this.samplingGroup = samplingGroup;
	}

}
