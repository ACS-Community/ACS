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

public interface IGraphicalUpdater {
	
	//public void updateValues(long time, double value);
	public void updateValues(long time, double value, int position);
	
	//public void setValues(String component, String property);
	public void setValues(String component, String property, int position);
	
	//public void setComponentAvailable(boolean tmp, String reason);
	public void setComponentAvailable(boolean tmp, String reason, int position);
	
	public void resetSampleCount();
	
	public void setTimeWindow(double frequency, int time);
}