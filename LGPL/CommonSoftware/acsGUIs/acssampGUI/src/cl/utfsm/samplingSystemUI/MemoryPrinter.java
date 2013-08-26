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
/**
 * @author Jorge Avarias <javarias[at]inf.utfsm.cl>
 * 
 */

package cl.utfsm.samplingSystemUI;

import java.util.ArrayList;

import cl.utfsm.samplingSystemUI.core.DataItem;

public class MemoryPrinter extends DataPrinter {

	protected ArrayList<DataItem> samples;
	protected static final int INITIAL_THREAD_SUPPORT_SIZE = 20;
	private long samplesCounter;
	
	public MemoryPrinter(SamplingSystemGUI ssg){
		super(ssg);
		samples=new ArrayList<DataItem>();
		samplesCounter=0;
		widget=new BeanMemoryWidget();
	}
	
	public void updateValue(DataItem item) {
		samples.add(item);
		samplesCounter++;
		widget.updateValues(item.getTime(), item.getValue(), 0);
	}

	/**
	 * In memory printer postProcesing do nothing with sampled data stored, 
	 * they will be destroyed in object destruction.
	 */
	public void postProcessing() {
		widget.resetSampleCount();
	}

	public void setComponent(String component) {
		super.setComponent(component);
		widget.setValues(component, property, 0);
	}

	public void setProperty(String property) {
		super.setProperty(property);
		widget.setValues(component, property, 0);
	}

	public ArrayList<DataItem> getSamples() {
		return samples;
	}
	
	public void setComponentAvailable(boolean available,String reason) {
		super.setComponentAvailable(available,reason);
		widget.setComponentAvailable(available,reason, 0);
	}
}
