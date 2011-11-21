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
package alma.acs.eventbrowser.model;

import gov.sandia.CosNotification.NotificationServiceMonitorControl;

public abstract class AbstractNotifyServiceElement {
	protected AbstractNotifyServiceElement parent;
	protected NotificationServiceMonitorControl mc;
	protected String name;
	
	private int numberConsumers;
	private int numberSuppliers;
	private int deltaConsumers;
	private int deltaSuppliers;

	public AbstractNotifyServiceElement(String name, AbstractNotifyServiceElement parent,
			NotificationServiceMonitorControl mc,
			int[] adminCounts, int[] adminDeltas) {
		this.name = name;
		this.parent = parent;
		this.numberConsumers = adminCounts[0];
		this.numberSuppliers = adminCounts[1];
		this.deltaConsumers = adminDeltas[0];
		this.deltaSuppliers = adminDeltas[1];
		this.mc = mc;
	}
	
	public AbstractNotifyServiceElement getParent() {
		return parent;
	}
	
	public void setParent(AbstractNotifyServiceElement parent) {
		this.parent = parent;
	}
	
	public String getName() {
		return name;
	}
	
	public NotificationServiceMonitorControl getMc() {
		return mc;
	}

	public int getNumberConsumers() {
		return numberConsumers;
	}

	public void setNumberConsumers(int numberConsumers) {
		this.numberConsumers = numberConsumers;
	}

	public void setDeltaConsumers(int deltaConsumers) {
		this.deltaConsumers = deltaConsumers;
	}

	public int getDeltaConsumers() {
		return deltaConsumers;
	}

	public int getNumberSuppliers() {
		return numberSuppliers;
	}

	public void setNumberSuppliers(int numberSuppliers) {
		this.numberSuppliers = numberSuppliers;
	}

	public void setDeltaSuppliers(int deltaSuppliers) {
		this.deltaSuppliers = deltaSuppliers;
	}

	public int getDeltaSuppliers() {
		return deltaSuppliers;
	}

	public String getNumConsumersAndDelta() {
		return ""+numberConsumers+(deltaConsumers !=0 ? " ("+(deltaConsumers > 0 ? "+" : "")+deltaConsumers+")":"");
	}

	public String getNumSuppliersAndDelta() {
		return ""+numberSuppliers+(deltaSuppliers !=0 ? " ("+(deltaSuppliers > 0 ? "+" : "")+deltaSuppliers+")":"");
	}

}
