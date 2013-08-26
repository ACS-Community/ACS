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


/**
 * 
 * Used as base class of ChannelData and NotifyServiceData, and as "tree parent" of ChannelConsumers, ChannelData, ChannelQueueSize, ChannelSuppliers, MCStatistics, SlowestConsumers
 * and in views/EventGuiAdapterFactory
 */
public abstract class AbstractNotifyServiceElement {
	
	protected final String name;
	
	public AbstractNotifyServiceElement(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name;
	}
	
	/**
	 * This method will be called by org.eclipse.jface.viewers.ViewerComparator.getLabel(Viewer, Object)
	 * to sort services and channels in UI parts.
	 */
	@Override
	public String toString() {
		return getName();
	}

	public abstract int getNumberConsumers();
	
	public abstract int getNumberSuppliers();
	
	public abstract int getDeltaConsumers();
	
	public abstract int getDeltaSuppliers();
	

	public String getNumConsumersAndDelta() {
		return "" + getNumberConsumers() + (getDeltaConsumers() !=0 ? " ("+(getDeltaConsumers() > 0 ? "+" : "")+getDeltaConsumers()+")" : "");
	}

	public String getNumSuppliersAndDelta() {
		return "" + getNumberSuppliers() + (getDeltaSuppliers() !=0 ? " ("+(getDeltaSuppliers() > 0 ? "+" : "")+getDeltaSuppliers()+")" : "");
	}

}
