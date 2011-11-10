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

import java.util.ArrayList;

public class ChannelData extends AbstractNotifyServiceElement implements Comparable<ChannelData> {
	
	private ArrayList<MCStatistics> statistics;
	
	public ChannelData(String name, AbstractNotifyServiceElement parent, int[] adminCounts, int[] adminDeltas) {
		super(name, parent, adminCounts, adminDeltas);
		statistics = new ArrayList<MCStatistics>(2); // Consumers and suppliers for now; TODO: Add TAO M&C
	}
	
	public void addStatistics(MCStatistics stat) {
		statistics.add(stat);
	}
	
	public ArrayList<MCStatistics> getStatistics() {
		return statistics;
	}

	@Override
	public int compareTo(ChannelData o) {
		return getName().compareTo(o.getName());
	}
	
	@Override
	public boolean equals(Object o) {
		if (o == null || !(o instanceof ChannelData)) {
			return false;
		}
		return getName().equals(((AbstractNotifyServiceElement)o).getName());
	}
	
	@Override
	public int hashCode() {
		return getName().hashCode();
	}
}

