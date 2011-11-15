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

import java.util.ArrayList;
import java.util.HashMap;

public class ChannelData extends AbstractNotifyServiceElement implements Comparable<ChannelData> {
	
	private ArrayList<MCStatistics> statistics= new ArrayList<MCStatistics>(10);
	private boolean subscribed = false;
	private static HashMap<String,ChannelData> map = new HashMap<String,ChannelData>();
	
	public ChannelData(String name, AbstractNotifyServiceElement parent, int[] adminCounts, int[] adminDeltas) {
		super(name, parent, ((NotifyServiceData)parent).getMc(), adminCounts, adminDeltas);
		statistics = new ArrayList<MCStatistics>(2); // Consumers and suppliers for now; TODO: Add TAO M&C
		statistics.add(new SupplierCounts(this));
		statistics.add(new ConsumerCounts(this));
		MCStatistics cqs = new ChannelQueueSize(this);
		statistics.add(cqs);
		map.put(name, this);
	}
	
	public static ChannelData returnInstanceForChannel(String channelName) {
		return map.get(channelName);
	}
	
	public void addStatistics(MCStatistics stat) {
		statistics.add(stat);
	}
	
	public ArrayList<MCStatistics> getStatistics() {
		return statistics;
	}
	
	// TODO: The following three methods are not used yet. They should make the model more Object-oriented
	
	public void subscribe() {
		subscribed = true;
	}
	
	public void unsubscribe() {
		subscribed = false;
	}
	
	public boolean isSubscribed() {
		return subscribed;
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

