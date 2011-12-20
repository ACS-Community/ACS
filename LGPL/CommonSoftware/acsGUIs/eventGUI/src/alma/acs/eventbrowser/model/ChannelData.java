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
import java.util.HashMap;

/**
 * @author jschwarz
 *
 */
public class ChannelData extends AbstractNotifyServiceElement implements Comparable<ChannelData> {
	
	private boolean subscribed = false;
	private static HashMap<String,ChannelData> map = new HashMap<String,ChannelData>();
	
	private final ChannelConsumers ccon;
	private final ChannelSuppliers csup;
	private final ChannelQueueSize cqs;
	private final SlowestConsumers slcon;
	
	public ChannelData(String name, AbstractNotifyServiceElement parent, int[] adminCounts, int[] adminDeltas) {
		super(name, parent, ((NotifyServiceData)parent).getMc(), adminCounts, adminDeltas);
//		statistics.add(new SupplierCounts(this));
//		statistics.add(new ConsumerCounts(this));
		
		ccon = new ChannelConsumers(this);
		csup = new ChannelSuppliers(this);
		cqs = new ChannelQueueSize(this);
		slcon = new SlowestConsumers(this);

		map.put(name, this); // Add this instance to the static map for easy access
	}
	
	/** This static method returns the ChannelData instance for a given channel name
	 * @param channelName
	 * @return
	 */
	public static ChannelData returnInstanceForChannel(String channelName) {
		return map.get(channelName);
	}
	
	
	public ArrayList<MCStatistics> getStatistics() {
		ArrayList<MCStatistics> statistics= new ArrayList<MCStatistics>(4);
		statistics.add(ccon);
		statistics.add(csup);
		statistics.add(cqs);
		if (cqs.getQueueSize() != 0) // Only display "slowest consumers" for a non-zero queue
			statistics.add(slcon);
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

