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

import org.omg.CosNotifyChannelAdmin.EventChannelFactory;

public class NotifyServiceData extends AbstractNotifyServiceElement implements Comparable<NotifyServiceData> {

	private HashMap<String, ChannelData> channels;
	private EventChannelFactory efact;
	private String factoryName;
	
	public NotifyServiceData(String name, String factoryName, EventChannelFactory ecf, NotificationServiceMonitorControl mc, int[] adminCounts, int[] adminDeltas) {
		super(name, null, mc, adminCounts, adminDeltas);
		channels = new HashMap<String, ChannelData>(10);
		efact = ecf;
		this.factoryName = factoryName;
	}
	
	public EventChannelFactory getEventChannelFactory() {
		return efact;
	}
	
	public ArrayList<ChannelData> getChannels() {
		return new ArrayList<ChannelData>(channels.values());
	}
	
	public ChannelData getChannel(String channelName) {
		return channels.get(channelName);
	}
	
	public void addChannelAndConfirm(String channelName, ChannelData cdata) {
			channels.put(channelName, cdata);
			return;
	}
	
	public void removeChannel(String channelName) {
		if (channels.containsKey(channelName))
			channels.remove(channelName);
	}

	public int compareTo(NotifyServiceData o) {
		return getName().compareTo(o.getName());
	}
	
	@Override
	public boolean equals(Object o) {
		if (o == null || !(o instanceof NotifyServiceData)) {
			return false;
		}
		return getName().equals(((NotifyServiceData)o).getName());
	}
	
	@Override
	public int hashCode() {
		return getName().hashCode();
	}

	public String getFactoryName() {
		return factoryName;
	}
}
