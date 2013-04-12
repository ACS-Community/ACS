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
import java.util.List;
import java.util.Map;

/**
 * Encapsulates a <code>List&lt;NotifyServiceData&gt;</code>.
 * It gets created by {@link EventModel} to pass around its list of Notify Services to GUI elements.
 * <p>
 * TODO: Replace with EMF generated class.
 */
public class NotifyServices {
	
	private final Map<String, NotifyServiceData> services;
	
	NotifyServices(Map<String, NotifyServiceData> services) {
		this.services = services;
	}
	
	public List<NotifyServiceData> getServices() {
		return new ArrayList<NotifyServiceData>(services.values());
	}
	
//	public void addService(NotifyServiceData data) {
//		services.add(data);
//	}

	public NotifyServiceData findHostingService(String channelName) {
		for (NotifyServiceData service : services.values()) {
			ChannelData channelData = service.getChannelByName(channelName);
			if (channelData != null) {
				return service;
			}
		}
		return null;
	}
	
	public ChannelData findChannel(String channelName) {
		for (NotifyServiceData service : services.values()) {
			ChannelData channelData = service.getChannelByName(channelName);
			if (channelData != null) {
				return channelData;
			}
		}
		return null;
	}

	public List<ChannelData> getAllChannels() {
		List<ChannelData> ret = new ArrayList<ChannelData>();
		for (NotifyServiceData service : services.values()) {
			ret.addAll(service.getChannels());
		}
		return ret;
	}


}
