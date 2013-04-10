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

import org.omg.CosNotifyChannelAdmin.EventChannelFactory;

import gov.sandia.CosNotification.NotificationServiceMonitorControl;


/**
 * Encapsulates a Corba notify service for use in the model and in GUI elements.
 * <p>
 * All equality and comparison methods are based on the simplified name.
 */
public class NotifyServiceData extends AbstractNotifyServiceElement implements Comparable<NotifyServiceData> {

	private final HashMap<String, ChannelData> channels;
	private final EventChannelFactory efact;
	private final String factoryName;
	
	/**
	 * @param name The simplified display name
	 * @param factoryName The full name (ID)
	 * @param ecf Corba reference to the notify service
	 * @param mc Corba reference to the monitor-control object (TAO extension)
	 */
	public NotifyServiceData(String name, String factoryName, EventChannelFactory ecf, NotificationServiceMonitorControl mc) {
		super(name, null, mc);
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
	
	public void addChannel(String channelName, ChannelData cdata) {
		channels.put(channelName, cdata);
	}
	
	public void removeChannel(String channelName) {
		channels.remove(channelName);
	}

	/////////////////////////////////////////////////////

	
	@Override
	public int getNumberConsumers() {
		int ret = 0;
		for (ChannelData channelData : getChannels()) {
			ret += channelData.getNumberConsumers();
		}
		return ret;
	}

	@Override
	public int getNumberSuppliers() {
		int ret = 0;
		for (ChannelData channelData : getChannels()) {
			ret += channelData.getNumberSuppliers();
		}
		return ret;
	}

	@Override
	public int getDeltaConsumers() {
		int ret = 0;
		for (ChannelData channelData : getChannels()) {
			ret += channelData.getDeltaConsumers();
		}
		return ret;
	}

	@Override
	public int getDeltaSuppliers() {
		int ret = 0;
		for (ChannelData channelData : getChannels()) {
			ret += channelData.getDeltaSuppliers();
		}
		return ret;
	}

	public String getFactoryName() {
		return factoryName;
	}

	/////////////////////////////////////////////////////
	
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

}
