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
import java.util.List;
import java.util.logging.Logger;

import org.omg.CosNotifyChannelAdmin.EventChannel;
import org.omg.CosNotifyChannelAdmin.EventChannelFactory;

import gov.sandia.CosNotification.NotificationServiceMonitorControl;

import alma.acscommon.ALARM_NOTIFICATION_FACTORY_NAME;
import alma.acscommon.ARCHIVE_NOTIFICATION_FACTORY_NAME;
import alma.acscommon.LOGGING_NOTIFICATION_FACTORY_NAME;
import alma.acscommon.NOTIFICATION_FACTORY_NAME;


/**
 * Encapsulates a Corba notify service for use in the model and in GUI elements.
 * <p>
 * All equality and comparison methods are based on the simplified name.
 */
public class NotifyServiceData extends AbstractNotifyServiceElement implements Comparable<NotifyServiceData> {

	private final HashMap<String, ChannelData> channels;
	private final String factoryName;
	
	/**
	 * The notify service reference. May be null for a while if the service is unreachable when the eventGUI is started.
	 */
	private EventChannelFactory efact;
	
	/**
	 * The MC object does not provide a persistent reference and must therefore be udpated after service restart.
	 * @see #updateMC(NotificationServiceMonitorControl)
	 */
	private volatile NotificationServiceMonitorControl mc;
	

	/**
	 * Flag that tells us if the corba notify service referenced by this class is reachable or not.
	 * In case of being unreachable, it may or may not exist.
	 * This flag is independent of {@link #isRegistered}.
	 */
	private volatile boolean isReachable;
	
//	/**
//	 * Flag that tells us if the corba notify service referenced by this class is registered in the naming service.
//	 * This flag is independent of {@link #isReachable}.
//	 * Note that a service that has died will typically still be registered in the naming service.
//	 */
//	private boolean isRegistered;
	
	private final Logger logger;
	
	
	
	/**
	 * TODO: Get MC object in the ctor instead of passing it. 
	 * Also get it again somehow after a service restart.
	 * 
	 * @param name The simplified display name
	 * @param factoryName The full name (ID)
	 * @param ecf Corba reference to the notify service
	 * @param mc Corba reference to the monitor-control object (TAO extension)
	 */
	public NotifyServiceData(String name, String factoryName, EventChannelFactory ecf, NotificationServiceMonitorControl mc, Logger logger) {
		super(name);
		channels = new HashMap<String, ChannelData>(10);
		efact = ecf;
		this.factoryName = factoryName;
		this.mc = mc;
//		this.isRegistered = true;
		this.isReachable = true;
		this.logger = logger;
	}
	
	/**
	 * @return The corba ref to the notify service, or <code>null</code> if the eventGUI has never found
	 * 			this service to be reachable and thus has not obtained a correctly narrowed reference yet. 
	 */
	public EventChannelFactory getEventChannelFactory() {
		return efact;
	}
	
	public NotificationServiceMonitorControl getMc() {
		return mc;
	}

	/**
	 * The notify service ref must be updated once after the notify service becomes available, after initial unreachability.
	 */
	public void updateEventChannelFactory(EventChannelFactory efact) {
		if (this.efact == null) {
			this.efact = efact;
		}
		else {
			logger.warning("Ignoring call to 'updateEventChannelFactory' for notify service '" + getFactoryName() + "' because we already have that corba ref!");
		}
	}

	/**
	 * The MC object ref must be updated when a notify service was restarted.
	 */
	public void updateMC(NotificationServiceMonitorControl monitorControl) {
		mc = monitorControl;
	}


	/**
	 * TODO: Possibly use this information to graphically distinguish between the notify service instances
	 * always started by ACS, and any additional service instances. 
	 */
	public boolean isStandardNotifyService() {
		return (factoryName.equals(NOTIFICATION_FACTORY_NAME.value) ||
				factoryName.equals(ALARM_NOTIFICATION_FACTORY_NAME.value) ||
				factoryName.equals(ARCHIVE_NOTIFICATION_FACTORY_NAME.value) || 
				factoryName.equals(LOGGING_NOTIFICATION_FACTORY_NAME.value) );
	}

	public boolean isReachable() {
		return isReachable;
	}

	public void setReachable(boolean isReachable) {
		this.isReachable = isReachable;
	}

//	public boolean isRegistered() {
//		return isRegistered;
//	}
//
//	public void setRegistered(boolean isRegistered) {
//		this.isRegistered = isRegistered;
//	}


	public ArrayList<ChannelData> getChannels() {
		return new ArrayList<ChannelData>(channels.values());
	}
	
	public ChannelData getChannelByName(String channelName) {
		return channels.get(channelName);
	}
	
	public ChannelData getChannelById(int ncId) {
		for (ChannelData nc : getChannels()) {
			if (nc.getNcId() == ncId) {
				return nc;
			}
		}
		return null;
	}
	
	/**
	 * This method can be used to merge incomplete NC information sets coming from the naming service,
	 * the regular notify service API, and the MC TAO extension API.
	 * <p>
	 * The matching may fail even if the corba references point to the same NC object,
	 * see {@link org.omg.CORBA.Object#_is_equivalent(org.omg.CORBA.Object)}.
	 */
	public ChannelData getChannelByCorbaRef(EventChannel corbaRef) {
		for (ChannelData nc : getChannels()) {
			if (nc.getCorbaRef()._is_equivalent(corbaRef)) {
				return nc;
			}
		}
		return null;
	}
	
	public List<ChannelData> getNewChannels() {
		List<ChannelData> ret = new ArrayList<ChannelData>();
		for (ChannelData nc : getChannels()) {
			if (nc.isNewNc()) {
				ret.add(nc);
			}
		}
		return ret;
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
