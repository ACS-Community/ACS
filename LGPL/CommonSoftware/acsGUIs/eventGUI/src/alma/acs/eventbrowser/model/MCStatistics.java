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

import org.apache.commons.lang.StringUtils;

import Monitor.UData;

import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;

public abstract class MCStatistics {
	
	private final ChannelData parent;
	protected final String mcStatName;
	protected final List<ChannelParticipantName> children = new ArrayList<ChannelParticipantName>();

	private String mcStatFullName;
	
	public MCStatistics(ChannelData parent, String mcStatName) {
		this.parent = parent;
		this.mcStatName = mcStatName;
	}
	
	/**
	 * Subclass implementations retrieve notify service statistics from {@link #mc}. 
	 */
	public abstract String getStatistics();
	
	/**
	 * This should be used by implementations of {@link #getStatistics()}.
	 */
	protected UData getMcData() {
		if (mcStatFullName == null) {
			mcStatFullName = getChannelPrefix(true) + mcStatName;
		}
		try {
			NotifyServiceData notifyServiceData = parent.getParent();
			if (notifyServiceData.isReachable()) {
				return notifyServiceData.getMc().get_statistic(mcStatFullName).data_union;
			}
			else {
				// This is a shortcut to not wait for a TRANSIENT or TIMEOUT
				throw new RuntimeException("Failed to get statistics from unreachable notify service " + notifyServiceData.getName());
			}
		} catch (InvalidName ex) {
			// Try again with channel Id instead of channel name (no TAO extensions used when creating this NC)
			String mcStatFullNameTmp = getChannelPrefix(false) + mcStatName;
			try {
				 UData ret = parent.getParent().getMc().get_statistic(mcStatFullNameTmp).data_union;
				 // next time use this ID-based stat name right away 
				 mcStatFullName = mcStatFullNameTmp;
				 return ret;
			} catch (InvalidName ex2) {
				// nothing else. It is enough to report on the first ex
			}
			// Todo: deal better with this error. Could be wrong channel name or an unsupported (misspelled etc) statistics name.
			System.out.println("Invalid name: '" + mcStatFullName + 
					"'; valid names are " + StringUtils.join(parent.getParent().getMc().get_statistic_names(), ' '));
			mcStatFullName = null; // not sure if trying again next time will help, but let's try
			throw new RuntimeException(ex);
		}
	}
	
	public ChannelData getParent() {
		return parent;
	}

	/**
	 * The NC prefix must be computed "on the fly", because at least in case of the NC ID 
	 * not all data is available when this MCStatistics is created.
	 * @param useNcName <code>true</code> means to use the NC name if available, <code>false</code> means to use the NC ID.
	 */
	private String getChannelPrefix(boolean useNcName) {
		String channelPrefix = null;
		String factoryName = getParent().getParent().getFactoryName();
		if (useNcName) {
			String channelName = getParent().getName();
			channelPrefix = factoryName + "/" + channelName + "/";
		}
		else {
			int channelId = getParent().getNcId();
			channelPrefix = factoryName + "/" + channelId + "/";
		}
		return channelPrefix;
	}
	
	/**
	 * Gets the (possibly empty) {@link #children} list an Object[], which becomes tree node children.
	 */
	public Object[] getChildren() {
		return children.toArray();
	}

	/**
	 * TODO: unify MC path handling with class MCProxy
	 * @param sc
	 * @param i
	 * @return
	 */
	protected String toSimpleName(String fullMcName, int i) {
		String[] nameParts = fullMcName.split("/");
		String nameSimple = nameParts[nameParts.length-1];
		return nameSimple;
	}

}
