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

	private final String channelPrefix;
	
	public MCStatistics(ChannelData parent, String mcStatName) {
		this.parent = parent;
		this.mcStatName = mcStatName;
		String factoryName = getParent().getParent().getFactoryName();
		String channelName = getParent().getName();
		channelPrefix = factoryName + "/" + channelName + "/";
	}
	
	/**
	 * Subclass implementations retrieve notify service statistics from {@link #mc}. 
	 */
	public abstract String getStatistics();
	
	/**
	 * This should be used by implementations of {@link #getStatistics()}.
	 */
	protected UData getMcData() {
		String fullName = channelPrefix + mcStatName;
		try {
			 return parent.getParent().getMc().get_statistic(fullName).data_union;
		} catch (InvalidName ex) {
			// Todo: deal better with this error. Could be wrong channel name or an unsupported (misspelled etc) statistics name.
			System.out.println("Invalid name: '" + fullName + 
					"'; valid names are " + StringUtils.join(parent.getParent().getMc().get_statistic_names(), ' '));
			throw new RuntimeException(ex);
		}
	}
	
	public ChannelData getParent() {
		return parent;
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
