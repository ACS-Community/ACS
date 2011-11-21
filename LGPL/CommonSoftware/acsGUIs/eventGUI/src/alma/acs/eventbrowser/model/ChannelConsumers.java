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

import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;

public class ChannelConsumers extends MCStatistics {
	private  ArrayList<ChannelParticipantName> consumerNames;

	public ChannelConsumers(AbstractNotifyServiceElement parent) {
		super(parent);
		statName = "ConsumerNames";
	}
	
	@Override
	public String getStatistics() {
		String sc[];
		consumerNames = new ArrayList<ChannelParticipantName>();
		try {
			sc = mc.get_statistic(channelPrefix+statName).data_union.list();
			for (int i = 0; i < sc.length; i++) {
				consumerNames.add(new ChannelParticipantName(sc[i], this));
			}
		} catch (InvalidName e) {
			System.out.println("Invalid name: "+channelPrefix+statName);
		}
		return "Consumers: "+parent.getNumConsumersAndDelta();
	}
	
	public Object[] getNames() {
		return consumerNames.toArray();
	}
	
}
