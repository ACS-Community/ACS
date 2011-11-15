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

public abstract class MCStatistics {
	
	protected AbstractNotifyServiceElement parent;
	protected String statistic;
	protected String channelPrefix;
	protected String factoryName;
	protected String statName;
	protected NotificationServiceMonitorControl mc;
	
	public MCStatistics(AbstractNotifyServiceElement parent) {
		this.parent = parent;
		factoryName = ((NotifyServiceData) ((ChannelData)getParent()).getParent()).getFactoryName();
		channelPrefix = factoryName+"/"+((ChannelData)getParent()).getName()+"/";
		mc = parent.getMc();
	}
	
	public abstract String getStatistics();
	
	public AbstractNotifyServiceElement getParent() {
		return parent;
	}

}
