/*
 * ALMA - Atacama Large Millimeter Array
 * (c) European Southern Observatory, 2002
 * (c) Associated Universities Inc., 2002
 * Copyright by ESO (in the framework of the ALMA collaboration),
 * Copyright by AUI (in the framework of the ALMA collaboration),
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY, without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 * MA 02111-1307  USA
 * File Container.java
 */
package alma.TMCDB.maci;

import java.util.HashMap;
import java.util.Map;

public class NotificationServiceMapping {

    @SuppressWarnings("unused")
	private int NotificationServiceMappingId;
    @SuppressWarnings("unused")
        private int ConfigurationId;

    private String DefaultNotificationService;
    private Map<String, DomainsMapping> Domains = new HashMap<String, DomainsMapping>();
    private Map<String, ChannelMapping> Channels_ = new HashMap<String, ChannelMapping>();

    /**
     * Default Constructor for Component.  Setter methods must be used to insert data.
     */
    public NotificationServiceMapping () {
    }

	/**
	 * @return the defaultNotificationService
	 */
	public String getDefaultNotificationService() {
		return DefaultNotificationService;
	}

	/**
	 * @param defaultNotificationService the defaultNotificationService to set
	 */
	public void setDefaultNotificationService(String defaultNotificationService) {
		DefaultNotificationService = defaultNotificationService;
	}

	/**
	 * @return the domains
	 */
	public Map<String, DomainsMapping> getDomains() {
		return Domains;
	}

	/**
	 * @param domains the domains to set
	 */
	public void setDomains(Map<String, DomainsMapping> domains) {
		Domains = domains;
	}

	/**
	 * @return the channels_
	 */
	public Map<String, ChannelMapping> getChannels_() {
		return Channels_;
	}

	/**
	 * @param channels the channels_ to set
	 */
	public void setChannels_(Map<String, ChannelMapping> channels) {
		Channels_ = channels;
	}

    
}

