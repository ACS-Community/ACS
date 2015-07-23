/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2013
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

package alma.acs.nsstatistics;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import Monitor.Numeric;

import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;

/**
 * Proxy for the TAO MC object that simplifies the calling syntax and 
 * could in the future batch calls for different statistics 
 * to improve performance and to get more consistent 'snapshot' data.
 * <p>
 * Comment about available statistics:
 * <ul>
 *   <li><code>FactoryNames</code>: Not useful because we only use one factory per notify service process. 
 * <ul>
 * @author hsommer
 */
public class MCProxy {

	private final NotifyServiceData service;
	private final Logger logger;

	public MCProxy(NotifyServiceData service, Logger logger) {
		this.service = service;
		this.logger = logger;
	}
	
	/**
	 * This will find even NCs in the logging and alarm notify service
	 * that are not registered in the naming service and thus usually hidden. 
	 */
	public List<String> listChannels() {
		
		List<String> ret = null;
		try {
			// 'active' NCs have at least one supplier or consumer
			ret = getStringValues(service.getFactoryName(), "ActiveEventChannelNames");
			// we also want to detect 'inactive' NCs of course. 
			ret.addAll(getStringValues(service.getFactoryName(), "InactiveEventChannelNames"));
		} catch (InvalidName ex) {
			logger.log(Level.WARNING, "Failed to retrieve NCs for notify service " + service.getName(), ex);
		}
		return ret;
	}
	
	private List<String> getStringValues(String path, String key) throws InvalidName {
		List<String> ret = new ArrayList<String>();
		String path2 = ( path.isEmpty() ? "" : path + "/" );
		String statName = path2 + key;
		String[] retArray = service.getMc().get_statistic(statName).data_union.list();
		for (String valueWithPath : retArray) {
			ret.add(valueWithPath.substring(path2.length()));
		}
		return ret;
	}
	
//	private Numeric getNumericValue(String path, String key) throws InvalidName {
//		List<String> ret = new ArrayList<String>();
//		String path2 = ( path.isEmpty() ? "" : path + "/" );
//		String statName = path2 + key;
//		Numeric num = service.getMc().get_statistic(statName).data_union.num();
//		return num;
//	}

}
