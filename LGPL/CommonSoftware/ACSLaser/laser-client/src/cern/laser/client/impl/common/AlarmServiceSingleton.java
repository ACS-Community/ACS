/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2009
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package cern.laser.client.impl.common;

import org.omg.CORBA.ORB;

import cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl;
import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.acs.util.AcsLocations;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.CERNAlarmService;
import alma.alarmsystem.corbaservice.CernAlarmServiceUtils;

/**
 * Singleton class to connect to and supply the AlarmService.
 * 
 * @author Igor Habjan, Cosylab
 */
public class AlarmServiceSingleton {

	private static CERNAlarmService instance = null;
	
	public static synchronized CERNAlarmService getInstance(ORB orb, AcsLogger logger) throws Exception {
		if (orb==null) {
			throw new IllegalArgumentException("ORB can't be null");
		}
		if (logger==null) {
			throw new IllegalArgumentException("The Logger can't be null");
		}
		if (instance==null) {
			CernAlarmServiceUtils alarmUtils = new CernAlarmServiceUtils(orb, logger);
			instance=alarmUtils.getCernAlarmService();
		}
		return instance;
	}
}
