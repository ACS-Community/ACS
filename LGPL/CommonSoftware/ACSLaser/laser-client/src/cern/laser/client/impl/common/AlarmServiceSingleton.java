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

import cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl;
import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServicesBase;
import alma.acs.util.AcsLocations;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.corbaservice.utils.AlarmServiceUtils;

/**
 * Singleton class to connect to and supply the AlarmService.
 * 
 * @author Igor Habjan, Cosylab
 */
public class AlarmServiceSingleton {

	private static AlarmService instance = null;
	private static ComponentClient client;
	private static ContainerServicesBase contSvcBase = null;
	
	/**
	 * The alarm service utils 
	 */
	private static AlarmServiceUtils alarmUtils=null;

	/**
	 * Get an instance of the alarm service
	 * <P>
	 * This class instantiates a component client to get the Container Services out of it.
	 * 
	 * @return A reference to the alarm service
	 * @throws ExceptionIn case of error getting the alarm service
	 */
	public static synchronized AlarmService getInstance() throws Exception {
		if (instance == null) {
			String managerLoc = AcsLocations.figureOutManagerLocation();
			String clientName = new String("laser-client");

			if (client == null) {
				try {
					client = new ComponentClient(null, managerLoc, clientName);
					contSvcBase=client.getContainerServices();
					ACSJMSTopicConnectionImpl.containerServices = contSvcBase;
				} catch (Exception e) {
					client = null;
					instance = null;
					System.out.println("AlarmServiceSingleton::AlarmServiceSingleton() Error instantiating the component client!");
					e.printStackTrace(System.out);
				}
			}
			// Get the AlarmService 
			if (client != null) {
				alarmUtils = new AlarmServiceUtils(client.getContainerServices());
				instance=alarmUtils.getAlarmService();
			}
		}
		return instance;
	}
	
	public static synchronized AlarmService getInstance(ContainerServicesBase contSvc) throws Exception {
		if (contSvc==null) {
			throw new IllegalArgumentException("ContainerServicesBase can't be null");
		}
		contSvcBase=contSvc;
		if (instance==null) {
			if (alarmUtils==null) {
				alarmUtils = new AlarmServiceUtils(contSvc);
			}
			instance=alarmUtils.getAlarmService();
		}
		return instance;
	}
	
	
	
//	public static synchronized ComponentClient getComponentClientInstance()
//	{
//		if (instance == null)
//		{
//			getInstance();
//		}
//		return client;
//	}
}
