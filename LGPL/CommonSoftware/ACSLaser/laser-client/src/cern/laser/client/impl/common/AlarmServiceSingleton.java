package cern.laser.client.impl.common;

import cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl;
import alma.acs.component.client.ComponentClient;
import alma.acs.util.AcsLocations;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.AlarmServiceHelper;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;

/**
 * Singleton class to connect to and supply the AlarmService.
 * 
 * @author Igor Habjan, Cosylab
 */
public class AlarmServiceSingleton {

	private static AlarmService instance = null;
	private static ComponentClient client = null;

	public static synchronized AlarmService getInstance() {
		if (instance == null) {
			String managerLoc = AcsLocations.figureOutManagerLocation();
			String clientName = new String("laser-client");

			if (client == null) {
				try {
					client = new ComponentClient(null, managerLoc, clientName);
					ACSJMSTopicConnectionImpl.containerServices = client.getContainerServices();
				} catch (Exception e) {
					client = null;
					instance = null;
					System.out.println("AlarmServiceSingleton::AlarmServiceSingleton() Error instantiating the component client!");
					e.printStackTrace(System.out);
				}
			}
			// Get the AlarmService component
			if (client != null) {
				try {
					instance = AlarmServiceHelper.narrow(client
							.getContainerServices()
							.getComponent("AlarmService"));
				} catch (AcsJContainerServicesEx ce) {
					System.out
					.println("Exception getting the AlarmService component!");
					ce.printStackTrace(System.out);
				}
			}
		}
		System.out.println("AlarmSevice instance " + instance);
		return instance;
	}
	
	public static synchronized ComponentClient getComponentClientInstance()
	{
		if (instance == null)
		{
			getInstance();
		}
		return client;
	}
}
