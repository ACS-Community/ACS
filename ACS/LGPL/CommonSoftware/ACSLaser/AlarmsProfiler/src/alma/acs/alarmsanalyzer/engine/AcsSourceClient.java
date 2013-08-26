/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
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
package alma.acs.alarmsanalyzer.engine;

import java.util.logging.Logger;

import com.cosylab.CDB.DAL;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.container.ContainerServices;
import alma.alarmsystem.clients.SourceClient;
import alma.alarmsystem.clients.source.SourceListener;

/**
 * AcsSimpleClient is a singleton encapsulating an ACS SimpleClient.
 *  
 * @author acaproni
 * @since ACS 8.2.0
 */
public class AcsSourceClient {
	
	/**
	 * The component client
	 */
	private static ComponentClient client=null;
	
	/**
	 * The singleton
	 */
	private static AcsSourceClient acsClient=null;
	
	/**
	 * The source client getting alarms from the SOURCE NC
	 */
	private static SourceClient alarmSourceClient=null;
	
	/**
	 * The category client
	 */
	private static AcsCategoryClient alarmCategoryClient=null;
	
	/**
	 * This is the factory of the singleton
	 * 
	 * @return The AcsSimpleClient
	 * @throws Exception If the connection with the manager failed
	 */
	public static AcsSourceClient getInstance() throws Exception {
		if (acsClient==null) {
			acsClient= new AcsSourceClient();
		}
		return acsClient;
	}
	
	/**
	 * Connect the client to the passed manager.
	 * If the passed loc is <code>null</code> or empty,
	 * the class tries to connect by getting the loc from the ACS.manager java property.
	 * 
	 * @param managerLoc The loc of the manager
	 * @throws Exception If the connection with the manager failed
	 */
	private AcsSourceClient() throws Exception {
		String managerLoc=System.getProperty("ACS.manager");
		System.out.println("Connecting to ACS: "+managerLoc);
		try {
			client = new ComponentClient(null, managerLoc, "AcsAlarmsAnalyzer");
			System.out.println("ACS SimpleClient connected");
		} catch (Throwable t) {
			t.printStackTrace();
			client=null;
			acsClient=null;
			alarmSourceClient=null;
			throw new Exception("Error connecting to the manager",t);
		}
		try {
			alarmSourceClient = new SourceClient(getContainerServices());
			System.out.println("Alarm source client instantiated");
		} catch (Throwable t) {
			alarmSourceClient=null;
			try {
				client.tearDown();
			} catch (Throwable t2) {}
			client=null;
			acsClient=null;
			throw new Exception("Error connetting the source client", t);
		}
		try {
			alarmCategoryClient= new AcsCategoryClient(getContainerServices());
			System.out.println("Alarm category client instantiated");
			
		} catch (Throwable t) {
			alarmSourceClient.close();
			client.tearDown();
			client=null;
			acsClient=null;
			alarmCategoryClient=null;
			throw new Exception("Error connetting the category client", t);
		}
	}
	
	/**
	 * 
	 * @return The ContainerServices
	 */
	public ContainerServices getContainerServices() {
		return client.getContainerServices();
	}
	
	/**
	 * @return The DAL
	 * @throws Exception In case of error getting the CDB from the <code>ContainerServices</code>
	 * @throws AcsJContainerServicesEx 
	 */
	public DAL getCDB() throws AcsJContainerServicesEx {
		return client.getContainerServices().getCDB();
	}
	
	public Logger getLogger() {
		return client.getContainerServices().getLogger();
	}
	
	/**
	 * Connect to alarm SourceClient and start sending alarms to the
	 * listeners
	 * 
	 * @throws Exception In case of failure connecting the SourceClient to the
	 * 					source NC
	 */
	public void connect() throws Exception {
		alarmSourceClient.connect();
		alarmCategoryClient.connect();
	}
	
	/**
	 * Add a listener to receive source alarms
	 * 
	 * @param listener The listener receiving source alarms
	 */
	public void addAlarmSourceListener(SourceListener listener) {
		alarmSourceClient.addAlarmListener(listener);
	}
	
	/**
	 * Remove a listener of source alarms
	 * 
	 * @param listener The listener of source alarms to remove
	 */
	public void removeAlarmSourceListener(SourceListener listener) {
		alarmSourceClient.removeListener(listener);
	}
	
	public static void shutdown() {
		if (alarmCategoryClient!=null) {
			try {
				alarmCategoryClient.close();
			} catch (Throwable t) {
				// Nothing to do
				t.printStackTrace();
			}
			alarmCategoryClient=null;
		}
		if (alarmSourceClient!=null) {
			alarmSourceClient.close();
			alarmSourceClient=null;
		}
		if (client==null) {
			return;
		}
		try {
			client.tearDown();
		} catch (Throwable t) {
			// Nothing to do
			t.printStackTrace(System.err);
		} finally {
			client=null;
			acsClient=null;
		}
	}

	/**
	 * Add a category listener
	 *  
	 * @param listener The listener
	 */
	public void addListener(AlarmCategoryListener listener) {
		alarmCategoryClient.addListener(listener);
	}

	/**
	 * Remove a category listener
	 *  
	 * @param listener The listener
	 */
	public void removeListener(AlarmCategoryListener listener) {
		alarmCategoryClient.removeListener(listener);
	}
}
