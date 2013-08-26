/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */
package alma.acs.alarmsystem.source;

import java.sql.Timestamp;
import java.util.Properties;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogLevel;
import alma.acsErrTypeAlarmSourceFactory.ACSASFactoryNotInitedEx;
import alma.acsErrTypeAlarmSourceFactory.SourceCreationErrorEx;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * A support class for sending alarms.
 * 
 * @author acaproni
 *
 */
public class AlarmSender {
	
	/**
	 * The source to send alarms.
	 */
	private ACSAlarmSystemInterface source=null; 
	
	/**
	 * Signal that the alarm factory has been closed and it is not
	 * possible to send alarms anymore
	 */
	private volatile boolean closed=false;
	
	/**
	 * The ContainerServicesBase
	 */
	private final ContainerServicesBase containerServices;
	
	/**
	 * Constructor
	 * 
	 * @param containerServices The ContainerServicesBase
	 */
	public AlarmSender(ContainerServicesBase containerServices) {
		if (containerServices==null) {
			throw new IllegalArgumentException("The containerServices can't be null");
		}
		this.containerServices=containerServices;
	}
	
	/**
	 * Send an alarm with user properties
	 * 
	 * @param FF The fault family
	 * @param FM The fault member
	 * @param FC The fault code
	 * @param properties The properties (can be <code>null</code> or empty)
	 * @param active If <code>true</code> the alarm is activated otherwise it is terminated
	 */
	public void sendAlarm(
			String FF, 
			String FM, 
			int FC, 
			Properties properties, 
			boolean active) {
		if (closed) {
			StringBuilder str = new StringBuilder("Alarm factory closed alarm sending disabled");
			containerServices.getLogger().log(AcsLogLevel.WARNING,str.toString()); 
			return;
		}
		if (source==null) {
			try {
				init();
			} catch (Throwable t) {
				containerServices.getLogger().log(AcsLogLevel.ERROR,"Error initializing the alarm service structs",t);
				// @TODO (hso): return? exception? Otherwise NPE from source.push below
			}
		}
		if (properties==null) {
			properties = new Properties();
		}
		try {
			
			ACSFaultState fs = ACSAlarmSystemInterfaceFactory.createFaultState(FF, FM, FC);
			if (active) {
				fs.setDescriptor(ACSFaultState.ACTIVE);
			} else {
				fs.setDescriptor(ACSFaultState.TERMINATE);
			}
			fs.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
			fs.setUserProperties(properties);

			source.push(fs);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	/**
	 * Send an alarm with no user properties
	 * 
	 * @param FF The fault family
	 * @param FM The fault member
	 * @param FC The fault code
	 * @param active If <code>true</code> the alarm is activated otherwise it is terminated
	 * 
	 * @see AlarmSender#sendAlarm(String, String, int, Properties, boolean)
	 */
	public void sendAlarm(
			String FF, 
			String FM, 
			int FC, 
			boolean active) {
		sendAlarm(FF,FM,FC,null,active);
	}
	
	/**
	 * Create the source and initialize the alarm service factory
	 * if needed.
	 * <P>
	 * If an error happens initializing the factory, 
	 * @throws AcsJContainerServicesEx  In case of error initializing the factory
	 * @throws SourceCreationErrorEx in Case of error creating the source
	 * @throws ACSASFactoryNotInitedEx In case of error initializing the factory
	 */
	private void init() throws AcsJContainerServicesEx, ACSASFactoryNotInitedEx, SourceCreationErrorEx {
		// Check if the factory has been already initialized
		try {
			ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem();
		} catch (ACSASFactoryNotInitedEx e) {
			// The factory has not yet been initialized
			ACSAlarmSystemInterfaceFactory.init(containerServices);
		}
		source = ACSAlarmSystemInterfaceFactory.createSource();
	}
	
	/**
	 * Closes the alarm sending and frees the resources.
	 * <P>
	 * This is the last method to be executed: after it is not possible to send alarms anymore.
	 */
	public void close() {
		closed=true;
	}
}
