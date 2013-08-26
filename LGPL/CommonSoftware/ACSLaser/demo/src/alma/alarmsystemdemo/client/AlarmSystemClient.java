/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
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

/** 
 * @author  almadev   
 * @version $Id: AlarmSystemClient.java,v 1.4 2007/10/17 16:26:52 hsommer Exp $
 * @since    
 */

package alma.alarmsystemdemo.client;

import java.sql.Timestamp;
import java.util.Properties;
import java.util.logging.Logger;

import cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.logging.ClientLogManager;
import alma.alarmsystem.source.ACSAlarmSystemInterface;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;
import alma.alarmsystem.source.ACSFaultState;

/**
 * A client that sends an alarm
 *
 */
public class AlarmSystemClient {
	
	private AdvancedComponentClient client;
	private Logger logger;
	
	// FF, FM and FC
	private final String faultFamily = "JavaClient";
	private final String faultMember = "AlarmSystemClient";
	private final int faultCode      = 1;
	private ACSAlarmSystemInterface source;
	private ACSFaultState faultState;
	
	/** 
	 * The constructor of the client
	 * 
	 * @throws Exception
	 */
	public AlarmSystemClient() throws Exception {
		// Handshake with ACS
		String clientName = getClass().getName();
		String managerLoc = System.getProperty("ACS.manager").trim();
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(clientName,true);
		client = new AdvancedComponentClient(logger, managerLoc, clientName);
		logger.info(clientName+" connected");
		// Hanshake with the alarm system
		ACSJMSTopicConnectionImpl.containerServices=client.getContainerServices();
		logger.info("AS factory inited");
		// Create the source
		source = ACSAlarmSystemInterfaceFactory.createSource(client.getContainerServices().getName());
		logger.info("Alarm source created");
		// Create the fault state
		faultState = ACSAlarmSystemInterfaceFactory.createFaultState(faultFamily, faultMember, faultCode);
		logger.info("FaultState instantiated");
		Properties props = new Properties();
		props.setProperty(ACSFaultState.ASI_PREFIX_PROPERTY, "prefix");
		props.setProperty(ACSFaultState.ASI_SUFFIX_PROPERTY, "suffix");
		faultState.setUserProperties(props);
	}
	
	public void sendAlarm(boolean active) {
		if (source==null || faultState==null) {
			throw new IllegalStateException("Alarm source and/or FaultState not properly initialized");
		}
		String alStr = " alarm: &lt;"+faultState.getFamily()+","+faultState.getMember()+","+faultState.getCode()+"&gt;";
		if (active) {
			logger.info("Sending ACTIVE"+alStr);
			faultState.setDescriptor(ACSFaultState.ACTIVE);
		} else {
			logger.info("Sending TERMINATE"+alStr);
			faultState.setDescriptor(ACSFaultState.TERMINATE);
		}
		faultState.setUserTimestamp(new Timestamp(System.currentTimeMillis()));
		source.push(faultState);
		logger.info("alarm sent");
	}
	
	public static void main(String[] args) {
		AlarmSystemClient alSysClient=null;
		try {
			alSysClient= new AlarmSystemClient();
		} catch (Exception e) {
			System.out.println("Error instantiating the client"+e.getMessage());
			e.printStackTrace();
		}
		alSysClient.sendAlarm(true);
		try {
			Thread.sleep(10000);
		} catch (Exception e) {}
		alSysClient.sendAlarm(false);
	}
}
