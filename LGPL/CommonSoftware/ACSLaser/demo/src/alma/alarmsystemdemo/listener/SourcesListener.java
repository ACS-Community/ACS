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
 * @version $Id: SourcesListener.java,v 1.6 2012/11/08 08:31:45 hsommer Exp $
 * @since    
 */

package alma.alarmsystemdemo.listener;

import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;

import cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl;
import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;

import com.cosylab.acs.jms.ACSJMSMessageEntity;

import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.container.ContainerServices;
import alma.acs.logging.ClientLogManager;
import alma.acs.nc.AcsEventSubscriber;
import alma.acscommon.ACS_NC_DOMAIN_ALARMSYSTEM;
import alma.acsnc.EventDescription;
import alma.alarmsystem.source.ACSAlarmSystemInterfaceFactory;

/**
 * A java client that listens for the alarms sent by sources.
 * For each received alarm, prints a message in the console.
 */
public class SourcesListener implements AcsEventSubscriber.Callback<ACSJMSMessageEntity> {
	
	/** 
	 * Actually there is only one channel used by all sources to publish alarms
	 **/ 
	private static final String srcChName = "CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES";
	
	private AdvancedComponentClient m_client;
	
	private ContainerServices m_contSvcs;
	
	private AcsEventSubscriber<ACSJMSMessageEntity> m_consumer;
	
	final Logger logger;
	
	/**
	 * Constructor
	 *
	 */
	public SourcesListener() {
		// Handshake with ACS
		String clientName = getClass().getName();
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(clientName,true);
		String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
                System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
                System.exit(-1);
        }
        try {
        	m_client = new AdvancedComponentClient(logger,managerLoc,clientName);
        } catch (Exception e) {
        	System.out.println("Error creating the AdvancedComponentClient: "+e.getMessage());
        	e.printStackTrace();
        	System.exit(-1);
        }
        logger.info(clientName+" connected");
        m_contSvcs=m_client.getContainerServices();
        
        try {
	        if (!ACSAlarmSystemInterfaceFactory.usingACSAlarmSystem()) {
	        	ACSJMSTopicConnectionImpl.containerServices=m_contSvcs;
	        }
        } catch (Exception e) {
        	logger.severe("AS factory not inited: "+e.getMessage());
        	e.printStackTrace();
        	System.exit(-1);
        }
		// Connect to the NC used by the sources
		try {
			m_consumer = m_contSvcs.createNotificationChannelSubscriber(srcChName,
					ACS_NC_DOMAIN_ALARMSYSTEM.value, ACSJMSMessageEntity.class);
		} catch (Exception e) {
			logger.severe("Error instantiating the consumer: " + e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		try {
			m_consumer.addSubscription(this);
		} catch (Exception e) {
			logger.severe("Error subscribing: " + e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		try {
			m_consumer.startReceivingEvents();
		} catch (Exception e) {
			logger.severe("Error in consumerReady: " + e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
	}
	
	/**
	 * The method receives all the messages published in the NC by the sources
	 * 
	 *  
	 * @param msg The message received from the NC
	 */
	@Override
	public void receive(ACSJMSMessageEntity msg, EventDescription eventDescrip) {
		logger.fine("Msg received");
		ASIMessage asiMsg;
		Collection<FaultState> faultStates;
		try {
			asiMsg = XMLMessageHelper.unmarshal(msg.text);
			faultStates = ASIMessageHelper.unmarshal(asiMsg);
		} catch (Exception e) {
			System.out.println("Exception caught while unmarshalling the msg "+e.getMessage());
			e.printStackTrace();
			return;
		}
		Iterator<FaultState> iter = faultStates.iterator();
		while (iter.hasNext()) {
			FaultState fs = iter.next();
			StringBuilder str = new StringBuilder("Alarm message received: <");
			str.append(fs.getFamily());
			str.append(",");
			str.append(fs.getMember());
			str.append(",");
			str.append(fs.getCode());
			str.append(">");
			str.append(" Status: ");
			str.append(fs.getDescriptor());
			System.out.println(str.toString());
		}
	}
	
	@Override
	public Class<ACSJMSMessageEntity> getEventType() {
		return ACSJMSMessageEntity.class;
	}
	

	
	public static void main(String[] args) {
		SourcesListener srcListener = new SourcesListener();
		// Wait forever
		synchronized (srcListener) {
			try {
				srcListener.wait();
			} catch (Exception e) {}
		}
	}

}
