/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Associated Universities Inc., 2002
 *    (c) European Southern Observatory, 2002
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
 *
 * ncTestCompImpl.java
 *
 * Created on April 11, 2003, 2:21 PM
 */
package alma.acs.eventbrowser.model;

import java.util.logging.Logger;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventPublisher;
import alma.acsnc.EventDescription;

/** Class designed for testing event suppliers.
 * @author original by dfugate, hacked for eventGUI testing by jschwarz
 * Refactored to replace deprecated SimpleSupplier with container services createNotificationChannelPublisher method
 */
public class EventSupplierImpl
{
	private Logger m_logger;
	private ContainerServices m_containerServices;
	
	private static final String channelName = "blar";

	public EventSupplierImpl(Logger logger, ContainerServices cs, String clientName)
			throws Exception {
		m_logger = logger;
		m_containerServices = cs;
	}

	private EventDescription t_block;
	private AcsEventPublisher<EventDescription> epub;

	

	/** Sets up the SimpleSupplier.
	 * @param containerServices Services to components.
	 * @throws ComponentLifecycleException Not thrown.
	 */
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {

		try {
			//Instantiate our supplier
			t_block = new EventDescription("no name", 32L, 64L);
			epub = m_containerServices.createNotificationChannelPublisher(channelName, EventDescription.class);
			m_logger.info("NC Publisher for '"+channelName+"' channel created.");
		} catch (Exception e) {
			throw new ComponentLifecycleException(e);
		}
	}

	
	/** Sends some events to an event channel.
	 * @param param number of events to send
	 */
	public void sendEvents(short param) {
		m_logger.info("Now sending test events...");
		try {
			//first send out some number of events.
			for (short i = 0; i < param; i++) {
				epub.publishEvent(t_block);
			}
		} catch (AcsJException e) {
			System.err.println(e);
		}
	}

	/** Disconnects the supplier. 
	 * @throws AcsJIllegalStateEventEx */
	public void cleanUp() throws AcsJIllegalStateEventEx {
		m_logger.info("cleanUp() called...");

		try {
			epub.disconnect();
		} catch (IllegalStateException e) {
			e.printStackTrace();
		}
	}
}
