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
 * EventConsumerImpl.java
 *
 * Created on April 11, 2003, 2:21 PM
 */

package alma.demo.EventConsumerImpl;

import java.util.logging.Level;

import alma.FRIDGE.CHANNELNAME_FRIDGE;
import alma.FRIDGE.temperatureDataBlockEvent;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.nc.AcsEventSubscriber;
import alma.acsnc.EventDescription;
import alma.demo.ConsumerCompOperations;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

/**
 * Implementation class of the ConsumerComp IDL interface.
 * 
 * @author dfugate
 */
public class EventConsumerImpl extends ComponentImplBase implements ConsumerCompOperations, AcsEventSubscriber.Callback<temperatureDataBlockEvent>
{
	private AcsEventSubscriber<temperatureDataBlockEvent> m_consumer;

	/**
	 * Sets up the {@link AcsEventSubscriber}.
	 */
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException {
		super.initialize(containerServices);

		try {
			m_consumer = containerServices.createNotificationChannelSubscriber(alma.FRIDGE.CHANNELNAME_FRIDGE.value,
					temperatureDataBlockEvent.class);
			m_consumer.addSubscription(this);
			m_consumer.startReceivingEvents();
			m_logger.info("ConsumerComp is waiting for 'temperatureDataBlockEvent' events.");
		} catch (Exception ex) {
			if (m_consumer != null) {
				try {
					m_consumer.disconnect();
				} catch (Exception ex2) {
					m_logger.log(Level.WARNING, "Failed to disconnect a messed-up NC subscriber", ex2);
				}
			}
			throw new ComponentLifecycleException("failed to connect as an event consumer to channel " + CHANNELNAME_FRIDGE.value, ex);
		}
	}

	/**
	 * This method will be called whenever a <code>temperatureDataBlockEvent</code> becomes available on the channel.
	 */
	@Override
	public void receive(temperatureDataBlockEvent joe, EventDescription eventDescrip) {
		m_logger.info("Received an event: the temp difference is: " + joe.absoluteDiff);
	}

	@Override
	public Class<temperatureDataBlockEvent> getEventType() {
		return temperatureDataBlockEvent.class;
	}

	/**
	 * Disconnects the Consumer
	 */
	public void cleanUp() throws AcsJComponentCleanUpEx {
		try {
			m_consumer.disconnect();
		} catch (Exception ex) {
			throw new AcsJComponentCleanUpEx(ex);
		}
	}

}
