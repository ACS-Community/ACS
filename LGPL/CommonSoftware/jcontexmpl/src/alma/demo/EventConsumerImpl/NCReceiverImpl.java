/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
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
package alma.demo.EventConsumerImpl;

import alma.FRIDGE.temperatureDataBlockEvent;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.AcsEventSubscriber.Callback;
import alma.acsnc.EventDescription;
import alma.demo.NCReceiverOperations;

/**
 * Implementation class of the ConsumerComp IDL interface,
 * but using the new approach to create consumers through the container services.
 * This implementation doesn't explicitly disconnect from the NC; instead, the container
 * services does it internally.
 *
 * @author  rtobar, Nov 8th, 2010
 */
public class NCReceiverImpl extends ComponentImplBase implements NCReceiverOperations, Callback<temperatureDataBlockEvent>
{
    private AcsEventSubscriber m_consumer;

    public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException
	{

        super.initialize(containerServices);

		try
		{
	        m_consumer = m_containerServices.createNotificationChannelSubscriber(alma.FRIDGE.CHANNELNAME_FRIDGE.value);
			m_consumer.addSubscription(this);
			m_consumer.startReceivingEvents();
			m_logger.info("ConsumerComp is waiting for 'temperatureDataBlockEvent' events.");
		}
		catch (Exception e)
		{
			if (m_consumer != null) {				
				m_consumer.disconnect();
			}
			throw new ComponentLifecycleException("failed to connect as an event consumer to channel " + alma.FRIDGE.CHANNELNAME_FRIDGE.value);
		}
	}

	@Override
	public void receive(temperatureDataBlockEvent joe, EventDescription desc) {
		m_logger.info("Received an event: the temp difference is: " + joe.absoluteDiff);
	}


	@Override
	public Class<temperatureDataBlockEvent> getEventType() {
		return temperatureDataBlockEvent.class;
	}

}
