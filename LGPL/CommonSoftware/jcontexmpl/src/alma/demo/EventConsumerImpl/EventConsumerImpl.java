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

import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.nc.Consumer;
import alma.demo.ConsumerCompOperations;

/**
 * Implementation class of the ConsumerComp IDL interface.
 * @author  dfugate
 */
public class EventConsumerImpl extends ComponentImplBase implements ConsumerCompOperations
{
    private Consumer m_consumer;

    /** 
	 * Sets up the {@link Consumer}.
     */
	public void initialize(ContainerServices containerServices)
		throws ComponentLifecycleException
	{
        super.initialize(containerServices);
		
		try
		{
	        m_consumer = new Consumer(alma.FRIDGE.CHANNELNAME_FRIDGE.value, m_containerServices);
			//Subscribe to a domain and event type.
			m_consumer.addSubscription(alma.FRIDGE.temperatureDataBlockEvent.class, this);
			m_consumer.consumerReady();
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

	
    /**
     * This method will be called whenever a <code>temperatureDataBlockEvent</code> becomes available 
     * on the channel.
     * <p>
     * Note that this method is found by the framework through introspection, 
     * see {@link Consumer#addSubscription(java.lang.Class, java.lang.Object)}.
     */
    public void receive(alma.FRIDGE.temperatureDataBlockEvent joe)
	{
	    m_logger.info("Received an event: the temp difference is: " + joe.absoluteDiff);
	}

    
	/** 
	 * Disconnects the Consumer
	 */
	public void cleanUp()
	{
		m_consumer.disconnect();
	}
     
}
