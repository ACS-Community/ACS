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
package alma.demo.EventSupplierImpl;

import java.util.logging.Level;

import org.omg.CORBA.portable.IDLEntity;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.FRIDGE.TemperatureStatus;
import alma.FRIDGE.temperatureDataBlockEvent;
import alma.FRIDGE.FridgeControlPackage.NestedFridgeEvent;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventPublisher;
import alma.demo.SupplierCompOperations;


/**
 * 
 * @author  dfugate
 */
public class EventSupplierImpl extends ComponentImplBase implements SupplierCompOperations
{
    /**
     * We publish the two event types temperatureDataBlockEvent and NestedFridgeEvent,
     * and thus can only restrict the publisher to the common base type IDLEntity.
     */
    private AcsEventPublisher<IDLEntity> m_supplier;

    /**
     * Sets up the AcsEventPublisher.
     */
    public void initialize(ContainerServices containerServices) throws ComponentLifecycleException
    {
    	super.initialize(containerServices);
    	try {
    		// Instantiate our supplier
    		m_supplier = containerServices.createNotificationChannelPublisher(
    						alma.FRIDGE.CHANNELNAME_FRIDGE.value, IDLEntity.class);
    		
    		// enable event queue and register callback handler, just to demonstrate how this is done
    		AcsEventPublisher.EventProcessingHandler<IDLEntity> cbHandler = new EventProcessingCallbackImpl();
    		m_supplier.enableEventQueue(100, cbHandler);
    	}
    	catch (Exception e) {
    		throw new ComponentLifecycleException("failed to create AcsEventPublisher for channel " + alma.FRIDGE.CHANNELNAME_FRIDGE.value, e);
    	}
    }

    /**
     * Disconnects the supplier before component is removed.
     */
    public void cleanUp()
    {
        m_supplier.disconnect();
    }
    

    /**
     * The IDL-defined method that sends the <code>temperatureDataBlockEvent</code> fridge event a given number of times.
     */
    public void sendEvents(short param) throws CouldntPerformActionEx
    {
    	m_logger.info("Now sending " + param + " temperatureDataBlockEvent events...");
        try {
            temperatureDataBlockEvent t_block = new temperatureDataBlockEvent(3.14F, TemperatureStatus.ATREF);
            for(short i=0; i<param; i++)
            {
                m_supplier.publishEvent(t_block);
            }
        }
        catch(Throwable thr) {
            m_logger.log(Level.WARNING, "failed to send temperatureDataBlockEvent. Will not try again.");
            throw (new AcsJCouldntPerformActionEx(thr)).toCouldntPerformActionEx();
        }
    }

    public void sendEventsSpecial(NestedFridgeEvent[] eventData) throws CouldntPerformActionEx {
        try {
        	m_logger.info("Now sending " + eventData.length + " NestedFridgeEvent events...");
            for (NestedFridgeEvent event : eventData) {
                m_supplier.publishEvent(event);
			}
        }
        catch(Throwable thr) {
            m_logger.log(Level.WARNING, "failed to send NestedFridgeEvent. Will not try again.");
            throw (new AcsJCouldntPerformActionEx(thr)).toCouldntPerformActionEx();
        }
    }
    
	private class EventProcessingCallbackImpl implements AcsEventPublisher.EventProcessingHandler<IDLEntity> 
	{
		@Override
		public void eventDropped(IDLEntity event) {
			m_logger.log(Level.WARNING, "CALLBACK: Event dropped, trying to send again");
			try {
				m_supplier.publishEvent(event);
			} catch (AcsJException e) {
				e.printStackTrace();
			}
		}

		@Override
		public void eventSent(IDLEntity event) {
			m_logger.log(Level.INFO, "CALLBACK: Event sent successfully");
			
		}

		@Override
		public void eventStoredInQueue(IDLEntity event) {
			m_logger.log(Level.INFO, "CALLBACK: Notify Service probably is down. Storing the event");
			
		}
	}
}
