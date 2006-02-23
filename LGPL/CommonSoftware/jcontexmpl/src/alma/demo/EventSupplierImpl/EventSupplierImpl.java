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

import alma.FRIDGE.TemperatureStatus;
import alma.FRIDGE.temperatureDataBlockEvent;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.SimpleSupplier;
import alma.demo.SupplierCompOperations;


/**
 * 
 * @author  dfugate
 */
public class EventSupplierImpl extends ComponentImplBase implements SupplierCompOperations
{
    private SimpleSupplier m_supplier;

    /**
     * Sets up the SimpleSupplier.
     */
    public void initialize(ContainerServices containerServices) throws ComponentLifecycleException
    {
    	super.initialize(containerServices);
    	
    	try {
    		// Instantiate our supplier
    		m_supplier = new SimpleSupplier(alma.FRIDGE.CHANNELNAME_FRIDGE.value,  //the channel's name 
    				m_containerServices);
    	}
    	catch (AcsJException e) {
    		throw new ComponentLifecycleException("failed to create SimpleSupplier for channel " + alma.FRIDGE.CHANNELNAME_FRIDGE.value, e);
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
    public void sendEvents(short param)
    {
    	m_logger.info("Now sending " + param + " simplesupplier events...");
        try {
            temperatureDataBlockEvent t_block = new temperatureDataBlockEvent(3.14F, TemperatureStatus.ATREF);
            for(short i=0; i<param; i++)
            {
                m_supplier.publishEvent(t_block);
            }
        }
        catch(Exception e) {
            m_logger.log(Level.WARNING, "failed to send temperatureDataBlockEvent. Will not try again.");
        }
    }

}
