/*
 *    ALMA - Atacama Large Millimiter Array
 *   (c) Associated Universities Inc., 2002 
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
 */
package alma.COUNTER.CounterSupplierImpl;

import java.util.logging.Logger;

import alma.ACS.ComponentStates;
import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.COUNTER.CHANNELNAME_COUNTER;
import alma.COUNTER.CounterSupplierOperations;
import alma.COUNTER.OnOffStates;
import alma.COUNTER.statusBlockEvent;
import alma.acs.component.ComponentLifecycle;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.nc.AcsEventPublisher;

/** 
 * CounterSupplier is a simple class that creates the "counter" notification channel,
 * generates events, and then disconnects from the channel.
 * @author eallaert
 */

public class CounterSupplierImpl implements ComponentLifecycle, CounterSupplierOperations
{
	public static final String PROP_ASSERTION_MESSAGE = "CounterSupplierAssert"; 

	private ContainerServices m_containerServices;
	private Logger m_logger;
	private AcsEventPublisher<statusBlockEvent> m_supplier;
	
   /** 
     * Total number of events that have been consumed.
     */    
    int eventCount = 0;
    volatile boolean flag = false;

	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle
	/////////////////////////////////////////////////////////////

    public void initialize(ContainerServices containerServices)
    {
    	m_containerServices = containerServices;
    	m_logger = m_containerServices.getLogger();
    	m_logger.info("initialize() called...");

    	try {
    		m_supplier = containerServices.createNotificationChannelPublisher(CHANNELNAME_COUNTER.value, statusBlockEvent.class);
            m_logger.info("CounterSupplier ready to send NC events...");
    	}
    	catch (Exception e) {
        	if (m_supplier != null) {
        		m_supplier.disconnect();
        	}
            m_logger.info("CounterSupplier failed to connect as an event supplier to channel " + CHANNELNAME_COUNTER.value);
        }

	}

	public void execute() throws ComponentLifecycleException {
		m_logger.info("execute() called...");
    	//try {
    	//	int i = sendBlocks (1, 20, 20, 0.5f);
    	//}
    	//catch (CouldntPerformActionEx e) {
		//	throw new ComponentLifecycleException("failed to connect as an event supplier to channel " + alma.COUNTER.CHANNELNAME_COUNTER.value);
    	//}
	}

	public void cleanUp() {
		if (m_supplier != null) {
			m_logger.info("cleanUp() called, disconnecting supplier from channel " + CHANNELNAME_COUNTER.value);
	    	m_supplier.disconnect();
		}
		else {
			m_logger.info("cleanUp() called..., nothing to clean up.");
		}
	}

	public void aboutToAbort() {
		cleanUp();
	//	m_logger.info("managed to abort...");
		System.out.println("CounterSupplier component managed to abort... you should know this even if the logger did not flush correctly!");
	}

	/////////////////////////////////////////////////////////////
	// Implementation of ACSComponent
	/////////////////////////////////////////////////////////////
	
	public ComponentStates componentState() {
		return m_containerServices.getComponentStateManager().getCurrentState();
	}
	public String name() {
		return m_containerServices.getName();
	}
	
	/////////////////////////////////////////////////////////////
	// Implementation of CounterSupplierOperations
	/////////////////////////////////////////////////////////////
	/**
	 * @throws CouldntPerformActionEx 
	 * @see alma.COUNTER.CounterConsumerOperations#getBlocks()
	 */
	public int sendBlocks(int initVal, int lastVal, int changeVal, float period) throws CouldntPerformActionEx {
    	try
    	{
    		String myString = "Java supplier";
            int periodMs = (int)(period * 1000.f);
            int val = initVal;

            while (val < lastVal) {
            	if (val < changeVal) {
                    flag = false;
            	}
                else {
                    flag = true;
                }
                m_supplier.publishEvent(new statusBlockEvent(OnOffStates.ON, myString, val, lastVal, changeVal, flag, period));
                //m_supplier.publishEvent(new statusBlockEvent(1.0f, val, lastVal, changeVal, flag, period));
                //m_logger.info("Counting ongoing with period " + period + "s up to " + lastVal + ", now " + val );
                val++;
                eventCount++;
                Thread.sleep(periodMs);
            }
            
           	// Tell consumers this is the last event. 
            // Note that this gets sent even if initVal > lastVal !!
            myString = "Last event from Java supplier";
            m_supplier.publishEvent(new statusBlockEvent(OnOffStates.OFF, myString, lastVal, lastVal, changeVal, true, period));
            //m_supplier.publishEvent(new statusBlockEvent(0.0f, lastVal, lastVal, changeVal, true, period));
            m_logger.info("Counter stopped, last value " + val);
            eventCount++;
            
    	}
    	catch (Exception e)
    	{
    		if (m_supplier != null) {				
    			m_supplier.disconnect();
    		}
			AcsJCouldntPerformActionEx ex = new AcsJCouldntPerformActionEx();
			ex.setProperty(PROP_ASSERTION_MESSAGE, "failed to connect as an event supplier to channel " + alma.COUNTER.CHANNELNAME_COUNTER.value);
			throw ex.toCouldntPerformActionEx();
    	}
    	return eventCount;
	}
    
    ////////////////////////////////////////////////////////////////////////////
}
