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
package alma.COUNTER.CounterConsumerImpl;

import java.util.logging.Logger;

import alma.ACS.ComponentStates;
import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.COUNTER.CounterConsumerOperations;
import alma.COUNTER.OnOffStates;
import alma.COUNTER.statusBlockEvent;
import alma.acs.component.ComponentLifecycle;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.nc.Consumer;
import alma.acs.util.StopWatch;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

/** 
 * CounterConsumer is a simple class that connects to the "counter"
 * notification channel, receives events, and then disconnects from the channel.
 * @author eallaert
 */

public class CounterConsumerImpl implements ComponentLifecycle, CounterConsumerOperations
{
	public static final String PROP_ASSERTION_MESSAGE = "CounterConsumerAssert"; 

	private ContainerServices m_containerServices;
	private Logger m_logger;
	private volatile Consumer m_consumer = null;
   /** 
     * Total number of events that have been consumed.
     */    
	private volatile int eventCount = 0;
    volatile boolean contFlag = true;

	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle
	/////////////////////////////////////////////////////////////
	@Override
    public void initialize(ContainerServices containerServices) throws ComponentLifecycleException
    {
    	m_containerServices = containerServices;
    	m_logger = m_containerServices.getLogger();
    	m_logger.info("initialize() called...");
	}

	@Override
	public void execute() {
		m_logger.info("execute() called...");
	}

	@Override
	public void cleanUp() throws AcsJComponentCleanUpEx {
		if (m_consumer != null) {
			m_logger.info("cleanUp() called, disconnecting from channel " + alma.COUNTER.CHANNELNAME_COUNTER.value);
			StopWatch sw = new StopWatch();
			try {
				m_consumer.disconnect();
				m_consumer = null;
			} catch (Exception ex) {
				// could be IllegalStateException if the consumer is already disconnected.
				throw new AcsJComponentCleanUpEx(ex);
			}
			long disconnectTimeMillis = sw.getLapTimeMillis();
			if (disconnectTimeMillis > 600) {
				m_logger.info("Suspiciously slow NC disconnect in " + disconnectTimeMillis + " ms.");
			}
		}
		else {
			m_logger.info("cleanUp() called..., nothing to clean up.");
		}
	}

	@Override
	public void aboutToAbort() {
		try {
			cleanUp();
		} catch (AcsJComponentCleanUpEx ex) {
			ex.printStackTrace();
		}
	//	m_logger.info("managed to abort...");
		System.out.println("CounterConsumer component managed to abort... you should know this even if the logger did not flush correctly!");
	}
	
	
    ////////////////////////////////////////////////////////////////////////////
    /** 
     * NC receiver method.
     */
    public void receive(statusBlockEvent someParam) {
    	// Know how many events this instance has received.
    	eventCount++;
    	if (contFlag) {
    		OnOffStates onOff        = someParam.onOff;
    		//float onOff        = someParam.onOff;
    		String myString  = someParam.myString;
    		int counter1     = someParam.counter1;
    		int counter2     = someParam.counter2;
    		int counter3     = someParam.counter3;
    		boolean lastFlag = someParam.flipFlop;
    		float period     = someParam.period;
        	//if (!lastFlag) {
    		if (onOff == OnOffStates.ON && !lastFlag) {
    			//m_logger.info("Counter now " + counter1 + " (max " + counter2 + "), flag  will flip at " + counter3);
    			//System.out.println("Counter now " + counter1 + " (max " + counter2 + "), flag  will flip at " + counter3);
    		} 
    		else {	
    			m_logger.info(myString + " received, counter is now " + counter1);
    			System.out.println(myString + " received, counter is now " + counter1);
    			// allow waitTillDone() to return so that this component can be released by the client.
    			contFlag = false;
    		}	
    	} 
    }

	/////////////////////////////////////////////////////////////
	// Implementation of ACSComponent
	/////////////////////////////////////////////////////////////
	
	@Override
	public ComponentStates componentState() {
		return m_containerServices.getComponentStateManager().getCurrentState();
	}
	@Override
	public String name() {
		return m_containerServices.getName();
	}
	
	/////////////////////////////////////////////////////////////
	// Implementation of CounterConsumerOperations
	/////////////////////////////////////////////////////////////
	/**
	 * @throws CouldntPerformActionEx 
	 * @see alma.COUNTER.CounterConsumerOperations#getBlocks()
	 */
	@Override
	public void getBlocks() throws CouldntPerformActionEx {
    	try
    	{
        	m_consumer = new Consumer(alma.COUNTER.CHANNELNAME_COUNTER.value, m_containerServices);
        	//Subscribe to an event type.
        	m_consumer.addSubscription(statusBlockEvent.class, this);

        	//After consumerReady() is invoked, receive(...) is invoked
        	//by the notification channel.  That is, we have no control over when
        	//that method is called.
        	m_consumer.consumerReady();

    		m_logger.info("CounterConsumer is ready to receive 'status' events.");
     	}
    	catch (Exception e)
    	{
    		if (m_consumer != null) {
    			m_consumer.disconnect();
    		}
			AcsJCouldntPerformActionEx ex = new AcsJCouldntPerformActionEx();
			ex.setProperty(PROP_ASSERTION_MESSAGE, "failed to connect as an event consumer to channel " + alma.COUNTER.CHANNELNAME_COUNTER.value);
			throw ex.toCouldntPerformActionEx();
    	}
    	return;
	}


	/**
	 * @throws CouldntPerformActionEx 
	 * @see alma.COUNTER.CounterConsumerOperations#waitTillDone()
	 */
	@Override
	public int waitTillDone() throws CouldntPerformActionEx {
		if (m_consumer == null) {
			AcsJCouldntPerformActionEx ex = new AcsJCouldntPerformActionEx();
			ex.setProperty(PROP_ASSERTION_MESSAGE, "Consumer didn't even start yet");
			throw ex.toCouldntPerformActionEx();
		}
		while (contFlag) {
			try {
	    		m_logger.info("CounterConsumer received " + eventCount + " blocks so far ... will sleep 1000 ms more.");
				Thread.sleep(1000);
			}
	    	catch (Exception e) {
	    	}
    	}	
    	return eventCount;
	}

   
    ////////////////////////////////////////////////////////////////////////////
}
