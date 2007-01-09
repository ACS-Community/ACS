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

package alma.demo.test.EventILConsumerImpl;

import java.util.logging.Logger;

import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.demo.ConsumerCompOperations;
import alma.acs.nc.Consumer;


import alma.acsnc.EventDescription;
/**
 *
 * @author  dfugate
 */
public class EventConsumerImpl extends ComponentImplBase implements ConsumerCompOperations
{
   private Consumer m_consumer = null;
   private long m_count = 0;
   
   /**
    * Sets up the {@link Consumer}.
    * <p>
    * {@inheritDoc}
    */
   public void initialize(ContainerServices containerServices)
   throws ComponentLifecycleException
   {
      super.initialize(containerServices);
      m_logger.info("initialize() called...");
      
      
      try
      {
         //subscribe to real channel and name
         m_consumer = new Consumer("blarIL", m_containerServices);
         m_consumer.addSubscription(EventDescription.class, this);
         
         //NOW FOR SOME TESTS!!!
         //try to add a subscription to the same type
	 try{
         m_consumer.addSubscription(EventDescription.class, this);
	 }
	 catch(Exception e)
	     {
	     //good, expected this.
	     }

	 //add a subscription to something that exists but we will never actually receive
	 try
	     {
	     m_consumer.addSubscription(alma.acstime.Epoch.class, this);
	     }
	 catch(Exception e)
	     {
	     e.printStackTrace();
	     System.out.println("Bad...cannot subscribe to multiple events types.");
	     }

         //add a subscription to something that exists but there is no receive method implemented for.
         try
         {
            m_consumer.addSubscription(org.omg.CosNotification.StructuredEvent.class, this);
            System.out.println("If you'return reading this message, addSubscription is broken!");
         }
         catch(Exception e)
         {
            System.out.println("Good...cannot subscribe to events where the receive method has not been implemented");
         }
         //try to add a bad filter
         try
         {
	 if(m_consumer.addFilter(EventDescription.class, "hope to god this filter doesn't work") != -1)
	     {
	     System.out.println("If you're reading this message, stupid API allows subscribing to bad filters!");
	     }
         }
         catch( alma.acs.exceptions.AcsJException e)
         {
            System.out.println("Good...cannot add a bad filter: " + e.getMessage());
         }
         //test the helper
         if(m_consumer.getHelper()==null)
         {
            System.out.println("Damn helper was null!");
         }
         //test offer_change
         m_consumer.offer_change(new org.omg.CosNotification.EventType[]
         {}, new org.omg.CosNotification.EventType[]
         {});
         //test disconnect_structured_push_consumer
         m_consumer.disconnect_structured_push_consumer();
         
         //start receiving events
         m_consumer.consumerReady();
         //test suspend
         m_consumer.suspend();
         //test resume
         m_consumer.resume();
         System.out.println("Waiting for events...consumer");
      }
      catch (Exception e)
      {
         e.printStackTrace();
      }
   }
   
   /**
    */
   public void receive(EventDescription joe)
   {
      //normal case
      if (m_count <5)
      {
         System.out.println("The component responsible for the supplier is:" + joe.name);
         try {
         Thread.sleep(3000);
         }
         catch(Exception e) {
            e.printStackTrace();
         }
      }
      else if(m_count == 5)
      {
         try
	     {
	     m_consumer.suspend();
	     //test remove subscription
	     m_consumer.removeSubscription(EventDescription.class);
	     m_consumer.resume();
	     }
	 catch(alma.acs.exceptions.AcsJException e)
	     {
	     e.printStackTrace();
	     }
      }
      m_count++;
   }

    /**
     * Method defined to ensure multiple event types can be subscribed too.
     */
    public void receive(alma.acstime.Epoch epch)
	{
	    System.out.println("Epoch event received...");
	}
   
   /**
    * Disconnects the Consumer
    * {@inheritDoc}
    */
   public void cleanUp()
   {
      m_logger.info("cleanUp() called...");
      m_consumer.disconnect();
   }
}

