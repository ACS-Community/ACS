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
package alma.demo.test.EventSupplierCDBChannel;

import java.util.logging.Logger;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.FRIDGE.FridgeControlPackage.NestedFridgeEvent;
import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.nc.SimpleSupplier;
import alma.demo.SupplierCompOperations;

import alma.acsnc.EventDescription;

/** Class designed for testing event suppliers.
 * @author dfugate
 */
public class EventSupplierCDBChannel extends ComponentImplBase implements SupplierCompOperations
{
   private SimpleSupplier m_supplier = null;
   
   /** Sends some events to an event channel.
    * @param param number of events to send
    */
   public void sendEvents(short param)
   {
      System.out.println("Now sending simplesupplier events...");
      try
      {
         //first send out some number of events.
         EventDescription t_block = new EventDescription("no name", 32L, 64L);
         for(short i=0; i<param; i++)
         {
            m_supplier.publishEvent(t_block);
         }
                  
	 //fake a subscription change
	 m_supplier.subscription_change(new org.omg.CosNotification.EventType[]
	     {}, new org.omg.CosNotification.EventType[]
	     {});
         
      }
      catch(Exception e)
      {
         System.err.println(e);
      }
   }
   
   /** Disconnects the supplier. */
   public void cleanUp()
   {
      m_logger.info("cleanUp() called...");
      
      try
      {
      
      //fake a consumer disconnecting...
      m_supplier.disconnect_structured_push_supplier();
      m_supplier.disconnect();
      }
      catch(Exception e)
      {
       e.printStackTrace();  
      }
   }
   
   
   /** Sets up the SimpleSupplier.
    * @param containerServices Services to components.
    * @throws ComponentLifecycleException Not thrown.
    */
   public void initialize(ContainerServices containerServices)
   throws ComponentLifecycleException
   {
      super.initialize(containerServices);
      m_logger.info("initialize() called...");
      
      try
      {
         //Instantiate our supplier
         //THIS IS THE SOLE LINE DIFFERENT FROM EventSupplierImpl. THIS IS 
         //EXACTLY THE SAME IMPLEMENTATION EXCEPT THAT THIS SUPPLIER CREATES
         //A CHANNEL DEFINED IN THE CDB
         m_supplier = new SimpleSupplier("cdb_channel",  //the channel's name
         m_containerServices);
      }
      catch(Exception e)
      {
         e.printStackTrace(System.err);
      }
   }
   
   public void sendEventsSpecial(NestedFridgeEvent[] eventData) throws CouldntPerformActionEx {
	   throw new org.omg.CORBA.NO_IMPLEMENT("Method not implemented yet");
   }
      
}

