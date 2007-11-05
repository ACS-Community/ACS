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
 * ExecutiveConsumer.java
 *
 * Created on 12 March 2004, 14:37
 */

package alma.acs.nc;

import java.lang.reflect.Method;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;

/** This is a prototype consumer designed to consume all types of events on a
 * given event channel and print a minimal amount of data to standard out.  Whether
 * it is used or not in the GUI is still up for debate.
 * @author dfugate
 */
public class ExecutiveConsumer extends alma.acs.nc.Consumer
{
   
   /** Creates a new instance of ExecutiveConsumer
    * @param channelName Name of the channel to consume events from.
    * @param services ContainerServices which provide access to the ACS logger among other things.
    * @throws AcsJException Thrown from superclass.
    */
   public ExecutiveConsumer(String channelName, ContainerServicesBase services) throws AcsJException
   {
      super(channelName, services);
      addSubscription(null);
   }
   /** CORBA method invoked each time an event is received.
    * @param structuredEvent CORBA structured event sent by a supplier.
    */   
   public void push_structured_event(org.omg.CosNotification.StructuredEvent structuredEvent)
   {
      try
      {
         String localHelperName = structuredEvent.filterable_data[0].value.type() + "Helper";
         localHelperName = localHelperName.replaceAll("::", ".");
         
         //IDL struct helper class
         Class localHelper = Class.forName(localHelperName);
         
         //extract method of helper class
         Method extract = localHelper.getMethod("extract", new Class[]
         {org.omg.CORBA.Any.class});
         Object[] args =
         {structuredEvent.filterable_data[0].value};
         
         //time to get the event description
         EventDescription eDescrip = EventDescriptionHelper.extract(structuredEvent.remainder_of_body);
         
         processEvent(extract.invoke(null, args), eDescrip);
      }
      catch(java.lang.ClassNotFoundException e)
      {
         //should never happen...
         String msg = "Failed to process an event on the '" + m_channelName + "' channel because the helper class does not exist: ";
         msg = msg + e.getMessage();
	 m_logger.warning(msg);
      }
      catch(java.lang.NoSuchMethodException e)
      {
         //should never happen...
         String msg = "Failed to process an event on the '" + m_channelName + "' channel because the helper class does not provide the 'extract' method: ";
         msg = msg + e.getMessage();
	 m_logger.warning(msg);
      }
      catch(java.lang.IllegalAccessException e)
      {
         //should never happen...
         String msg = "Failed to process an event on the '" + m_channelName + "' channel because: ";
         msg = msg + e.getMessage();
	 m_logger.warning(msg);
      }
      catch(java.lang.reflect.InvocationTargetException e)
      {
         //should never happen...
         String msg = "Failed to process an event on the '" + m_channelName + "' channel because: ";
         msg = msg + e.getMessage();
	 m_logger.warning(msg);
      }
   }
   
   /** Invoked each time an event is received. To be overriden.
    * @param corbaObject The ICD event extracted from a CORBA Any.
    * @param eventDescrip Description of the ICD event as defined in acsnc.idl
    */   
   protected void processEvent(java.lang.Object corbaObject, alma.acsnc.EventDescription eventDescrip)
   {
      String msg = "Channel ='" + m_channelName + "'; ";
      msg = msg +  "event type ='" + corbaObject.getClass().getName() + "'; ";
      msg = msg +  "publisher name ='" + eventDescrip.name + "'; ";
      msg = msg +  "timestamp ='" + eventDescrip.timestamp + "'; ";
      msg = msg +  "event number ='" + eventDescrip.count + "'; ";
      
      m_logger.info(msg);
   }
}
