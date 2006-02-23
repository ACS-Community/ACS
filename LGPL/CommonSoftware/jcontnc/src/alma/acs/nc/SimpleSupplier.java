/*
 * ALMA - Atacama Large Millimiter Array (c) Associated Universities Inc., 2002
 * (c) European Southern Observatory, 2002 Copyright by ESO (in the framework of
 * the ALMA collaboration), All rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * SimpleSupplier.java
 * 
 * Created on April 9, 2003, 1:17 PM
 */
// //////////////////////////////////////////////////////////////////////////////
package alma.acs.nc;

import java.util.logging.Logger;
import java.util.logging.Level;

import org.omg.CORBA.IntHolder;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotification.EventHeader;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.FixedEventHeader;
import org.omg.CosNotification.Property;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.EventChannel;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushConsumer;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushConsumerHelper;
import org.omg.CosNotifyChannelAdmin.SupplierAdmin;
import org.omg.CosNotifyComm.InvalidEventType;

import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;

import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;
import alma.acsnc.OSPushSupplier;
import alma.acsnc.OSPushSupplierHelper;
import alma.acsnc.OSPushSupplierPOA;

// //////////////////////////////////////////////////////////////////////////////
/**
 * SimpleSupplier is the standard class to be used with the event channel API to
 * publish events using the Java programming language. It supports publishing
 * events where the data is really a user-defined IDL struct.
 * 
 * @author dfugate 
 */
public class SimpleSupplier extends OSPushSupplierPOA implements CommonNC
{
   /**
    * Creates a new instance of SimpleSupplier
    * 
    * @param cName
    *           name of the notification channel events will be published to.
    * @param services
    *           This is used to get the name of the component and to access the
    *           ACS logging system.
    * @throws AcsJException
    *            There are literally dozens of CORBA exceptions that could be
    *            thrown by the SimpleSupplier class. Instead, these are
    *            converted into an ACS Error System exception for the
    *            developer's convenience.
    */
   public SimpleSupplier(String cName, ContainerServices services)
         throws AcsJException
   {
      m_channelName = cName;

      m_logger = services.getLogger();

      m_services = services;

      m_anyAide = new AnyAide(services);

      // sanity check
      if (m_channelName == null)
      {
         String reason = "Null reference obtained for the channel name!";
         throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
      }

      // naming service, POA, and Any generator
      m_helper = new Helper(services);

      // get the Supplier admin object
      m_supplierAdmin = getNotificationChannel(cName).new_for_suppliers(
            m_ifgop, new IntHolder());
      // sanity check
      if (m_supplierAdmin == null)
      {
         String reason = "Null reference obtained for the Supplier Admin!";
         throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
      }

      // get the Consumer proxy
      try
      {
         org.omg.CORBA.Object tempCorbaObj = m_supplierAdmin
               .obtain_notification_push_consumer(ClientType.STRUCTURED_EVENT,
                     new IntHolder());
         if (tempCorbaObj == null)
         {
            String reason = "Null reference obtained for the Proxy Push Consumer!";
            throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
         }
         m_proxyConsumer = StructuredProxyPushConsumerHelper
               .narrow(tempCorbaObj);
      }
      catch (org.omg.CosNotifyChannelAdmin.AdminLimitExceeded e)
      {
         // convert it into an exception developers care about
         throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e
               .getMessage());
      }

      // Need Java container to provide access to it's rootPOA and/or ORB
      // to get around this.
      try
	  {
	  alma.ACS.OffShoot myOS = getHelper().getContainerServices().activateOffShoot(this);
	  
	  // must connect the to the proxy consumer or events would never be
	  // sent anywhere.
	  m_corbaRef = OSPushSupplierHelper.narrow(myOS);
	  
	  org.omg.CosNotifyComm.StructuredPushSupplier mySps = org.omg.CosNotifyComm.StructuredPushSupplierHelper.narrow(m_corbaRef);
	  m_proxyConsumer.connect_structured_push_supplier(mySps);
	  }
      catch (alma.acs.container.ContainerException e)
      {
         // convert it to an ACS Error System Exception
         throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e);
      }
      catch (org.omg.CosEventChannelAdmin.AlreadyConnected e)
      {
         // Think there is virtually no chance of this every happening but...
         throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e
               .getMessage());
      }
      
      if (m_helper.getChannelProperties().getIntegrationLogs(m_channelName)==true)
      {
         m_integrationLogs = true;
      }
   }

   // //////////////////////////////////////////////////////////////////////////
   /**
    * User code <b>call this method when the Supplier is no longer useful</b>.
    * Failure to do so can result in remote memory leaks. User should not call
    * this method multiple times either. Once disconnect has been called, all of
    * SimpleSupplier's methods will cease to function properly.
    */
   public void disconnect()
   {
      try
      {
         // handle notification channel cleanup
         m_proxyConsumer.disconnect_structured_push_consumer();
         m_supplierAdmin.destroy();

         // clean-up CORBA stuff
         getHelper().getContainerServices().deactivateOffShoot(this);
         m_corbaRef = null;
         m_proxyConsumer = null;
         m_supplierAdmin = null;
      }
      catch (Exception e)
      {
         e.printStackTrace();
      }
   }

   // //////////////////////////////////////////////////////////////////////////
   /**
    * This method gets a reference to the event channel. If it is not already
    * registered with the naming service, it is created.
    * 
    * @return Reference to the event channel specified by channelName.
    * @param channelName
    *           Name of the event channel registered with the CORBA Naming
    *           Service
    * @throws AcsJException
    *            Standard ACS Java exception.
    */
   protected EventChannel getNotificationChannel(String channelName)
         throws AcsJException
   {
      if (m_channel == null)
      {
         m_channel = getHelper().getNotificationChannel(channelName,
							getChannelKind(),
							getNotificationFactoryName());
      }
      return m_channel;

   }

   protected void destroyNotificationChannel() throws AcsJException
   {
      getHelper().destroyNotificationChannel(m_channelName, getChannelKind(),
            m_channel);
   }

   // //////////////////////////////////////////////////////////////////////////
   /**
    * This method returns a constant character pointer to the "kind" of
    * notification channel as registered with the naming service (i.e., the kind
    * field of a CosNaming::Name) which is normally equivalent to
    * acscommon::NC_KIND. The sole reason this method is provided is to
    * accomodate subclasses which subscribe/publish non-ICD style events (ACS
    * archiving channel for example).In that case, the developer would override
    * this method.
    * 
    * @return string
    */
   protected String getChannelKind()
   {
      return alma.acscommon.NC_KIND.value;
   }

   // //////////////////////////////////////////////////////////////////////////
   /**
    * This method returns a constant character pointer to the notification
    * channel domain which is normally equivalent to acscommon::ALMADOMAIN. The
    * sole reason this method is provided is to accomodate subclasses which
    * subscribe/publish non-ICD style events (ACS archiving channel for
    * example).In that case, the developer would override this method.
    * 
    * @return string
    */
   protected String getChannelDomain()
   {
      return alma.acscommon.ALMADOMAIN.value;
   }

    /**
    * This method returns a the notify service name as registered with the
    * CORBA Naming Service. This is normally equivalent to acscommon::ALMADOMAIN. The
    * sole reason this method is provided is to accomodate subclasses which
    * subscribe/publish non-ICD style events (ACS archiving channel for
    * example).In that case, the developer would override this method.
    * 
    * @return string
    */
   protected String getNotificationFactoryName()
   {
      return alma.acscommon.NOTIFICATION_FACTORY_NAME.value;
   }

   // //////////////////////////////////////////////////////////////////////////
   /**
    * Gets a reference to the event channel specified during construction.
    * 
    * @return Reference to the event channel.
    * @throws AcsJException
    *            Standard ACS Java exception.
    */
   protected EventChannel getNotificationChannel() throws AcsJException
   {
      if (m_channel == null)
      {
         String reason = "Null reference obtained for the Notification Channel!";
         throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
      }
      return m_channel;
   }

   // //////////////////////////////////////////////////////////////////////////
   /**
    * Override this method so a "smart" Supplier subclass can publish (or not
    * pubish) events based on Consumer demands. Not very useful when there are
    * more than one Supplier instances for a given channel.
    * 
    * This method becomes extremely useful if we could assume there is only one
    * supplier per channel. That is, the API could intelligently publish events
    * to a given domain/type only when there are consumers subscribed. However,
    * there are problems when there are multiple supplier instances for a
    * channel.
    * 
    * <b>Do not call it from your code!</b>
    * 
    * @param eventType
    *           Added subscription array.
    * @param eventType1
    *           Removed subscription array.
    * @throws InvalidEventType
    *            Throw this exception when a consumer subscribes (or
    *            unsubscribes) to a domain/type that does not exist.
    */
   public void subscription_change(EventType[] eventType, EventType[] eventType1)
         throws InvalidEventType
   {
      // This seems to have confused developers in the past so now just silently
      // return.
      return;
   }

   // //////////////////////////////////////////////////////////////////////////
   /**
    * Override this method to do something when a consumer unsubscribes from the
    * channel. <b>Do not call it from your code!</b>
    */
   public void disconnect_structured_push_supplier()
   {
      String msg = "A Consumer has disconnected from the '" + m_channelName
            + "' channel";
      m_logger.info(msg);
   }

    /**
     * Method which publishes an entire CORBA StructuredEvent without making
     * any modifications to it.
     * @param se A complete structured event
     * @throws AcsJException if the event cannot be published for some reason
     * or another.
     */
    protected void publishCORBAEvent(StructuredEvent se)
	throws AcsJException
	{
	    try
		{
		m_proxyConsumer.push_structured_event(se);
		}
	    catch (org.omg.CosEventComm.Disconnected e)
		{
		String reason = "Eevent for the   '"
		    + m_channelName + "' channel cannot be published: ";
		throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(reason
									    + e.getMessage());
		}
	}

    /**
    * Method used to return a pre-filled CORBA event. 
    * @param typeName The structured event's type_name.
    * @param eventName Name of the event.
    * @return A pre-filled CORBA event.
     *
     */
    protected StructuredEvent getCORBAEvent(String typeName,
					    String eventName)
	{
	    //return value
	    StructuredEvent event = new StructuredEvent();
	    
	    // event.header.fixed_header.event_type
	    EventType event_type = new EventType(getChannelDomain(), typeName);
	    
	    //
	    FixedEventHeader fixed_header = new FixedEventHeader(event_type,
								 eventName);

	    // event.header.variable_header
	    Property[] variable_header = new Property[0];
	    
	    // event.header
	    event.header = new EventHeader(fixed_header, variable_header);

	    return event;
	}     


    // //////////////////////////////////////////////////////////////////////////
    /**
     * Takes a generic Java object and tries to pack it into a CORBA Any and
     * publish it to the event channel. This will fail if the parameter is not
     * CORBA-generated from a user-defined IDL struct. In simple terms, trying to
     * publish native Java types is impossible because they have no CORBA mapping
     * to say Python or C++ types.
     * 
     * @param customStruct
     *           An instance of the IDL struct (Java class) to be published.
     * @throws AcsJException
     *            There are an enormous amount of possibilities pertaining to why
     *            an AcsJException would be thrown by publishEvent.
     */
    public void publishEvent(java.lang.Object customStruct) throws AcsJException
	{
	    // This is the name of the "event type".
	    String typeName  = customStruct.getClass().getName().substring(
		customStruct.getClass().getName().lastIndexOf('.') + 1);
	    
	    // event to send
	    StructuredEvent event = getCORBAEvent(typeName, "");
	    
	    
	    // Store the info for Exec/I&T into the event.
	    // create the any
	    event.remainder_of_body = m_services.getAdvancedContainerServices()
		.getAny();
	    // get the useful data which includes the component's name, timestamp,
	    // and event count
	    EventDescription descrip = new EventDescription(getHelper()
							    .getContainerServices().getName(),
							    alma.acs.util.UTCUtility
							    .utcJavaToOmg(System.currentTimeMillis()), m_count);
	    // store the IDL struct into the structured event
	    EventDescriptionHelper.insert(event.remainder_of_body, descrip);
	    
	    // preallocate one name/value pair
	    event.filterable_data = new Property[1];
	    event.filterable_data[0] = new Property(alma.acscommon.DEFAULTDATANAME.value, 
						    m_anyAide.complexObjectToCorbaAny(customStruct));
	    
       if(m_integrationLogs==true)
       {
          m_logger.log(Level.INFO, "Channel:" + m_channelName + ", Event Type:" + typeName);
       }
       
	    publishCORBAEvent(event);
	    m_count++;
	}
    
    // //////////////////////////////////////////////////////////////////////////
    /**
    * Returns a reference to the Helper instance. Not too useful outside this
    * class.
    * 
    * @return Returns a reference to the Helper instance.
    */
   public alma.acs.nc.Helper getHelper()
   {
      return m_helper;
   }

   // //////////////////////////////////////////////////////////////////////////
   /**
    * Provides access to the naming service and a POA to activate this
    * Supplier's stub. Supplier subclasses can use it to create additional
    * Any's.
    */
   protected Helper                      m_helper        = null;

   /** The event channel has exactly one name registered in the naming service. */
   protected String                      m_channelName   = null;

   /**
    * Supplier Admin object is responsible for creating & managing proxy
    * consumers.
    */
   protected SupplierAdmin               m_supplierAdmin = null;

   /**
    * The proxy consumer object used by supplier to push events onto the
    * channel.
    */
   protected StructuredProxyPushConsumer m_proxyConsumer = null;

   /**
    * The total number of successful events published by this particular
    * supplier.
    */
   protected long                        m_count         = 0;

   /** CORBA reference to ourself */
   protected OSPushSupplier              m_corbaRef      = null;

   protected EventChannel                m_channel       = null;

   protected Logger                      m_logger        = null;

   protected ContainerServices           m_services      = null;

    /** Helper class used to manipulate CORBA anys */
   protected AnyAide m_anyAide = null;
   
   private boolean m_integrationLogs = false;

   // //////////////////////////////////////////////////////////////////////////
}
