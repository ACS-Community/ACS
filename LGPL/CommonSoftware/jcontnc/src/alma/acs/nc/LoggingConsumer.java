/*
 * ALMA - Atacama Large Millimiter Array (c) UNSPECIFIED - FILL IN, 2005
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
 */
package alma.acs.nc;

/**
 * @author dfugate
 * @version $Id: LoggingConsumer.java,v 1.5 2007/11/05 20:20:56 hsommer Exp $
 * @since
 */

import java.lang.reflect.Method;

import org.omg.CosNotification.StructuredEvent;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;

/**
 * LoggingConsumer is a a Consumer-derived class designed solely for the purpose
 * of processing notification channel structured events sent automatically by
 * the logging system. Basically all one has to do to use this class is create a
 * LoggingConsumer object providing an object which implements "receive(String
 * xml)" and then invoke the consumerReady() method. Since logging events do not
 * contain complex IDL structs, filtering using the extended trader constraint
 * language should work as well.
 * 
 * @author dfugate
 */
public class LoggingConsumer extends Consumer {

   /**
    * Creates a new instance of LoggingConsumer
    * 
    * @param services
    *           This is used to access ACS logging system.
    * @param receiver
    *           An object which implements a method called "receive". The
    *           "receive" method must accept one parameter, a string, which is
    *           an XML representing the log.
    * @throws AcsJException
    *            Thrown on any <I>really bad</I> error conditions encountered.
    */
   public LoggingConsumer(ContainerServicesBase services, Object receiver)
         throws AcsJException {
      // call the super.
      super(alma.acscommon.LOGGING_CHANNEL_NAME.value, services);

      // check to ensure receiver is capable to processing the event
      Class receiverClass = receiver.getClass();
      Method receiveMethod = null;
      // receive will have four parameters - all strings
      Class[] parm = { String.class };
      // if this fails we know that the developer has not defined "receive"
      // correctly at not at all. we can do nothing more
      try {
         receiveMethod = receiverClass.getMethod("receive", parm);
      }
      catch (NoSuchMethodException err) {
         // Well the method doesn't exist...that sucks!
         String reason = "The '"
               + m_channelName
               + "' channel: the receiver object is incapable of handling events!";
         throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
      }
      catch (SecurityException err) {
         // Developer has defined the method to be protected or private...this
         // doesn't work either.
         String reason = "The '"
               + m_channelName
               + "' channel: the receiver method of the object is protected/private for logging events!";
         throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
      }

      // save the receive method for later use.
      receiveMethod_m = receiveMethod;

      // save the receiver for later use.
      receiver_m = receiver;
   }

   /**
    * Overridden
    * 
    * @return string
    */
   protected String getChannelKind() {
      // because logging channels are registered differently
      // int the CORBA naming service than ICD-style channels
      return alma.acscommon.LOGGING_CHANNEL_KIND.value;
   }

   /**
    * Overridden.
    * 
    * @return string
    */
   protected String getNotificationFactoryName() {
      return alma.acscommon.LOGGING_NOTIFICATION_FACTORY_NAME.value;
   }

   /**
    * Overridden
    */
   protected void configSubscriptions() {
      // calling addsubscription on null automatically subscribes
      // to all event types.
      try {
         addSubscription(null);
      }
      catch (Exception e) {
         String msg = "Failed to subscribe to logging events: ";
         msg = msg + e.getMessage();
         m_logger.severe(msg);
      }

      return;
   }

   /**
    * Overridden.
    */
   public void push_structured_event(StructuredEvent structuredEvent) {
      try {
         String xml = structuredEvent.remainder_of_body.extract_string();

         // organize it into an argument list to be sent to the receive method.
         // order is critical here
         Object[] arg = { xml };

         // try sending it to the receiver.
         receiveMethod_m.invoke(receiver_m, arg);
      }
      catch (java.lang.IllegalAccessException e) {
         // should never happen...
         String msg = "Failed to process an event on the '" + m_channelName
               + "' channel because: ";
         msg = msg + e.getMessage();
         m_logger.warning(msg);
      }
      catch (java.lang.reflect.InvocationTargetException e) {
         // should never happen...
         String msg = "Failed to process an event on the '" + m_channelName
               + "' channel because: ";
         msg = msg + e.getMessage();
         m_logger.warning(msg);
      }
   }

   /**
    * There is exactly one receive method that will be invoked per
    * LoggingConsumer object.
    */
   private Method receiveMethod_m = null;

   /**
    * There is exactly one receiver that will be used by each LoggingConsumer
    * object.
    */
   private Object receiver_m      = null;
}
