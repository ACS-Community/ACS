/*
 * ALMA - Atacama Large Millimiter Array (c) National Research Council of
 * Canada, 2005
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

import alma.acs.container.ContainerServices;
import alma.acs.exceptions.AcsJException;

import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotification.Property;

import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;
/**
 * Used to supply (BACI property) events to the archiving notification channel.
 * 
 * @author dfugate
 * @version $Id: ArchiveSupplier.java,v 1.7 2006/11/22 09:46:27 cparedes Exp $
 */
public class ArchiveSupplier extends SimpleSupplier {
   /**
    * Creates a new instance of ArchiveSupplier
    * 
    * @param services
    *           This is used to get the name of the component and to access the
    *           ACS logging system.
    * @throws AcsJException
    *            There are literally dozens of CORBA exceptions that could be
    *            thrown by the ArchiveSupplier class. Instead, these are
    *            converted into an ACS Error System exception for the
    *            developer's convenience.
    */
   public ArchiveSupplier(ContainerServices services) throws AcsJException {
      super(alma.acscommon.ARCHIVING_CHANNEL_NAME.value, services);
   }

   /**
    * Overridden.
    * 
    * @return string
    */
   protected String getChannelKind() {
      return alma.acscommon.ARCHIVING_CHANNEL_KIND.value;
   }

   /**
    * Overridden.
    * 
    * @return string
    */
   protected String getNotificationFactoryName() {
      return alma.acscommon.ARCHIVE_NOTIFICATION_FACTORY_NAME.value;
   }

   /**
    * Overridden.
    * 
    * @return string
    */
   protected String getChannelDomain() {
      return alma.acscommon.ARCHIVING_DOMAIN.value;
   }

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
   public void publishEvent(java.lang.Object customStruct)throws AcsJException {

    //the eventName consists of container named concatenated with the
    //component and property names, delimited by ':'s.
      String typeName = customStruct.getClass().getName().substring(
            customStruct.getClass().getName().lastIndexOf('.') + 1);
      String containerName = getHelper().getContainerServices().getName();
      String param = "no_param" ;
      String device = "no_device" ;
      String eventName = containerName + ":" + device + ":" + param;

      // event to send
      StructuredEvent event = getCORBAEvent(typeName, eventName);

      // Store the info for Exec/I&T into the event.
      // create the any
      event.remainder_of_body = m_services.getAdvancedContainerServices()
            .getAny();
      // get the useful data which includes the component's name, timestamp,
      // and event count
      EventDescription descrip = new EventDescription(containerName,
	     alma.acs.util.UTCUtility
            .utcJavaToOmg(System.currentTimeMillis()),1 );
      // store the IDL struct into the structured event
      EventDescriptionHelper.insert(event.remainder_of_body, descrip);
      Long time_stamp = new Long(alma.acs.util.UTCUtility.utcJavaToOmg(System.currentTimeMillis()));      

      event.filterable_data = new Property[2];
      event.filterable_data[0] = new Property(
            "time_stamp",m_anyAide.objectToCorbaAny(time_stamp)); 
	     
      event.filterable_data[1] = new Property(
            "value", m_anyAide.objectToCorbaAny(customStruct));
      publishCORBAEvent(event);
   }
}
