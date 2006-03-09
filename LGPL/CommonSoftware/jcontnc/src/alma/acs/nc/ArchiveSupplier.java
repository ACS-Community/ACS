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

/**
 * Used to supply (BACI property) events to the archiving notification channel.
 * 
 * @author dfugate
 * @version $Id: ArchiveSupplier.java,v 1.4 2006/03/09 21:52:10 dfugate Exp $
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
   public void publishEvent(java.lang.Object customStruct) throws AcsJException {

      // This is the name of the "event type".
      String typeName = customStruct.getClass().getName().substring(
            customStruct.getClass().getName().lastIndexOf('.') + 1);

      // event to send
      StructuredEvent event = getCORBAEvent(typeName, "");

      // preallocate one name/value pair
      event.filterable_data = new Property[1];
      event.filterable_data[0] = new Property(
            alma.acscommon.DEFAULTDATANAME.value, m_anyAide
                  .objectToCorbaAny(customStruct));

      publishCORBAEvent(event);
   }
}
