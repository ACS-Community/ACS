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

import org.omg.CosNotification.Property;
import org.omg.CosNotification.StructuredEvent;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.util.UTCUtility;
import alma.acscommon.ACS_NC_DOMAIN_ARCHIVING;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;

/**
 * Used to supply (BACI property) events to the archiving notification channel.
 * 
 * @author dfugate
 * @version $Id: ArchiveSupplier.java,v 1.12 2012/07/06 15:08:29 hsommer Exp $
 */
public class ArchiveSupplier extends NCPublisher<Object> {
	
// TODO: ideally we delegate to an AcsEventPublisher created by ContainerServices, 
//       but for now we must override getNotificationFactoryName and related methods
//       and thus continue to subclass (now NCPublisher instead of SimpleSupplier).
//	private final AcsEventPublisher pub; 
	
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
	public ArchiveSupplier(ContainerServicesBase services) throws AcsJException {
		super(alma.acscommon.ARCHIVING_CHANNEL_NAME.value, ACS_NC_DOMAIN_ARCHIVING.value, services, Helper.getNamingServiceInitial(services));
//		pub = services.createNotificationChannelPublisher(alma.acscommon.ARCHIVING_CHANNEL_NAME.value, IDLEntity.class);
	}

	/**
	 * Takes an object and tries to pack it into a CORBA Any and
	 * publish it to the event channel. 
	 * <p>
	 * The parameter <code>obj</code> can be one of the JDK classes <code>Integer</code>, <code>Long</code>, 
	 * <code>Float</code>, <code>Double</code>, <code>String</code>. 
	 * It could also be <code>null</code>, or come from an IDL-defined struct (thus implementing <code>IDLEntity</code>);
	 * at least the latter should never occur since baci properties don't have complex values. 
	 * 
	 * @param value
	 *           Value to be published.
	 * @throws AcsJException
	 *            There are an enormous amount of possibilities pertaining to why
	 *            a subclass of AcsJException would be thrown by publishEvent.
	 */
	public void publishEvent(Object value) throws AcsJException {

		//the eventName consists of container named concatenated with the
		//component and property names, delimited by ':'s.
		String typeName = value.getClass().getName().substring(value.getClass().getName().lastIndexOf('.') + 1);
		String containerName = services.getName();
		String param = "no_param";
		String device = "no_device"; // @TODO use real component/device name
		String eventName = containerName + ":" + device + ":" + param;

		// event to send
		StructuredEvent event = getCORBAEvent(typeName, eventName);
		event.remainder_of_body = services.getAdvancedContainerServices().getAny();

		// get the useful data which includes the component's name, timestamp, and event count
		long utcTime = UTCUtility.utcJavaToOmg(System.currentTimeMillis());
		EventDescription descrip = new EventDescription(containerName, utcTime, 1);

		// store all data into the structured event
		EventDescriptionHelper.insert(event.remainder_of_body, descrip);
		event.filterable_data = new Property[2];
		event.filterable_data[0] = new Property("time_stamp", anyAide.objectToCorbaAny(new Long(utcTime)));
		event.filterable_data[1] = new Property("value", anyAide.objectToCorbaAny(value));

		publishCORBAEvent(event, value);
	}
}
