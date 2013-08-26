/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.nc;

import java.util.logging.Logger;

import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNotification.EventHeader;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.FixedEventHeader;
import org.omg.CosNotification.Property;
import org.omg.CosNotification.StructuredEvent;

import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;

public class StructuredEventCreator {
	private ContainerServicesBase m_services;
	
	private AnyAide m_anyAide;
	private Logger m_logger;

	public StructuredEventCreator(ContainerServicesBase services) {
		m_services = services;
		m_logger = services.getLogger();

		m_anyAide = new AnyAide(services);
	}
	
	public StructuredEvent createEvent(IDLEntity customStruct) throws AcsJException {
		// The Java class name without package becomes the name of the "event type".
		String typeName = customStruct.getClass().getName().substring(
				customStruct.getClass().getName().lastIndexOf('.') + 1);

		// event to send
		StructuredEvent event = getCORBAEvent(typeName, "");

		// Store the info for Exec/I&T into the event.
		// create the any
		event.remainder_of_body = m_services.getAdvancedContainerServices().getAny();
		// get the useful data which includes the component's name, timestamp, and event count (=1 here)
		EventDescription descrip = new EventDescription(m_services.getName(),
				alma.acs.util.UTCUtility.utcJavaToOmg(System.currentTimeMillis()), 1);
		// store the IDL struct into the structured event
		EventDescriptionHelper.insert(event.remainder_of_body, descrip);

		// preallocate one name/value pair

		event.filterable_data = new Property[1];
		event.filterable_data[0] = new Property(
				alma.acscommon.DEFAULTDATANAME.value, 
				m_anyAide.complexObjectToCorbaAny(customStruct) );
		return event;
	}
	
	/**
	 * Method used to create a pre-filled CORBA event.
	 * 
	 * @param typeName
	 *           The structured event's type_name.
	 * @param eventName
	 *           Name of the event.
	 * @return A pre-filled CORBA event.
	 */
	private StructuredEvent getCORBAEvent(String typeName, String eventName) {
		// return value
		StructuredEvent event = new StructuredEvent();

		// event.header.fixed_header.event_type
		EventType event_type = new EventType(getChannelDomain(), typeName);

		//
		FixedEventHeader fixed_header = new FixedEventHeader(event_type, eventName);

		// event.header.variable_header
		Property[] variable_header = new Property[0];

		// event.header
		event.header = new EventHeader(fixed_header, variable_header);

		return event;
	}
	
	/**
	 * This method returns a constant character pointer to the notification
	 * channel domain which is normally equivalent to acscommon::ALMADOMAIN. The
	 * sole reason this method is provided is to accomodate subclasses which
	 * subscribe/publish non-ICD style events (ACS archiving channel for
	 * example).In that case, the developer would override this method.
	 * 
	 * @return string
	 */
	private String getChannelDomain() {
		return alma.acscommon.ALMADOMAIN.value;
	}
	
	
}