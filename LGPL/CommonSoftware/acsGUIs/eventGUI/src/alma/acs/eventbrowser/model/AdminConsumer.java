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
package alma.acs.eventbrowser.model;

import java.util.HashMap;
import java.util.concurrent.atomic.AtomicLong;

import org.omg.CORBA.Any;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNotification.StructuredEvent;

import alma.acs.container.ContainerServicesBase;
import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.views.EventData;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.NCSubscriber;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;

/**
 * 
 */
public class AdminConsumer extends NCSubscriber<IDLEntity> {
	
	private static long totalEventCount = 0;
	private final AtomicLong channelEventCount = new AtomicLong(0);
	
	/**
	 * key = type name, value = counter for events of the given type.
	 */
	private final HashMap<String, AtomicLong> evtCounter;
	
	private static boolean printDetails;

	public AdminConsumer(String channelName, ContainerServicesBase services, NamingContext namingService)
			throws AcsJException {
		super(channelName, null, services, namingService, "EventGuiNcConsumer", IDLEntity.class);
		
		addGenericSubscription(new GenericCallback() {
			@Override
			public void receiveGeneric(Object event, EventDescription eventDescrip) {
				logger.warning("Unexpected call to 'receiveGeneric', which should not happen with our non-standard use of NCSubscriber.");
			}
		});
		
		evtCounter = new HashMap<String, AtomicLong>();
	}


	@Override
	public boolean push_structured_event_called(StructuredEvent evt) {
		channelEventCount.incrementAndGet();
//		String evtName = evt.header.fixed_header.event_name; // Normally empty, as ACSEventAdmin.py states?
		String evtTypeName = evt.header.fixed_header.event_type.type_name;
		
		AtomicLong typeCounter = evtCounter.get(evtTypeName);
		if (typeCounter == null) {
			synchronized (evtCounter) {
				typeCounter = evtCounter.get(evtTypeName);
				if (typeCounter == null) {
					typeCounter = new AtomicLong(0);
					evtCounter.put(evtTypeName, typeCounter);
				}
			} 
		}
		typeCounter.incrementAndGet();

//		String domainName = evt.header.fixed_header.event_type.domain_name; // Always ALMA?
//		Any data = evt.filterable_data[0].value;
		EventDescription eDescrip = EventDescriptionHelper.extract(evt.remainder_of_body);
//		long timeStamp = eDescrip.timestamp;
//		String component = eDescrip.name;
//		long count = eDescrip.count;
		Any eventAny = evt.filterable_data[0].value;
		boolean oresult = Application.equeue.offer(
				new EventData(eDescrip.timestamp, eDescrip.name, eDescrip.count, evt.header.fixed_header.event_type.type_name, typeCounter.get(), channelName, eventAny) );
		// TEST CODE FOR SLOW RECEIVER
//		try {
//			Thread.sleep(5000);
//		}
//		catch (InterruptedException e) {
//			
//		}
		if (!oresult)
			logger.severe("Couldn't queue event # "+channelEventCount.get());
		if (++totalEventCount % 100 == 0) { // Maybe this is redundant with the "Total rows processed" log
			logger.fine("A total of "+totalEventCount+" events have been received.");
			logger.fine("Event queue size is now: "
					+ Application.equeue.size() + " elements.");
		}
			//		m_logger.fine("Time "+eDescrip.timestamp+" "+m_channelName+" "+eDescrip.name+" "+eDescrip.count+" "+channelEventCount+" "
			//				+" "+evtTypeName+" "+evtCounter.get(evtTypeName));
			// Uncomment following line only for stress testing.
			//try { Thread.sleep(5000); } catch(InterruptedException e) {}// EVIL!!!!!!!!!!
		
		// Prevent further processing of this event by the subscriber framework
		return false;
	}

	public static boolean getPrintDetails() {
		return printDetails;
	}

	public static void setPrintDetails(boolean printDetails) {
		AdminConsumer.printDetails = printDetails;
	}
	
	public static long getTotalEventCount() {
		return totalEventCount;
	}

}
