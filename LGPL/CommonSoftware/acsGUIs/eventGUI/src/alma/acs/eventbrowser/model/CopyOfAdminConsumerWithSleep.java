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

import org.omg.CORBA.Any;
import org.omg.CORBA.TypeCode;
import org.omg.CORBA.TypeCodePackage.BadKind;
import org.omg.CORBA.TypeCodePackage.Bounds;
import org.omg.CosEventComm.Disconnected;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotifyComm.InvalidEventType;

import alma.acs.container.ContainerServices;
import alma.acs.container.ContainerServicesBase;
import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.views.EventData;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.Consumer;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;

public class CopyOfAdminConsumerWithSleep extends Consumer {
	
	private static final EventType et = new EventType("*","*");
	private static final EventType[] eta = {et};
	private static final EventType[] etNone = {};
	private static long totalEventCount = 0;
	private int channelEventCount;
	private HashMap<String, Integer> evtCounter;
	private static boolean printDetails;

	public CopyOfAdminConsumerWithSleep(String channelName, ContainerServicesBase services)
			throws AcsJException {
		super(channelName, services);
		try {
			evtCounter = new HashMap<String, Integer>();
			m_consumerAdmin.subscription_change(eta, etNone);
		} catch (InvalidEventType e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public CopyOfAdminConsumerWithSleep(String arg0, String arg1, ContainerServicesBase arg2)
			throws AcsJException {
		super(arg0, arg1, arg2);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected String getNotificationFactoryName() {
		// TODO Auto-generated method stub
		return super.getNotificationFactoryName();
	}

	@Override
	public void push_structured_event(StructuredEvent evt) throws Disconnected {
//		super.push_structured_event(evt);
		channelEventCount++;
//		String evtName = evt.header.fixed_header.event_name; // Normally empty, as ACSEventAdmin.py states?
		String evtTypeName = evt.header.fixed_header.event_type.type_name;
		if (evtCounter.containsKey(evtTypeName))
			evtCounter.put(evtTypeName, evtCounter.get(evtTypeName)+1);
		else
			evtCounter.put(evtTypeName, 1);

//		String domainName = evt.header.fixed_header.event_type.domain_name; // Always ALMA?
//		Any data = evt.filterable_data[0].value;
		EventDescription eDescrip = EventDescriptionHelper.extract(evt.remainder_of_body);
//		long timeStamp = eDescrip.timestamp;
//		String component = eDescrip.name;
//		long count = eDescrip.count;
		Any eventAny = evt.filterable_data[0].value;
		boolean oresult = Application.equeue.offer(new EventData(eDescrip.timestamp,eDescrip.name,eDescrip.count,evt.header.fixed_header.event_type.type_name,evtCounter.get(evtTypeName),m_channelName, eventAny));
		// TEST CODE FOR SLOW RECEIVER
		try {
			Thread.sleep(5000);
		}
		catch (InterruptedException e) {
			
		}
		if (!oresult)
			m_logger.severe("Couldn't queue event # "+channelEventCount);
		if (++totalEventCount % 100 == 0) { // Maybe this is redundant with the "Total rows processed" log
			m_logger.info("A total of "+totalEventCount+" events have been received.");
			m_logger.info("Event queue size is now: "
					+ Application.equeue.size() + " elements.");
		}
			//		m_logger.fine("Time "+eDescrip.timestamp+" "+m_channelName+" "+eDescrip.name+" "+eDescrip.count+" "+channelEventCount+" "
			//				+" "+evtTypeName+" "+evtCounter.get(evtTypeName));
			// Uncomment following line only for stress testing.
			//try { Thread.sleep(5000); } catch(InterruptedException e) {}// EVIL!!!!!!!!!!

	}

	public static boolean getPrintDetails() {
		return printDetails;
	}

	public static void setPrintDetails(boolean printDetails) {
		CopyOfAdminConsumerWithSleep.printDetails = printDetails;
	}

}
