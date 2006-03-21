/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
 */
package com.cosylab.logging.engine.ACS;

import java.util.AbstractList;
import java.util.ArrayList;

import org.omg.CosNotification.Property;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.ProxySupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplierHelper;



import com.cosylab.logging.engine.log.LogEntryXML;

/**
 * ACSStructuredPushConsumer gets an XML log from the Engine 
 * and stores it in a list called XmlLogs.
 * Creation date: (10/24/2001 12:27:34 PM)
 * @author: 
 */
public final class ACSStructuredPushConsumer extends org.omg.CosNotifyComm.StructuredPushConsumerPOA
{
	private class Dispatcher extends Thread
	{
		public void run()
		{
			String log = null;
			LogEntryXML logEntry = null;
			while (true) {
				try {
					if (!xmlLogs.isEmpty()) {
						log = (String) xmlLogs.remove(0);
						logEntry = parser.parse(log);
						acsra.getEngine().pushStructuredEvent(logEntry);
						//System.out.println("An XML string that was parsed: " + log);
					} else
						synchronized (xmlLogs)
						{
							xmlLogs.wait();
						}
				}
				catch (InterruptedException e) {
					System.err.println("InterruptedException occurred.");
				}
				catch (Exception e) {
					acsra.getEngine().reportStatus("Exception occurred while dispatching the XML log.");
					System.err.println("Exception in ACSStructuredPushConsumer$Dispatcher::run(): " + e);
					System.err.println("An XML string that could not be parsed: " + log);
				}
			}
		}
	}
	
	protected StructuredProxyPushSupplier structuredProxyPushSupplier = null;
	protected boolean isConnected = false;
	protected boolean isEventSetup = false;
	protected boolean isInitialized = false;

	private ACSLogParser parser = null;
	private ACSRemoteAccess acsra = null;
	private AbstractList xmlLogs = new ArrayList();
	private Dispatcher dispatcher = new Dispatcher();
	/**
	 * StructuredPushConsumer constructor comment.
	 */
	public ACSStructuredPushConsumer(ACSRemoteAccess acsra)
	{
		this.acsra = acsra;
		initialize();
	}

	public AbstractList getXmlLogs()
	{
		AbstractList XmlLogs = xmlLogs;
		XmlLogs.add(
			"<Info TimeStamp=\"2001-11-07T09:24:11.096\" Routine=\"msaci::ContainerImpl::init\" Host=\"disna\" Process=\"maciManager\" Thread=\"main\" Context=\"\"><Data Name=\"StupidData\">All your base are belong to us.</Data>Connected to the Centralized Logger.</Info>");
		XmlLogs.add(
			"<Info TimeStamp=\"2001-11-07T09:24:11.096\" Routine=\"msaci::ContainerImpl::init\" Host=\"disna\" Process=\"maciManager\" Thread=\"main\" Context=\"\"><Data Name=\"StupidData\">All your base are belong to us.</Data>Connected to the Centralized Logger.</Info>");
		return XmlLogs;
	}
	/**
	 * Connects the push supplier to the push consumer.
	 */
	public void connect()
	{
		dispatcher.start();
		try
		{
			structuredProxyPushSupplier.connect_structured_push_consumer(this._this(acsra.getORB()));
		}
		catch (Exception e)
		{
			acsra.getEngine().reportStatus("Exception occurred when connecting to structured push consumer.");
			System.out.println("Exception in ACSStructuredPushConsumer::connect(): " + e);
			return;
		}
		isConnected = true;
	}
	
	public void destroy()
	{
		structuredProxyPushSupplier.disconnect_structured_push_supplier();
	}

	// disconnect_structured_push_consumer method comment.

	public void disconnect_structured_push_consumer()
	{
		//	System.out.println(">>>disconnect_structured_push_consumer called.");
		/*	try {
				byte[] oid = acsra.getPOA().servant_to_id(this);
				acsra.getPOA().deactivate_object(oid);
				//acsra.getORB().shutdown(false);
			} catch (Exception e) {
				acsra.getEngine().reportStatus("Exception occurred when disconnecting from structured push consumer.");
				System.out.println("Exception in disconnect_structured_push_consumer(): " + e);
			}*/
	}
	/**
	 * Initializes the parser.
	 * Creation date: (10/24/2001 12:48:32 PM)
	 */
	private void initialize()
	{
		try
		{
			parser = new ACSLogParser();
		}
		catch (javax.xml.parsers.ParserConfigurationException pce)
		{
			acsra.getEngine().reportStatus("Exception occurred when initializing the XML parser.");
			System.out.println("Exception in ACSStructuredPushConsumer::initialize(): " + pce);
			return;
		}
		org.omg.CORBA.IntHolder proxyId = new org.omg.CORBA.IntHolder();

		ProxySupplier proxySupplier = null;
		try
		{
			proxySupplier =
				acsra.getConsumerAdmin().obtain_notification_push_supplier(ClientType.STRUCTURED_EVENT, proxyId);
		}
		catch (Exception e)
		{
			acsra.getEngine().reportStatus("Exception occurred when obtaining notification push supplier.");
			System.out.println("Exception in ACSStructuredPushConsumer::initialize(): " + e);
			return;
		}

		structuredProxyPushSupplier = StructuredProxyPushSupplierHelper.narrow(proxySupplier);
		isInitialized = true;
	}
	public boolean isInitialized()
	{
		return isInitialized;
	}
	/**
	 * Used for testing purposes only. 
	 * Creation date: (10/25/2001 12:31:21 PM)
	 * @param args java.lang.String[]
	 */
	public static void main(String[] args)
	{
		/*	ACSRemoteAccess sub = null;
			
		//	sub = new ACSRemoteAccess();
		//	sub.resolveNamingService();
		//	sub.resolveNotifyChannel(Subscribe.LOGGING_CHANNEL);
		//	sub.createConsumerAdmin();
		
			ACSStructuredPushConsumer acsSPS = new ACSStructuredPushConsumer(sub);
			acsSPS.connect(); 
			acsSPS.setupEvents(); */
	}
	// offer_change method comment.

	public void offer_change(org.omg.CosNotification.EventType[] added, org.omg.CosNotification.EventType[] removed)
		throws org.omg.CosNotifyComm.InvalidEventType
	{
		// not implemented...
	}
	/**
	 * Adds all the logs to a list in a synchronized manner.
	 */
	public void push_structured_event(StructuredEvent event) throws org.omg.CosEventComm.Disconnected
	{
		//System.out.println("Pushed...: "  + event.remainder_of_body.toString());

		/* extract event data */
		// For logging, only the following is used
		// check whether eventName is defined at all.
		String domainName = event.header.fixed_header.event_type.domain_name;
		String typeName = event.header.fixed_header.event_type.type_name;
		String eventName = event.header.fixed_header.event_name;
		/////////////////////////////////////////////////////////
		Property[] variableHeaders = event.header.variable_header;
		Property[] filterableFields = event.filterable_data;
		/////////////////////////////////////////////////////////
		String xmlLog = event.remainder_of_body.extract_string();

		//System.out.println("***********Log**********\n" + xmlLog);
		//System.out.println("************************");

		//	System.out.println("Log added.");
		synchronized (xmlLogs)
		{
			xmlLogs.add(xmlLog);
			xmlLogs.notifyAll();
		}
	}

	/*// This is how it should have been done...
		// This syntax will be useful for arhiving
	
		// Event Header
		//   Fixed Header
		String domainName = event.header.fixed_header.event_type.domain_name;
		String typeName = event.header.fixed_header.event_type.type_name;
		String eventName = event.header.fixed_header.event_name;
		//   Variable Header
		Property[] variableHeaders = event.header.variable_header;
	
		// Event Body
		//   Filterable Fields
		Property[] filterableFields = event.filterable_data;
		//   Remaining Body, returned as CORBA Any. Converted to String. Is this OK?
		String remainingBody = event.remainder_of_body.extract_string();
	
		// Property has
		// String property.name
		// CORBA::Any property.value
	
	*/
	/**
	 * Changes subscription on ConsumerAdmin.
	 */
	public void setupEvents()
	{
		org.omg.CosNotification.EventType[] added = new org.omg.CosNotification.EventType[1];
		org.omg.CosNotification.EventType[] removed = new org.omg.CosNotification.EventType[0];

		added[0] = new org.omg.CosNotification.EventType();
		added[0].domain_name = "*";
		added[0].type_name = "*";
		try
		{
			acsra.getConsumerAdmin().subscription_change(added, removed);
		}
		catch (Exception e)
		{
			acsra.getEngine().reportStatus("Exception occurred when changing subscription on Consumer Admin.");
			System.out.println("Exception in ACSStructuredPushConsumer::setupEvents(): " + e);
			return;
		}
		isEventSetup = true;
	}

	/**
	 * Check if the consumer is connecetd by reconnecting the channel 
	 * 
	 * @return true if the consumer is connected
	 */
	public boolean isConnected() {
		if (structuredProxyPushSupplier==null) {
			isConnected=false;
			return false;
		}
		try {
			structuredProxyPushSupplier.resume_connection();
		} catch ( org.omg.CosNotifyChannelAdmin.ConnectionAlreadyActive caa) {
			isConnected=true;
			return true;
		} catch (Exception e) {
			isConnected=false;
			return false;
		}
		// No exceptions so the pusher reconneted
		isConnected=true;
		return true;
	}
}

