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

import org.omg.CORBA.Any;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.ProxySupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplierHelper;
import org.omg.CosNotifyComm.StructuredPushConsumerPOA;

import alma.ACSLoggingLog.LogBinaryRecord;
import alma.ACSLoggingLog.LogBinaryRecordHelper;
import alma.Logging.XmlLogRecord;
import alma.Logging.XmlLogRecordSeqHelper;
import alma.Logging.XmlLogRecordSeqHolder;

import com.cosylab.logging.engine.FiltersVector;

/**
 * ACSStructuredPushConsumer gets logs from the NC 
 * and stores them in a list called receivedLogs.
 * 
 * It supports binary and XML formats.
 */
public final class ACSStructuredPushConsumer extends StructuredPushConsumerPOA
{

	
	protected StructuredProxyPushSupplier structuredProxyPushSupplier = null;
	protected boolean isConnected = false;
	protected boolean isEventSetup = false;
	protected boolean isInitialized = false;

	private ACSRemoteAccess acsra = null;
	
	// true if the binary format is in use, falso otherwise
	private boolean binaryFormat;
	
	/**
	 * If it is suspended then the incoming messages are discarded
	 * instead of being notified to the listeners
	 */
	private boolean suspended=false;
	
	// The object to dispatch messages to the listeners
	private ACSListenersDispatcher listenersDispatcher = null;
	
	// This boolean signal that the object has been closed:
	// all the logs received while closed will be discarded
	private volatile boolean closed=false;
	
	// The object to send new logs to
	private ACSLogRetrieval logRetrieval;
	
	/**
	 * StructuredPushConsumer constructor comment.
	 * 
	 * @param acsra The remote access obj to ACS NC
	 * @param theEngine The LCEngine
	 */
	public ACSStructuredPushConsumer(ACSRemoteAccess acsra, ACSListenersDispatcher listenersDispatcher, ACSLogRetrieval logRetrieval, boolean binaryFormat)
	{
		if (acsra==null || listenersDispatcher==null) {
			throw new IllegalArgumentException("Illegal null argument");
		}
		if (logRetrieval==null) {
			throw new IllegalArgumentException("The ACSLogRetrieval can't be null");
		}
		this.binaryFormat=binaryFormat;
		this.acsra = acsra;
		this.listenersDispatcher=listenersDispatcher;
		this.logRetrieval = logRetrieval;
		initialize();
	}

	/**
	 * Connects the push supplier to the push consumer.
	 */
	public void connect()
	{
		try
		{
			structuredProxyPushSupplier.connect_structured_push_consumer(this._this(acsra.getORB()));
		}
		catch (Exception e)
		{
			listenersDispatcher.publishReport("Exception occurred when connecting to structured push consumer.");
			System.out.println("Exception in ACSStructuredPushConsumer::connect(): " + e);
			return;
		}
		isConnected = true;
	}
	
	public void destroy()
	{
		try {
		    teardownEvents();
			structuredProxyPushSupplier.disconnect_structured_push_supplier();
			acsra.getConsumerAdmin().destroy();
			
		} catch (Throwable t) {
		    System.out.println("Exception in ACSStructuredPushConsumer::destroy(): " + t);
		    t.printStackTrace();
		}
	}			

	public void disconnect_structured_push_consumer()
	{
		System.out.println(">>>disconnect_structured_push_consumer called.");
	}
	
	/**
	 * Initializes the parser.
	 * Creation date: (10/24/2001 12:48:32 PM)
	 */
	private void initialize()
	{
		org.omg.CORBA.IntHolder proxyId = new org.omg.CORBA.IntHolder();

		ProxySupplier proxySupplier = null;
		try
		{
			proxySupplier =
				acsra.getConsumerAdmin().obtain_notification_push_supplier(ClientType.STRUCTURED_EVENT, proxyId);
		}
		catch (Exception e)
		{
			listenersDispatcher.publishReport("Exception occurred when obtaining notification push supplier.");
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
		if (suspended || closed) {
			return;
		}
		if (binaryFormat) {
			Any any = event.remainder_of_body;
			LogBinaryRecord logRecord = LogBinaryRecordHelper.extract(any);
			String binStr = CacheUtils.toCacheString(logRecord);
			logRetrieval.addLog(binStr);
		} else {
			try{
				String xmlLog = event.remainder_of_body.extract_string();
				logRetrieval.addLog(xmlLog);
			}catch(org.omg.CORBA.BAD_OPERATION ex){
				XmlLogRecord[] xmlLogs = XmlLogRecordSeqHelper.extract(event.remainder_of_body);
				for(int i = 0; i < xmlLogs.length ; i++)
					logRetrieval.addLog(xmlLogs[i].xml);
			}
		}
	}

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
			listenersDispatcher.publishReport("Exception occurred when changing subscription on Consumer Admin.");
			System.out.println("Exception in ACSStructuredPushConsumer::setupEvents(): " + e);
			return;
		}
		isEventSetup = true;
	}

	/**
	 * Remove subscription on ConsumerAdmin.
	 */
	public void teardownEvents()
	{
		org.omg.CosNotification.EventType[] added = new org.omg.CosNotification.EventType[0];
		org.omg.CosNotification.EventType[] removed = new org.omg.CosNotification.EventType[1];

		removed[0] = new org.omg.CosNotification.EventType();
		removed[0].domain_name = "*";
		removed[0].type_name = "*";
		try
		{
			acsra.getConsumerAdmin().subscription_change(added, removed);
		}
		catch (Exception e)
		{
			listenersDispatcher.publishReport("Exception occurred when changing subscription on Consumer Admin.");
			System.out.println("Exception in ACSStructuredPushConsumer::setupEvents(): " + e);
			return;
		}
		isEventSetup = true;
	}

	/**
	 * Check if the consumer is connected by reconnecting the channel 
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
		// No exceptions so the pusher reconnected
		isConnected=true;
		return true;
	}
	
	/**
	 * 
	 * @return true if the consumer is suspened
	 */
	public boolean isSuspended() {
		return suspended;
	}
	
	/**
	 * Suspend the notification of the incoming logs
	 * The logs received while suspended discarded
	 * (i.e. lost forever)
	 * 
	 * @see LCEngine
	 * @param suspend If true suspend the notification of new logs
	 */
	public void setSupended(boolean suspended) {
		this.suspended=suspended;
		if (suspended) {
			listenersDispatcher.publishSuspended();
		} else {
			listenersDispatcher.publishConnected(true);
		}
	}
	
	/**
	 * Close the threads and free all the resources
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		closed=true;
	}
	

}

