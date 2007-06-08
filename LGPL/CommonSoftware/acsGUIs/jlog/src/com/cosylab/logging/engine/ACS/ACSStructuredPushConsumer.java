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

import java.util.Date;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.omg.CORBA.Any;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.ProxySupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplierHelper;
import org.omg.CosNotifyComm.StructuredPushConsumerPOA;

import alma.ACSLoggingLog.LogBinaryRecord;
import alma.ACSLoggingLog.LogBinaryRecordHelper;
import alma.ACSLoggingLog.NameValue;

import com.cosylab.logging.engine.log.LogTypeHelper;

import com.cosylab.logging.client.cache.CacheUtils;
import com.cosylab.logging.client.cache.LogBufferedFileCache;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.settings.ErrorLogDialog;
import com.cosylab.logging.settings.LogTypeRenderer;

/**
 * ACSStructuredPushConsumer gets logs from the NC 
 * and stores them in a list called receivedLogs.
 * 
 * It supports binary and XML formats.
 */
public final class ACSStructuredPushConsumer extends StructuredPushConsumerPOA
{
	private class Dispatcher extends Thread
	{
		// Signal the thread to terminate
		private volatile boolean terminateThread=false;
		/**
		 * Constructor 
		 *
		 */
		public Dispatcher() {
			super("Dispatcher");
		}
		
		/**
		 * Close the threads and free all the resources
		 * @param sync If it is true wait the termination of the threads before returning
		 */
		public void close(boolean sync) {
			terminateThread=true;
			if (sync) {
				while (isAlive()) {
					try {
						Thread.sleep(250);
					} catch (InterruptedException ie) {
						continue;
					}
				}
			}
		}
		
		/**
		 * The thread that takes log from the vector and publish the log to the
		 * listeners
		 */
		public void run()
		{
			String log = null;
			ILogEntry logEntry = null;
			while (!terminateThread) {
				try {
					log = xmlLogs.poll(250,TimeUnit.MILLISECONDS);
				} catch (InterruptedException ie) {
					System.out.println("Exception while taking a log out of the queue: "+ie.getMessage());
					ie.printStackTrace();
					continue;
				}
                if (log==null) {
                    // No logs received before the timeout elapsed
                	continue;
                }
                listenersDispatcher.publishRawLog(log);
				try {
					logEntry = parser.parse(log);
				} catch (Exception e) {
					StringBuilder strB = new StringBuilder("\nException occurred while dispatching the XML log.\n");
					strB.append("This log has been lost: "+log);
					ErrorLogDialog.getErrorLogDlg(true).appendText(strB.toString());
					listenersDispatcher.publishReport(strB.toString());
					System.err.println("Exception in ACSStructuredPushConsumer$Dispatcher::run(): " + e.getMessage());
					System.err.println("An XML string that could not be parsed: " + log);
					e.printStackTrace(System.err);
					continue;
				}
				listenersDispatcher.publishLog(logEntry);
			}
		}
	}
	
	protected StructuredProxyPushSupplier structuredProxyPushSupplier = null;
	protected boolean isConnected = false;
	protected boolean isEventSetup = false;
	protected boolean isInitialized = false;

	private ACSLogParser parser = null;
	private ACSRemoteAccess acsra = null;
	
	// true if the binary format is in use, falso otherwise
	private boolean binaryFormat;
	
	/**
	 * true if the consumer is discarding logs because is not able
	 * to follow the flow of the incoming messages
	 */
	private boolean discarding=false;
	
	/**
	 * If it is supended then the incoming messages are discarded
	 * instead of being notified to the listeners
	 */
	private boolean suspended=false;
	
	/// The queue where the logs read from the NC are pushed
	// The dispatcher pos up and injects the logs in the GUI
	private LinkedBlockingQueue<String> xmlLogs = new LinkedBlockingQueue<String>(2048);
	
	private Dispatcher dispatcher = new Dispatcher();
	
	// The object to dispatch messages to the listeners
	private ACSListenersDispatcher listenersDispatcher = null;
	
	// The log retrieval object
	private ACSLogRetrieval logRetrieval;
	
	// This boolean signal that the object has been closed:
	// all the logs received while closed will be discarded
	private volatile boolean closed=false;
	
	/**
	 * StructuredPushConsumer constructor comment.
	 * 
	 * @param acsra The remote access obj to ACS NC
	 * @param theEngine The LCEngine
	 */
	public ACSStructuredPushConsumer(ACSRemoteAccess acsra, ACSListenersDispatcher listenersDispatcher, boolean binaryFormat)
	{
		if (acsra==null || listenersDispatcher==null) {
			throw new IllegalArgumentException("Illegal null argument");
		}
		this.binaryFormat=binaryFormat;
		this.acsra = acsra;
		this.listenersDispatcher=listenersDispatcher;
		dispatcher.setPriority(Thread.MAX_PRIORITY);
		logRetrieval = new ACSLogRetrieval(listenersDispatcher,this.binaryFormat);
		initialize();
	}

	public LinkedBlockingQueue<String> getXmlLogs()
	{
		LinkedBlockingQueue<String> XmlLogs = xmlLogs;
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
			listenersDispatcher.publishReport("Exception occurred when connecting to structured push consumer.");
			System.out.println("Exception in ACSStructuredPushConsumer::connect(): " + e);
			return;
		}
		isConnected = true;
	}
	
	public void destroy()
	{
		structuredProxyPushSupplier.disconnect_structured_push_supplier();
		close(false);
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
		if (!binaryFormat) {
			try
			{
				parser = new ACSLogParserDOM();
			}
			catch (javax.xml.parsers.ParserConfigurationException pce)
			{
				listenersDispatcher.publishReport("Exception occurred when initializing the XML parser.");
				System.out.println("Exception in ACSStructuredPushConsumer::initialize(): " + pce);
				return;
			}
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
			String xmlLog = event.remainder_of_body.extract_string();
			logRetrieval.addLog(xmlLog);
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
	 * Pause/unpause the thread that publishes logs
	 * The logs received in pause mode are cached and will be
	 * published wneh the application will be unpaused
	 * 
	 * @param pause
	 */
	public void setPaused(boolean pause) {
		logRetrieval.pause(pause);
	}
	
	/**
	 * Close the threads and free all the resources
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		closed=true;
		if (dispatcher!=null) {
			dispatcher.close(sync);
		}
		if (logRetrieval!=null) {
			logRetrieval.close(sync);
		}
	}
	
	
}

