/*
 * ALMA - Atacama Large Millimiter Array (c) Associated Universities Inc., 2002
 * (c) European Southern Observatory, 2002 Copyright by ESO (in the framework of
 * the ALMA collaboration), All rights reserved
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
 * 
 * SimpleSupplier.java
 * 
 * Created on April 9, 2003, 1:17 PM
 */
// //////////////////////////////////////////////////////////////////////////////
package alma.acs.nc;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.IntHolder;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosEventChannelAdmin.AlreadyConnected;
import org.omg.CosNotification.EventHeader;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.FixedEventHeader;
import org.omg.CosNotification.Property;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotification.UnsupportedAdmin;
import org.omg.CosNotification.UnsupportedQoS;
import org.omg.CosNotifyChannelAdmin.AdminLimitExceeded;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushConsumer;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushConsumerHelper;
import org.omg.CosNotifyComm.InvalidEventType;
import org.omg.CosNotifyComm.StructuredPushSupplier;
import org.omg.CosNotifyComm.StructuredPushSupplierHelper;

import gov.sandia.NotifyMonitoringExt.EventChannel;
import gov.sandia.NotifyMonitoringExt.NameAlreadyUsed;
import gov.sandia.NotifyMonitoringExt.NameMapError;
import gov.sandia.NotifyMonitoringExt.SupplierAdmin;

import alma.ACSErrTypeCORBA.wrappers.AcsJCORBAReferenceNilEx;
import alma.ACSErrTypeCORBA.wrappers.AcsJNarrowFailedEx;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.AcsNCTraceLog.LOG_NC_ConsumerProxyCreation_FAIL;
import alma.AcsNCTraceLog.LOG_NC_ConsumerProxyCreation_OK;
import alma.AcsNCTraceLog.LOG_NC_TaoExtensionsSubtypeMissing;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServices;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;
import alma.acsnc.OSPushSupplierPOA;


// //////////////////////////////////////////////////////////////////////////////
/**
 * SimpleSupplier is the standard class to be used with the event channel API to
 * publish events using the Java programming language. It supports publishing
 * events where the data is really a user-defined IDL struct.
 * <p>
 * Design note on CORBA usage (generally not relevant to ACS NC users): 
 * the IDL-struct-data is wrapped by a corba Any, but then pushed on the notification channel inside a "Structured Event" 
 * (with the Any object in StructuredEvent#filterable_data[0]).
 * Don't confuse this with Corba's option of sending events directly as Anys. 
 * As of 2006-12, HSO is not sure why this complex design was chosen, instead of using structured events without the Any wrapping inside. 
 * Possibly it offers some flexibility for generic consumer tools written in languages that have no introspection. 
 * <p>
 * @TODO (HSO): figure out if the CORBA impl is thread safe. Fix this class accordingly, 
 * or document that it is not thread safe otherwise.
 * 
 * @author dfugate
 * 
 * @deprecated since ACS 10.1. Use {@link ContainerServices#createNotificationChannelPublisher(String, Class)} or 
 *             {@link ContainerServices#createNotificationChannelPublisher(String, String, Class)} instead.
 */
public class SimpleSupplier extends OSPushSupplierPOA implements ReconnectableSubscriber
{
	/**
	 * Creates a new instance of SimpleSupplier.
	 * Make sure you call {@link #disconnect()} when you no longer need this event supplier object.
	 * 
	 * @param cName
	 *           name of the notification channel events will be published to.
	 * @param services
	 *           This is used to get the name of the component and to access the
	 *           ACS logging system.
	 * @throws AcsJException
	 *            There are literally dozens of CORBA exceptions that could be
	 *            thrown by the SimpleSupplier class. Instead, these are
	 *            converted into an ACS Error System exception for the
	 *            developer's convenience.
	 */
	public SimpleSupplier(String cName, ContainerServicesBase services) throws AcsJException 
	{
		this(cName, null, services, null);
	}
	
	/**
	 * Creates a new instance of SimpleSupplier.
	 * Make sure you call {@link #disconnect()} when you no longer need this event supplier object.
	 * 
	 * @param cName
	 *           name of the notification channel events will be published to.
	 * @param services
	 *           This is used to get the name of the component and to access the
	 *           ACS logging system.
	 * @param evProcCallback
	 * 			Callback that handles the events' fate
	 * @throws AcsJException
	 *            There are literally dozens of CORBA exceptions that could be
	 *            thrown by the SimpleSupplier class. Instead, these are
	 *            converted into an ACS Error System exception for the
	 *            developer's convenience.
	 */
	public SimpleSupplier(String cName, ContainerServicesBase services, 
			EventProcessingCallback<? extends IDLEntity> evProcCallback) throws AcsJException 
	{
		this(cName, null, services, evProcCallback);
	}

	/**
	 * Creates a new instance of SimpleSupplier.
	 * Make sure you call {@link #disconnect()} when you no longer need this event supplier object.
	 * 
	 * @param cName
	 *           name of the notification channel events will be published to.
	 * @param channelNotifyServiceDomainName
	 *           Channel domain name, which is being used to determine notification service.
	 * @param services
	 *           This is used to get the name of the component and to access the
	 *           ACS logging system.
	 * @throws AcsJException
	 *            There are literally dozens of CORBA exceptions that could be
	 *            thrown by the SimpleSupplier class. Instead, these are
	 *            converted into an ACS Error System exception for the
	 *            developer's convenience.
	 */
	public SimpleSupplier(String cName, String channelNotifyServiceDomainName, ContainerServicesBase services) throws AcsJException
	{
		this(cName, channelNotifyServiceDomainName, services, null);
	}
	
	/**
	 * Creates a new instance of SimpleSupplier.
	 * Make sure you call {@link #disconnect()} when you no longer need this event supplier object.
	 * 
	 * @param cName
	 *           name of the notification channel events will be published to.
	 * @param channelNotifyServiceDomainName
	 *           Channel domain name, which is being used to determine notification service.
	 * @param services
	 *           This is used to get the name of the component and to access the
	 *           ACS logging system.
	 * @param evProcCallback
	 * 			Callback that handles the events' fate
	 * @throws AcsJException
	 *            There are literally dozens of CORBA exceptions that could be
	 *            thrown by the SimpleSupplier class. Instead, these are
	 *            converted into an ACS Error System exception for the
	 *            developer's convenience.
	 */
	public SimpleSupplier(String cName, String channelNotifyServiceDomainName, ContainerServicesBase services, 
			EventProcessingCallback<? extends IDLEntity> evProcCallback) throws AcsJException 
	{
		// sanity check
		if (cName == null) {
			String reason = "Null reference obtained for the channel name!";
			throw new AcsJBadParameterEx(reason);
		}

		m_channelName = cName;
		m_channelNotifyServiceDomainName = channelNotifyServiceDomainName;
		m_services = services;
		m_logger = services.getLogger();

		m_anyAide = new AnyAide(services);
		m_helper = new Helper(services);
		isTraceEventsEnabled = m_helper.getChannelProperties().isTraceEventsEnabled(m_channelName);
		
		// get the channel and the Supplier admin object
		m_channel = m_helper.getNotificationChannel(m_channelName, getChannelKind(), getNotificationFactoryName());

		// Corba NC spec about adminId: a unique identifier assigned by the target EventChannel instance that is unique among all 
		// SupplierAdmin instances currently associated with the channel.
		// We are currently not using it, but it could be logged to help with NC debugging.
		IntHolder adminIdHolder = new IntHolder(); 
		org.omg.CosNotifyChannelAdmin.SupplierAdmin supplierAdminBase = m_channel.new_for_suppliers(InterFilterGroupOperator.AND_OP, adminIdHolder);
		if (supplierAdminBase == null) {
			String reason = "Null reference obtained for the supplier admin for channel " + m_channelName;
			throw new AcsJCORBAReferenceNilEx(reason);
		}
		try {
			m_supplierAdmin = gov.sandia.NotifyMonitoringExt.SupplierAdminHelper.narrow(supplierAdminBase);
		} catch (BAD_PARAM ex) {
			// This should never happen, since we already enforced the presence of TAO extensions in Helper#initializeNotifyFactory
			String specialSupplierAdminId = gov.sandia.NotifyMonitoringExt.SupplierAdminHelper.id();
			String standardSupplierAdminId = org.omg.CosNotifyChannelAdmin.SupplierAdminHelper.id();
			LOG_NC_TaoExtensionsSubtypeMissing.log(m_logger, m_channelName + "-SupplierAdmin", specialSupplierAdminId, standardSupplierAdminId);
			AcsJNarrowFailedEx ex2 = new AcsJNarrowFailedEx(ex);
			ex2.setNarrowType(specialSupplierAdminId);
			throw ex2;
		}

		int proxyCreationAttempts = 0;
		while( m_proxyConsumer == null) {
			String randomizedClientName = m_helper.createRandomizedClientName(m_services.getName());
			// Holder for the unique ID assigned by the admin object. It is different from the name we set, and will be discarded.
			IntHolder proxyIdHolder = new IntHolder();
			proxyCreationAttempts++;
			try {
				// Create the consumer proxy (to which the published events will be fed) with a name.
				// The client type parameter selects a StructuredProxyPushConsumer (based on Structured Events),
				// as opposed to ProxyPushConsumer (based on Anys), or SequenceProxyPushConsumer (based on sequences of Structured Events).
				org.omg.CORBA.Object tempCorbaObj = m_supplierAdmin.obtain_named_notification_push_consumer(ClientType.STRUCTURED_EVENT, proxyIdHolder, randomizedClientName.toString());
				if (tempCorbaObj == null) {
					String reason = "Null reference obtained for the Proxy Push Consumer!";
					throw new AcsJCORBAReferenceNilEx(reason);
				}
				m_proxyConsumer = StructuredProxyPushConsumerHelper.narrow(tempCorbaObj);
				LOG_NC_ConsumerProxyCreation_OK.log(m_logger, proxyIdHolder.value, randomizedClientName, proxyCreationAttempts, m_services.getName(), m_channelName, getNotificationFactoryName());
			} catch (NameAlreadyUsed e) {
				// Hopefully we won't run into this situation. Still, try to go on in the loop,
				// with a different client name next time.
				m_logger.fine("Consumer proxy name '" + randomizedClientName + "' already in use. Will try again with different random number appended.");
			} catch (NameMapError ex) {
				// Default to the unnamed version
				try {
					m_proxyConsumer = StructuredProxyPushConsumerHelper.narrow(m_supplierAdmin.obtain_notification_push_consumer(ClientType.STRUCTURED_EVENT, proxyIdHolder));
					LOG_NC_ConsumerProxyCreation_OK.log(m_logger, proxyIdHolder.value, "-unknown-", proxyCreationAttempts, m_services.getName(), m_channelName, getNotificationFactoryName());
				} catch (AdminLimitExceeded ex2) {
					LOG_NC_ConsumerProxyCreation_FAIL.log(m_logger, m_services.getName(), m_channelName, getNotificationFactoryName(), ex2.getMessage());
					throw new AcsJCORBAProblemEx(ex2);
				}
			} catch (AdminLimitExceeded e) {
				LOG_NC_ConsumerProxyCreation_FAIL.log(m_logger, m_services.getName(), m_channelName, getNotificationFactoryName(), e.getMessage());
				throw new AcsJCORBAProblemEx(e);
			}
		}

		// must connect this StructuredPushSupplier to the proxy consumer, or events would never be sent anywhere.
		// see 3.4.4.1 of Notification Service, v1.1
		try {
			StructuredPushSupplier thisSps = StructuredPushSupplierHelper.narrow(m_services.activateOffShoot(this));
			m_proxyConsumer.connect_structured_push_supplier(thisSps);
		} 
		catch (AcsJContainerServicesEx e) {
			// convert it to an ACS Error System Exception
			throw new AcsJCORBAProblemEx(e);
		} 
		catch (AlreadyConnected e) {
			// Think there is virtually no chance of this every happening but...
			throw new AcsJCORBAProblemEx(e);
		}
		
		eventBuff = new CircularQueue<IDLEntity>();
		
		reconnectCallback = new AcsNcReconnectionCallback(this);
		reconnectCallback.init(services, m_helper.getNotifyFactory());
		
		eventProcCallback = evProcCallback;
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * User code <b>must call this method when the Supplier is no longer useful</b>.
	 * Failure to do so can result in remote memory leaks. User should not call
	 * this method multiple times either. Once disconnect has been called, all of
	 * SimpleSupplier's methods will cease to function properly.
	 */
	public void disconnect() {
		if (m_supplierAdmin == null) {
			m_logger.info("Ignoring request to disconnect an unconnected SimpleSupplier for channel " + m_channelName);
			return;
		}
		
		String errMsg = "Failed to cleanly disconnect SimpleSupplier for channel '" + m_channelName + "': "; 
		try {
			// handle notification channel cleanup
			m_proxyConsumer.disconnect_structured_push_consumer();
		} 
		catch (Throwable thr) {
			m_logger.log(Level.WARNING, errMsg + "could not disconnect push consumer", thr);
		}
		
		try {
			m_supplierAdmin.destroy();
		} 
		catch (Throwable thr) {
			m_logger.log(Level.WARNING, errMsg + "could not destroy supplier admin", thr);
		}

		try {
			// clean-up CORBA stuff
			reconnectCallback.disconnect();
			m_services.deactivateOffShoot(this);
		} 
		catch (Throwable thr) {
			m_logger.log(Level.WARNING, errMsg + "could not deactivate the SimpleSupplier offshoot.", thr);
		}
		reconnectCallback = null;
		m_proxyConsumer = null;
		m_supplierAdmin = null;
		
		
	}

	
	/**
	 * Destroys the notification channel, which may be in use by other suppliers or receivers.
	 * @Deprecated  This feature is luckily not used as of ALMA-5_0_1_9 (2007-12). We must first investigate 
	 *              when/how we can know that it is safe to destroy a channel object.
	 */
	protected void destroyNotificationChannel() throws AcsJException {
		m_helper.destroyNotificationChannel(m_channelName, getChannelKind(), m_channel);
	}

	
	// //////////////////////////////////////////////////////////////////////////
	/**
	 * This method returns a constant character pointer to the "kind" of
	 * notification channel as registered with the naming service (i.e., the kind
	 * field of a CosNaming::Name) which is normally equivalent to
	 * acscommon::NC_KIND. The sole reason this method is provided is to
	 * accomodate subclasses which subscribe/publish non-ICD style events (ACS
	 * archiving channel for example). In that case, the developer would override
	 * this method.
	 * 
	 * @return string
	 */
	protected String getChannelKind() {
		return alma.acscommon.NC_KIND.value;
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * This method returns a constant character pointer to the notification
	 * channel domain which is normally equivalent to acscommon::ALMADOMAIN. The
	 * sole reason this method is provided is to accomodate subclasses which
	 * subscribe/publish non-ICD style events (ACS archiving channel for
	 * example).In that case, the developer would override this method.
	 * 
	 * @return string
	 */
	protected String getChannelDomain() {
		return alma.acscommon.ALMADOMAIN.value;
	}

	/**
	 * This method returns a the notify service name as registered with the CORBA
	 * Naming Service. This is normally equivalent to acscommon::ALMADOMAIN. The
	 * sole reason this method is provided is to accomodate subclasses which
	 * subscribe/publish non-ICD style events (ACS archiving channel for
	 * example).In that case, the developer would override this method.
	 * 
	 * @return string
	 */
	protected String getNotificationFactoryName() {
		return m_helper.getNotificationFactoryNameForChannel(m_channelName, m_channelNotifyServiceDomainName);
	}
	
	/**
	 * Returns the name of the channel.
	 */
	public String getChannelName() {
		return m_channelName;
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Gets a reference to the event channel specified during construction.
	 * 
	 * @return Reference to the event channel.
	 */
	protected EventChannel getNotificationChannel() {
		return m_channel;
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * This method gets called by the CORBA framework to notify us that the subscriber 
	 * situation has changed. By default we ignore this information.
	 * See 2.6.3 of Notification Service, v1.1
	 * <p>
	 * Override this method so your "smart" Supplier subclass can publish (or not
	 * publish) events based on Consumer demands. Not very useful when there are
	 * more than one Supplier instances for a given channel.
	 * TODO (HSO): not clear what this comment means. Why should not multiple suppliers adjust the events they publish 
	 *             to what the consumers want?
	 *             Maybe the concern was meant for multiple consumers that have different needs? 
	 *             See also method obtain_subscription_types.
	 * 
	 * This method becomes extremely useful if we could assume there is only one
	 * supplier per channel. That is, the API could intelligently publish events
	 * to a given domain/type only when there are consumers subscribed. However,
	 * there are problems when there are multiple supplier instances for a
	 * channel.
	 * 
	 * <b>Do not call it from your code!</b>
	 * 
	 * @param eventType
	 *           Added subscription array.
	 * @param eventType1
	 *           Removed subscription array.
	 * @throws InvalidEventType
	 *            Throw this exception when a consumer subscribes (or
	 *            unsubscribes) to a domain/type that does not exist.
	 */
	public void subscription_change(EventType[] eventType, EventType[] eventType1) throws InvalidEventType {
		// This seems to have confused developers in the past so now just silently return.
		// @TODO (HSO): corba spec suggests to raise the NO_IMPLEMENT exception
		return;
	}

	/**
	 * Override this method to do something when a consumer unsubscribes from the
	 * channel. <b>Do not call it from your code!</b>
	 * <p>
	 * @TODO (HSO): The CORBA NC spec (3.3.10.1) says:
	 *   The disconnect_structured_push_supplier operation is invoked to terminate a connection between the target StructuredPushSupplier 
	 *   and its associated consumer. This operation takes no input parameters and returns no values. 
	 *   The result of this operation is that the target StructuredPushSupplier will release all resources it had
	 *   allocated to support the connection, and dispose its own object reference.
	 * Is it really true what the log message says, that one of many consumers has disconnected, and we should continue for our other consumers?
	 * It may be so, given that the life cycle of a SimpleSupplier seemss unaffected of consumers in the ACS NC design. 
	 */
	public void disconnect_structured_push_supplier() {
		String msg = "A Consumer has disconnected from the '" + m_channelName + "' channel";
		m_logger.info(msg);
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Method which publishes an entire CORBA StructuredEvent without making any
	 * modifications to it.
	 * 
	 * @param se A complete structured event
	 * @throws AcsJException if the event cannot be published for some reason or another.
	 */
	protected <T extends IDLEntity> void publishCORBAEvent(StructuredEvent se,
			T event) throws AcsJException {
		try {
			//Check the queue for remaining events
			CircularQueue<IDLEntity>.Data tmp;
			while ((tmp = eventBuff.pop()) != null)
				m_proxyConsumer.push_structured_event(tmp.corbaData);
			// Publish directly the given event (see CORBA NC spec 3.3.7.1)
			m_proxyConsumer.push_structured_event(se);
			if(eventProcCallback != null)
				eventProcCallback.eventSent(event);
		} catch (org.omg.CORBA.TRANSIENT e){
			// the Notify Service is down...
			CircularQueue<IDLEntity>.Data dropped = eventBuff.push(se, event);
			eventProcCallback.eventStoredInQueue(event);
			if (dropped != null) {
				eventProcCallback.eventDropped(dropped.userData);
			}
		} catch (org.omg.CosEventComm.Disconnected e) {
			// declared CORBA ex
			String reason = "Failed to publish event on channel '" + m_channelName + "': org.omg.CosEventComm.Disconnected was thrown.";
			AcsJCORBAProblemEx jex = new AcsJCORBAProblemEx();
			jex.setInfo(reason);
			throw jex;
		} catch (org.omg.CORBA.SystemException ex) {
			// CORBA runtime ex (with minor code)
			String reason = "Failed to publish event on channel '" + m_channelName + "': " + ex.getClass().getName() + " was thrown.";
			AcsJCORBAProblemEx jex = new AcsJCORBAProblemEx(ex);
			jex.setMinor(ex.minor);
			jex.setInfo(reason);
			throw jex;
		} catch (Throwable thr) {
			// other ex
			String reason = "Failed to publish event on channel '" + m_channelName + "'.";
			AcsJUnexpectedExceptionEx jex = new AcsJUnexpectedExceptionEx(reason, thr);
			throw jex;
		}
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
	protected StructuredEvent getCORBAEvent(String typeName, String eventName) {
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

	// //////////////////////////////////////////////////////////////////////////
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
	public void publishEvent(IDLEntity customStruct) throws AcsJException {
		// The Java class name without package becomes the name of the "event type".
		String typeName = customStruct.getClass().getName().substring(
				customStruct.getClass().getName().lastIndexOf('.') + 1);

		// event to send
		StructuredEvent event = getCORBAEvent(typeName, "");

		// Store the info for Exec/I&T into the event.
		// create the any
		event.remainder_of_body = m_services.getAdvancedContainerServices().getAny();
		// get the useful data which includes the component's name, timestamp, and event count
		EventDescription descrip = new EventDescription(m_services.getName(),
				alma.acs.util.UTCUtility.utcJavaToOmg(System.currentTimeMillis()), m_count);
		// store the IDL struct into the structured event
		EventDescriptionHelper.insert(event.remainder_of_body, descrip);

		// preallocate one name/value pair
		event.filterable_data = new Property[1];
		event.filterable_data[0] = new Property(
				alma.acscommon.DEFAULTDATANAME.value, 
				m_anyAide.complexObjectToCorbaAny(customStruct) );

		if (isTraceEventsEnabled) {
			m_logger.log(Level.INFO, "Channel:" + m_channelName + ", Event Type:" + typeName);
		}

		publishCORBAEvent(event, customStruct);
		m_count++;
	}
	
	@Override
	public void reconnect(gov.sandia.NotifyMonitoringExt.EventChannelFactory ecf) {
		if (m_channel != null)
			m_channel = m_helper.getNotificationChannel(ecf);
			if (m_channel == null)
				m_logger.log(Level.WARNING, "Cannot reconnect to the channel: " + 
						m_channelName);
		/*Re apply the Channel QoS and admin properties*/
		try {
			m_channel.set_qos(m_helper.getChannelProperties().
					getCDBQoSProps(m_channelName));
			m_channel.set_admin(m_helper.getChannelProperties().
					getCDBAdminProps(m_channelName));
		} catch (UnsupportedQoS e) {
		} catch (AcsJException e) {
		} catch (UnsupportedAdmin ex) {
			m_logger.warning(m_helper.createUnsupportedAdminLogMessage(ex, m_channelName));
		} catch (NullPointerException e) {
		}
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Provides channel properties. 
	 */
	protected final Helper m_helper;

	/** The event channel has exactly one name registered in the naming service. */
	protected final String m_channelName;

	/** The channel notification service domain name, can be <code>null</code>. */
	protected final String m_channelNotifyServiceDomainName;

	/**
	 * Supplier Admin object is responsible for creating & managing proxy
	 * consumers.
	 */
	protected SupplierAdmin m_supplierAdmin;

	/**
	 * The proxy consumer object used by supplier to push events onto the
	 * channel.
	 */
	protected StructuredProxyPushConsumer m_proxyConsumer;

	/**
	 * The total number of successful events published by this particular supplier.
	 * The current count is attached to the EventDescription that gets sent along as additional data (remainder_of_body). 
	 */
	protected volatile long m_count = 0;

	/** Channel we'll be sending events to*/
	protected EventChannel m_channel;

	/** Standard logger*/
	protected final Logger m_logger;

	/** To access the ORB among other things*/
	protected final ContainerServicesBase m_services;

	/** Helper class used to manipulate CORBA anys */
	protected final AnyAide m_anyAide;

	/** Whether sending of events should be logged */
	private final boolean isTraceEventsEnabled;
	
	private AcsNcReconnectionCallback reconnectCallback;
	
	private CircularQueue<IDLEntity> eventBuff;
	
	protected EventProcessingCallback eventProcCallback;

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Defines the different handlers according of the event's fate in the
	 * Notification Channel. The developer is responsible to implement the handlers
	 */
	public static interface EventProcessingCallback<T extends IDLEntity>{
		/**
		 * Handler if the event was dropped
		 * @param event 
		 * 			the event to be handled
		 */
		public void eventDropped(T event);
		
		/**
		 * Handler if the event was sent
		 * @param event 
		 * 			the event to be handled
		 */
		public void eventSent(T event);
		
		/**
		 * Handler if the event was stored in the circular queue
		 * @param event
		 * 			the event to be handled
		 */
		public void eventStoredInQueue(T event);
	}
}
