/*
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2009 
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.acs.nc.refactored;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.IntHolder;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosEventChannelAdmin.AlreadyConnected;
import org.omg.CosNotification.EventHeader;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.FixedEventHeader;
import org.omg.CosNotification.Property;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotification.UnsupportedAdmin;
import org.omg.CosNotification.UnsupportedQoS;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.EventChannel;
import org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushConsumer;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushConsumerHelper;
import org.omg.CosNotifyChannelAdmin.SupplierAdmin;
import org.omg.CosNotifyComm.InvalidEventType;
import org.omg.CosNotifyComm.StructuredPushSupplier;
import org.omg.CosNotifyComm.StructuredPushSupplierHelper;

import gov.sandia.NotifyMonitoringExt.EventChannelFactory;

import alma.ACSErrTypeCORBA.wrappers.AcsJCORBAReferenceNilEx;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsNcReconnectionCallback;
import alma.acs.nc.AnyAide;
import alma.acs.nc.Helper;
import alma.acs.nc.ReconnectableSubscriber;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;
import alma.acsnc.OSPushSupplierPOA;

/**
 * NCPublisher is the Notificaction Channel implementation to be used with the
 * event channel API to publish events using the Java programming language.

 * @author jslopez
 */
public class NCPublisher extends OSPushSupplierPOA implements AcsEventPublisher, ReconnectableSubscriber {

	/** Provides useful methods. */
	protected final Helper helper;

	/** The event channel has exactly one name registered in the naming service. */
	protected final String channelName;

	/** The channel notification service domain name, can be <code>null</code>. */
	protected final String channelNotifyServiceDomainName;

	/**
	 * Supplier Admin object is responsible for creating & managing proxy
	 * consumers.
	 */
	protected SupplierAdmin supplierAdmin;

	/**
	 * The proxy consumer object used by supplier to push events onto the
	 * channel.
	 */
	protected StructuredProxyPushConsumer proxyConsumer;

	/**
	 * The total number of successful events published by this particular
	 * supplier. The current count is attached to the EventDescription that gets
	 * sent along as additional data (remainder_of_body).
	 */
	protected volatile long count = 0;

	/** Channel we'll be sending events to */
	protected EventChannel channel;

	/** Provides access to the ACS logging system. */
	protected final Logger logger;

	/** To access the ORB among other things */
	protected final ContainerServicesBase services;

	/** Helper class used to manipulate CORBA anys */
	protected final AnyAide anyAide;
	
	protected AcsNcReconnectionCallback m_callback;

	/** Whether sending of events should be logged */
	private final boolean isTraceEventsEnabled;

	private IntHolder supplierAdminID;

	private IntHolder proxyID;


	/**
	 * Creates a new instance of NCPublisher. Make sure you call
	 * {@link #disconnect()} when you no longer need this event supplier object.
	 * 
	 * @param channelName
	 *            Name of the notification channel events will be published to.
	 * @param services
	 *            This is used to get the name of the component and to access
	 *            the ACS logging system.
	 * @throws AcsJException
	 *             There are literally dozens of CORBA exceptions that could be
	 *             thrown by the NCPublisher class. Instead, these are converted
	 *             into an ACS Error System exception for the developer's
	 *             convenience.
	 */
	public NCPublisher(String channelName, ContainerServicesBase services)
			throws AcsJException {
		this(channelName, null, services);
	}

	/**
	 * Creates a new instance of SimpleSupplier. Make sure you call
	 * {@link #disconnect()} when you no longer need this event supplier object.
	 * 
	 * @param channelName
	 *            name of the notification channel events will be published to.
	 * @param channelNotifyServiceDomainName
	 *            Channel domain name, which is being used to determine
	 *            notification service.
	 * @param services
	 *            This is used to get the name of the component and to access
	 *            the ACS logging system.
	 * @throws AcsJException
	 *             There are literally dozens of CORBA exceptions that could be
	 *             thrown by the SimpleSupplier class. Instead, these are
	 *             converted into an ACS Error System exception for the
	 *             developer's convenience.
	 */
	public NCPublisher(String channelName,
			String channelNotifyServiceDomainName,
			ContainerServicesBase services) throws AcsJException {
		if (channelName == null) {
			Throwable cause = new Throwable(
					"Null reference obtained for the channel name!");
			throw new AcsJBadParameterEx(cause);
		}

		this.channelName = channelName;
		this.channelNotifyServiceDomainName = channelNotifyServiceDomainName;
		this.services = services;
		logger = services.getLogger();

		anyAide = new AnyAide(this.services);
		helper = new Helper(this.services);
		isTraceEventsEnabled = helper.getChannelProperties()
				.isTraceEventsEnabled(this.channelName);

		// get the channel
		channel = helper.getNotificationChannel(this.channelName,
				getChannelKind(), getNotificationFactoryName());
		if (channel == null) {
			Throwable cause = new Throwable(
					"Null reference obtained for the notification channel "
							+ this.channelName);
			throw new AcsJCORBAReferenceNilEx(cause);
		}
		
		// get the Supplier admin
		supplierAdminID = new IntHolder();
		supplierAdmin = channel.new_for_suppliers(
				InterFilterGroupOperator.AND_OP, supplierAdminID);
		if (supplierAdmin == null) {
			Throwable cause = new Throwable(
					"Null reference obtained for the supplier admin for channel "
							+ this.channelName);
			throw new AcsJCORBAReferenceNilEx(cause);
		}

		// get the Consumer proxy
		try {
			proxyID = new IntHolder();
			proxyConsumer = StructuredProxyPushConsumerHelper
					.narrow(supplierAdmin.obtain_notification_push_consumer(
							ClientType.STRUCTURED_EVENT, proxyID));
		} catch (org.omg.CosNotifyChannelAdmin.AdminLimitExceeded e) {
			Throwable cause = new Throwable(e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		}

		// must connect this StructuredPushSupplier to the proxy consumer, or
		// events would never be sent anywhere.
		// see 3.4.4.1 of Notification Service, v1.1
		try {
			StructuredPushSupplier thisSps = StructuredPushSupplierHelper
					.narrow(this.services.activateOffShoot(this));
			proxyConsumer.connect_structured_push_supplier(thisSps);
		} catch (AcsJContainerServicesEx e) {
			// convert it to an ACS Error System Exception
			throw new AcsJCORBAProblemEx(e);
		} catch (AlreadyConnected e) {
			// Think there is virtually no chance of this every happening but...
			throw new AcsJCORBAProblemEx(e);
		}
		
		m_callback = new AcsNcReconnectionCallback(this);
		m_callback.init(services, helper.getNotifyFactory());

	}

	/**
	 * User code <b>must call this method when the Supplier is no longer useful</b>.
	 * Failure to do so can result in remote memory leaks. User should not call
	 * this method multiple times either. Once disconnect has been called, all
	 * of SimpleSupplier's methods will cease to function properly.
	 */
	public void disconnect() {
		if (supplierAdmin == null) {
			throw new IllegalStateException("Publisher already disconnected");
		}

		String errMsg = "Failed to cleanly disconnect SimpleSupplier for channel '"
				+ channelName + "': ";
		try {
			// handle notification channel cleanup
			proxyConsumer.disconnect_structured_push_consumer();
		} catch (Throwable thr) {
			logger.log(Level.WARNING, errMsg
					+ "could not disconnect push consumer", thr);
		}

		try {
			supplierAdmin.destroy();
		} catch (Throwable thr) {
			logger.log(Level.WARNING, errMsg
					+ "could not destroy supplier admin", thr);
		}

		try {
			// clean-up CORBA stuff
			services.deactivateOffShoot(this);
		} catch (Throwable thr) {
			logger.log(Level.WARNING, errMsg
					+ "could not deactivate the SimpleSupplier offshoot.", thr);
		}
		proxyConsumer = null;
		supplierAdmin = null;
	}

	/**
	 * This method returns a constant character pointer to the "kind" of
	 * notification channel as registered with the naming service (i.e., the
	 * kind field of a CosNaming::Name) which is normally equivalent to
	 * acscommon::NC_KIND. The sole reason this method is provided is to
	 * accomodate subclasses which subscribe/publish non-ICD style events (ACS
	 * archiving channel for example). In that case, the developer would
	 * override this method.
	 * 
	 * @return string
	 */
	protected String getChannelKind() {
		return alma.acscommon.NC_KIND.value;
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
	protected String getChannelDomain() {
		return alma.acscommon.ALMADOMAIN.value;
	}

	/**
	 * This method returns a the notify service name as registered with the
	 * CORBA Naming Service. This is normally equivalent to
	 * acscommon::ALMADOMAIN. The sole reason this method is provided is to
	 * accomodate subclasses which subscribe/publish non-ICD style events (ACS
	 * archiving channel for example).In that case, the developer would override
	 * this method.
	 * 
	 * @return string
	 */
	protected String getNotificationFactoryName() {
		return helper.getNotificationFactoryNameForChannel(channelName,
				channelNotifyServiceDomainName);
	}

	/**
	 * ACS does not provide an implementation of this method.
	 * 
	 * @see org.omg.CosNotifyComm.NotifySubscribeOperations#subscription_change(org.omg.CosNotification.EventType[],
	 *      org.omg.CosNotification.EventType[])
	 */
	public void subscription_change(EventType[] added, EventType[] removed)
			throws InvalidEventType {
		throw new NO_IMPLEMENT();
	}

	/**
	 * Override this method to do something when a consumer unsubscribes from
	 * the channel. <b>Do not call it from your code!</b>
	 * <p>
	 * 
	 * @TODO (HSO): The CORBA NC spec (3.3.10.1) says: The
	 *       disconnect_structured_push_supplier operation is invoked to
	 *       terminate a connection between the target StructuredPushSupplier
	 *       and its associated consumer. This operation takes no input
	 *       parameters and returns no values. The result of this operation is
	 *       that the target StructuredPushSupplier will release all resources
	 *       it had allocated to support the connection, and dispose its own
	 *       object reference. Is it really true what the log message says, that
	 *       one of many consumers has disconnected, and we should continue for
	 *       our other consumers? It may be so, given that the life cycle of a
	 *       SimpleSupplier seemss unaffected of consumers in the ACS NC design.
	 */
	public void disconnect_structured_push_supplier() {
		String msg = "A Consumer has disconnected from the '" + channelName
				+ "' channel";
		logger.info(msg);
	}

	/**
	 * Method which publishes an entire CORBA StructuredEvent without making any
	 * modifications to it.
	 * 
	 * @param se
	 *            A complete structured event
	 * @throws AcsJException
	 *             if the event cannot be published for some reason or another.
	 */
	protected void publishCORBAEvent(StructuredEvent se) throws AcsJException {
		try {
			// Publish directly the given event (see CORBA NC spec 3.3.7.1)
			proxyConsumer.push_structured_event(se);
		} catch (org.omg.CosEventComm.Disconnected e) {
			// declared CORBA ex
			String reason = "Failed to publish event on channel '"
					+ channelName
					+ "': org.omg.CosEventComm.Disconnected was thrown.";
			AcsJCORBAProblemEx jex = new AcsJCORBAProblemEx();
			jex.setInfo(reason);
			throw jex;
		} catch (org.omg.CORBA.SystemException ex) {
			// CORBA runtime ex (with minor code)
			String reason = "Failed to publish event on channel '"
					+ channelName + "': " + ex.getClass().getName()
					+ " was thrown.";
			AcsJCORBAProblemEx jex = new AcsJCORBAProblemEx(ex);
			jex.setMinor(ex.minor);
			jex.setInfo(reason);
			throw jex;
		} catch (Throwable thr) {
			// other ex
			Throwable cause = new Throwable(
					"Failed to publish event on channel '" + channelName
							+ "'. " + thr.getMessage());
			AcsJUnexpectedExceptionEx jex = new AcsJUnexpectedExceptionEx(cause);
			throw jex;
		}
	}

	/**
	 * Method used to create a pre-filled CORBA event.
	 * 
	 * @param typeName
	 *            The structured event's type_name.
	 * @param eventName
	 *            Name of the event.
	 * @return A pre-filled CORBA event.
	 */
	protected StructuredEvent getCORBAEvent(String typeName, String eventName) {
		// return value
		StructuredEvent event = new StructuredEvent();

		// event.header.fixed_header.event_type
		EventType event_type = new EventType(getChannelDomain(), typeName);
		FixedEventHeader fixed_header = new FixedEventHeader(event_type,
				eventName);

		// event.header.variable_header
		Property[] variable_header = new Property[0];

		// event.header
		event.header = new EventHeader(fixed_header, variable_header);

		return event;
	}

	/**
	 * Takes a generic Java object and tries to pack it into a CORBA Any and
	 * publish it to the event channel. This will fail if the parameter is not
	 * CORBA-generated from a user-defined IDL struct. In simple terms, trying
	 * to publish native Java types is impossible because they have no CORBA
	 * mapping to say Python or C++ types.
	 * 
	 * @param customStruct
	 *            An instance of the IDL struct (Java class) to be published.
	 * @throws AcsJException
	 *             There are an enormous amount of possibilities pertaining to
	 *             why an AcsJException would be thrown by publishEvent.
	 */
	public void publishEvent(IDLEntity customStruct) throws AcsJException {
		String typeName = customStruct.getClass().getSimpleName();
		
		// event to send
		StructuredEvent event = getCORBAEvent(typeName, "");

		// Store the info for Exec/I&T into the event.
		// create the any
		event.remainder_of_body = services.getAdvancedContainerServices()
				.getAny();
		// get the useful data which includes the component's name, timestamp,
		// and event count
		EventDescription descrip = new EventDescription(services.getName(),
				alma.acs.util.UTCUtility.utcJavaToOmg(System
						.currentTimeMillis()), count);
		// store the IDL struct into the structured event
		EventDescriptionHelper.insert(event.remainder_of_body, descrip);

		// preallocate one name/value pair
		event.filterable_data = new Property[1];
		event.filterable_data[0] = new Property(
				alma.acscommon.DEFAULTDATANAME.value, anyAide
						.complexObjectToCorbaAny(customStruct));

		if (isTraceEventsEnabled) {
			logger.log(Level.INFO, "Channel:" + channelName + ", Event Type:"
					+ typeName);
		}

		publishCORBAEvent(event);
		count++;
	}

	@Override
	public void reconnect(EventChannelFactory ecf) {

		logger.log(AcsLogLevel.NOTICE, "Reconnecting publisher with channel '" + channelName + "' after Notify Service recovery");

		if (channel != null) {
			channel = helper.getNotificationChannel(ecf);
			if (channel == null) {
				logger.log(Level.WARNING, "Cannot reconnect to the channel: " + channelName + "'");
				return;
			}
		}

		try {
			channel.set_qos(helper.getChannelProperties().
					getCDBQoSProps(channelName));
			channel.set_admin(helper.getChannelProperties().
					getCDBAdminProps(channelName));
		} catch (UnsupportedQoS e) {
		} catch (AcsJException e) {
		} catch (UnsupportedAdmin e) {
		}
		
	}

}
