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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.IntHolder;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosEventComm.Disconnected;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotification.UnsupportedAdmin;
import org.omg.CosNotification.UnsupportedQoS;
import org.omg.CosNotifyChannelAdmin.AdminNotFound;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.ConsumerAdmin;
import org.omg.CosNotifyChannelAdmin.EventChannel;
import org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplierHelper;
import org.omg.CosNotifyComm.InvalidEventType;
import org.omg.CosNotifyFilter.ConstraintExp;
import org.omg.CosNotifyFilter.Filter;
import org.omg.CosNotifyFilter.FilterFactory;
import org.omg.CosNotifyFilter.FilterNotFound;

import gov.sandia.NotifyMonitoringExt.EventChannelFactory;

import alma.ACSErrTypeCORBA.wrappers.AcsJCORBAReferenceNilEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeJavaNative.wrappers.AcsJJavaAnyEx;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_FAIL;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_OK;
import alma.AcsNCTraceLog.LOG_NC_ProcessingTimeExceeded;
import alma.AcsNCTraceLog.LOG_NC_ProcessingWithoutHandler;
import alma.AcsNCTraceLog.LOG_NC_SubscriptionConnect_FAIL;
import alma.AcsNCTraceLog.LOG_NC_SubscriptionConnect_OK;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.MultipleRepeatGuard;
import alma.acs.logging.RepeatGuard.Logic;
import alma.acs.nc.AcsNcReconnectionCallback;
import alma.acs.nc.AnyAide;
import alma.acs.nc.Helper;
import alma.acs.nc.ReconnectableSubscriber;
import alma.acs.util.StopWatch;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;
import alma.acsnc.OSPushConsumer;
import alma.acsnc.OSPushConsumerHelper;
import alma.acsnc.OSPushConsumerPOA;

/**
 * NCSubscriber is the Java implementation of the Notification Channel 
 * subscriber.
 * 
 * This class is used to receive events asynchronously from notification
 * channel suppliers. 
 * Replacement of {@link alma.acs.nc.Consumer}, no longer supports inheritance mode.
 * 
 * @author jslopez
 */
public class NCSubscriber <T extends IDLEntity> 
		extends OSPushConsumerPOA 
		implements AcsEventSubscriber<T>, ReconnectableSubscriber {
	/**
	 * The default maximum amount of time an event handler is given to process
	 * event before a warning message is logged. This is used when an end-user
	 * does *not* define the appropriate XML elements within the ACS CDB. See
	 * the inline doc on EventChannel.xsd for more info. The time unit is
	 * floating point seconds.
	 */
	private static final double DEFAULT_MAX_PROCESS_TIME_SECONDS = 2.0;

	/** Used to time the execution of receive methods */
	private final StopWatch profiler;

	/** Provides access to the ACS logging system. */
	protected final Logger logger;

	/** Provides access to the naming service among other things. */
	protected final Helper helper;

	/**
	 * There can be only one notification channel for any given subscriber.
	 * Constructed on demand.
	 */
	protected EventChannel channel;

	/** The channel has exactly one name registered in the CORBA Naming Service. */
	protected final String channelName;

	/** The channel notification service domain name, can be <code>null</code>. */
	protected final String channelNotifyServiceDomainName;

	/**
	 * The consumer admin object used by consumers to get a reference to the
	 * structured supplier proxy.
	 */
	protected ConsumerAdmin consumerAdmin;

	/** The supplier proxy we are connected to. */
	protected StructuredProxyPushSupplier proxySupplier;

	/** CORBA reference to ourself. */
	protected OSPushConsumer corbaRef = null;

	/** Helper class used to manipulate CORBA anys. */
	protected AnyAide anyAide;

	/** Whether sending of events should be logged. */
	private final boolean isTraceNCEventsEnabled;

	/**
	 * Maps event names to the maximum amount of time allowed for receiver
	 * methods to complete. Time is given in floating point seconds.
	 */
	protected final HashMap<String, Double> handlerTimeoutMap;

	/**
	 * Contains a generic receiver to be used by the
	 * {@link #addGenericSubscription()} method.
	 */
	protected AcsEventSubscriber.GenericCallback genericReceiver = null;

	/**
	 * Contains a list of receiver functions to be invoked when an event of a
	 * particular type is received.
	 */
	protected Map<String, AcsEventSubscriber.Callback<T>> receivers = new HashMap<String, AcsEventSubscriber.Callback<T>>();

	/**
	 * Contains a list of the added and removed subscriptions filters applied.
	 * Events on this list will be processed to check if the event should be
	 * accepted or discarded.
	 */
	protected Map<String, Integer> subscriptionsFilters = new HashMap<String, Integer>();

	/**
	 * Contains a list of repeat guards for each different type of event.
	 */
	protected MultipleRepeatGuard multiRepeatGuard;

	private AcsNcReconnectionCallback m_callback;

	private IntHolder consumerAdminID;

	private IntHolder proxyID;
	
	private final ReentrantLock disconnectLock = new ReentrantLock();


	/**
	 * Creates a new instance of NCSubscriber.
	 * 
	 * @param channelName
	 *            Subscribe to events on this channel registered in the CORBA
	 *            Naming Service.
	 * @param services
	 *            This is used to access ACS logging system.
	 * @throws AcsJException
	 *             Thrown on any <I>really bad</I> error conditions
	 *             encountered.
	 */
	public NCSubscriber(String channelName, ContainerServicesBase services)
			throws AcsJException {
		this(channelName, null, services);
	}

	/**
	 * Creates a new instance of NCSubscriber.
	 * 
	 * @param channelName
	 *            Subscribe to events on this channel registered in the CORBA
	 *            Naming Service. If the channel does not exist, it's
	 *            registered.
	 * @param channelNotifyServiceDomainName
	 *            Channel domain name, which is being used to determine
	 *            notification service.
	 * @param services
	 *            This is used to access ACS logging system.
	 * @throws AcsJException
	 *             Thrown on any <I>really bad</I> error conditions
	 *             encountered.
	 */
	public NCSubscriber(String channelName,
			String channelNotifyServiceDomainName,
			ContainerServicesBase services) throws AcsJException {
		if (channelName == null) {
			Throwable cause = new Throwable(
					"Null reference obtained for the channel name!");
			throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(cause);
		}

		this.channelName = channelName;
		this.channelNotifyServiceDomainName = channelNotifyServiceDomainName;

		profiler = new StopWatch();
		logger = services.getLogger();

		anyAide = new AnyAide(services);
		helper = new Helper(services);

		// get the channel
		channel = helper.getNotificationChannel(this.channelName,
				getChannelKind(), getNotificationFactoryName());
		if (channel == null) {
			Throwable cause = new Throwable(
					"Null reference obtained for the notification channel "
							+ this.channelName);
			throw new AcsJCORBAReferenceNilEx(cause);
		}

		// populate the map with the maxProcessTime an event receiver processing
		// should take
		handlerTimeoutMap = helper.getEventHandlerTimeoutMap(this.channelName);

		// get the admin object
		getAdmin();

		// get the proxy Supplier
		createProxySupplier();

		// Little hack for testing purposes
		// TODO: Clean this hack
		isTraceNCEventsEnabled = true;
		// isTraceEventsEnabled =
		// helper.getChannelProperties().isTraceEventsEnabled(this.channelName);

		// Repeat guard for maxProcessTime warning logging
		multiRepeatGuard = new MultipleRepeatGuard(0, TimeUnit.SECONDS, 2,
				Logic.COUNTER, 100);
		
		// if the factory is null, the reconnection callback is not registered
		m_callback = new AcsNcReconnectionCallback(this);
		m_callback.init(services, helper.getNotifyFactory());
	}

	/**
	 * Gets the existing admin object or creates a new one. 
	 * TODO
	 * @todo Allow load balance
	 * 
	 * @throws AcsJException
	 */
	private void getAdmin() throws AcsJException {
		int consumerAdmins[] = channel.get_all_consumeradmins();
		try {
			if (consumerAdmins.length == 0){
				consumerAdminID = new IntHolder();
				consumerAdmin = channel.new_for_consumers(
						InterFilterGroupOperator.AND_OP, consumerAdminID);
			}
			else
				consumerAdmin = channel.get_consumeradmin(consumerAdmins[0]);
		} catch (AdminNotFound e) {
			Throwable cause = new Throwable(
					"The attempt to reuse the admin object failed. "
							+ e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		}

		if (consumerAdmin == null) {
			Throwable cause = new Throwable("The '" + channelName
					+ "' channel: null consumer admin");
			throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(cause);
		}
	}

	/**
	 * Creates the proxy Supplier that will be connected to the admin,
	 * and stores it in the fields {@link #proxyID} and {@link #proxySupplier}.
	 * 
	 * @throws AcsJException
	 */
	private void createProxySupplier() throws AcsJCORBAProblemEx {
		try {
			proxyID = new IntHolder();
			proxySupplier = StructuredProxyPushSupplierHelper
					.narrow(consumerAdmin.obtain_notification_push_supplier(
							ClientType.STRUCTURED_EVENT, proxyID));
		} catch (org.omg.CosNotifyChannelAdmin.AdminLimitExceeded e) {
			throw new AcsJCORBAProblemEx(e);
		}
		if (proxySupplier == null) {
			Throwable cause = new NullPointerException("The '" + channelName + "' channel: null proxy supplier");
			throw new AcsJCORBAProblemEx(cause);
		}
	}

	/**
	 * After invoking this method, the user has no control over when
	 * push_structured_event is invoked by the notification channel. User may
	 * still add and remove subscriptions at any given time though. Also, the
	 * connection can be suspended and resumed.
	 * 
	 * <b>If this method is not called, no event will be received</b>
	 * 
	 * @throws AcsJException
	 *             Thrown if the consumer cannot begin receiving events for some
	 *             CORBA reason.
	 */
	public void startReceivingEvents() throws AcsJException {

		try {
			corbaRef = OSPushConsumerHelper.narrow(helper
					.getContainerServices().activateOffShoot(this));
			proxySupplier
					.connect_structured_push_consumer(org.omg.CosNotifyComm.StructuredPushConsumerHelper
							.narrow(corbaRef));
		} catch (AcsJContainerServicesEx e) {
			LOG_NC_SubscriptionConnect_FAIL.log(logger, channelName,
					getNotificationFactoryName());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e);
		} catch (org.omg.CosEventChannelAdmin.AlreadyConnected e) {
			LOG_NC_SubscriptionConnect_FAIL.log(logger, channelName,
					getNotificationFactoryName());
			Throwable cause = new Throwable(e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		} catch (org.omg.CosEventChannelAdmin.TypeError e) {
			LOG_NC_SubscriptionConnect_FAIL.log(logger, channelName,
					getNotificationFactoryName());
			Throwable cause = new Throwable(e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		}

		LOG_NC_SubscriptionConnect_OK.log(logger, channelName,
				getNotificationFactoryName());
	}

	/**
	 * Called from Corba (via push_structured_event)
	 * 
	 * The implementation cannot assume any particular event type.
	 */
	protected void processEvent(T corbaData, EventDescription eventDescrip) {
		String eventName = corbaData.getClass().getName();

		// figure out how much time this event has to be processed
		if (!handlerTimeoutMap.containsKey(eventName)) {
			// setup a timeout if it's undefined
			handlerTimeoutMap.put(eventName, DEFAULT_MAX_PROCESS_TIME_SECONDS);
		}
		double maxProcessTimeSeconds = handlerTimeoutMap.get(eventName);

		// Here we search the hash of registered receiver objects.
		if (receivers.containsKey(eventName)) {
			AcsEventSubscriber.Callback<T> receiver = receivers.get(eventName);

			// start timing
			profiler.reset();
			receiver.receive(corbaData, eventDescrip);
			double usedSecondsToProcess = (profiler.getLapTimeMillis() / 1000.0);

			// warn the end-user if the receiver is taking too long using a
			// repeat guard
			if (usedSecondsToProcess > maxProcessTimeSeconds
					&& multiRepeatGuard.checkAndIncrement(eventName))
				LOG_NC_ProcessingTimeExceeded.log(logger, channelName,
						getNotificationFactoryName(), eventName,
						multiRepeatGuard.counterAtLastExecution(eventName));
		} else if (genericReceiver != null) {
			// start timing
			profiler.reset();
			genericReceiver.receive(corbaData, eventDescrip);
			double usedSecondsToProcess = (profiler.getLapTimeMillis() / 1000.0);

			// warn the end-user if the receiver is taking too long using a
			// repeat guard.
			if (usedSecondsToProcess > maxProcessTimeSeconds
					&& multiRepeatGuard.checkAndIncrement(eventName))
				LOG_NC_ProcessingTimeExceeded.log(logger, channelName,
						getNotificationFactoryName(), eventName,
						multiRepeatGuard.counterAtLastExecution(eventName));
		} else {
			if (isTraceNCEventsEnabled)
				LOG_NC_ProcessingWithoutHandler.log(logger, channelName,
						getNotificationFactoryName(), eventName);
		}
	}

	/**
	 * Subscribes to all events. If in addition to this we also have
	 * subscriptions via
	 * {@link #addSubscription(Class, alma.acs.nc.refactored.AcsEventSubscriber.Callback)}
	 * then those more specific subscriptions will take precedence.
	 */
	public void addGenericSubscription(GenericCallback receiver) {
		genericReceiver = receiver;
		proxySupplier.remove_all_filters();
		subscriptionsFilters.clear();
	}

	/**
	 * Removes the generic receiver handler.
	 * 
	 * @throws AcsJCORBAProblemEx
	 */
	public void removeGenericSubscription() throws AcsJCORBAProblemEx {
		if (genericReceiver == null)
			throw new IllegalStateException(
					"Unsubscribing from generic subscription when not actually subscribed.");
		
		genericReceiver = null;

		// If receivers is empty we just discard everything
		if (receivers.isEmpty())
			discardAllEvents();
		else {
			// Since we have specific subscriptions, we have to apply the
			// corresponding filters
			Iterator it = receivers.keySet().iterator();
			while (it.hasNext()) {
				String keyName = (String) it.next();
				String filter = keyName.substring(keyName.lastIndexOf('.') + 1);
				subscriptionsFilters.put(keyName, addFilter(filter));
			}
		}
	}

	/**
	 * Add a subscription to a given type of event through filters.The main
	 * advantage of using java generics here: Compiler ensure that receiver is
	 * consistent with event type.
	 * 
	 * @param structClass
	 *            Type of event to subscribe.
	 * @param receiver
	 *            An object which implements a method called "receive". The
	 *            "receive" method must accept an instance of a structClass
	 *            object as its sole parameter.
	 * 
	 * @throws AcsJException
	 *             Thrown if there is some CORBA problem.
	 */
	public void addSubscription(Class<T> structClass,
			AcsEventSubscriber.Callback<T> receiver) throws AcsJException {

		// Adding the subscription to the receivers list
		receivers.put(structClass.getName(), receiver);

		// Adding the filter to the proxy only if there is not generic handler
		if (genericReceiver == null)
			subscriptionsFilters.put(structClass.getName(),
					addFilter(structClass.getSimpleName()));
	};

	/**
	 * Remove a subscription by removing the corresponding gilter. After
	 * invoking this, events of the parameter's type will no longer be received.
	 * 
	 * @param structClass
	 *            Unsubscribes from this IDL struct (Java class). <b>By passing
	 *            in null here, no events of any type will be received, even if
	 *            there was a generic subscription.</b>
	 * @throws AcsJException
	 *             Thrown if there is some CORBA problem (like this subscriber
	 *             has never subscribed to the given type).
	 * @throws FilterNotFound
	 *             Thrown if there is no such filter.
	 * @throws InvalidEventType
	 *             Thrown if the given type is invalid.
	 */
	public void removeSubscription(Class<T> structClass)
			throws AcsJException, FilterNotFound, InvalidEventType {

		// Removing subscription from receivers list
		if (structClass != null) {
			String keyName = structClass.getName();

			if (receivers.containsKey(keyName)) {
				receivers.remove(keyName);
				proxySupplier.remove_filter(subscriptionsFilters.get(keyName));
				subscriptionsFilters.remove(keyName);

				if (receivers.isEmpty())
					discardAllEvents();

			} else {
				Throwable cause = new Throwable(
						"Unsubscribing from '"
								+ keyName
								+ "' type of event when not actually subscribed to this type.");
				throw new AcsJJavaAnyEx(cause);
			}
		} else {
			// Removing every type of event
			discardAllEvents();
			receivers.clear();
		}
	}

	/**
	 * This method manages the filtering capabilities used to control
	 * subscriptions.
	 * 
	 * @throws AcsJCORBAProblemEx
	 */
	private int addFilter(String eventType) throws AcsJCORBAProblemEx {
		// Construction of the constraint expression attribute
		String eventName = "($type_name == '" + eventType + "')";

		try {
			// Create the filter
			FilterFactory filterFactory = channel.default_filter_factory();
			Filter filter = filterFactory.create_filter(getFilterLanguage());

			// Information needed to construct the constraint expression object
			// (any domain, any type)
			EventType[] t_info = { new EventType("*", "*") };

			// Add constraint expression object to the filter
			ConstraintExp[] cexp = { new ConstraintExp(t_info, eventName) };
			filter.add_constraints(cexp);

			// Add the filter to the proxy and return the filter ID
			return proxySupplier.add_filter(filter);

		} catch (org.omg.CosNotifyFilter.InvalidGrammar e) {
			Throwable cause = new Throwable("'" + eventType
					+ "' filter is invalid for the '" + channelName
					+ "' channel: " + e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		} catch (org.omg.CosNotifyFilter.InvalidConstraint e) {
			Throwable cause = new Throwable("'" + eventType
					+ "' filter is invalid for the '" + channelName
					+ "' channel: " + e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		}

	}

	/**
	 * This method <B>must</B> be invoked before a component or client is
	 * destroyed. Failure to do so can cause remote memory leaks. Make sure it
	 * is not invoked multiple times. Once it has been called, events will no
	 * longer be received.
	 * 
	 * @todo Notice that the consumer admin object is not being destroyed. For
	 * doing this we must check if there's no more clients using it. Check if
	 * it's possible using TAO extensions.
	 */
	public void disconnect() {
		disconnectLock.lock();
		boolean success = false;
		try {
			if (consumerAdmin == null) {
				throw new IllegalStateException("Consumer already disconnected");
			}
			
			// stop receiving events
			suspend();

			// remove all filters
			proxySupplier.remove_all_filters();

			// remove all subscriptions
			// DWF-fix me!
			// removeSubscription(null);

			// handle notification channel cleanup
			proxySupplier.disconnect_structured_push_supplier();

			// clean-up CORBA stuff
			if (corbaRef != null) { // this check avoids ugly "offshoot was not activated" messages in certain scenarios
				helper.getContainerServices().deactivateOffShoot(this);
			}
			logger.finer("Disconnected from NC '" + channelName + "'.");
			success = true;
		}
		catch (org.omg.CORBA.OBJECT_NOT_EXIST ex1) {
			// this is OK, because someone else has already destroyed the remote resources
			logger.fine("No need to release resources for channel " + channelName + " because the NC has been destroyed already.");
			success = true;
		}
		catch (Exception ex2) {
			logger.log(Level.WARNING, "Failed to disconnect from NC '" + channelName + "'.", ex2);
		}
		finally {
			if (success) {
				// null the refs if everything was fine, or if we got the OBJECT_NOT_EXIST
				m_callback = null;
				corbaRef = null;
				consumerAdmin = null;
				proxySupplier = null;
			}
			disconnectLock.unlock();
		}
	}

	/** 
	 * Used to temporarily halt receiving events of all types.
	 * <p>
	 * If the Subscriber has been connected already (method {@link #startReceivingEvents()}, 
	 * then after calling this method, incoming events will be buffered instead of being discarded; 
	 * unexpired events will be received later, after a call to {@link #resume()}.
	 * <p>
	 * This design follows CORBA NC standard, as described in 
	 * <it>Notification Service Specification, Version 1.1, formal/04-10-11, 3.4.13 The StructuredProxyPushSupplier Interface.</it>
	 */
	public void suspend() {
		disconnectLock.lock();
		try {
			if (proxySupplier == null) {
				throw new IllegalStateException("Consumer already disconnected");
			}
			proxySupplier.suspend_connection();
		} catch (org.omg.CosNotifyChannelAdmin.ConnectionAlreadyInactive e) {
			// if this fails, it does not matter because the connection
			// has already been suspended.
		} catch (org.omg.CosNotifyChannelAdmin.NotConnected e) {
			// if this fails, it does not matter because we cannot suspend
			// a connection that isn't really connected in the first place.
		}
		finally {
			disconnectLock.unlock();
		}
	}

	/**
	 * Used to reenable the Subscriber after a call to the
	 * <code>suspend()</code> method. Queued events will be received after
	 * this call, see {@link #suspend()}.
	 * 
	 * This call has no effect if the Subscriber is not connected.
	 */
	public void resume() {
		try {
			proxySupplier.resume_connection();
		} catch (org.omg.CosNotifyChannelAdmin.ConnectionAlreadyActive e) {
			// if this fails, it does not matter because the connection
			// has already been resumed.
		} catch (org.omg.CosNotifyChannelAdmin.NotConnected e) {
			// if this fails, it does not matter because we cannot resume
			// a connection that isn't connected in the first place.
		}
	}

	/**
	 * This method is used to discard all events. Is called when there are no
	 * subscriptions left or if the {@link #removeSubscription()} method is
	 * called with null as parameter.
	 * 
	 * @throws AcsJCORBAProblemEx
	 * 
	 */
	private void discardAllEvents() throws AcsJCORBAProblemEx {
		String eventType = null;
		proxySupplier.remove_all_filters();
		subscriptionsFilters.clear();

		try {
			// Create the filter
			FilterFactory filterFactory = channel.default_filter_factory();
			Filter filter = filterFactory.create_filter(getFilterLanguage());

			// Constraint expression that mades the trick to discard every type
			eventType = "($type_name == '')";

			// Information needed to construct the constraint
			// expression
			// object (any domain, any type)
			EventType[] t_info = { new EventType("*", "*") };

			// Add constraint expression object to the filter
			ConstraintExp[] cexp = { new ConstraintExp(t_info, eventType) };
			filter.add_constraints(cexp);

			// Add the filter to the proxy
			proxySupplier.add_filter(filter);

		} catch (org.omg.CosNotifyFilter.InvalidGrammar e) {
			Throwable cause = new Throwable("'" + eventType
					+ "' filter is invalid for the '" + channelName
					+ "' channel: " + e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		} catch (org.omg.CosNotifyFilter.InvalidConstraint e) {
			Throwable cause = new Throwable("'" + eventType
					+ "' filter is invalid for the '" + channelName
					+ "' channel: " + e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		}
	}

	/**
	 * This method returns a constant character pointer to the "kind" of
	 * notification channel as registered with the naming service (i.e., the
	 * kind field of a CosNaming::Name) which is normally equivalent to
	 * acscommon::NC_KIND. The sole reason this method is provided is to
	 * accomodate subclasses which subscribe/publish non-ICD style events (ACS
	 * archiving channel for example).In that case, the developer would override
	 * this method.
	 * 
	 * @return string
	 */
	protected String getChannelKind() {
		return alma.acscommon.NC_KIND.value;
	}

	/**
	 * This method returns a constant character pointer to the notification
	 * channel domain which is normally equivalent to acscommon::ALMADOMAIN.
	 * 
	 * @return string
	 */
	protected String getChannelDomain() {
		return alma.acscommon.ALMADOMAIN.value;
	}

	/**
	 * This method returns the notify service name as registered with the CORBA
	 * Naming Service. This is normally equivalent to
	 * <code>NotifyEventChannelFactory</code>.
	 * 
	 * @return string
	 */
	protected String getNotificationFactoryName() {
		return helper.getNotificationFactoryNameForChannel(channelName,
				channelNotifyServiceDomainName);
	}

	/**
	 * 
	 * This method returns a string to the type of filter constraint language to
	 * be used for filtering events which is normally equivalent to
	 * acsnc::FILTER_LANGUAGE_NAME.
	 * 
	 * @return pointer to a constant string.
	 */
	protected String getFilterLanguage() {
		return alma.acsnc.FILTER_LANGUAGE_NAME.value;
	}

	/**
	 * This method is called each time an event is received.
	 * 
	 * @param structuredEvent
	 *            The structured event sent by a supplier.
	 * @throws Disconnected
	 * 
	 * @see org.omg.CosNotifyComm.StructuredPushConsumerOperations#push_structured_event(org.omg.CosNotification.StructuredEvent)
	 */
	public final void push_structured_event(StructuredEvent structuredEvent)
			throws Disconnected {
		EventDescription eDescrip = EventDescriptionHelper.extract(structuredEvent.remainder_of_body);

		Object convertedAny = anyAide.complexAnyToObject(structuredEvent.filterable_data[0].value);

		IDLEntity struct = null;
		try {
			struct = (IDLEntity) convertedAny;

			if (isTraceNCEventsEnabled)
				LOG_NC_EventReceive_OK.log(
								logger,
								channelName,
								getNotificationFactoryName(),
								structuredEvent.header.fixed_header.event_type.type_name);
		} 
		catch (ClassCastException e) {
			if (isTraceNCEventsEnabled && convertedAny != null)
				LOG_NC_EventReceive_FAIL.log(
								logger,
								channelName,
								getNotificationFactoryName(),
								structuredEvent.header.fixed_header.event_type.type_name);
		}

		if (struct != null) {
			// process the extracted data
			processEvent((T)struct, eDescrip);
		} else {
			// @todo (HSO) unclear why we ignore events w/o data attached. At
			// least now we log it so people can complain.
			// Should compare this with specs and C++ impl
			if (isTraceNCEventsEnabled) {
				logger
						.info("Will ignore event of type "
								+ structuredEvent.header.fixed_header.event_type.type_name
								+ " which has no data attached.");
			}
		}

	}

	/**
	 * ACS does not provide an implementation of this method.
	 * 
	 * @see org.omg.CosNotifyComm.StructuredPushConsumerOperations#disconnect_structured_push_consumer()
	 */
	public void disconnect_structured_push_consumer() {
		throw new NO_IMPLEMENT();

	}

	/**
	 * ACS does not provide an implementation of this method.
	 * 
	 * @see org.omg.CosNotifyComm.NotifyPublishOperations#offer_change(org.omg.CosNotification.EventType[],
	 *      org.omg.CosNotification.EventType[])
	 */
	public void offer_change(EventType[] added, EventType[] removed)
			throws InvalidEventType {
		throw new NO_IMPLEMENT();
	}

	@Override
	public void reconnect(EventChannelFactory ecf) {
		if (channel != null)
			channel = helper.getNotificationChannel(ecf);
			if (channel == null)
				logger.log(Level.WARNING, "Cannot reconnect to the channel: " + 
						channel);
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