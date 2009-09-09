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
 * Consumer.java
 * 
 * Created on March 5, 2003, 4:02 PM
 */
// ----------------------------------------------------------------------////
package alma.acs.nc;

import gov.sandia.NotifyMonitoringExt.EventChannelFactory;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.IntHolder;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotification.UnsupportedAdmin;
import org.omg.CosNotification.UnsupportedQoS;
import org.omg.CosNotifyChannelAdmin.AdminNotFound;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.ConsumerAdmin;
import org.omg.CosNotifyChannelAdmin.EventChannel;
import org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator;
import org.omg.CosNotifyChannelAdmin.ProxyNotFound;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplierHelper;
import org.omg.CosNotifyFilter.ConstraintExp;
import org.omg.CosNotifyFilter.Filter;
import org.omg.CosNotifyFilter.FilterFactory;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.ACSErrTypeJavaNative.wrappers.AcsJJavaAnyEx;
import alma.AcsNCTraceLog.LOG_NC_SubscriptionConnect_OK;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.util.StopWatch;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;
import alma.acsnc.OSPushConsumer;
import alma.acsnc.OSPushConsumerHelper;
import alma.acsnc.OSPushConsumerPOA;
import alma.acsncErrType.wrappers.AcsJEventSubscriptionFailureEx;

/**
 * Consumer is the Java implementation of a structured push consumer
 * notification channel class. In short, this class is used to receive events
 * asynchronously from notification channel suppliers. It can either be used
 * as-is (assuming receivers/handlerFunctions are being utilized) or subclassed.
 * Developers must remember to use the disconnect method after they are done
 * receiving events.
 * 
 * @author dfugate
 */
public class Consumer extends OSPushConsumerPOA implements ReconnectableSubscriber{
	
	protected static final String RECEIVE_METHOD_NAME = "receive";

	/**
	 * The default maximum amount of time an event handler is given to process
	 * event before an exception is logged. This is used when an end user does
	 * *not* define the appropriate XML elements within the ACS CDB. 
	 * See the EventChannel.xsd for more info, e.g. at
	 * http://www.eso.org/projects/alma/develop/acs/OnlineDocs/ACS_docs/schemas/urn_schemas-cosylab-com_EventChannel_1.0/complexType/EventDescriptor.html#attr_MaxProcessTime .
	 */
	private static final double DEFAULT_MAX_PROCESS_TIME = 2000.0;

	
	/** helper object contains various info about the notification channel */
	protected final ChannelInfo m_channelInfo;

	/** used to time the execution of receive methods */
	private final StopWatch profiler;

	/** Provides access to the ACS logging system. */
	protected final Logger m_logger;

	/** Provides access to the naming service among other things. */
	protected final Helper m_helper;

	/**
	 * Name of the notification service that hosts the channel that we consume event from.
	 */
	protected final String m_notifyServiceName;
	
	/** The channel has exactly one name registered in the CORBA Naming Service. */
	protected final String m_channelName;
	
	/** 
	 * There can be only one notification channel for any given consumer. 
	 */
	protected EventChannel m_channel;

	/** The channel notification service domain name, can be <code>null</code>. */
	protected final String m_channelNotifyServiceDomainName;

	/**
	 * Contains a list of handler/receiver objects whose "receive" method (see {@link #RECEIVE_METHOD_NAME}) 
	 * will be invoked when an event of a particular type is received.
	 */
	protected final HashMap<String, Object> m_handlerFunctions = new HashMap<String, Object>();

	/** 
	 * maps event names to the maximum amount of time allowed for
	 * receiver methods to complete. Time is given in floating point seconds.
	 */ 
	protected final HashMap<String, Double> m_handlerTimeoutMap;

	/**
	 * The consumer admin object used by consumers to get a reference to the
	 * structured supplier proxy.
	 */
	protected ConsumerAdmin m_consumerAdmin;

	/** The supplier proxy we are connected to. */
	protected StructuredProxyPushSupplier m_proxySupplier;

	/** CORBA reference to ourself */
	protected OSPushConsumer m_corbaRef;

	/** Helper class used to manipulate CORBA anys */
	protected AnyAide m_anyAide;

	/** Whether sending of events should be logged */
	private final boolean isTraceEventsEnabled;
	
	private IntHolder consumerAdminID;

	private IntHolder proxyID;
	
	private AcsNcReconnectionCallback m_callback;

	
	/**
	 * Creates a new instance of Consumer
	 * 
	 * @param channelName
	 *           Subscribe to events on this channel registered in the CORBA
	 *           Naming Service.
	 * @param services
	 *           This is used to access ACS logging system.
	 * @throws AcsJException
	 *            Thrown on any <I>really bad</I> error conditions encountered.
	 */
	public Consumer(String channelName, ContainerServicesBase services) throws AcsJException {
		this(channelName, null, services);
	}

	/**
	 * Creates a new instance of Consumer
	 * 
	 * @param channelName
	 *           Subscribe to events on this channel registered in the CORBA
	 *           Naming Service.
	 * @param channelNotifyServiceDomainName
	 *           Channel domain name, which is being used to determine notification service. May be <code>null</code>.
	 * @param services
	 *           This is used to access ACS logging system.
	 * @throws AcsJException
	 *            Thrown on any <I>really bad</I> error conditions encountered.
	 */
	public Consumer(String channelName, String channelNotifyServiceDomainName, ContainerServicesBase services) throws AcsJException {
		// sanity checks
		if (channelName == null) {
			AcsJIllegalArgumentEx ex = new AcsJIllegalArgumentEx();
			ex.setVariable("channelName");
			ex.setValue("null");
			throw ex;
		}
		if (services == null) {
			AcsJIllegalArgumentEx ex = new AcsJIllegalArgumentEx();
			ex.setVariable("services");
			ex.setValue("null");
			throw ex;
		}
		
		m_channelName = channelName;
		m_channelNotifyServiceDomainName = channelNotifyServiceDomainName;
		m_logger = services.getLogger();

		// naming service, POA, and Any generator
		m_helper = new Helper(services);

		m_notifyServiceName = getNotificationFactoryName();

		profiler = new StopWatch();

		m_anyAide = new AnyAide(services);

		m_channelInfo = new ChannelInfo(services);

		m_handlerTimeoutMap = m_channelInfo.getEventHandlerTimeoutMap(channelName);



		// get the channel
		m_channel = getHelper().getNotificationChannel(m_channelName, getChannelKind(), m_notifyServiceName);

		// go ahead configured CORBA stuff
		createConsumer();

		// if the developer has overriden these, they will subscribe to
		// subscriptions
		// automatically without having to call addSubscrib../addFilter n times
		// for each consumer subclass instance.
		configSubscriptions();
		configFilters();

		isTraceEventsEnabled = m_helper.getChannelProperties()
				.isTraceEventsEnabled(m_channelName);
		// if the factory is null, the callback is not registered
		m_callback = new AcsNcReconnectionCallback(this);
		m_callback.init(services, m_helper.getNotifyFactory());
	}
	

	/**
	 * This method returns a constant character pointer to the "kind" of
	 * notification channel as registered with the naming service (i.e., the kind
	 * field of a CosNaming::Name) which is normally equivalent to
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
	 * This method returns the notify service name as registered with the CORBA
	 * Naming Service. This is normally equivalent to acscommon::ALMADOMAIN. The
	 * sole reason this method is provided is to accommodate subclasses which
	 * subscribe/publish non-ICD style events (ACS archiving channel for
	 * example). 
	 * In that case, the developer would override this method 
	 * (e.g. to return or logging channel with alma.acscommon.LOGGING_NOTIFICATION_FACTORY_NAME.value)
	 * 
	 * @return string
	 */
	protected String getNotificationFactoryName() {
		return m_helper.getNotificationFactoryNameForChannel(m_channelName, m_channelNotifyServiceDomainName);
	}

	/**
	 * Handles the CORBA creation of a consumer.
	 * Changed to private because only ctor of this class call this method as of Alma 5.0.2
	 * 
	 * @throws AcsJException
	 *            Any CORBA exceptions encountered are converted to an
	 *            AcsJException for developer's ease of use.
	 */
	private void createConsumer() throws AcsJException {
		consumerAdminID = new IntHolder();

		// get the Consumer admin object
		// @TODO: investigate reuse of admin objects. Otherwise we may allocate a new thread in the notification service for every subscriber.  
		m_consumerAdmin = m_channel.new_for_consumers(InterFilterGroupOperator.AND_OP, consumerAdminID);
		
		// sanity check
		if (m_consumerAdmin == null) {
			String reason = "The '" + m_channelName + "' channel: null consumer admin";
			throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
		}

		// get the Supplier proxy
		proxyID = new IntHolder();
		try {
			m_proxySupplier = StructuredProxyPushSupplierHelper.narrow(
				m_consumerAdmin.obtain_notification_push_supplier(ClientType.STRUCTURED_EVENT, proxyID));
		} catch (org.omg.CosNotifyChannelAdmin.AdminLimitExceeded e) {
			// convert it into an exception developers care about
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e);
		}
		// sanity check
		if (m_proxySupplier == null) {
			String reason = "The '" + m_channelName + "' channel: null proxy supplier";
			throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
		}
		
		LOG_NC_SubscriptionConnect_OK.log(m_logger, m_channelName, m_notifyServiceName); 
	}

	/**
	 * After invoking this method, the user has no control over when
	 * push_structured_event is invoked by the notification channel. User may
	 * still add and remove subscriptions at any given time though. Also, the
	 * connection can be suspended and resumed.
	 * 
	 * Finally, consumerReady may spawn a thread to search for the channel if it
	 * does not already exist. Once the channel becomes available, all saved
	 * subscription are subscribed to and consumerReady is automatically invoked
	 * again.
	 * 
	 * @throws AcsJException
	 *            Thrown if the consumer cannot begin receiving events for some
	 *            CORBA reason.
	 */
	public void consumerReady() throws AcsJException {

		try {
			m_corbaRef = OSPushConsumerHelper.narrow(getHelper().getContainerServices().activateOffShoot(this));
			m_proxySupplier.connect_structured_push_consumer(org.omg.CosNotifyComm.StructuredPushConsumerHelper.narrow(m_corbaRef));
		} catch (AcsJContainerServicesEx e) {
			// convert it to an ACS Error System Exception
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e);
		} catch (org.omg.CosEventChannelAdmin.AlreadyConnected e) {
			// Think there is virtually no chance of this every happening but...
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e.getMessage());
		} catch (org.omg.CosEventChannelAdmin.TypeError e) {
			// Think there is virtually no chance of this every happening but...
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e.getMessage());
		}
	}


	/**
	 * Override this method to setup any number of event subscriptions. That is,
	 * this method is invoked by consumer's constructor after everything else has
	 * been initialized.
	 */
	protected void configSubscriptions() throws AcsJEventSubscriptionFailureEx{
		// addSubscription();
		return;
	}

	/**
	 * Add a subscription to a given (IDL struct) Java class. Use this method
	 * only when Consumer has been subclassed and processEvent overridden.
	 * 
	 * @param structClass
	 *           Type of event to subscribe to (i.e., alma.CORR.DataStruct.class).
	 *           If <code>null</code> then all events are subscribed.
	 * @throws AcsJEventSubscriptionFailureEx
	 *            Thrown if the subscription failed.
	 */
	public void addSubscription(Class<? extends IDLEntity> structClass) throws AcsJEventSubscriptionFailureEx {
		String type = "*";
		String domain = "*";
		if (structClass != null) {
			type = structClass.getSimpleName();
			domain = getChannelDomain(); // "ALMA"
		}

		try {
			// Subscribe to events
			EventType[] added = { new EventType(domain, type) };
			EventType[] removed = {};

			// really subscribe to the events
			m_consumerAdmin.subscription_change(added, removed);
		} catch (Throwable thr) { // org.omg.CosNotifyComm.InvalidEventType or other
			AcsJEventSubscriptionFailureEx ex = new AcsJEventSubscriptionFailureEx(thr);
			ex.setChannelName(m_channelName);
			ex.setEventName(type);
			throw ex;
		}
	}

	/**
	 * Add a subscription to a given (IDL struct) Java class and also register a
	 * method capable of processing that event (IDL struct). Each time an event
	 * of the (IDL struct) type is received, the Consumer will automatically
	 * invoke the receiver object's "receive" method using the (IDL struct) data
	 * extracted from the CORBA event.
	 * 
	 * @param structClass
	 *           Type of event to subscribe to (i.e., alma.CORR.DataStruct.class).
	 * @param receiver
	 *           An object which implements a method called "receive". The
	 *           "receive" method must accept an instance of a structClass
	 *           object as its sole parameter.
	 * @throws AcsJException
	 *            Thrown if there is some CORBA problem.
	 */
	public void addSubscription(Class<? extends IDLEntity> structClass, Object receiver) throws AcsJException {
		// check to ensure receiver is capable to processing the event
		Class receiverClass = receiver.getClass();
		Class[] parm = { structClass };
		try {
			receiverClass.getMethod(RECEIVE_METHOD_NAME, parm);
		} catch (NoSuchMethodException err) {
			// Well the method doesn't exist...that sucks!
			String reason = "The '" + m_channelName
					+ "' channel: the receiver object is incapable of handling '" + structClass.getName()
					+ "' type of events! " + "It must have a method 'public void " + RECEIVE_METHOD_NAME
					+ "(" + structClass.getName() + ")'.";
			m_logger.log(Level.WARNING, reason, err);
			throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
		} catch (SecurityException err) {
			// Developer has defined the method to be protected or private...this
			// doesn't work either.
			String reason = "The '" + m_channelName
					+ "' channel: the receiver method of the object is protected/private for '"
					+ structClass.getName() + "' type of events!";
			m_logger.log(Level.WARNING, reason, err);
			throw new alma.ACSErrTypeJavaNative.wrappers.AcsJJavaLangEx(reason);
		}

		// Add this eventTypeName/receiver to the list of m_handlerFunctions.
		synchronized (m_handlerFunctions) {
			// make sure the developer has not subscribed to the same
			// event type with two different handlers. makes unsubscribing
			// easier (i.e., developer need not keep track of receiver
			// objects that would otherwise have to be passed
			// back to the removeSubscription method
			if (m_handlerFunctions.containsKey(structClass.getName())) {
				// throw an exception
				throw new AcsJJavaAnyEx("Type already subscribed to.");
			}
			m_handlerFunctions.put(structClass.getName(), receiver);
		}

		// Next call the real addSubscription method.
		this.addSubscription(structClass);
	}

	/**
	 * Remove a subscription from this consumer. After invoking this, events of
	 * the parameter's type will no longer be received.
	 * 
	 * @param structClassName
	 *           Unsubscribes from this IDL struct (Java class). By passing in
	 *           null here, no events of any type will be received.
	 * @throws AcsJException
	 *            Thrown if there is some CORBA problem (like this consumer has
	 *            never subscribed to the IDL struct).
	 */
	public void removeSubscription(Class structClass) throws AcsJException {
		String type = "*";
		String domain = "*";

		// If the developer is not unsubscribing from everything...
		if (structClass != null) {
			// get the type/domain to unsubscribe from
			type = structClass.getName().substring(structClass.getName().lastIndexOf('.') + 1);
			domain = getChannelDomain();

			// Remove the handler function if there is one...
			synchronized (m_handlerFunctions) {
				if (m_handlerFunctions.containsKey(structClass.getName())) {
					// remove the subscription from the hash
					m_handlerFunctions.remove(structClass.getName());
				} else {
					throw new AcsJJavaAnyEx("Unsubscribing from '" + structClass.getName()
							+ "' type of event when not actually subscribed to this type.");
				}
			}
		}
		// they're removing all subscriptions so let's clear the hash
		else {
			m_handlerFunctions.clear();
		}

		try {
			// Unsubscribe to events
			EventType[] added = {};
			EventType[] removed = { new EventType(domain, type) };

			// really unsubscribe from events
			m_consumerAdmin.subscription_change(added, removed);
		} catch (org.omg.CosNotifyComm.InvalidEventType e) {
			String msg = "'" + type + "' event type is invalid for the '" + m_channelName + "' channel: ";
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(msg + e.getMessage());
		}
	}

	/**
	 * Override this method in subclasses to specify all filters this Consumer
	 * will use. The method would then just make a lot of calls to
	 * addFilter(...). Important to note this is the last method invoked by
	 * consumer's constructor.
	 */
	protected void configFilters() {
		return;
	}

	/**
	 * Adds a single filter to this consumer. With ALMA's use of IDL structs for
	 * ICD events, this method is no longer useful. May become deprecated in
	 * future ACS releases.
	 * 
	 * @return The filter ID of the newly created filter. This is only useful for
	 *         removing filters. Returns -1 on failure.
	 * @param structClassName
	 *           IDL struct Java class filter is to be applied to.
	 * @param filter
	 *           The filter string in extended trader constraint language.
	 * @throws AcsJException
	 *            Thrown if there is some CORBA problem (like the filter is not
	 *            using the correct grammar).
	 */
	public int addFilter(Class<? extends IDLEntity> structClassName, String filter) throws AcsJException {
		String type = structClassName.getName().substring(structClassName.getName().lastIndexOf('.') + 1);

		try {
			FilterFactory t_filterFactory = m_channel.default_filter_factory();

			// create the filter
			Filter t_filter = t_filterFactory.create_filter(getFilterLanguage());
			EventType[] t_eType = { new EventType(getChannelDomain(), type) };
			ConstraintExp[] t_cexp = { new ConstraintExp(t_eType, filter) };
			t_filter.add_constraints(t_cexp);

			// add the filter to the proxy
			return m_proxySupplier.add_filter(t_filter);
		} catch (org.omg.CosNotifyFilter.InvalidGrammar e) {
			String msg = "'" + filter + "' filter is invalid for the '" + m_channelName + "' channel: ";
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(msg + e.getMessage());
		} catch (org.omg.CosNotifyFilter.InvalidConstraint e) {
			String msg = "'" + filter + "' filter is invalid for the '" + m_channelName + "' channel: ";
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(msg + e.getMessage());
		}
	}

	/**
	 * Removes a single filter from this consumer. See deprecation comments on
	 * the addFilter method.
	 * 
	 * @param filter
	 *           The filter's unique ID. This parameter is the return value of
	 *           the addFilter method.
	 * @throws AcsJException
	 *            Thrown if CORBA problems are encountered.
	 * @return True if the filter was succesfully removed and false otherwise.
	 *         TODO: this method should not throw an AcsJException
	 */
	public boolean removeFilter(int filter) throws AcsJException {
		// not a real filter in the first place
		if (filter == -1) {
			String msg = "Cannot remove the '" + filter + "' filter ID for the '" + m_channelName
					+ "' channel: bad filter ID!";
			m_logger.warning(msg);
			return false;
		}

		try {
			m_proxySupplier.remove_filter(filter);
			return true;
		} catch (org.omg.CosNotifyFilter.FilterNotFound e) {
			String msg = "Cannot remove the '" + filter + "' filter ID for the '" + m_channelName
					+ "' channel: filter not found!";
			m_logger.warning(msg);
			return false;
		}
	}

	/**
	 * This method returns a string to the type of filter constraint language to
	 * be used for filtering events which is normally equivalent to
	 * acsnc::FILTER_LANGUAGE_NAME. Override to change this behavior.
	 * 
	 * @return pointer to a constant string.
	 */
	protected String getFilterLanguage() {
		// return a constant defined in acsnc.idl to be portable in the other
		// programming languages supported by ACS.
		return alma.acsnc.FILTER_LANGUAGE_NAME.value;
	}

	/**
	 * Override this method to do some "housekeeping". Invoked by the CORBA
	 * Notification Service itself each time a Supplier subclass is destroyed.
	 * <b>Do not call it from your code!</b>
	 */
	public void disconnect_structured_push_consumer() {
		// ACS does not provide an implementation of this method. developers
		// are free to override it if they want though.
	}

	/**
	 * As of ACS 3.0, override {@link #processEvent(Object)} instead of this
	 * CORBA method. push_structured_event is what is called each time an event
	 * is received. <b>Do not call it from your code!</b>
	 * 
	 * @param structuredEvent
	 *           The structured event sent by a supplier subclass.
	 * @throws org.omg.CosEventComm.Disconnected
	 */
	public void push_structured_event(StructuredEvent structuredEvent)
		throws org.omg.CosEventComm.Disconnected {
		// time to get the event description
		EventDescription eDescrip = EventDescriptionHelper.extract(structuredEvent.remainder_of_body);

		Object convertedAny = m_anyAide.complexAnyToObject(structuredEvent.filterable_data[0].value);
		IDLEntity struct = null; 
		try {
			struct = (IDLEntity) convertedAny;
			if (isTraceEventsEnabled) {
				m_logger.log(Level.INFO, "Channel:" + m_channelName + ", Publisher:" + eDescrip.name
						+ ", Event Type:" + structuredEvent.header.fixed_header.event_type.type_name);
			}
		} catch (ClassCastException ex) {
			if (isTraceEventsEnabled && convertedAny != null) {
				m_logger.log(Level.INFO, "Channel:" + m_channelName + ", Publisher:" + eDescrip.name
						+ ", Event Type:" + structuredEvent.header.fixed_header.event_type.type_name + 
						". Failed to convert event data of type '" + convertedAny.getClass().getName() + "' which is not derived from an IDL-defined struct.");
			}
		}

		if (struct != null) {
			// process the extracted data
			processEvent(struct, eDescrip);
		}
		else {
			// @todo (HSO) unclear why we ignore events w/o data attached. At least now we log it so people can complain.
			// Should compare this with specs and C++ impl
			if (isTraceEventsEnabled) {
				m_logger.info("Will ignore event of type " + structuredEvent.header.fixed_header.event_type.type_name + " which has no data attached.");
			}
		}
	}

	/**
	 * The method invoked each time an ICD-style event is received. Consumer
	 * subclasses <B>must</B> override this method or the other signature of
	 * processEvent (if they do not care about the EventDescription parameter).
	 * If receiver object(s) have been registered using the addSubscription
	 * method, this method does not need to be overriden.
	 * 
	 * @param corbaData
	 *           Generally an IDL struct. It has already been extracted from the
	 *           CORBA Any (when it was packed into the CORBA structured event)
	 *           and a simple typecast on it to the correct type should be enough
	 *           to make it usable.
	 * @param eventDescrip
	 *           An instance of an IDL struct which describes the event. See
	 *           acsnc.idl for the defintion.
	 */
	protected void processEvent(IDLEntity corbaData, EventDescription eventDescrip) {

		m_logger.finer("Consumer#processEvent received an event of type " + eventDescrip.name);
		// Create the IDL class
		// The only reason we have to do this is in case the developer has
		// created
		// and registered a receiver object with this instance.
		Class[] parm = { corbaData.getClass() };

		// event name
		String eventName = corbaData.getClass().getName();

		// figure out how much time this event has to be processed
		if (!m_handlerTimeoutMap.containsKey(eventName)) {
			// setup a timeout if it's undefined
			m_handlerTimeoutMap.put(eventName, DEFAULT_MAX_PROCESS_TIME);
		}
		Double maxProcessTimeDouble = m_handlerTimeoutMap.get(eventName);
		long maxProcessTime = (long) maxProcessTimeDouble.doubleValue(); // @TODO: the cast is stupid if time is actually given in fractional seconds.

		// Here we search the hash of registered receiver objects.
		// If a receiver capable of processing this event is found, it is invoked.
		// Otherwise the developer should have overriden the process event method.
		if (m_handlerFunctions.containsKey(eventName)) {
			try {
				// get the receive method
				Method handlerFunction = m_handlerFunctions.get(eventName).getClass().getMethod(
						RECEIVE_METHOD_NAME, parm);
				// get the parameters of this method...
				Class[] tArray = handlerFunction.getParameterTypes();
				// if the first parameter to the receiver method is identical
				// to the Java class...
				if (tArray[0].equals(corbaData.getClass())) {
					// good...user has registered a receiver
					// parameters are the IDL struct
					Object[] arg = { corbaData };

					// finally we can invoke the "receive" method on the receiver object and start the timing
					profiler.reset();
					handlerFunction.invoke(m_handlerFunctions.get(eventName), arg);

					// get the execution time of 'receive'.
					// @TODO: it looks like a bug to compare profiled milliseconds with fractional seconds,
					// unless the comments in ChannelInfo about time given in seconds are wrong!
					long timeToRun = profiler.getLapTimeMillis();

					// warn the end-user if the receiver is taking too long
					if (timeToRun > maxProcessTime) {
						m_logger.warning("Took too long to handle an '" + eventName + "' event: " + timeToRun / 1000.0 + " seconds.");
						m_logger.info("Maximum time to process an event is: " + maxProcessTime / 1000.0 + " seconds.");
					}

					// everything looks OK...return control.
					return;
				}
			} catch (Exception e) {
				e.printStackTrace();
				// if there's an exception in this case, we don't really care!
			}
		} else {
			//m_logger.fine("Did not find a handler for event " + eventName);
		}
		// If this isn't overriden, just pass it on down the chain.
		profiler.reset();
		this.processEvent(corbaData);
		long timeToRun = profiler.getLapTimeMillis();

		if (timeToRun > maxProcessTime) {
			m_logger.warning("Took too long to handle an '" + eventName + "' event: " + timeToRun / 1000.0 + " seconds.");
			m_logger.info("Maximum time to process an event is: " + maxProcessTime / 1000.0 + " seconds.");
		}
	}

	/**
	 * The method invoked each time an ICD-style event is received. Consumer
	 * subclasses <B>must</B> override this method. If receiver object(s) have
	 * been registered using the addSubscription method, this method does not
	 * need to be overriden.
	 * 
	 * @param corbaData
	 *           Generally an IDL struct. It has already been extracted from the
	 *           CORBA Any (when it was packed into the CORBA structured event)
	 *           and a simple typecast on it to the correct type should be enough
	 *           to make it usable.
	 */
	protected void processEvent(Object corbaData) {
		String msg = "Consumer.processEvent(...) the '" + m_channelName + "' channel: "
				+ "override this method in derived classes for '" + corbaData.getClass().getName()
				+ "' objects!";
		m_logger.finest(msg);
	}

	/**
	 * A "smart" consumer will override this method to subscribe to new
	 * domain/type events as suppliers offer them. A fairly advanced feature.
	 * <b>Do not call it from your code!</b>
	 * 
	 * @param eventType
	 *           Domain/type pairs of events that have been added to this
	 *           channel.
	 * @param eventType1
	 *           Domain/type pairs of events that have been removed from this
	 *           channel.
	 * @throws org.omg.CosNotifyComm.InvalidEventType
	 *            ...
	 */
	public void offer_change(EventType[] eventType, EventType[] eventType1)
			throws org.omg.CosNotifyComm.InvalidEventType {
		// ACS does not provide an implementation of this method although
		// developers are free to do so if they like.
	}

	/**
	 * This method <B>must</B> be invoked before a component or client is
	 * destroyed. Failure to do so can cause remote memory leaks. Make sure it is
	 * not invoked multiple times. Once it has been called, events will no longer
	 * be received.
	 */
	public void disconnect() {
		// do better than NPE if someone actually calls this twice
		if (m_proxySupplier == null) {
			throw new IllegalStateException("Consumer already disconnected");
		}
		
		try {
			// stop receiving events
			suspend();

			// remove all filters
			m_proxySupplier.remove_all_filters();

			// remove all subscriptions
			// DWF-fix me!
			removeSubscription(null);

			// handle notification channel cleanup
			m_proxySupplier.disconnect_structured_push_supplier();
			m_consumerAdmin.destroy();

			// clean-up CORBA stuff
			m_callback.disconnect();
			getHelper().getContainerServices().deactivateOffShoot(m_callback);
			getHelper().getContainerServices().deactivateOffShoot(this);
			m_callback = null;
			m_corbaRef = null;
			m_consumerAdmin = null;
			m_proxySupplier = null;
		} catch (Exception e) {
			m_logger.log(Level.INFO, "Failed to disconnect from NC '" + m_channelName + "'.", e);
		}
	}

	/** 
	 * Used to temporarily halt receiving events of all types.
	 * <p>
	 * If the consumer has been connected already (method {@link #consumerReady()}, 
	 * then after calling this method, incoming events will be buffered instead of being discarded; 
	 * unexpired events will be received later, after a call to {@link #resume()}. <br>
	 * This design follows CORBA NC standard, as described in 
	 * <it>Notification Service Specification, Version 1.1, formal/04-10-11, 3.4.13 The StructuredProxyPushSupplier Interface.</it>
	 */
	public void suspend() {
		
		// do better than NPE if someone actually calls this twice
		if (m_proxySupplier == null) {
			throw new IllegalStateException("Consumer already disconnected");
		}
		
		try {
			m_proxySupplier.suspend_connection();
		} catch (org.omg.CosNotifyChannelAdmin.ConnectionAlreadyInactive e) {
			// if this fails, it does not matter because the connection
			// has already been suspended.
		} catch (org.omg.CosNotifyChannelAdmin.NotConnected e) {
			// if this fails, it does not matter because we cannot suspend
			// a connection that isn't really connected in the first place.
		}
	}

	/**
	 * Used to reenable the Consumer after a call to the <code>suspend()</code> method.
	 * Queued events will be received after this call, see {@link #suspend()}.
	 * <p> 
	 * This call has no effect if the consumer is not connected at all (see {@link #consumerReady()}),
	 * or if it has not been suspended. 
	 */
	public void resume() {
		try {

			m_proxySupplier.resume_connection();

		} catch (org.omg.CosNotifyChannelAdmin.ConnectionAlreadyActive e) {
			// if this fails, it does not matter because the connection
			// has already been resumed.
		} catch (org.omg.CosNotifyChannelAdmin.NotConnected e) {
			// if this fails, it does not matter because we cannot resume
			// a connection that isn't connected in the first place.
		}
	}

	
	/**
	 * Returns a reference to this instance's helper. Not too useful outside this
	 * class.
	 * 
	 * @return A valid reference to this instances helper.
	 */
	public alma.acs.nc.Helper getHelper() {
		return m_helper;
	}

	@Override
	public void reconnect(EventChannelFactory ecf) {
		if (m_channel!=null)
				m_channel = m_helper.getNotificationChannel(ecf);
		try {
			if (m_consumerAdmin ==null)
				m_consumerAdmin = m_channel.get_consumeradmin(consumerAdminID.value);
			if (m_proxySupplier == null)
				m_proxySupplier = StructuredProxyPushSupplierHelper.narrow(m_consumerAdmin.get_proxy_supplier(proxyID.value));
			//consumerReady();
		} catch (AdminNotFound e) {
			//do something here;
		} catch (ProxyNotFound e) {
			//do something here;
		}
		try {
			m_channel.set_qos(m_helper.getChannelProperties().
					getCDBQoSProps(m_channelName));
			m_channel.set_admin(m_helper.getChannelProperties().
					getCDBAdminProps(m_channelName));
		} catch (UnsupportedQoS e) {
			e.printStackTrace();
		} catch (AcsJException e) {
			e.printStackTrace();
		} catch (UnsupportedAdmin e) {
			e.printStackTrace();
		}
		
	}

}
