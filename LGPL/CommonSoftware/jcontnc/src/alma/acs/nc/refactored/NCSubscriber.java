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

import gov.sandia.NotifyMonitoringExt.ConsumerAdmin;
import gov.sandia.NotifyMonitoringExt.ConsumerAdminHelper;
import gov.sandia.NotifyMonitoringExt.EventChannel;
import gov.sandia.NotifyMonitoringExt.EventChannelFactory;
import gov.sandia.NotifyMonitoringExt.NameAlreadyUsed;
import gov.sandia.NotifyMonitoringExt.NameMapError;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.IntHolder;
import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosEventComm.Disconnected;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNotification.EventType;
import org.omg.CosNotification.StructuredEvent;
import org.omg.CosNotification.UnsupportedAdmin;
import org.omg.CosNotification.UnsupportedQoS;
import org.omg.CosNotifyChannelAdmin.AdminLimitExceeded;
import org.omg.CosNotifyChannelAdmin.AdminNotFound;
import org.omg.CosNotifyChannelAdmin.ClientType;
import org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplierHelper;
import org.omg.CosNotifyComm.InvalidEventType;
import org.omg.CosNotifyFilter.ConstraintExp;
import org.omg.CosNotifyFilter.Filter;
import org.omg.CosNotifyFilter.FilterFactory;
import org.omg.CosNotifyFilter.FilterNotFound;

import alma.ACSErrTypeCORBA.wrappers.AcsJCORBAReferenceNilEx;
import alma.ACSErrTypeCORBA.wrappers.AcsJNarrowFailedEx;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeJavaNative.wrappers.AcsJJavaAnyEx;
import alma.AcsNCTraceLog.LOG_NC_ConsumerAdminObtained_OK;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_FAIL;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_HandlerException;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_NoHandler;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_OK;
import alma.AcsNCTraceLog.LOG_NC_ProcessingTimeExceeded;
import alma.AcsNCTraceLog.LOG_NC_SubscriptionConnect_FAIL;
import alma.AcsNCTraceLog.LOG_NC_SubscriptionConnect_OK;
import alma.AcsNCTraceLog.LOG_NC_SupplierProxyCreation_FAIL;
import alma.AcsNCTraceLog.LOG_NC_SupplierProxyCreation_OK;
import alma.AcsNCTraceLog.LOG_NC_TaoExtensionsSubtypeMissing;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.MultipleRepeatGuard;
import alma.acs.logging.RepeatGuard.Logic;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.AcsNcReconnectionCallback;
import alma.acs.nc.AnyAide;
import alma.acs.nc.Helper;
import alma.acs.nc.ReconnectableSubscriber;
import alma.acs.ncconfig.EventDescriptor;
import alma.acs.util.StopWatch;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;
import alma.acsnc.OSPushConsumer;
import alma.acsnc.OSPushConsumerHelper;
import alma.acsnc.OSPushConsumerPOA;

/**
 * NCSubscriber is the Java implementation of the Notification Channel subscriber.
 * <p>
 * This class is used to receive events asynchronously from notification channel suppliers. 
 * It is the replacement of {@link alma.acs.nc.Consumer}, and to keep things simple no longer supports the inheritance mode, 
 * but instead supports type-safe delegation of incoming calls to a user-supplied handler.
 * <p>
 * The lifecycle steps are:
 * <ul>
 *   <li>During creation of an NCSubscriber, the NC and consumer admin  objects are either created or reused,
 *        and a proxy supplier object is created, all inside the notify service. <br>
 *        The reason for creating the proxy supplier (and the other objects along) already at this stage is 
 *        to support event filtering on the server side, with {@link Filter} objects getting attached to the 
 *        proxy supplier, see {@link #addSubscription(alma.acs.nc.AcsEventSubscriber.Callback)}. <br>
 *        TODO: This implementation could be changed to create the server-side filters on demand (e.g. in startReceivingEvents),
 *        so that addSubscription only stores the event type information without yet creating the filter.
 *   <li>Handlers for specialized events ({@link #addSubscription(alma.acs.nc.AcsEventSubscriber.Callback)})
 *       and/or for all events ({@link #addGenericSubscription(alma.acs.nc.AcsEventSubscriber.GenericCallback)}) can be registered.
 *   <li>Once {@link #startReceivingEvents()} is called, Corba NCs push events to this class, which delegates 
 *       the events to the registered handlers.
 *   <li>blabla... suspend...resume... disconnect
 *   >li>
 * </ul> 
 * The NCSubscriber is intended to be created (and cleaned up if needed) through the container services, 
 * for which we still need some refactoring. At the moment it is still under development and not used in operational code.
 * 
 * @author jslopez, hsommer
 */
public class NCSubscriber extends OSPushConsumerPOA implements AcsEventSubscriber, ReconnectableSubscriber {
	
	/**
	 * The default maximum amount of time an event handler is given to process
	 * event before a warning message is logged. The time unit is floating point seconds.
	 * Here we cache the default value defined in EventChannel.xsd, using an XSD binding class.
	 */
	private static final double DEFAULT_MAX_PROCESS_TIME_SECONDS = (new EventDescriptor()).getMaxProcessTime();

	/**
	 * A name that identifies the client of this NCSubscriber, to be used
	 * both for logging and also (if possible) as the supplier proxy name so that 
	 * looking at the proxy objects of the NC we can figure out who the clients are.
	 */
	private final String clientName;

	/** Used to time the execution of receive methods */
	private final StopWatch profiler;

	/** Provides access to the ACS logging system. */
	protected final Logger logger;

	/** Provides access to the notify service and CDB, creates NCs, etc */
	protected final Helper helper;

	/**
	 * There can be only one notification channel for any given subscriber.
	 * The NC is created on demand. 
	 * Already in the constructor of this class, the NC's admin object and proxy supplier objects are created or reused. 
	 */
	protected EventChannel channel;

	/** The channel has exactly one name registered in the CORBA Naming Service. */
	protected final String channelName;

	/** The channel notification service domain name, can be <code>null</code>. */
	protected final String channelNotifyServiceDomainName;

	/**
	 * The consumer admin object attached to the NC,
	 * which is used by subscribers to get a reference to the structured supplier proxy.
	 * This reference is <code>null</code> when the subscriber is not connected to a NC.
	 * <p>
	 * The TAO extensions allow us to set a meaningful name for the admin object, but it 
	 * probably will not be used as the ID, but as a separate name field.
	 * You can get the consumer admin object ID from here, see {@link ConsumerAdmin#MyID()}. 
	 * (In the NC spec, it says "The MyID attribute is a readonly attribute that maintains 
	 * the unique identifier of the target ConsumerAdmin instance, which is assigned to it 
	 * upon creation by the Notification Service event channel.) It is an integer type, which makes 
	 * it necessarily different from the name used with the TAO extensions.
	 * <p>
	 * We try to reuse a single admin object, to not allocate a new thread in the notification service for every subscriber.
	 */
	protected ConsumerAdmin sharedConsumerAdmin;

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
	 * <p>
	 * key = the event type (IDL-defined struct, which is an {@link IDLEntity} subclass). <br>
	 * value = the matching event handler.
	 */
	protected final Map<Class<? extends IDLEntity>, AcsEventSubscriber.Callback<? extends IDLEntity>> receivers = 
							new HashMap<Class<? extends IDLEntity>, AcsEventSubscriber.Callback<? extends IDLEntity>>();

	/**
	 * Contains a list of the added and removed subscriptions filters applied.
	 * Events on this list will be processed to check if the event should be
	 * accepted or discarded.
	 */
	protected final Map<String, Integer> subscriptionsFilters = new HashMap<String, Integer>();

	/**
	 * Contains a list of repeat guards for each different type of event.
	 */
	protected final MultipleRepeatGuard processTimeLogRepeatGuard;

	private AcsNcReconnectionCallback channelReconnectionCallback;

	private final ReentrantLock disconnectLock = new ReentrantLock();


	/**
	 * Creates a new instance of NCSubscriber.
	 * Normally an ACS class such as container services will act as the factory for NCSubscriber objects, 
	 * but for exceptional cases it is also possible to create one stand-alone, 
	 * as long as the required parameters can be provided.
	 * 
	 * @param channelName
	 *            Subscribe to events on this channel registered in the CORBA
	 *            Naming Service. If the channel does not exist, it's
	 *            registered.
	 * @param channelNotifyServiceDomainName
	 *            Channel domain name, which is being used to determine the 
	 *            notification service that should host the NC.
	 *            Passing <code>null</code> results in the default notify service "NotifyEventChannelFactory" being used.
	 * @param services
	 *            To get ACS logger, access to the CDB, etc.
	 * @param namingService
	 *            Must be passed explicitly, instead of the old hidden approach via <code>ORBInitRef.NameService</code> property.
	 * @throws AcsJException
	 *             Thrown on any <I>really bad</I> error conditions encountered.
	 */
	public NCSubscriber(String channelName, String channelNotifyServiceDomainName, 
			ContainerServicesBase services, NamingContext namingService, String clientName) 
			throws AcsJException {
		
		if (channelName == null) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setParameter("channelName");
			ex.setParameterValue("null");
			throw ex;
		}

		if (services == null) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setParameter("services");
			ex.setParameterValue("null");
			throw ex;
		}

		if (namingService == null) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setParameter("namingService");
			ex.setParameterValue("null");
			throw ex;
		}

		this.channelName = channelName;
		this.channelNotifyServiceDomainName = channelNotifyServiceDomainName;
		this.clientName = clientName;
		
		profiler = new StopWatch();
		logger = services.getLogger();

		anyAide = new AnyAide(services);
		helper = new Helper(services, namingService);

		// get the channel
		channel = helper.getNotificationChannel(channelName, getChannelKind(), getNotificationFactoryName());
		if (channel == null) {
			AcsJCORBAReferenceNilEx ex = new AcsJCORBAReferenceNilEx();
			ex.setContext("Null reference obtained for the notification channel " + channelName);
			ex.setVariable("channel");
			throw ex;
		}

		// populate the map with the maxProcessTime an event receiver processing should take
		handlerTimeoutMap = helper.getEventHandlerTimeoutMap(this.channelName);

		// get the admin object
		sharedConsumerAdmin = getSharedAdmin();

		// get the proxy Supplier
		proxySupplier = createProxySupplier();

		isTraceNCEventsEnabled = helper.getChannelProperties().isTraceEventsEnabled(this.channelName);

		// @TODO Set more realistic guarding parameters, e.g. max 1 identical log in 10 seconds
		processTimeLogRepeatGuard = new MultipleRepeatGuard(0, TimeUnit.SECONDS, 1, Logic.COUNTER, 100);

		// if the factory is null, the reconnection callback is not registered
		channelReconnectionCallback = new AcsNcReconnectionCallback(this);
		channelReconnectionCallback.init(services, helper.getNotifyFactory());
	}

	/**
	 * Creates or reuses a shared server-side NC consumer admin object.
	 * <p>
	 * @TODO Investigate how the (TAO) NotifyService allocates threads to ConsumerAdmin objects,
	 *       and try to optimize this by using a single ConsumerAdmin (what we do now) or a few such instances.
	 * 
	 * @throws AcsJException
	 */
	private ConsumerAdmin getSharedAdmin() throws AcsJCORBAProblemEx, AcsJCORBAReferenceNilEx, AcsJNarrowFailedEx {
		
		ConsumerAdmin ret = null;
		org.omg.CosNotifyChannelAdmin.ConsumerAdmin retBase = null;
		
		int consumerAdminIds[] = channel.get_all_consumeradmins();
		int consumerAdminId = -1;
		boolean created = false;
		if (consumerAdminIds.length == 0) {
			// create an NC admin object
			IntHolder consumerAdminIDHolder = new IntHolder();
			retBase = channel.new_for_consumers(InterFilterGroupOperator.AND_OP, consumerAdminIDHolder);
			consumerAdminId = consumerAdminIDHolder.value;
			created = true;
		}
		else {
			try {
				// reuse existing admin object
				retBase = channel.get_consumeradmin(consumerAdminIds[0]);
				consumerAdminId = consumerAdminIds[0];
				if (consumerAdminIds.length > 1) {
					// @TODO Log a warning if in the future we decide that there should be only a single consumer admin object per NC.
					//       This would mean that
					//       - all code uses the new NC libs that reuse admin objects (the old Consumer classes legally use many admin objects for an NC object)
					//       - we do not need multiple admin objects for thread optimization or per-subscriber-group configurations
					//       - there cannot be race conditions which may sometimes lead to erroneous but harmless creation of a few admin objects.
				}
			} catch (AdminNotFound e) {
				AcsJCORBAProblemEx e2 = new AcsJCORBAProblemEx();
				e2.setInfo("The attempt to reuse the admin object for channel '" + channelName + "' failed: " + e.toString());
				throw e2;
			}
		}
		if (retBase == null) {
			AcsJCORBAReferenceNilEx ex = new AcsJCORBAReferenceNilEx();
			ex.setVariable(getClass().getName() + "#getSharedAdmin :: retBase");
			ex.setContext("Failed to create or retrieve consumer admin object for channel '" + channelName + "'.");
			throw ex;
		}

		try {
			ret = ConsumerAdminHelper.narrow(retBase);
		} catch (BAD_PARAM ex) {
			if (created) {
				// @TODO destroy the admin obj
			}
			LOG_NC_TaoExtensionsSubtypeMissing.log(logger, "ConsumerAdmin for channel " + channelName, ConsumerAdminHelper.id(), org.omg.CosNotifyChannelAdmin.ConsumerAdminHelper.id());
			AcsJNarrowFailedEx ex2 = new AcsJNarrowFailedEx(ex);
			ex2.setNarrowType(ConsumerAdminHelper.id());
			throw ex2;
		}
		
		LOG_NC_ConsumerAdminObtained_OK.log(logger, consumerAdminId, (created ? "created" : "reused"), clientName, channelName, getNotificationFactoryName());
		
		return ret;
	}

	/**
	 * Creates the proxy supplier (push-style, for structured events) 
	 * that lives in the Notify server process, managed by the consumer admin object, and
	 * will later be connected to this client-side subscriber object.
	 * 
	 * @throws AcsJCORBAProblemEx If creation of the proxy supplier failed.
	 */
	private StructuredProxyPushSupplier createProxySupplier() throws AcsJCORBAProblemEx {
		StructuredProxyPushSupplier ret = null;
		String errMsg = null;
		IntHolder proxyIdHolder = new IntHolder(); // will get assigned "a numeric identifier [...] that is unique among all proxy suppliers [the admin object] has created"
		try {
			ret = StructuredProxyPushSupplierHelper.narrow(
						// Providing a name is a TAO extension. Otherwise call "obtain_notification_push_supplier"
						sharedConsumerAdmin.obtain_named_notification_push_supplier(ClientType.STRUCTURED_EVENT, proxyIdHolder, clientName)
					);
		} catch (AdminLimitExceeded ex) {
			// See NC spec 3.4.15.10
			// If the number of consumers currently connected to the channel with which the target ConsumerAdmin object is associated 
			// exceeds the value of the MaxConsumers administrative property, the AdminLimitExceeded exception is raised.
			String limit = ex.admin_property_err.value.extract_string();
			errMsg = "NC '" + channelName + "' is configured for a maximum of " + limit + 
					" subscribers, which does not allow this client to subscribe."; 
		} catch (NameAlreadyUsed ex) {
			errMsg = "This subscriber client already owns a proxy supplier on that NC.";
			// @TODO append/increment a number to find a unique clientName, and try again.
		} catch (NameMapError ex) {
			errMsg = ex.toString(); // TODO understand when this gets thrown, and refine the message accordingly.
		}

		if (ret == null) {
			LOG_NC_SupplierProxyCreation_FAIL.log(logger, clientName, channelName, getNotificationFactoryName(), errMsg);
			AcsJCORBAProblemEx ex2 = new AcsJCORBAProblemEx();
			ex2.setInfo("Failed to create proxy supplier on NC '" + channelName + "' for client '" + clientName + "': " + errMsg);
			throw ex2;
		}
		LOG_NC_SupplierProxyCreation_OK.log(logger, proxyIdHolder.value, clientName, channelName, getNotificationFactoryName());
		return ret;
	}

	/**
	 * After invoking this method, the user has no control over when
	 * push_structured_event is invoked by the notification channel. User may
	 * still add and remove subscriptions at any given time though. Also, the
	 * connection can be suspended and resumed.
	 * 
	 * <b>If this method is not called, no event will be received</b>
	 * <p>
	 * @TODO Try to defer contacting the NC and creating server-side objcts until this method gets called.
	 *  
	 * @throws AcsJException
	 *             Thrown if the consumer cannot begin receiving events for some
	 *             CORBA reason.
	 */
	@Override
	public void startReceivingEvents() throws AcsJException {

		try {
			corbaRef = OSPushConsumerHelper.narrow(helper.getContainerServices().activateOffShoot(this));
			proxySupplier.connect_structured_push_consumer(org.omg.CosNotifyComm.StructuredPushConsumerHelper.narrow(corbaRef));
		} catch (AcsJContainerServicesEx e) {
			LOG_NC_SubscriptionConnect_FAIL.log(logger, channelName, getNotificationFactoryName());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(e);
		} catch (org.omg.CosEventChannelAdmin.AlreadyConnected e) {
			LOG_NC_SubscriptionConnect_FAIL.log(logger, channelName, getNotificationFactoryName());
			Throwable cause = new Throwable(e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		} catch (org.omg.CosEventChannelAdmin.TypeError e) {
			LOG_NC_SubscriptionConnect_FAIL.log(logger, channelName, getNotificationFactoryName());
			Throwable cause = new Throwable(e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		}

		LOG_NC_SubscriptionConnect_OK.log(logger, channelName, getNotificationFactoryName());
	}

	/**
	 * Called from Corba (via push_structured_event).
	 * The implementation cannot assume any particular event type.
	 * <p>
	 * No exception is allowed to be thrown by this method, even if the receiver implementation throws a RuntimeExecption
	 */
	protected void processEvent(IDLEntity corbaData, EventDescription eventDescrip) {
		
		Class<? extends IDLEntity> eventType = corbaData.getClass();
		String eventName = eventType.getName();

		// figure out how much time this event has to be processed
		if (!handlerTimeoutMap.containsKey(eventName)) {
			// setup a timeout if it's undefined
			handlerTimeoutMap.put(eventName, DEFAULT_MAX_PROCESS_TIME_SECONDS);
		}
//System.out.println("Using handlerTimeout=" + handlerTimeoutMap.get(eventName) + " for event " + eventName);
		double maxProcessTimeSeconds = handlerTimeoutMap.get(eventName);

		// we give preference to a receiver that has registered for this concrete subtype of IDLEntity
		if (receivers.containsKey(eventType)) {
			AcsEventSubscriber.Callback<? extends IDLEntity> receiver = receivers.get(eventType);

			profiler.reset();
			try {
				_process(receiver, corbaData, eventDescrip);
			} 
			catch (Throwable thr) {
				LOG_NC_EventReceive_HandlerException.log(logger, channelName, 
						getNotificationFactoryName(), eventName, 
						receiver.getClass().getName(), thr.toString());
			}
			double usedSecondsToProcess = (profiler.getLapTimeMillis() / 1000.0);

			// warn the end-user if the receiver is taking too long, using a repeat guard
			if (usedSecondsToProcess > maxProcessTimeSeconds && processTimeLogRepeatGuard.checkAndIncrement(eventName)) {
				LOG_NC_ProcessingTimeExceeded.log(logger, channelName,
						getNotificationFactoryName(), eventName,
						processTimeLogRepeatGuard.counterAtLastExecution(eventName));
			}
		} 
		// fallback to generic receive method
		else if (genericReceiver != null) {
			
			// start timing
			profiler.reset();
			genericReceiver.receive(corbaData, eventDescrip);
			double usedSecondsToProcess = (profiler.getLapTimeMillis() / 1000.0);

			// warn the end-user if the receiver is taking too long 
			if (usedSecondsToProcess > maxProcessTimeSeconds && processTimeLogRepeatGuard.checkAndIncrement(eventName)) {
				LOG_NC_ProcessingTimeExceeded.log(logger, channelName,
						getNotificationFactoryName(), eventName,
						processTimeLogRepeatGuard.counterAtLastExecution(eventName));
			}
		} 
		// no receiver found 
		// TODO: Check if the filtering for the subscribed events happens now really on the server side. 
		// If so, then we should never get an event for which there is no receiver, and should thus
		// log an error regardless of isTraceNCEventsEnabled
		else {
			if (isTraceNCEventsEnabled) {
				LOG_NC_EventReceive_NoHandler.log(logger, channelName, getNotificationFactoryName(), eventName);
			}
		}
	}

	/**
	 * "Generic helper method" to enforce type argument inference by the compiler,
	 * see http://www.angelikalanger.com/GenericsFAQ/FAQSections/ProgrammingIdioms.html#FAQ207
	 */
	private <T extends IDLEntity> void _process(AcsEventSubscriber.Callback<T> receiver, IDLEntity corbaData, EventDescription eventDescrip) {
		T castCorbaData = null;
		try {
			castCorbaData = receiver.getEventType().cast(corbaData);
		}
		catch (ClassCastException ex) {
			// This should never happen and would be an ACS error
			logger.warning("Failed to deliver incompatible data '" + corbaData.getClass().getName() + 
					"' to subscriber '" + receiver.getEventType().getName() + "'. Fix data subscription handling in " + getClass().getName() + "!");
		} 
		// user code errors (runtime ex etc) we let fly up
		receiver.receive(castCorbaData, eventDescrip);
	}


	/**
	 * Subscribes to all events. The latest generic receiver displaces the previous one. 
	 * <p>
	 * If in addition to this generic subscription we also have specific subscriptions via
	 * {@link #addSubscription(Class, alma.acs.nc.AcsEventSubscriber.Callback)},
	 * then those more specific subscriptions will take precedence in receiving an event.
	 * <p>
	 * Notice though that any server-side filters previously created for the event type specific 
	 * subscriptions get deleted when calling this method, so that even after removing a generic 
	 * subscription the network performance gain of server-side filtering is lost. 
	 * @TODO: Couldn't this be fixed by creating the server-side filters on demand (see also javadoc class comment about lifecycle)?
	 */
	@Override
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
	@Override
	public void removeGenericSubscription() throws AcsJCORBAProblemEx {
		if (genericReceiver == null) {
			throw new IllegalStateException("Failed to remove generic subscription when not actually subscribed.");
		}
		genericReceiver = null;

		// If receivers is empty we just discard everything
		if (receivers.isEmpty())
			discardAllEvents();
		else {
			// Since we have specific subscriptions, we have to apply the corresponding filters
			for (Class<?> eventType : receivers.keySet()) {
				String keyName = eventType.getName();
				String filter = keyName.substring(keyName.lastIndexOf('.') + 1);
				subscriptionsFilters.put(keyName, addFilter(filter));
			}
		}
	}

	/**
	 * Adds a subscription to a given type of event through filters. 
	 * <p>
	 * The main advantage of using java generics here: Compiler ensures that receiver method 
	 * is consistent with event type.
	 * 
	 * @throws AcsJException
	 *             Thrown if there is some CORBA problem.
	 */
	@Override
	public void addSubscription(AcsEventSubscriber.Callback<? extends IDLEntity> receiver) 
			throws AcsJException {

		// TODO Runtime type checks, to avoid NPE and raw type issues
		
		Class<? extends IDLEntity> structClass = receiver.getEventType();

		// Adding the subscription to the receivers list
		receivers.put(structClass, receiver);

		// Adding the filter to the proxy only if there is not generic handler
		if (genericReceiver == null) {
			subscriptionsFilters.put(structClass.getName(),
					addFilter(structClass.getSimpleName()));
		}
	}

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
	@Override
	public void removeSubscription(Class<? extends IDLEntity> structClass)
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
	 * This method manages the filtering capabilities used to control subscriptions.
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

	/* TODO Notice that the consumer admin object is not being destroyed. For
	 * doing this we must check if there's no more clients using it. Check if
	 * it's possible using TAO extensions.
	 */
	@Override
	public void disconnect() throws IllegalStateException {
		disconnectLock.lock();
		boolean success = false;
		try {
			checkConnection(); // A possible IllegalStateException will be logged below
			
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
		catch (IllegalStateException ex) {
			throw ex;
		}
		catch (Exception ex2) {
			
		}
		finally {
			if (success) {
				// null the refs if everything was fine, or if we got the OBJECT_NOT_EXIST
				channelReconnectionCallback = null;
				corbaRef = null;
				sharedConsumerAdmin = null;
				proxySupplier = null;
				channel = null;
			}
			disconnectLock.unlock();
		}
	}

	public boolean isDisconnected() {
		disconnectLock.lock();
		boolean ret = (sharedConsumerAdmin == null);
		disconnectLock.unlock();
		return ret;
	}

	private void checkConnection() throws IllegalStateException {
		if (isDisconnected()) {
			throw new IllegalStateException("Consumer already disconnected");
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * <p>
	 * The design of this implementation follows CORBA NC standard, as described in 
	 * <it>Notification Service Specification, Version 1.1, formal/04-10-11, 3.4.13 The StructuredProxyPushSupplier Interface.</it>
	 */
	@Override
	public void suspend() {
		disconnectLock.lock();
		checkConnection();
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

	@Override
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
	 * accommodate subclasses which subscribe/publish non-ICD style events (ACS
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
		return helper.getNotificationFactoryNameForChannel(channelName, channelNotifyServiceDomainName);
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
	 * This method is called by the notification channel (supplier proxy) each time an event is received.
	 * <p>
	 * It is declared <code>final</code> because it is crucial for the functioning of the NC library
	 * and thus cannot be overwritten by a subclass. 
	 * If for special purposes a notification of raw event reception is needed, 
	 * a subclass can implement {@link #push_structured_event_called()}, which gets called from this
	 * method as the first thing it does.
	 * 
	 * @param structuredEvent
	 *            The structured event sent by a supplier.
	 * @throws Disconnected If this subscriber is disconnected from the NC.
	 * 
	 * @see org.omg.CosNotifyComm.StructuredPushConsumerOperations#push_structured_event(org.omg.CosNotification.StructuredEvent)
	 */
	@Override
	public final void push_structured_event(StructuredEvent structuredEvent)
			throws Disconnected {
		
		try {
			push_structured_event_called(structuredEvent);
		} catch (Throwable thr) {
			// ignore any exception, since push_structured_event_called is only meant for 
			// notification, to enable special tests or other exotic purposes.
		}

		if (isDisconnected()) {
			throw new Disconnected();
		}

		EventDescription eDescrip = EventDescriptionHelper.extract(structuredEvent.remainder_of_body);

		Object convertedAny = anyAide.complexAnyToObject(structuredEvent.filterable_data[0].value);

		if (convertedAny == null || !(convertedAny instanceof IDLEntity)) {
			// @TODO: compare with ACS-NC specs and C++ impl, and perhaps call generic receiver with null data,
			//        if the event does not carry any data.
			// @TODO: check that we do not somewhere in ACS allow also *sequences of IDL structs* as event data,
			//        because here they will lead to this error.
			String badDataType = ( convertedAny == null ? "null" : convertedAny.getClass().getName() );
			LOG_NC_EventReceive_FAIL.log(
					logger,
					channelName,
					getNotificationFactoryName(),
					structuredEvent.header.fixed_header.event_type.type_name,
					badDataType);
		}
		else {
			// got good event data, will give it to the registered receiver
			IDLEntity struct = (IDLEntity) convertedAny;
			
			if (isTraceNCEventsEnabled) {
				LOG_NC_EventReceive_OK.log(
						logger,
						channelName,
						getNotificationFactoryName(),
						structuredEvent.header.fixed_header.event_type.type_name);
			}

			// process the extracted data
			processEvent(struct, eDescrip);
		}
	}

	protected void push_structured_event_called(StructuredEvent structuredEvent) {
		//System.out.println("********** got a call to push_structured_event **********");
	}

	/**
	 * ACS does not provide an implementation of this method.
	 * 
	 * @see org.omg.CosNotifyComm.StructuredPushConsumerOperations#disconnect_structured_push_consumer()
	 * @throws NO_IMPLEMENT
	 */
	@Override
	public void disconnect_structured_push_consumer() {
		throw new NO_IMPLEMENT();

	}

	/**
	 * ACS does not provide an implementation of this method.
	 * 
	 * @see org.omg.CosNotifyComm.NotifyPublishOperations#offer_change(org.omg.CosNotification.EventType[],
	 *      org.omg.CosNotification.EventType[])
	 * @throws NO_IMPLEMENT
	 */
	@Override
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