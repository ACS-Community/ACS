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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;
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
import org.omg.CosNotifyChannelAdmin.ProxyNotFound;
import org.omg.CosNotifyChannelAdmin.ProxySupplier;
import org.omg.CosNotifyChannelAdmin.ProxyType;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplier;
import org.omg.CosNotifyChannelAdmin.StructuredProxyPushSupplierHelper;
import org.omg.CosNotifyComm.InvalidEventType;
import org.omg.CosNotifyFilter.ConstraintExp;
import org.omg.CosNotifyFilter.Filter;
import org.omg.CosNotifyFilter.FilterFactory;
import org.omg.CosNotifyFilter.FilterNotFound;

import gov.sandia.NotifyMonitoringExt.ConsumerAdmin;
import gov.sandia.NotifyMonitoringExt.ConsumerAdminHelper;
import gov.sandia.NotifyMonitoringExt.EventChannel;
import gov.sandia.NotifyMonitoringExt.EventChannelFactory;
import gov.sandia.NotifyMonitoringExt.NameAlreadyUsed;
import gov.sandia.NotifyMonitoringExt.NameMapError;

import alma.ACSErrTypeCORBA.wrappers.AcsJNarrowFailedEx;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;
import alma.AcsNCTraceLog.LOG_NC_ConsumerAdminObtained_OK;
import alma.AcsNCTraceLog.LOG_NC_ConsumerAdmin_Overloaded;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_FAIL;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_HandlerException;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_NoHandler;
import alma.AcsNCTraceLog.LOG_NC_EventReceive_OK;
import alma.AcsNCTraceLog.LOG_NC_ProcessingTimeExceeded;
import alma.AcsNCTraceLog.LOG_NC_ReceiverTooSlow;
import alma.AcsNCTraceLog.LOG_NC_SubscriptionConnect_FAIL;
import alma.AcsNCTraceLog.LOG_NC_SubscriptionConnect_OK;
import alma.AcsNCTraceLog.LOG_NC_SupplierProxyCreation_FAIL;
import alma.AcsNCTraceLog.LOG_NC_SupplierProxyCreation_OK;
import alma.AcsNCTraceLog.LOG_NC_TaoExtensionsSubtypeMissing;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.AcsLogLevel;
import alma.acs.nc.AcsEventSubscriber;
import alma.acs.nc.AcsEventSubscriberImplBase;
import alma.acs.nc.AcsNcReconnectionCallback;
import alma.acs.nc.AnyAide;
import alma.acs.nc.Helper;
import alma.acs.nc.ReconnectableParticipant;
import alma.acs.ncconfig.EventDescriptor;
import alma.acsErrTypeLifeCycle.wrappers.AcsJEventSubscriptionEx;
import alma.acsnc.EventDescription;
import alma.acsnc.EventDescriptionHelper;
import alma.acsnc.OSPushConsumer;
import alma.acsnc.OSPushConsumerHelper;
import alma.acsnc.OSPushConsumerOperations;
import alma.acsnc.OSPushConsumerPOA;
import alma.acsnc.OSPushConsumerPOATie;

/**
 * NCSubscriber is the Java implementation of the Notification Channel subscriber,
 * while following the more generic {@link AcsEventSubscriber} interface.
 * <p>
 * This class is used to receive events asynchronously from notification channel suppliers. 
 * It is the replacement of {@link alma.acs.nc.Consumer}, and to keep things simple it no longer
 * supports the inheritance mode, but instead supports type-safe delegation of incoming calls to
 * a user-supplied handler.
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
 *   <li>The connection can then be suspended or resumed.
 * </ul> 
 * The NCSubscriber gets created (and cleaned up if needed) through the container services. 
 * Note about refactoring: NCSubscriber gets instantiated in module jcont using java reflection.
 * Thus if you change the package, name, or constructor of this class, make sure to fix the corresponding "forName" call in jcont.
 * 
 * @param <T>  See base class.
 * 
 * @author jslopez, hsommer, rtobar
 */
public class NCSubscriber<T extends IDLEntity> extends AcsEventSubscriberImplBase<T> 
		implements OSPushConsumerOperations, ReconnectableParticipant {
	
	/**
	 * The default maximum amount of time an event handler is given to process
	 * event before a warning message is logged. The time unit is floating point seconds.
	 * Here we cache the default value defined in EventChannel.xsd, using an XSD binding class.
	 */
	private static final double DEFAULT_MAX_PROCESS_TIME_SECONDS = (new EventDescriptor()).getMaxProcessTime();

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
	 * The TAO extensions would allow us to set a meaningful name for the admin object, but it 
	 * still does not get used as the ID, but as a separate name field.
	 * You can get the consumer admin object ID from here, see {@link ConsumerAdmin#MyID()}. 
	 * (In the NC spec, it says "The MyID attribute is a readonly attribute that maintains 
	 * the unique identifier of the target ConsumerAdmin instance, which is assigned to it 
	 * upon creation by the Notification Service event channel.) It is an integer type, which makes 
	 * it necessarily different from the name used with the TAO extensions.
	 * <p>
	 * We try to reuse an admin object for a limited number of subscribers, 
	 * to not allocate a new thread in the notification service for every subscriber
	 * but instead get a flexible thread::subscriber mapping.
	 * 
	 * @see #PROXIES_PER_ADMIN
	 */
	protected ConsumerAdmin sharedConsumerAdmin;

	/**
	 * Maximum number of proxies (subscribers) per admin object.
	 * @see #sharedConsumerAdmin
	 */
	protected static final int PROXIES_PER_ADMIN = 5;

	/** 
	 * The supplier proxy we are connected to. 
	 */
	protected StructuredProxyPushSupplier proxySupplier;

	/**
	 * The tie poa wrapped around this object, so that we can receive event data over corba.
	 */
	private OSPushConsumerPOA corbaObj; 

	/**
	 * Like {@link #corbaObj}, but activated.
	 */
	private OSPushConsumer corbaRef;

	/** 
	 * Helper class used to manipulate CORBA anys. 
	 */
	protected AnyAide anyAide;

	/** 
	 * Whether receiving events should be logged. 
	 */
	private final boolean isTraceNCEventsEnabled;

	/**
	 * Maps event names to the maximum amount of time allowed for receiver
	 * methods to complete. Time is given in floating point seconds.
	 */
	protected final HashMap<String, Double> handlerTimeoutMap;

	/**
	 * Contains a list of the added and removed subscription filters applied.
	 * <p>
	 * TODO: Does this sentence make sense? Events on this list will be processed to check if the event should be
	 * accepted or discarded.
	 */
	protected final Map<String, Integer> subscriptionsFilters = new HashMap<String, Integer>();

	/**
	 * Supports reconnection after service restart, see TAO's "topology persistence" extension.
	 * @see #reconnect(EventChannelFactory)
	 * @see NotifyExt.ReconnectionCallbackOperations
	 */
	private AcsNcReconnectionCallback channelReconnectionCallback;

	/**
	 * We log it only once if {@link #push_structured_event_called(StructuredEvent)}
	 * vetoes down the regular event processing by this NCSubscriber.
	 */
	private boolean firstSubclassVeto = true;

	
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
	 * @param clientName
	 * @param eventType Our type parameter, either <code>IDLEntity</code> as base type or a concrete IDL-defined struct.
	 * @throws AcsJException
	 *             Thrown on any <I>really bad</I> error conditions encountered.
	 */
	public NCSubscriber(String channelName, String channelNotifyServiceDomainName, 
			ContainerServicesBase services, NamingContext namingService, String clientName, Class<T> eventType) 
			throws AcsJException {
		
		super(services, clientName, eventType);
		
		// This class will be instantiated through reflection, with an ugly cast,
		// so that in spite of the declaration "NCSubscriber<T extends IDLEntity>" we must verify that eventType is an IDLEntity.
		if (!IDLEntity.class.isAssignableFrom(eventType)) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setParameter("eventType");
			ex.setParameterValue(eventType.getName());
			ex.setReason("For NCSubscriber, 'eventType' must be (a subtype of) IDLEntity.");
			throw ex;
		}

		if (channelName == null) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setParameter("channelName");
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

		anyAide = new AnyAide(services);
		helper = new Helper(services, namingService);

		// populate the map with the maxProcessTime an event receiver processing should take
		handlerTimeoutMap = helper.getEventHandlerTimeoutMap(this.channelName);

		isTraceNCEventsEnabled = helper.getChannelProperties().isTraceEventsEnabled(this.channelName);

		// this call is mandatory, see base class ctor comment.
		// It will lead to a call to 'EnvironmentActionHandler#create', 
		// see 'createEnvironmentActionHandler' below.
		stateMachineSignalDispatcher.setUpEnvironment();
	}

	
	////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////// State machine actions //////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////
	
	protected void createEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
				throws AcsJStateMachineActionEx {
		
		super.createEnvironmentAction(evtDispatcher, errRep, scInstance, derivedEvents);
		try {

			// get the channel
			channel = helper.getNotificationChannel(channelName, getChannelKind(), getNotificationFactoryName());

			// get the admin object
			
			// Note that admin creation and proxy supplier creation are not synchronized across subscribers,
			// which means that concurrent creation of subscribers can lead to race conditions
			// where we end up with too many (> PROXIES_PER_ADMIN) subscribers
			// for the same admin object.
			// It would be easy to put a static lock around these two calls, which would take care of 
			// concurrent subscribers from the same component or client. Still there would be the same 
			// racing issues coming from distributed subscribers.
			// We prefer to not even do local synchronization because then even in simple unit tests
			// from a single process we can verify the concurrency behavior of subscribers and notifyService.
			// TODO: Revisit the "synchronized(NCSubscriber.class)" block we currently have inside getSharedAdmin(),
			// which is giving partial local synchronization, leading to fewer race conditions.
			// Probably should be removed, or pulled up here and extended around createProxySupplier.
			sharedConsumerAdmin = getSharedAdmin();
			
			// get the proxy Supplier
			proxySupplier = createProxySupplier();

			// Just check if our shared consumer admin is handling more proxies than it should, and log it
			// (11) goes for the dummy proxy that we're using the transition between old and new NC classes
			int currentProxies = sharedConsumerAdmin.push_suppliers().length - 1;
			if( currentProxies > PROXIES_PER_ADMIN )
				LOG_NC_ConsumerAdmin_Overloaded.log(logger, sharedConsumerAdmin.MyID(),
						currentProxies, PROXIES_PER_ADMIN, channelName, channelNotifyServiceDomainName == null ? "none" : channelNotifyServiceDomainName);

			// The user might create this object, and later call startReceivingEvents(), without attaching any receiver.
			// If so, it's useless to get all the events, so we start with an all-exclusive filter in the server
			discardAllEvents();
		} catch (Throwable thr) {
			throw new AcsJStateMachineActionEx(thr);
		}
	}
		
	protected void destroyEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
		
		try {
			if (proxySupplier != null) {
				// spec 3.3.10.1: "The disconnect_structured_push_supplier operation is invoked to terminate a
				// connection between the target StructuredPushSupplier and its associated consumer.
				// This operation takes no input parameters and returns no values. The result of this
				// operation is that the target StructuredPushSupplier will release all resources it had
				// allocated to support the connection, and dispose its own object reference."
				proxySupplier.disconnect_structured_push_supplier();
				proxySupplier = null;
				logger.info("Disconnected and destroyed the supplier proxy");
			}
		} catch (org.omg.CORBA.OBJECT_NOT_EXIST ex1) {
			// This is unexpected but OK, because someone else has already destroyed the remote resources
			logger.fine("No need to release resources for channel " + channelName
					+ " because the NC has been destroyed already.");
		} finally {
			// TODO: Should we not try to destroy an empty consumer admin object? 
			// Or is it too risky because of possible race conditions with newly created other subscribers,
			// given that we don't have a clear distributed locking strategy?
			sharedConsumerAdmin = null;
			channel = null;
		}
		
		super.destroyEnvironmentAction(evtDispatcher, errRep, scInstance, derivedEvents);
	}

	protected void createConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
		
		super.createConnectionAction(evtDispatcher, errRep, scInstance, derivedEvents);
		
		try {
			// Register callback for subscribed events
			if (corbaRef == null) {
				corbaObj = new OSPushConsumerPOATie(NCSubscriber.this);
				corbaRef = OSPushConsumerHelper.narrow(helper.getContainerServices().activateOffShoot(corbaObj));
			}
			// Register callback for reconnection requests
			channelReconnectionCallback = new AcsNcReconnectionCallback(NCSubscriber.this, logger);
			channelReconnectionCallback.registerForReconnect(services, helper.getNotifyFactory()); // if the factory is null, the reconnection callback is not registered

			proxySupplier.connect_structured_push_consumer(org.omg.CosNotifyComm.StructuredPushConsumerHelper.narrow(corbaRef));
		} catch (AcsJContainerServicesEx e) {
			LOG_NC_SubscriptionConnect_FAIL.log(logger, channelName, getNotificationFactoryName());
			throw new AcsJStateMachineActionEx(e);
		} catch (org.omg.CosEventChannelAdmin.AlreadyConnected e) {
			throw new AcsJStateMachineActionEx(new AcsJIllegalStateEventEx(e));
		} catch (org.omg.CosEventChannelAdmin.TypeError ex) {
			LOG_NC_SubscriptionConnect_FAIL.log(logger, channelName, getNotificationFactoryName());
			throw new AcsJStateMachineActionEx(ex);
		} catch (AcsJIllegalArgumentEx ex) {
			throw new AcsJStateMachineActionEx(ex);
		}

		LOG_NC_SubscriptionConnect_OK.log(logger, channelName, getNotificationFactoryName());
	}

	protected void destroyConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
		
		// TODO: CHeck if we need to suspend first (was like that in older impl)
		// try {
		// suspendAction(evtDispatcher, errRep, scInstance, derivedEvents);
		// } catch () {
		//
		// }

		/*
		 * TODO: (rtobar) Maybe this code can be written more nicely, but always taking care that, if not in an illegal
		 * state, then we should destroy the removed proxySupplier object
		 */
		boolean success = false;

		try {
			// Clean up callback for reconnection requests
			channelReconnectionCallback.disconnect();
			
			// remove all filters and destroy the proxy supplier
			proxySupplier.remove_all_filters();

			try {
				// Clean up callback for subscribed events
				if (corbaRef != null) { // this check avoids ugly "offshoot was not activated" messages in certain scenarios
					helper.getContainerServices().deactivateOffShoot(corbaObj);
				}
			} catch (AcsJContainerServicesEx e) {
				logger.log(Level.INFO, "Failed to Corba-deactivate NCSubscriber " + clientName, e);
			}

			logger.finer("Disconnected from NC '" + channelName + "'.");
			success = true;
		} catch (org.omg.CORBA.OBJECT_NOT_EXIST ex1) {
			// this is OK, because someone else has already destroyed the remote resources
			logger.fine("No need to release resources for channel " + channelName
					+ " because the NC has been destroyed already.");
			success = true;
//		} catch (Throwable thr) {
//			// TODO remove this hack
//			throw new AcsJStateMachineActionEx(thr);
		}
		finally {
			if (success) {
				// null the refs if everything was fine, or if we got the OBJECT_NOT_EXIST
				channelReconnectionCallback = null;
				corbaRef = null;
				corbaObj = null;
			}
		}
		
		super.destroyConnectionAction(evtDispatcher, errRep, scInstance, derivedEvents);
	}

	
	protected void suspendAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {

		super.suspendAction(evtDispatcher, errRep, scInstance, derivedEvents);
		try {
			// See OMG NC spec 3.4.13.2. Server will continue to queue events.
			proxySupplier.suspend_connection();
		} catch (org.omg.CosNotifyChannelAdmin.ConnectionAlreadyInactive ex) {
			throw new AcsJStateMachineActionEx(ex);
		} catch (org.omg.CosNotifyChannelAdmin.NotConnected ex) {
			throw new AcsJStateMachineActionEx(ex);
		} catch (org.omg.CORBA.OBJECT_NOT_EXIST ex) {
			throw new AcsJStateMachineActionEx("Remote resources already destroyed.", ex);
		}
	}

	protected void resumeAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {

		try {
			proxySupplier.resume_connection();
		} catch (org.omg.CosNotifyChannelAdmin.ConnectionAlreadyActive ex) {
			throw new AcsJStateMachineActionEx(ex);
		} catch (org.omg.CosNotifyChannelAdmin.NotConnected ex) {
			throw new AcsJStateMachineActionEx(ex);
		}
		super.resumeAction(evtDispatcher, errRep, scInstance, derivedEvents);
	}

	
	////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////// Various template method impls //////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////
	
	@Override
	protected boolean isTraceEventsEnabled() {
		return isTraceNCEventsEnabled;
	}
	
	@Override
	protected void logEventReceiveHandlerException(String eventName, String receiverClassName, Throwable thr) {
		LOG_NC_EventReceive_HandlerException.log(logger, channelName, 
				getNotificationFactoryName(), eventName, 
				receiverClassName, thr.toString());
	}
	
	@Override
	protected void logEventProcessingTimeExceeded(String eventName, long logOcurrencesNumber) {
		LOG_NC_ProcessingTimeExceeded.log(logger, channelName,
				getNotificationFactoryName(), eventName,
				logOcurrencesNumber);
	}
	
	@Override
	protected void logEventProcessingTooSlowForEventRate(long numEventsDiscarded, String eventName) {
		LOG_NC_ReceiverTooSlow.log(logger, clientName, numEventsDiscarded, eventName, channelName,
				getNotificationFactoryName());
	}
	
	@Override
	protected void logNoEventReceiver(String eventName) {
		// With server-side filtering set up, we should never get an unexpected event type.
		// Thus we log this problem.
		LOG_NC_EventReceive_NoHandler.log(logger, channelName, getNotificationFactoryName(), eventName);
	}

	@Override
	protected void logQueueShutdownError(int timeoutMillis, int remainingEvents) {
		logger.info("Disconnecting from NC '" + channelName + "' before all events have been processed, in spite of " +
				timeoutMillis + " 500 ms timeout grace period. " +
				remainingEvents+ " events are now still in the queue and may continue to be processed by the receiver.");
	}

	@Override
	protected double getMaxProcessTimeSeconds(String eventName) {
		if (!handlerTimeoutMap.containsKey(eventName)) {
			// setup a timeout if it's undefined
			handlerTimeoutMap.put(eventName, DEFAULT_MAX_PROCESS_TIME_SECONDS);
		}
//System.out.println("Using handlerTimeout=" + handlerTimeoutMap.get(eventName) + " for event " + eventName);
		double maxProcessTimeSeconds = handlerTimeoutMap.get(eventName);
		return maxProcessTimeSeconds;
	}
	


	/**
	 * @TODO: Unify treatment of event name. Currently we sometimes use qualified or unqualified names,
	 * which will create chaos if anyone uses similar event types with same name but different package. 
	 * Must be fixed across supplier and subscribers, taking into account IDL names vs. Java packages, 
	 * because also C++ events carry the name in the Corba structure.
	 * 
	 * @param structClass
	 * @throws AcsJEventSubscriptionEx
	 */
	@Override
	protected void notifyFirstSubscription(Class<?> structClass) throws AcsJEventSubscriptionEx {
		String eventTypeNameShort = ( structClass == null ? "*" : structClass.getSimpleName() );
		String eventTypeNameLong = ( structClass == null ? "*" : structClass.getName() );
		try {
			int filterId = addFilter(eventTypeNameShort);
			subscriptionsFilters.put(eventTypeNameLong, filterId);
		} catch (AcsJCORBAProblemEx e) {
			throw new AcsJEventSubscriptionEx(e);
		}
	}
	
	@Override
	protected void notifySubscriptionRemoved(Class<?> structClass) throws AcsJEventSubscriptionEx {
//		String eventTypeNameShort = ( structClass == null ? "*" : structClass.getSimpleName() );
		String eventTypeNameLong = ( structClass == null ? "*" : structClass.getName() );
		try {
			proxySupplier.remove_filter(subscriptionsFilters.get(eventTypeNameLong));
			subscriptionsFilters.remove(eventTypeNameLong);
		} catch (FilterNotFound e) {
			throw new AcsJEventSubscriptionEx("Filter for '"
					+ eventTypeNameLong + "' not found on the server side: ", e);
		}
		// If receivers is empty we just discard everything
		if (receivers.isEmpty()) {
			discardAllEvents();
		}
	}
	
	@Override
	protected void notifyNoSubscription() {
		discardAllEvents();
	}

	
	////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////// Helper methods  ////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Creates or reuses a shared server-side NC consumer admin object.
	 * 
	 * @throws AcsJException
	 */
	private ConsumerAdmin getSharedAdmin() throws AcsJCORBAProblemEx, AcsJNarrowFailedEx {
		
		ConsumerAdmin ret = null;
		org.omg.CosNotifyChannelAdmin.ConsumerAdmin retBase = null;
		boolean created = false;
		int consumerAdminId = -1;

		// @TODO (HSO): Why use a static lock here? This gives a false sense of safety in single-program unit tests,
		// while in real life we can have concurrent admin creation requests from different processes.
		synchronized(NCSubscriber.class) {

			// Check if we can reuse an already existing consumer admin
			int consumerAdminIds[] = channel.get_all_consumeradmins();
			for(int i=0; i!=consumerAdminIds.length && retBase == null; i++) {

				int adminID = consumerAdminIds[i];
				try {

					// Get the consumer admin. Then, check if any of its proxies is of type ANY_EVENT
					// This is a temporary hack to recognize shared admins from those created by the old
					// NC classes, and that are not meant to be reused
					org.omg.CosNotifyChannelAdmin.ConsumerAdmin tmpAdmin = channel.get_consumeradmin(adminID);

					int[] push_suppliers_ids = tmpAdmin.push_suppliers();
					for(int proxyID: push_suppliers_ids) {
						try {
							ProxySupplier proxy = tmpAdmin.get_proxy_supplier(proxyID);
							if(ProxyType.PUSH_ANY.equals(proxy.MyType())) {

								// OK, we have a shared admin. Now, we do some simple load balancing,
								// so we use this shared admin only if it has space for more proxies
								// (the -1 goes because of the dummy proxy that is attached to the shared admin)
								if( push_suppliers_ids.length-1 < PROXIES_PER_ADMIN) {
									retBase = tmpAdmin;
									consumerAdminId = adminID;
									break;
								}
								// shortcut: don't check the remaining proxies and continue with the next admin
								else
									break;
							}
						} catch(ProxyNotFound e) {
							logger.log(AcsLogLevel.NOTICE, "Proxy with ID='" + proxyID + "' not found for consumer admin with ID='" + adminID + "', " +
									"will continue anyway to search for shared consumer admins", e);
						}
					}

				} catch (AdminNotFound e) {
					logger.log(AcsLogLevel.NOTICE, "Consumer admin with ID='" + adminID + "' not found for channel '" + channelName + "', " +
							"will continue anyway to search for shared consumer admins", e);
				}
			}

			// If no suitable consumer admin was found, we create a new one 
			if (retBase == null) {

				// create a new consumer admin
				IntHolder consumerAdminIDHolder = new IntHolder();
				retBase = channel.new_for_consumers(InterFilterGroupOperator.AND_OP, consumerAdminIDHolder);

				// Create a dummy proxy in the new consumer admin.
				// 
				// rtobar, Apr. 13th, 2011:
				// This dummy proxy is not of a StructuredProxyPushSupplier, like the rest of the proxies
				// that are created for the NC in the admin objects. This way we can recognize a shared consumer admin
				// by looking at its proxies, and checking if their "MyType" property is ANY_EVENT.
				//
				// This hack is only needed while we are in transition between the old and new NC classes,
				// and should get removed once the old classes are not used anymore
				try {
					retBase.obtain_notification_push_supplier(ClientType.ANY_EVENT, new IntHolder());
				} catch (AdminLimitExceeded e) {
					retBase.destroy();
					AcsJCORBAProblemEx e2 = new AcsJCORBAProblemEx(e);
					e2.setInfo("Coundn't attach dummy ANY_EVENT proxy to newly created shared admin consumer for channel '" + channelName + "'. Newly created shared admin is now destroyed.");
					throw e2;
				}

				consumerAdminId = consumerAdminIDHolder.value;
				created = true;
			}

		} // synchronize(NCSubscriber.class) ...

		try {
			ret = ConsumerAdminHelper.narrow(retBase);
		} catch (BAD_PARAM ex) {

			if (created)
				retBase.destroy();

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
			ProxySupplier proxy = null;
			while( proxy == null ) {
				// See the comments on Consumer#createConsumer() for a nice explanation of why this randomness is happening here
				String randomizedClientName = Helper.createRandomizedClientName(clientName);
				try {
					proxy = sharedConsumerAdmin.obtain_named_notification_push_supplier(ClientType.STRUCTURED_EVENT, proxyIdHolder, randomizedClientName);
				} catch (NameAlreadyUsed e) {
					// Hopefully we won't run into this situation. Still, try to go on in the loop,
					// with a different client name next time.
				} catch (NameMapError e) {
					// Default to the unnamed version
					proxy = sharedConsumerAdmin.obtain_notification_push_supplier(ClientType.STRUCTURED_EVENT, proxyIdHolder);
				}
			}

			ret = StructuredProxyPushSupplierHelper.narrow(proxy);
		} catch (AdminLimitExceeded ex) {
			// See NC spec 3.4.15.10
			// If the number of consumers currently connected to the channel with which the target ConsumerAdmin object is associated 
			// exceeds the value of the MaxConsumers administrative property, the AdminLimitExceeded exception is raised.
			String limit = ex.admin_property_err.value.extract_string();
			errMsg = "NC '" + channelName + "' is configured for a maximum of " + limit + 
					" subscribers, which does not allow this client to subscribe."; 
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
	 * This method manages the filtering capabilities used to control subscriptions.
	 * 
	 * @return FilterID (see OMG NotificationService spec 3.2.4.1)
	 * @throws AcsJCORBAProblemEx
	 */
	private int addFilter(String eventTypeName) throws AcsJCORBAProblemEx {

		try {
			// Create the filter
			FilterFactory filterFactory = channel.default_filter_factory();
			Filter filter = filterFactory.create_filter(getFilterLanguage());

			// Information needed to construct the constraint expression object
			// (any domain, THE type)
			EventType[] t_info = { new EventType("*", eventTypeName) };

			// Add constraint expression object to the filter
			ConstraintExp[] cexp = { new ConstraintExp(t_info, "") };
			filter.add_constraints(cexp);

			// Add the filter to the proxy and return the filter ID
			return proxySupplier.add_filter(filter);

		} catch (org.omg.CosNotifyFilter.InvalidGrammar e) {
			Throwable cause = new Throwable("'" + eventTypeName
					+ "' filter is invalid for the '" + channelName
					+ "' channel: " + e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		} catch (org.omg.CosNotifyFilter.InvalidConstraint e) {
			Throwable cause = new Throwable("'" + eventTypeName
					+ "' filter is invalid for the '" + channelName
					+ "' channel: " + e.getMessage());
			throw new alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx(cause);
		}

	}
	

	/**
	 * This method is used to discard all events. Is called when there are no
	 * subscriptions left or if the {@link #removeSubscription()} method is
	 * called with null as parameter.
	 */
	private void discardAllEvents() {

		// For safety, remove all filters in the server, clear the local references,
		// and put a dummy filter that filters out everything
		proxySupplier.remove_all_filters();
		subscriptionsFilters.clear();
		try {
			addFilter("");
		} catch (AcsJCORBAProblemEx e) {
			logger.log(AcsLogLevel.ERROR, "Cannot add all-exclusive filter, we'll keep receiving events, but no handler will receive them");
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

	
	////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////// Corba callback methods  ////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * This method is called by the notification channel (supplier proxy) each time an event is received.
	 * <p>
	 * It is declared <code>final</code> because it is crucial for the functioning of the NC library
	 * and thus cannot be overwritten by a subclass. 
	 * If for special purposes a notification of raw event reception is needed, 
	 * a subclass can implement {@link #push_structured_event_called(StructuredEvent)}, which gets called from this
	 * method as the first thing it does. 
	 * @param structuredEvent
	 *            The structured event sent by a supplier.
	 * @throws Disconnected If this subscriber is disconnected from the NC. 
	 *         See NC spec 3.3.7.1: "if the invocation of push_structured_event upon a StructuredPushConsumer instance 
	 *         by a StructuredProxyPushSupplier instance results in the Disconnected exception being raised, 
	 *         the StructuredProxyPushSupplier will invoke its own disconnect_structured_push_supplier operation, 
	 *         resulting in the destruction of that StructuredProxyPushSupplier instance."
	 *         This serves only as a backup mechanism, since normally we explicitly disconnect the subscriber.
	 * 
	 * @see org.omg.CosNotifyComm.StructuredPushConsumerOperations#push_structured_event(org.omg.CosNotification.StructuredEvent)
	 */
	@Override
	public final void push_structured_event(StructuredEvent structuredEvent)
			throws Disconnected {
		
		boolean shouldProcessEvent = true;
		
		try {
			shouldProcessEvent = push_structured_event_called(structuredEvent);
		} catch (Throwable thr) {
			// ignore any exception, since push_structured_event_called is only meant for 
			// notification, to enable special tests or other exotic purposes.
			// In this case we also keep shouldProcessEvent=true, just in case.
		}

		// got a subclass 'veto'?
		if (!shouldProcessEvent) {
			if (firstSubclassVeto) {
				logger.info("Event subscriber '" + getClass().getSimpleName() + "' handles one or more raw NC events itself, bypassing '" + NCSubscriber.class + 
								"'. This non-standard behavior will not be logged again by this NCSubscriber.");
				firstSubclassVeto = false;
			}
			return;
		}
		
		if (isDisconnected()) {
			throw new Disconnected();
		}

		Object convertedAny = anyAide.complexAnyToObject(structuredEvent.filterable_data[0].value);

		if (convertedAny == null) {
			// @TODO: compare with ACS-NC specs and C++ impl, and perhaps call generic receiver with null data,
			//        if the event does not carry any data.
			LOG_NC_EventReceive_FAIL.log(
					logger,
					channelName,
					getNotificationFactoryName(),
					structuredEvent.header.fixed_header.event_type.type_name,
					"null");
		}
		else {
			// An optimization: If the event type cannot match a typed or generic receiver
			// then we don't put it into the queue. We could improve this by checking for registered receivers already here...
			if (!eventType.isInstance(convertedAny) && !hasGenericReceiver()) {
				logNoEventReceiver(convertedAny.getClass().getName());
			}
			
			EventDescription eventDesc = EventDescriptionHelper.extract(structuredEvent.remainder_of_body);
			
			if (isTraceEventsEnabled()) {
				LOG_NC_EventReceive_OK.log(
						logger,
						channelName,
						getNotificationFactoryName(),
						structuredEvent.header.fixed_header.event_type.type_name);
			}

			// let the base class deal with queue and dispatching to receiver
			processEventAsync(convertedAny, eventDesc);
		}
	}

	/**
	 * Users can override this method to get notified of raw events, for additional statistics, 
	 * to handle event data given as a sequence of IDL structs (exceptional case in acssamp),
	 * or for DynAny access (eventGUI).
	 * <p>
	 * Usually this method should not be overridden.
	 * 
	 * @param structuredEvent
	 * @return <code>true</code> if normal event processing should continue, 
	 *         <code>false</code> if NCSubscriber should not process this event. 
	 */
	protected boolean push_structured_event_called(StructuredEvent structuredEvent) {
		//System.out.println("********** got a call to push_structured_event **********");
		return true;
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

	/**
	 * @TODO: Perhaps integrate reconnection into the state machine.
	 * 
	 * @see alma.acs.nc.ReconnectableParticipant#reconnect(gov.sandia.NotifyMonitoringExt.EventChannelFactory)
	 */
	@Override
	public void reconnect(EventChannelFactory ecf) {

		logger.log(AcsLogLevel.NOTICE, "Reconnecting subscriber with channel '" + channelName + "' after Notify Service recovery");

		if (channel != null) {
			channel = helper.getNotificationChannel(ecf);
			if (channel == null) {
				logger.log(Level.WARNING, "Cannot reconnect to the channel '" + channelName + "'");
				return;
			}
		}

		try {
			channel.set_qos(helper.getChannelProperties().getCDBQoSProps(channelName));
			channel.set_admin(helper.getChannelProperties().getCDBAdminProps(channelName));
		} catch (UnsupportedQoS e) {
		} catch (AcsJException e) {
		} catch (UnsupportedAdmin ex) {
			logger.warning(helper.createUnsupportedAdminLogMessage(ex, channelName));
		}
		
	}
}