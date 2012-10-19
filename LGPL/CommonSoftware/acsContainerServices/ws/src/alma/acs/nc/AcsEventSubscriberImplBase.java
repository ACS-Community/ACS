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

package alma.acs.nc;

import static alma.acs.nc.sm.generated.EventSubscriberAction.createConnection;
import static alma.acs.nc.sm.generated.EventSubscriberAction.createEnvironment;
import static alma.acs.nc.sm.generated.EventSubscriberAction.destroyConnection;
import static alma.acs.nc.sm.generated.EventSubscriberAction.destroyEnvironment;
import static alma.acs.nc.sm.generated.EventSubscriberAction.resumeConnection;
import static alma.acs.nc.sm.generated.EventSubscriberAction.suspendConnection;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Logger;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.MultipleRepeatGuard;
import alma.acs.logging.RepeatGuard;
import alma.acs.logging.RepeatGuard.Logic;
import alma.acs.nc.sm.generated.EventSubscriberAction;
import alma.acs.nc.sm.generated.EventSubscriberSignal;
import alma.acs.nc.sm.generated.EventSubscriberSignalDispatcher;
import alma.acs.nc.sm.generic.AcsScxmlActionDispatcher;
import alma.acs.nc.sm.generic.AcsScxmlActionExecutor;
import alma.acs.nc.sm.generic.AcsScxmlEngine;
import alma.acs.util.StopWatch;
import alma.acsErrTypeLifeCycle.wrappers.AcsJEventSubscriptionEx;
import alma.acsnc.EventDescription;

/**
 * Base class for an event subscriber, that can be used for both Corba NC and for in-memory test NC.
 * <p>
 * We will have to see if it can also be used for DDS-based events, or if the required modifications will be too heavy.
 * 
 * @param <T> The event (base) type. If all events are of the same type then that type should be used; 
 *            otherwise a common base type for all events that may be sent on the given "channel" should be used, 
 *            such as <code>Object</code> or <code>IDLEntity</code>. 
 */
public abstract class AcsEventSubscriberImplBase<T> 
		implements AcsEventSubscriber<T>, AcsScxmlActionExecutor<EventSubscriberAction> {
	
	/**
	 * A name that identifies the client of this NCSubscriber, to be used
	 * both for logging and also (if applicable) as the supplier proxy name so that 
	 * looking at the proxy objects of the NC we can figure out who the clients are.
	 */
	protected final String clientName;

	protected final ContainerServicesBase services;

	/** 
	 * Provides access to the ACS logging system. 
	 */
	protected final Logger logger;

	/**
	 * State machine for the subscriber lifecycle. 
	 * It is not used for data handling itself, even though the measured overhead 
	 * of less than 0.2 ms per signal might allow this.
	 */
	protected final AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction> stateMachine;
	
	private static final String scxmlFileName = "/alma/acs/nc/sm/generated/EventSubscriberSCXML.xml";

	protected final EventSubscriberSignalDispatcher stateMachineSignalDispatcher;
	
	/**
	 * Event queue should hold at least two events to avoid unnecessary scary logs about slow receivers,
	 * but must be short enough to get receivers to actually implement their own queue and discard mechanism
	 * instead of relying on this ACS queue which may buffer events for a limited time and thus obscure the problem.
	 * @see #eventHandlingExecutor
	 */
	public static final int EVENT_QUEUE_CAPACITY = 50;
	
	/**
	 * Single-thread executor with a small queue (size given in {@link #EVENT_QUEUE_CAPACITY}, 
	 * used to stay responsive toward the NC even if receivers are slow. 
	 * The purpose is only to track down receivers that don't keep up with the event rate, and not to provide reliable event buffering, 
	 * see http://jira.alma.cl/browse/COMP-5767.
	 * <p>
	 * Errors are logged, using also fields {@link #numEventsDiscarded} and {@link #receiverTooSlowLogRepeatGuard}.
	 * <p>
	 * The difference of this mechanism compared to the logs controlled by {@link #processTimeLogRepeatGuard} is that
	 * here we take into account the actual event rate and check whether the receiver can handle it, 
	 * whereas {@link #processTimeLogRepeatGuard} only compares the actual process times against pre-configured values.
	 */
	private ThreadPoolExecutor eventHandlingExecutor;

	/**
	 * @see #eventHandlingExecutor
	 */
	private final AtomicLong numEventsDiscarded = new AtomicLong(0);

	/**
	 * Throttles logs from {@link #logEventProcessingTooSlowForEventRate(long, String, long)}.
	 */
	private final RepeatGuard receiverTooSlowLogRepeatGuard;

	
	/**
	 * Contains a generic receiver to be used by the
	 * {@link #addGenericSubscription()} method.
	 */
	protected AcsEventSubscriber.GenericCallback genericReceiver;

	/**
	 * Contains a list of receiver functions to be invoked when an event 
	 * of a particular type is received.
	 * <p>
	 * key = the event type name (IDL-defined struct). <br>
	 * value = the matching event handler.
	 */
	protected final Map<Class<? extends T>, Callback<? extends T>> receivers = 
							new HashMap<Class<? extends T>, Callback<? extends T>>();
	
	/**
	 * Contains a list of repeat guards for each different type of event.
	 */
	protected final MultipleRepeatGuard processTimeLogRepeatGuard;

	/**
	 * Runtime access to the type parameter &lt;T&gt;.
	 */
	protected final Class<T> eventType;


	/**
	 * @return <code>true</code> if events should be logged when they are received. 
	 */
	protected abstract boolean isTraceEventsEnabled();
	
	
	/**
	 * Base class constructor, to be called from subclass ctor.
	 * IMPORTANT: Subclasses MUST call "stateMachine.setUpEnvironment()" at the end of their constructors.
	 * This is because Java does not support template method design for constructors 
	 * (see for example http://stackoverflow.com/questions/2906958/running-a-method-after-the-constructor-of-any-derived-class).
	 * Since the subclasses are all meant to be produced within ACS I did not want to go for dirty tricks to work around this.
	 * We also don't want to call a public init() method after constructing the subscriber. 
	 * <p>
	 * Normally an ACS class such as container services will act as the factory for event subscriber objects, 
	 * but for exceptional cases it is also possible to create one stand-alone, 
	 * as long as the required parameters can be provided.
	 * 
	 * @param services
	 *            To get ACS logger, access to the CDB, etc.
	 * @param clientName
	 *            A name that identifies the client of this NCSubscriber. 
	 *            TODO: Check if we still need this name to be specified separately from {@link services#getName()}.
	 * @throws AcsJException
	 *             Thrown on any <I>really bad</I> error conditions encountered.
	 */
	public AcsEventSubscriberImplBase(ContainerServicesBase services, String clientName, Class<T> eventType) throws AcsJException {
		
		if (services == null) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setParameter("services");
			ex.setParameterValue("null");
			throw ex;
		}
		this.services = services;

		if (clientName == null) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setParameter("clientName");
			ex.setParameterValue("null");
			throw ex;
		}
		this.clientName = clientName;

		this.eventType = eventType;
		logger = services.getLogger();

		// @TODO Set more realistic guarding parameters, e.g. max 1 identical log in 10 seconds
		processTimeLogRepeatGuard = new MultipleRepeatGuard(0, TimeUnit.SECONDS, 1, Logic.COUNTER, 100);

		// log slow receiver error only every 30 seconds or every 100 times, whatever comes first
		receiverTooSlowLogRepeatGuard = new RepeatGuard(30, TimeUnit.SECONDS, 100, Logic.OR);
		
		// set up the state machine
		
		AcsScxmlActionDispatcher<EventSubscriberAction> actionDispatcher = 
				new AcsScxmlActionDispatcher<EventSubscriberAction>(logger, EventSubscriberAction.class);
		
		// As a design choice, we implement all SM actions directly here in this class and its subclasses.
		// The state machine framework expects an object per action, so that we must register ourselves for every action type.
		// We could do this as a for loop over EventSubscriberAction.values(), but better hardcode the registration
		// for every action type so that SM changes will be noticed more easily.
		actionDispatcher.registerActionHandler(createEnvironment, this);
		actionDispatcher.registerActionHandler(destroyEnvironment, this);
		actionDispatcher.registerActionHandler(createConnection, this);
		actionDispatcher.registerActionHandler(destroyConnection, this);
		actionDispatcher.registerActionHandler(suspendConnection, this);
		actionDispatcher.registerActionHandler(resumeConnection, this);
		
		// Here the AcsScxmlEngine constructor will load the scxml file and start the state machine
		stateMachine = new AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction>(
				scxmlFileName, logger, actionDispatcher, EventSubscriberSignal.class);
		
		// Convenience method to send events to the SM. 
		// Usually meant to be used as a base class, but here we don't want to expose the SM to the user.
		stateMachineSignalDispatcher = new EventSubscriberSignalDispatcher() {
			@Override
			protected AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction> getScxmlEngine() {
				return stateMachine;
			}
		};
		
		// Later at the end of the subclass ctor there should be the call to 
		// stateMachine.setUpEnvironment(), leaving our state machine in state EnvironmentCreated
	}

	////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////// State machine //////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Dispatches the action enum to one of the action handler methods.
	 * <p>
	 * The overhead of implementing this method is the price we pay for avoiding reflection
	 * and allowing flexible action implementation in the SM design.
	 * @see alma.acs.nc.sm.generic.AcsScxmlActionExecutor#execute(java.lang.Enum, org.apache.commons.scxml.EventDispatcher, org.apache.commons.scxml.ErrorReporter, org.apache.commons.scxml.SCInstance, java.util.Collection)
	 */
	@Override
	public boolean execute(EventSubscriberAction action, EventDispatcher evtDispatcher, ErrorReporter errRep,
			SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
					throws AcsJStateMachineActionEx {
		
//		logger.fine("Will handle action " + action.name());
		boolean ret = true;
		
		switch (action) {
		
		case createEnvironment:
			createEnvironmentAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case destroyEnvironment:
			destroyEnvironmentAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case createConnection:
			createConnectionAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case destroyConnection:
			destroyConnectionAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case suspendConnection:
			suspendAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		case resumeConnection:
			resumeAction(evtDispatcher, errRep, scInstance, derivedEvents);
			break;

		default:
			ret = false;
		}
		
//		logger.fine("Done handling action " + action.name());
		return ret;
	}

	
	/**
	 * Subclass may override, but must call super.createEnvironmentAction().
	 */
	protected void createEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx {
		// nada
	}

	/**
	 * Subclass may override, but must call super.destroyEnvironmentAction().
	 */
	protected void destroyEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx {
	}

	/**
	 * Subclass may override, but must call super.createConnectionAction().
	 */
	protected void createConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx {
		eventHandlingExecutor = new ThreadPoolExecutor(0, 1, 1L, TimeUnit.MINUTES,
				new ArrayBlockingQueue<Runnable>(EVENT_QUEUE_CAPACITY), services.getThreadFactory(), new ThreadPoolExecutor.AbortPolicy() );
	}
	
	/**
	 * Handler for "destroyConnection" state machine action.
	 * <p>
	 * Shuts down the event queue. 
	 * Queued events may still be processed by the receivers afterwards, 
	 * but here we wait for up to 500 ms to log it if it is the case.
	 * <p>
	 * Further events delivered to {@link #processEventAsync(Object, EventDescription)}
	 * will cause an exception there.
	 * <p>
	 * Subclass may override, but must call super.destroyConnectionAction().
	 */
	protected void destroyConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
		
		eventHandlingExecutor.shutdown();
		boolean queueOK = false; // just in case we want to report the success of this shutdown
		try {
			queueOK = eventHandlingExecutor.awaitTermination(500, TimeUnit.MILLISECONDS);
		} catch (InterruptedException ex) {
			// just leave queueOK == false
		}
		if (!queueOK) {
			// interrupted or timeout occurred, may still have events in the queue. Terminate with error message
			int remainingEvents = eventHandlingExecutor.getQueue().size();
			logQueueShutdownError(500, remainingEvents);
		}
	}
	
	/**
	 * Subclass may override, but must call super.suspendAction().
	 */
	protected void suspendAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx {
		// nada
	}
	
	/**
	 * Subclass may override, but must call super.resumeAction().
	 */
	protected void resumeAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
			throws AcsJStateMachineActionEx {
		// nada
	}

	/**
	 * Gets the configured (or default) max time that a receiver may take to process an event,
	 * regardless of the actual event rate.
	 */
	protected abstract double getMaxProcessTimeSeconds(String eventName);
	
	/**
	 * Logs an exception thrown by an event handler (user code).
	 */
	protected abstract void logEventReceiveHandlerException(String eventName, String receiverClassName, Throwable thr);
	
	/**
	 * Logs the error that event processing time was exceeded.
	 */
	protected abstract void logEventProcessingTimeExceeded(String eventName, long logOcurrencesNumber);
	
	/**
	 * Logs the error that the receiver cannot keep up with the actual event rate,
	 * in spite of the small event buffering done.
	 * 
	 * @param numEventsDiscarded   The number of events that have actually been discarded since the last log. 
	 *                             Will often be 0 when we just warn about the queue filling up.
	 * @param eventName
	 */
	protected abstract void logEventProcessingTooSlowForEventRate(long numEventsDiscarded, String eventName);
	
	/**
	 * Logs or ignores the fact that an event was received for which no receiver could be found.
	 * <p>
	 * The subclass must know whether such a condition is expected or not, 
	 * e.g. because event filtering is set up outside of the subscriber
	 * and only subscribed event types are expected to arrive. 
	 */
	protected abstract void logNoEventReceiver(String eventName);

	/**
	 * Logs the error that the local event buffer could not be emptied before shutting down the subscriber. 
	 */
	protected abstract void logQueueShutdownError(int timeoutMillis, int remainingEvents);

	
	/**
	 * Asynchronously calls {@link #processEvent(Object, EventDescription)}, 
	 * using {@link #eventHandlingExecutor}.
	 * <p>
	 * This method should be called from the subclass-specific method that receives the event,
	 * for example <code>push_structured_event</code> in case of Corba NC.
	 * <p>
	 * This method is thread-safe.
	 * 
	 * @param eventData (defined as <code>Object</code> instead of <code>T</code> to include data for generic subscription).
	 * @param eventDesc event meta data
	 */
	protected void processEventAsync(final Object eventData, final EventDescription eventDesc) {

		// to avoid unnecessary scary logs, we tolerate previous events up to half the queue size
		boolean isReceiverBusyWithPreviousEvent = ( eventHandlingExecutor.getQueue().size() > EVENT_QUEUE_CAPACITY / 2 );
		
//			logger.info("Queue size: " + eventHandlingExecutor.getQueue().size());
		
		boolean thisEventDiscarded = false;
		try {
			eventHandlingExecutor.execute(
					new Runnable() {
						public void run() {
							// here we call processEvent from the worker thread
							processEvent(eventData, eventDesc);
						}
					});
		} catch (RejectedExecutionException ex) {
			// receivers have been too slow, queue is actually full, will drop data.
			thisEventDiscarded = true;
			numEventsDiscarded.incrementAndGet();
		}
		if (thisEventDiscarded || isReceiverBusyWithPreviousEvent) {
			// receiverTooSlowLogRepeatGuard currently not thread-safe, therefore synchronize
			synchronized (receiverTooSlowLogRepeatGuard) {
				if (receiverTooSlowLogRepeatGuard.checkAndIncrement()) {
					// About numEventsDiscarded and concurrency: 
					// That counter may have been incremented by other threads between the above RejectedExecutionException
					// and here. These threads are blocked now, and have not yet incremented the repeat guard.
					// This can lead to some harmless irregularities in how often we actually log the message.
					// What matters is that we report correctly the number of discarded events. 
					logEventProcessingTooSlowForEventRate(
							numEventsDiscarded.getAndSet(0), 
							eventData.getClass().getName());
				}
			}
		}
	}


	/**
	 * This method should be called from the subclass-specific method that receives the event,
	 * for example <code>push_structured_event</code> in case of Corba NC,
	 * or preferably via {@link #processEventAsync(Object, EventDescription)}.
	 * <p>
	 * No exception is allowed to be thrown by this method, even if the receiver implementation throws a RuntimeExecption
	 * @param eventData (defined as <code>Object</code> instead of <code>T</code> to include data for generic subscription).
	 * @param eventDesc
	 */
	protected void processEvent(Object eventData, EventDescription eventDesc) {
		
		Class<?> incomingEventType = eventData.getClass();
		String eventName = incomingEventType.getName();
		
		// figure out how much time this event has to be processed (according to configuration)
		double maxProcessTimeSeconds = getMaxProcessTimeSeconds(eventName);

		StopWatch profiler = new StopWatch();
		
		// we give preference to a receiver that has registered for this event type T or a subtype
		if (eventType.isAssignableFrom(incomingEventType) && receivers.containsKey(incomingEventType)) {
	
			@SuppressWarnings("unchecked")
			T typedEventData = (T) eventData;
			
			Callback<? extends T> receiver = receivers.get(incomingEventType);

			profiler.reset();
			try {
				_process(receiver, typedEventData, eventDesc);
			} 
			catch (Throwable thr) {
				logEventReceiveHandlerException(eventName, receiver.getClass().getName(), thr);
			}
			double usedSecondsToProcess = (profiler.getLapTimeMillis() / 1000.0);

			// warn the end-user if the receiver is taking too long, using a repeat guard
			if (usedSecondsToProcess > maxProcessTimeSeconds && processTimeLogRepeatGuard.checkAndIncrement(eventName)) {
				logEventProcessingTimeExceeded(eventName, processTimeLogRepeatGuard.counterAtLastExecution(eventName));
			}
		} 
		// fallback to generic receive method
		else if (genericReceiver != null) {
			
			profiler.reset();
			genericReceiver.receiveGeneric(eventData, eventDesc);
			double usedSecondsToProcess = (profiler.getLapTimeMillis() / 1000.0);

			// warn the end-user if the receiver is taking too long 
			if (usedSecondsToProcess > maxProcessTimeSeconds && processTimeLogRepeatGuard.checkAndIncrement(eventName)) {
				logEventProcessingTimeExceeded(eventName, processTimeLogRepeatGuard.counterAtLastExecution(eventName));
			}
		} 
		// no receiver found.
		// This may be OK or not, depending on whether the subclass sets up filtering in the underlying notification framework
		// that ensures that only subscribed event types reach the subscriber.
		else {
			logNoEventReceiver(eventName);
		}
	}

	/**
	 * "Generic helper method" to enforce type argument inference by the compiler,
	 * see http://www.angelikalanger.com/GenericsFAQ/FAQSections/ProgrammingIdioms.html#FAQ207
	 */
	private <U extends T> void _process(Callback<U> receiver, T eventData, EventDescription eventDescrip) {
		U castCorbaData = null;
		try {
			castCorbaData = receiver.getEventType().cast(eventData);
		}
		catch (ClassCastException ex) {
			// This should never happen and would be an ACS error
			logger.warning("Failed to deliver incompatible data '" + eventData.getClass().getName() + 
					"' to subscriber '" + receiver.getEventType().getName() + "'. Fix data subscription handling in " + getClass().getName() + "!");
		} 
		// user code errors (runtime ex etc) we let fly up
		receiver.receive(castCorbaData, eventDescrip);
	}


	@Override
	public String getLifecycleState() {
		return stateMachine.getCurrentState();
	}

	
	/**
	 * Use only for unit testing!
	 */
	public boolean hasGenericReceiver() {
		return ( genericReceiver != null );
	}
	
	/**
	 * Use only for unit testing!
	 */
	public int getNumberOfReceivers() {
		return receivers.size();
	}

	////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////// AcsEventSubscriber impl //////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * Subscribes to all events. The latest generic receiver displaces the previous one. 
	 * <p>
	 * If in addition to this generic subscription we also have specific subscriptions via
	 * {@link #addSubscription(Class, Callback)},
	 * then those more specific subscriptions will take precedence in receiving an event.
	 * <p>
	 * Notice though that any server-side filters previously created for the event type specific 
	 * subscriptions get deleted when calling this method, so that even after removing a generic 
	 * subscription the network performance gain of server-side filtering is lost. 
	 * @TODO: Couldn't this be fixed by creating the server-side filters on demand (see also javadoc class comment about lifecycle)?
	 */
	@Override
	public final void addGenericSubscription(GenericCallback receiver) throws AcsJEventSubscriptionEx {

		// First time we create the filter and set the receiver
		if( genericReceiver == null ) {
			notifyFirstSubscription(null);
		}
		// After the filter is created, we just replace the receiver
		genericReceiver = receiver;
	}

	/**
	 * Removes the generic receiver handler.
	 * 
	 * @throws AcsJCORBAProblemEx
	 */
	@Override
	public final void removeGenericSubscription() throws AcsJEventSubscriptionEx {

		if (genericReceiver == null ) {
			AcsJEventSubscriptionEx ex = new AcsJEventSubscriptionEx();
			ex.setContext("Failed to remove generic subscription when not actually subscribed.");
			ex.setEventType("generic");
			throw ex;
		}

		notifySubscriptionRemoved(null);
		genericReceiver = null;
	}

	@Override
	public final <U extends T> void addSubscription(Callback<U> receiver) 
			throws AcsJEventSubscriptionEx {

		Class<U> subscribedEventType = receiver.getEventType();

		if (subscribedEventType == null || !(eventType.isAssignableFrom(subscribedEventType)) ) {
			AcsJEventSubscriptionEx ex = new AcsJEventSubscriptionEx();
			ex.setContext("Receiver is returning a null or invalid event type. " +
					"Check the getEventType() method implementation and try again.");
			ex.setEventType(subscribedEventType == null ? "null" : subscribedEventType.getName());
			throw ex;
		}
		
		// First time we create the filter and set the receiver
		if (!receivers.containsKey(subscribedEventType)) {
			notifyFirstSubscription(subscribedEventType);
		}
		// After the filter is created, we just replace the corresponding receivers
		receivers.put(subscribedEventType, receiver);
	}

	
	@Override
	public final <U extends T> void removeSubscription(Class<U> structClass) throws AcsJEventSubscriptionEx {

		// Removing subscription from receivers list
		if (structClass != null) {

			if (receivers.containsKey(structClass)) {
				receivers.remove(structClass);
				notifySubscriptionRemoved(structClass);
			} 
			else {
				AcsJEventSubscriptionEx ex = new AcsJEventSubscriptionEx();
				ex.setContext("Trying to unsubscribe from an event type not being subscribed to.");
				ex.setEventType(structClass.getName());
				throw ex;
			}
		} 
		else {
			// Removing every type of event
			receivers.clear();
			notifyNoSubscription();
		}
	}

	@Override
	public final void startReceivingEvents() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		try {
			stateMachineSignalDispatcher.startReceivingEvents();
		} catch (AcsJStateMachineActionEx ex) {
			throw new AcsJCouldntPerformActionEx(ex);
		}
	}


	/** 
	 * @see alma.acs.nc.AcsEventSubscriber#disconnect()
	 */
	@Override
	public final void disconnect() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		try {
			stateMachineSignalDispatcher.stopReceivingEvents();
		}
		catch (AcsJIllegalStateEventEx ex) {
			// ignore. If the state is totally wrong then the subsequent cleanUpEnvironment
			// will also throw AcsJIllegalStateEventEx
		}
		catch (AcsJStateMachineActionEx ex) {
			throw new AcsJCouldntPerformActionEx(ex);
		}
		finally {
			// even after AcsJIllegalStateEventEx in stopReceivingEvents we want to do the second clean-up step
			try {
				stateMachineSignalDispatcher.cleanUpEnvironment();
			} catch (AcsJStateMachineActionEx ex) {
				throw new AcsJCouldntPerformActionEx(ex);
			}
		}
	}
	
	@Override
	public final void suspend() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		try {
			stateMachineSignalDispatcher.suspend();
		} catch (AcsJStateMachineActionEx ex) {
			throw new AcsJCouldntPerformActionEx(ex);
		}
	}

	
	@Override
	public final void resume() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx {
		try {
			stateMachineSignalDispatcher.resume();
		} catch (AcsJStateMachineActionEx ex) {
			throw new AcsJCouldntPerformActionEx(ex);
		}
	}


	////////////////////////////////////////////////////////////////////////////////////////
	///////////////////////// Subscription helper methods //////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * @param structClass Can be <code>null</code> in case of generic subscription.
	 */
	protected abstract void notifyFirstSubscription(Class<?> structClass) throws AcsJEventSubscriptionEx;
	
	/**
	 * @param structClass
	 * @throws AcsJEventSubscriptionEx
	 */
	protected abstract void notifySubscriptionRemoved(Class<?> structClass) throws AcsJEventSubscriptionEx;
	
	/**
	 * 
	 */
	protected abstract void notifyNoSubscription();

	
	@Override
	public boolean isSuspended() {
		return stateMachine.isStateActive("EnvironmentCreated::Connected::Suspended");
	}
	
	public final boolean isDisconnected() {
		return (
				stateMachine.isStateActive("EnvironmentCreated::Disconnected") ||
				stateMachine.isStateActive("EnvironmentUnknown")
			);
	}

}