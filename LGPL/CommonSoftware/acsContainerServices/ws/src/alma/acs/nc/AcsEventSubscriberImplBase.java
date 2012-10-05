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

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;

import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.MultipleRepeatGuard;
import alma.acs.logging.RepeatGuard;
import alma.acs.logging.RepeatGuard.Logic;
import alma.acs.nc.sm.ConnectionActionHandler;
import alma.acs.nc.sm.EnvironmentActionHandler;
import alma.acs.nc.sm.EventSubscriberStateMachine;
import alma.acs.nc.sm.SuspendResumeActionHandler;
import alma.acs.util.StopWatch;
import alma.acsnc.EventDescription;

/**
 * Base class for an event subscriber, that can be used for both Corba NC and for in-memory test NC.
 * <p>
 * We will have to see if it can also be used for DDS-based events, or if the required modifications will be too heavy.
 * The parameterization (T) is an attempt to abstract away from the IDLEntity event base type.
 */
public abstract class AcsEventSubscriberImplBase<T> implements AcsEventSubscriber<T> {
	
	/**
	 * A name that identifies the client of this NCSubscriber, to be used
	 * both for logging and also (if applicable) as the supplier proxy name so that 
	 * looking at the proxy objects of the NC we can figure out who the clients are.
	 */
	protected final String clientName;

	/** 
	 * Provides access to the ACS logging system. 
	 */
	protected final Logger logger;

	/**
	 * State machine for the subscriber lifecycle. 
	 * It is not used for data handling itself, even though the measured overhead of less than 0.1 ms 
	 * per signal might allow this.
	 */
	protected final EventSubscriberStateMachine stateMachine;
	
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
	private final ThreadPoolExecutor eventHandlingExecutor;

	/**
	 * @see #eventHandlingExecutor
	 */
	private long numEventsDiscarded;

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
	 * Lock to make connection and disconnection activities thread-safe. 
	 */
	protected final ReentrantLock connectionLock = new ReentrantLock();

	protected final Class<T> eventType;

	/**
	 * @return <code>true</code> if events should be logged when they are received. 
	 */
	protected abstract boolean isTraceEventsEnabled();
	
	
	/**
	 * Base class constructor, to be called from subclass ctor.
	 * IMPORTANT: Subclasses MUST call "stateMachine.setUpEnvironment()" at the end of their constructors.
	 * This is because Java does not support template method design for constructors 
	 * (see for example http://stackoverflow.com/questions/2906958/running-a-method-after-the-constructor-of-any-derived-class)
	 * and since the subclasses are all meant to be produced within ACS I did not want to go for dirty tricks to work around this.
	 * We also don't want user code to call a public init() method after constructing the subscriber. 
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

		if (clientName == null) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setParameter("clientName");
			ex.setParameterValue("null");
			throw ex;
		}

		logger = services.getLogger();
		this.clientName = clientName;
		this.eventType = eventType;
		
		stateMachine = new EventSubscriberStateMachine(logger, 
									createEnvironmentActionHandler(),
									createConnectionActionHandler(),
									createSuspendResumeActionHandler() );
		
		// @TODO Set more realistic guarding parameters, e.g. max 1 identical log in 10 seconds
		processTimeLogRepeatGuard = new MultipleRepeatGuard(0, TimeUnit.SECONDS, 1, Logic.COUNTER, 100);

		// log slow receiver error only every 30 seconds or every 100 times, whatever comes first
		receiverTooSlowLogRepeatGuard = new RepeatGuard(30, TimeUnit.SECONDS, 100, Logic.OR);
		
		eventHandlingExecutor = new ThreadPoolExecutor(0, 1, 1L, TimeUnit.MINUTES,
				new ArrayBlockingQueue<Runnable>(EVENT_QUEUE_CAPACITY), services.getThreadFactory(), new ThreadPoolExecutor.AbortPolicy() );
		
		
	}

	/**
	 * Subclasses can override EnvironmentActionHandler
	 */
	protected EnvironmentActionHandler createEnvironmentActionHandler() {
		return new EnvironmentActionHandler(logger);
	}

	/**
	 * Subclasses can override ConnectionActionHandler
	 */
	protected ConnectionActionHandler createConnectionActionHandler() {
		return new ConnectionActionHandler(logger);
	}

	/**
	 * Subclasses can override SuspendResumeActionHandler
	 */
	protected SuspendResumeActionHandler createSuspendResumeActionHandler() {
		return new SuspendResumeActionHandler(logger);
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
	 */
	protected abstract void logEventProcessingTooSlowForEventRate(long numEventsDiscarded, String eventName, long logOcurrencesNumber);
	
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
	 *  
	 * @param structToProcess
	 * @param eDescrip
	 */
	protected void processEventAsync(final T structToProcess, final EventDescription eDescrip) {

		// to avoid unnecessary scary logs, we tolerate previous events up to half the queue size
		boolean isReceiverBusyWithPreviousEvent = ( eventHandlingExecutor.getQueue().size() > EVENT_QUEUE_CAPACITY / 2 );
		
//			logger.info("Queue size: " + eventHandlingExecutor.getQueue().size());
		
		boolean thisEventDiscarded = false;
		try {
			eventHandlingExecutor.execute(
					new Runnable() {
						public void run() {
							// here we call processEvent from the worker thread
							processEvent(structToProcess, eDescrip);
						}
					});
		} catch (RejectedExecutionException ex) {
			// receivers have been too slow, queue is full, will drop data.
			thisEventDiscarded = true;
			numEventsDiscarded++;
		}
		if ( (thisEventDiscarded || isReceiverBusyWithPreviousEvent) && receiverTooSlowLogRepeatGuard.checkAndIncrement()) {
			logEventProcessingTooSlowForEventRate(numEventsDiscarded, structToProcess.getClass().getName(), receiverTooSlowLogRepeatGuard.counterAtLastExecution());
			numEventsDiscarded = 0;
		}
	}


	/**
	 * This method should be called from the subclass-specific method that receives the event,
	 * for example <code>push_structured_event</code> in case of Corba NC,
	 * or preferably via {@link #processEventAsync(Object, EventDescription)}.
	 * <p>
	 * The implementation cannot assume any particular event type.
	 * <p>
	 * No exception is allowed to be thrown by this method, even if the receiver implementation throws a RuntimeExecption
	 */
	protected void processEvent(T corbaData, EventDescription eventDescrip) {
		
		@SuppressWarnings("unchecked")
		Class<T> eventType = (Class<T>) corbaData.getClass();
		
		String eventName = eventType.getName();

		// figure out how much time this event has to be processed (according to configuration)
		double maxProcessTimeSeconds = getMaxProcessTimeSeconds(eventName);

		StopWatch profiler = new StopWatch();
		// we give preference to a receiver that has registered for this concrete subtype of T
		if (receivers.containsKey(eventType)) {
			Callback<? extends T> receiver = receivers.get(eventType);

			profiler.reset();
			try {
				_process(receiver, corbaData, eventDescrip);
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
			genericReceiver.receive(corbaData, eventDescrip);
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
	public final void addGenericSubscription(GenericCallback receiver) throws CannotAddSubscriptionException {

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
	public final void removeGenericSubscription() throws SubscriptionNotFoundException {

		if (genericReceiver == null ) {
			throw new SubscriptionNotFoundException("Failed to remove generic subscription when not actually subscribed");
		}

		notifySubscriptionRemoved(null);
		genericReceiver = null;
	}

	/**
	 * @param structClass Can be <code>null</code> in case of generic subscription.
	 */
	protected abstract void notifyFirstSubscription(Class<?> structClass) throws CannotAddSubscriptionException;
	
	protected abstract void notifySubscriptionRemoved(Class<?> structClass) throws SubscriptionNotFoundException;
	
	protected abstract void notifyNoSubscription();

	
	@Override
	public final <U extends T> void addSubscription(Callback<U> receiver) 
			throws CannotAddSubscriptionException {

		Class<U> structClass = receiver.getEventType();

		if (structClass == null || !(eventType.isAssignableFrom(structClass)) )
			throw new CannotAddSubscriptionException("Receiver is returning a null or invalid event type. " +
					"Check the getEventType() method implementation and try again");

		// First time we create the filter and set the receiver
		if (!receivers.containsKey(structClass)) {
			notifyFirstSubscription(structClass);
		}
		// After the filter is created, we just replace the corresponding receivers
		receivers.put(structClass, receiver);
	}

	
	@Override
	public final <U extends T> void removeSubscription(Class<U> structClass) throws SubscriptionNotFoundException {

		// Removing subscription from receivers list
		if (structClass != null) {

			if (receivers.containsKey(structClass)) {
				receivers.remove(structClass);
				notifySubscriptionRemoved(structClass);
			} 
			else {
				throw new SubscriptionNotFoundException("Trying to unsubscribe from '"
						+ structClass.getName() + "' type of event when not actually subscribed to this type.");
			}
		} else {
			// Removing every type of event
			receivers.clear();
			notifyNoSubscription();
		}
	}

	/** 
	 * @see alma.acs.nc.AcsEventSubscriber#disconnect()
	 */
	@Override
	public final void disconnect() throws IllegalStateException {

		connectionLock.lock();
		try {
			checkConnection();

			// stop receiving events
			try {
				suspend();
			} finally {
				// An IllegalStateException we let fly, but still must call the rest
				disconnect_ImplSpecific();
				shutdownAsyncEventProcessing();
			}
		} 
		finally {
			connectionLock.unlock();
		}
	}
	
	/**
	 * Called from {@link #disconnect()}.
	 */
	protected abstract void disconnect_ImplSpecific() throws IllegalStateException;
	
	
	public final boolean isDisconnected() {
		connectionLock.lock();
		try {
			return isDisconnected_ImplSpecific();
		}
		finally {
			connectionLock.unlock();
		}
	}

	/**
	 * Called by {@link #isDisconnected()}, for the pub-sub technology specific part.
	 */
	protected abstract boolean isDisconnected_ImplSpecific();
	

	private void checkConnection() throws IllegalStateException {
		if (isDisconnected()) {
			throw new IllegalStateException("Subscriber already disconnected");
		}
	}

	
	@Override
	public final void suspend() throws IllegalStateException {
		connectionLock.lock();
		try {
			checkConnection();
			suspend_ImplSpecific(); // currently may throw OBJECT_NOT_EXIST for NC
		}
		finally {
			connectionLock.unlock();
		}
	}
	
	/**
	 * Called by {@link #suspend()}, for the pub-sub technology specific part.
	 * @throws IllegalStateException
	 */
	protected abstract void suspend_ImplSpecific() throws IllegalStateException;

	
	@Override
	public final void resume() throws IllegalStateException {
		connectionLock.lock();
		try {
			checkConnection();
			resume_ImplSpecific();
		}
		finally {
			connectionLock.unlock();
		}
	}

	/**
	 * Called by {@link #resume()}, for the pub-sub technology specific part.
	 * @throws IllegalStateException
	 */
	protected abstract void resume_ImplSpecific() throws IllegalStateException;
	
	/**
	 * Shuts down the event queue. 
	 * Queued events may still be processed by the receivers afterwards, 
	 * but here we wait for up to 500 ms to log it if it is the case.
	 * <p>
	 * Further events delivered to {@link #processEventAsync(Object, EventDescription)}
	 * will cause an exception there.
	 * 
	 * @return <code>true</code> if all events in the queue were processed before returning.
	 */
	private boolean shutdownAsyncEventProcessing() {
		eventHandlingExecutor.shutdown();
		boolean queueOK = false;
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
		return queueOK;
	}

}