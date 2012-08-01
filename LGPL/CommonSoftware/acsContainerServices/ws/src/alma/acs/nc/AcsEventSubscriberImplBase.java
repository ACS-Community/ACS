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

import org.omg.CORBA.portable.IDLEntity;

import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.MultipleRepeatGuard;
import alma.acs.logging.RepeatGuard;
import alma.acs.logging.RepeatGuard.Logic;
import alma.acs.util.StopWatch;
import alma.acsnc.EventDescription;
import alma.acsnc.OSPushConsumerPOA;

/**
 * 
 */
public abstract class AcsEventSubscriberImplBase extends OSPushConsumerPOA implements AcsEventSubscriber {
	

	/**
	 * A name that identifies the client of this NCSubscriber, to be used
	 * both for logging and also (if possible) as the supplier proxy name so that 
	 * looking at the proxy objects of the NC we can figure out who the clients are.
	 */
	protected final String clientName;

	/** 
	 * Used to time the execution of receive methods 
	 * @TODO HSO: Is it thread-safe to share a profiler?
	 */
	protected final StopWatch profiler;

	/** Provides access to the ACS logging system. */
	protected final Logger logger;

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
	 * Errors are logged using type-safe {@link LOG_NC_ReceiverTooSlow}, using also fields {@link #numEventsDiscarded} and {@link #receiverTooSlowLogRepeatGuard}.
	 * <p>
	 * The difference of this mechanism compared to the logs controlled by {@link #processTimeLogRepeatGuard} is that
	 * here we take into account the actual event rate and check whether the receiver can handle it, 
	 * whereas {@link #processTimeLogRepeatGuard} logs only compare the actual process times against pre-configured values.
	 */
	private final ThreadPoolExecutor eventHandlingExecutor;

	/**
	 * @see #eventHandlingExecutor
	 */
	private long numEventsDiscarded;

	/**
	 * Throttles {@link LOG_NC_ReceiverTooSlow} logs, see {@link #eventHandlingExecutor}.
	 */
	private final RepeatGuard receiverTooSlowLogRepeatGuard;

	
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
	 * Contains a list of repeat guards for each different type of event.
	 */
	protected final MultipleRepeatGuard processTimeLogRepeatGuard;

	protected final ReentrantLock disconnectLock = new ReentrantLock();

	protected abstract boolean isTraceEventsEnabled();
	
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
	public AcsEventSubscriberImplBase(ContainerServicesBase services, String clientName) throws AcsJException {
		
		if (services == null) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx();
			ex.setParameter("services");
			ex.setParameterValue("null");
			throw ex;
		}

		this.clientName = clientName;
		
		profiler = new StopWatch();
		logger = services.getLogger();

		// @TODO Set more realistic guarding parameters, e.g. max 1 identical log in 10 seconds
		processTimeLogRepeatGuard = new MultipleRepeatGuard(0, TimeUnit.SECONDS, 1, Logic.COUNTER, 100);

		// log slow receiver error only every 30 seconds or every 100 times, whatever comes first
		receiverTooSlowLogRepeatGuard = new RepeatGuard(30, TimeUnit.SECONDS, 100, Logic.OR);
		
		eventHandlingExecutor = new ThreadPoolExecutor(0, 1, 1L, TimeUnit.MINUTES,
				new ArrayBlockingQueue<Runnable>(EVENT_QUEUE_CAPACITY), services.getThreadFactory(), new ThreadPoolExecutor.AbortPolicy() );
		
	}

	protected abstract double getMaxProcessTimeSeconds(String eventName);
	
	protected abstract void logEventReceiveHandlerException(String eventName, String receiverClassName, Throwable thr);
	
	protected abstract void logEventProcessingTimeExceeded(String eventName, long logOcurrencesNumber);
	
	protected abstract void logEventProcessingTooSlowForEventRate(long numEventsDiscarded, String eventName, long logOcurrencesNumber);
	
	protected abstract void logNoEventReceiver(String eventName);

	protected abstract void logQueueShutdownError(int timeoutMillis, int remainingEvents);

	
	/**
	 * Asynchronously calls {@link #processEvent(IDLEntity, EventDescription)}, 
	 * using {@link #eventHandlingExecutor}.
	 * @param structuredEvent
	 * @param eDescrip
	 * @param structToProcess
	 */
	protected void processEventAsync(final IDLEntity structToProcess, final EventDescription eDescrip) {

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
	 * Called from Corba (via push_structured_event).
	 * The implementation cannot assume any particular event type.
	 * <p>
	 * No exception is allowed to be thrown by this method, even if the receiver implementation throws a RuntimeExecption
	 */
	protected void processEvent(IDLEntity corbaData, EventDescription eventDescrip) {
		
		Class<? extends IDLEntity> eventType = corbaData.getClass();
		String eventName = eventType.getName();

		// figure out how much time this event has to be processed (according to configuration)
		double maxProcessTimeSeconds = getMaxProcessTimeSeconds(eventName);

		// we give preference to a receiver that has registered for this concrete subtype of IDLEntity
		if (receivers.containsKey(eventType)) {
			AcsEventSubscriber.Callback<? extends IDLEntity> receiver = receivers.get(eventType);

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
			
			// start timing
			profiler.reset();
			genericReceiver.receive(corbaData, eventDescrip);
			double usedSecondsToProcess = (profiler.getLapTimeMillis() / 1000.0);

			// warn the end-user if the receiver is taking too long 
			if (usedSecondsToProcess > maxProcessTimeSeconds && processTimeLogRepeatGuard.checkAndIncrement(eventName)) {
				logEventProcessingTimeExceeded(eventName, processTimeLogRepeatGuard.counterAtLastExecution(eventName));
			}
		} 
		// no receiver found 
		// TODO: Check if the filtering for the subscribed events happens now really on the server side. 
		// If so, then we should never get an event for which there is no receiver, and should thus
		// log an error regardless of isTraceEventsEnabled
		else {
			if (isTraceEventsEnabled()) {
				logNoEventReceiver(eventName);
			}
		}
	}

	/**
	 * "Generic helper method" to enforce type argument inference by the compiler,
	 * see http://www.angelikalanger.com/GenericsFAQ/FAQSections/ProgrammingIdioms.html#FAQ207
	 */
	private <T extends IDLEntity> void _process(AcsEventSubscriber.Callback<T> receiver, Object corbaData, EventDescription eventDescrip) {
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
	public void addGenericSubscription(GenericCallback receiver) throws CannotAddSubscriptionException {

		// First time we create the filter and set the receiver
		if( genericReceiver == null ) {
			notifyFirstSubscription(null);
		}
		// After the filter is created, we just replace the receiver
		genericReceiver = receiver;
	}

	protected abstract void notifyFirstSubscription(Class<?> structClass) throws CannotAddSubscriptionException;
	
	protected abstract void notifySubscriptionRemoved(Class<?> structClass) throws SubscriptionNotFoundException;
	
	protected abstract void notifyNoSubscription();

	
	/**
	 * Removes the generic receiver handler.
	 * 
	 * @throws AcsJCORBAProblemEx
	 */
	@Override
	public void removeGenericSubscription() throws SubscriptionNotFoundException {

		if (genericReceiver == null ) {
			throw new SubscriptionNotFoundException("Failed to remove generic subscription when not actually subscribed");
		}

		notifySubscriptionRemoved(null);
		genericReceiver = null;
	}


	@Override
	@SuppressWarnings("unchecked")
	public void addSubscription(AcsEventSubscriber.Callback<?> receiver) 
			throws CannotAddSubscriptionException {

		Class<?> structClass = receiver.getEventType();

		if( structClass == null || !(IDLEntity.class.isAssignableFrom(structClass)) )
			throw new CannotAddSubscriptionException("Receiver is returning a null or invalid event type. " +
					"Check the getEventType() method implementation and try again");

		// These casts are already safe now
		Callback<? extends IDLEntity> typedReceiver = (Callback<? extends IDLEntity>)receiver;
		Class<? extends IDLEntity> typedStructClass = (Class<? extends IDLEntity>)structClass;

		// First time we create the filter and set the receiver
		if (!receivers.containsKey(structClass)) {
			notifyFirstSubscription(structClass);
		}
		// After the filter is created, we just replace the corresponding receivers
		receivers.put(typedStructClass, typedReceiver);
	}

	@Override
	public void removeSubscription(Class<?> structClass) throws SubscriptionNotFoundException {

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
	 * @TODO: Implement this method and call into abstract Corba-specific methods 
	 * implemented by the subclass. For the time being we rely on the subclass to implement the disconnect
	 * method in a way that calls {@link #shutdownAsyncEventProcessing()}, which after the inversion we should do ourselves.
	 * @see alma.acs.nc.AcsEventSubscriber#disconnect()
	 */
	@Override
	public abstract void disconnect() throws IllegalStateException;
	
	
	/**
	 * Shuts down the event queue. 
	 * Queued events may still be processed by the receivers afterwards, 
	 * but here we wait for up to 500 ms to log it if it is the case.
	 * @return <code>true</code> if all events in the queue were processed before returning.
	 */
	protected boolean shutdownAsyncEventProcessing() {
		eventHandlingExecutor.shutdown();
		boolean queueOK = false;
		try {
			queueOK = eventHandlingExecutor.awaitTermination(500, TimeUnit.MILLISECONDS);
		} catch (InterruptedException ex) {
			// just leave queueOK == false
		}
		if (!queueOK) {
			// timeout occurred, may still have events in the queue. Terminate with error message
			int remainingEvents = eventHandlingExecutor.getQueue().size();
			logQueueShutdownError(500, remainingEvents);
		}
		return queueOK;
	}

}