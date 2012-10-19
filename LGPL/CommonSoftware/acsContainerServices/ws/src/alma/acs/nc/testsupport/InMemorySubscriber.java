package alma.acs.nc.testsupport;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;

import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.TriggerEvent;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventSubscriberImplBase;
import alma.acsErrTypeLifeCycle.wrappers.AcsJEventSubscriptionEx;
import alma.acsnc.EventDescription;



/**
 * In-memory subscriber. 
 * @param <T> See {@link AcsEventSubscriberImplBase}.
 * @see InMemoryNcFake
 */
class InMemorySubscriber<T> extends AcsEventSubscriberImplBase<T>
{
	/**
	 * Reference to factory.
	 */
	private final InMemoryNcFake nc;

	/**
	 * Used for {@link InMemorySubscriber#suspendBuffer}.
	 */
	private static class CachedEvent {
		CachedEvent(Object eventData, EventDescription eventDesc) {
			this.eventData = eventData;
			this.eventDesc = eventDesc;
		}
		Object eventData;
		EventDescription eventDesc;
	}
	
	/**
	 * Buffer for data we got while being suspended.
	 * Currently of unlimited size.
	 */
	private final List<CachedEvent> suspendBuffer;
	
	
	/**
	 * @param nc
	 * @param services
	 * @param clientName
	 * @param eventType
	 * @throws AcsJException
	 */
	InMemorySubscriber(InMemoryNcFake nc, ContainerServicesBase services, String clientName, Class<T> eventType) 
			throws AcsJException {

		super(services, clientName, eventType);
		this.nc = nc;
		suspendBuffer = new ArrayList<CachedEvent>();
		
		// this call is required, see base class ctor
		stateMachineSignalDispatcher.setUpEnvironment();
	}


	
	/**
	 * InMemoryNcFake should call this method.
	 * See <code>NCSubscriber#push_structured_event(StructuredEvent</code>.
	 * @throws AcsJIllegalStateEventEx  If this subscriber is disconnected.
	 */
	void pushData(Object eventData, EventDescription eventDesc) throws AcsJIllegalStateEventEx {
		
		// Here we use the state machine in the data flow, something we do not yet 
		// dare to do in the real NCSubscriber, being afraid of performance risks. Seems fine though.
		if (isDisconnected()) {
			AcsJIllegalStateEventEx ex = new AcsJIllegalStateEventEx("Subscriber '" + clientName + "' is disconnected.");
			ex.setState("disconnected");
			// todo ex.set context... instead of above message
			throw ex;
		}
		
		if (eventData == null) {
			// see LOG_NC_EventReceive_FAIL
			logger.warning("Received 'null' event.");
		}
		else {
			// Here we fake server-side suspension by storing the event in a local queue.
			// TODO: Would be nice to reuse eventHandlingExecutor from the base class,
			// but currently we cannot tap into the queue-receive chain and the data is attached to Runnable objects.
			// If the state machine call is too slow then we could also work with "suspendBuffer != null" logic.
			if (isSuspended()) {
				synchronized (suspendBuffer) {
					suspendBuffer.add(new CachedEvent(eventData, eventDesc));
				}
			}
			else {
				// Here we fake server-side filtering by skipping the event processing if we
				// know that the event type cannot be handled
				if (hasGenericReceiver() || receivers.containsKey(eventData.getClass())) {
					// TODO: Log something as in LOG_NC_EventReceive_OK
					processEventAsync(eventData, eventDesc);
				}
			}
		}
	}

	/**
	 * Gives access to the client name. 
	 */
	String getClientName() {
		return clientName;
	}
	
	////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////// State machine actions //////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////
	
	protected void createEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance, Collection<TriggerEvent> derivedEvents) 
				throws AcsJStateMachineActionEx {
		
		super.createEnvironmentAction(evtDispatcher, errRep, scInstance, derivedEvents);
	}
		
	protected void destroyEnvironmentAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
		
		super.destroyEnvironmentAction(evtDispatcher, errRep, scInstance, derivedEvents);
	}

	protected void createConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
		
		super.createConnectionAction(evtDispatcher, errRep, scInstance, derivedEvents);
	}

	protected void destroyConnectionAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
		
		nc.disconnectSubscriber(this);
		super.destroyConnectionAction(evtDispatcher, errRep, scInstance, derivedEvents);
	}

	
	protected void suspendAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
		super.suspendAction(evtDispatcher, errRep, scInstance, derivedEvents);
		// nothing else to do. We'll just collect incoming events in suspendBuffer
	}

	protected void resumeAction(EventDispatcher evtDispatcher, ErrorReporter errRep, SCInstance scInstance,
			Collection<TriggerEvent> derivedEvents) throws AcsJStateMachineActionEx {
		super.resumeAction(evtDispatcher, errRep, scInstance, derivedEvents);
		
		// async re-sending of suspendBuffer data
		final ArrayList<CachedEvent> oldBuffer = new ArrayList<CachedEvent>(suspendBuffer.size());
		synchronized (suspendBuffer) {
			oldBuffer.addAll(suspendBuffer);
			suspendBuffer.clear();
		}
		Runnable processor = new Runnable() {
			@Override
			public void run() {
				try {
					for (CachedEvent cachedEvent : oldBuffer) {
						// if in the meantime we get suspended again, suspendBuffer will again hold our data.
						pushData(cachedEvent.eventData, cachedEvent.eventDesc);
					}
				} catch (AcsJIllegalStateEventEx ex) {
					logger.log(Level.WARNING, "Failed to deliver buffered events (suspended time) because subscriber is now disconnected.", ex);
				}
			}
		};
		services.getThreadFactory().newThread(processor).start();
	}



	////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////// Various template method impls //////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////////

	@Override
	protected boolean isTraceEventsEnabled() {
		return false;
	}

	@Override
	protected double getMaxProcessTimeSeconds(String eventName) {
		// make this configurable if needed
		return 2.0;
	}

	@Override
	protected void logEventReceiveHandlerException(String eventName, String receiverClassName, Throwable thr) {
		logger.log(Level.WARNING, "The registered event handler of type '" + 
				receiverClassName + "' illegally threw an exception for event '" + eventName + "'.", thr);
	}

	@Override
	protected void logEventProcessingTimeExceeded(String eventName, long logOcurrencesNumber) {
		logger.warning("Took too long to process event '" + eventName + "' (logOcurrencesNumber=" + logOcurrencesNumber + ")."); 
	
	}

	@Override
	protected void logEventProcessingTooSlowForEventRate(long numEventsDiscarded, String eventName) {
		logger.warning("More events came in from the NC than the receiver processed. eventName=" + eventName +
				"; numEventsDiscarded=" + numEventsDiscarded + "."); 
	}

	@Override
	protected void logNoEventReceiver(String eventName) {
		// we fake server-side filtering #pushData and thus have to treat missing matching receiver as a problem
		logger.warning("logNoEventReceiver: clientName=" + clientName + ", eventName=" + eventName);
	}

	@Override
	protected void logQueueShutdownError(int timeoutMillis, int remainingEvents) {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void notifyFirstSubscription(Class<?> structClass) {
		// nothing
	}

	@Override
	protected void notifySubscriptionRemoved(Class<?> structClass) throws AcsJEventSubscriptionEx {
		// nothing
	}

	@Override
	protected void notifyNoSubscription() {
		// nothing
	}

}
