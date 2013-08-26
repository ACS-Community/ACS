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

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acsErrTypeLifeCycle.wrappers.AcsJEventSubscriptionEx;
import alma.acsnc.EventDescription;

/**
 * This interface provides an event subscriber
 * that can be implemented on top of Corba NC or DDS, in-memory for local testing, 
 * or hopefully also on top of other technologies such as ZeroMQ.
 * <p>
 * Subscriber lifecycle:
 * <ul>
 *   <li> Creation of the subscriber object may already allocate remote resources.
 *        Therefore you <b>must always call {@link #disconnect()} when done using the subscriber</b>,
 *        even if {@link #startReceivingEvents()} was never called.
 *   <li> Subscriptions by event type or for all types of events can be set up
 *        using {@link #addSubscription(Callback)} and {@link #addGenericSubscription(GenericCallback)}. <br>
 *        Subscriptions can optionally be removed again using {@link #removeSubscription(Class)}
 *        and {@link #removeGenericSubscription()}.
 *        TODO: Verify that these can be called both before and after startReceivingEvents. <br>
 *   <li> Call {@link #startReceivingEvents()} to actually subscribe.
 *   <li> Call {@link #suspend()} and {@link #resume()} to temporarily suspend and later resume
 *        the subscription. Depending on the underlying framework, suspending will typically
 *        cause events to be queued somewhere.
 *   <li> Call {@link #disconnect()} to destroy all resources. The subscriber cannot be used afterwards.
 * </ul>
 * The lifecycle is internally implemented using a state machine, see {@link #getLifecycleState()}.
 * <p>
 * Discussion:
 * <ul>
 *   <li> One AcsEventSubscriber object may subscribe to different types of events,
 *        which for a Corba NC is normal, but for DDS requires tricks with topics and partitions.
 *        We might change this API to allow only one event type per NC.
 *   <li> TODO: We need to check if this API actually works for DDS, by integrating Rodrigo's RTI prototype.
 *   <li> For historical reasons the API is asymmetric with respect to creation and destruction of resources,
 *        because resources are allocated both in the ctor and in {@link #startReceivingEvents()}, 
 *        whereas they are released only in the single method {@link #disconnect()}.
 *        However, the underlying state machine knows two 'connect' and two 'disconnect' transitions
 *        and artificially maps them to the asymmetric API.
 *        If we want to make the API symmetric, we could break up 'disconnect' into 'stopReceivingEvents' 
 *        and 'destroy'. 
 * </ul>
 * @param <T> The event (base) type. If all events are of the same type then that type should be used; otherwise a common
 *            base type for all applicable events should be used, such as <code>Object</code> or <code>IDLEntity</code>. 
 * 
 * @author jslopez, hsommer
 */
public interface AcsEventSubscriber<T> {

	/*===========================================*/
	/*   Subscription management methods         */
	/*===========================================*/
	/**
	 * Adds a handler that will receive events of a specific type. The event type
	 * is determined by the value returned by calling {@link Callback#getEventType()}
	 * on the <code>receiver</code> parameter.
	 * <p>
	 * Note that the same event type can only be subscribed to with one handler,
	 * which means that another handler added for the same type will replace the previous handler.
	 * <p>
	 * The event type must be that of actual events, not of base classes. 
	 * For example, the AcsEventSubscriber could be parameterized with base type <code>IDLEntity</code>
	 * and we could then add two subscriptions for events defined as IDL structs, 
	 * say AntennaStatus and TemperatureData.
	 * If you want to subscribe to events without knowing their exact type, 
	 * use {@link #addGenericSubscription(GenericCallback)} instead.
	 * 
	 * @param receiver The callback to use when receiving events for the specified type.
	 *
	 * @throws AcsJEventSubscriptionEx If there is a problem and the receiver cannot be added
	 */
	public <U extends T> void addSubscription(Callback<U> receiver) 
		throws AcsJEventSubscriptionEx;

	/**
	 * Removes the subscription for a specified event type or for all events types, so that the handler previously
	 * registered for that event type will no longer receive events.
	 * 
	 * @param structClass
	 *            the event type to be unsubscribed. If <code>null</code>, then all subscriptions but the generic
	 *            subscription are removed.
	 * 
	 * @throws AcsJEventSubscriptionEx
	 *             if the specified event type has not been previously subscribed or if the removal fails with a
	 *             technical problem.
	 */
	public <U extends T> void removeSubscription(Class<U> structClass) 
		throws AcsJEventSubscriptionEx;

	/**
	 * Adds a generic handler for all types of events.
	 * This should only be used by monitoring tools and other special applications.
	 * For normal applications, use one or many calls to {@link #addSubscription(Callback)}, 
	 * each with a Callback object that matches exactly the subscribed event type.
	 * <p> 
	 * It is possible to add a generic handler in addition to event type-specific handlers
	 * (where the latter will get precedence). 
	 * Adding another generic handler will replace the previous generic handler.
	 *
	 * @param receiver The callback to use when receiving events
	 *
	 * @throws AcsJEventSubscriptionEx If there is a problem and the generic receiver cannot
	 *  be added
	 */
	public void addGenericSubscription(GenericCallback receiver)
		throws AcsJEventSubscriptionEx;
	
	/**
	 * Removes the generic event handler, so that it will no longer receive events.
	 * Event specific handlers may still receive events.
	 *
	 * @throws SubscriptionNotFoundException If a generic receiver has not been previously subscribed
	 * @throws AcsJEventSubscriptionEx If there is any problem while unsubscribing the generic receiver
	 */
	public void removeGenericSubscription()
		throws AcsJEventSubscriptionEx;

	
	/*===========================================*/
	/*   Lifecycle methods                       */
	/*===========================================*/
	
	/**
	 * Returns the lifecycle state as obtained from an internal state machine.
	 * <p>
	 * This state can be used only for debug output.
	 * <b>The state names may change over time and should not be used to control execution.</b>
	 */
	public String getLifecycleState();
	
	
	/**
	 * This method must be called to actually start receiving events.
	 * Typically it is called after the subscriptions are set up.
	 * User may still add and remove subscriptions at any given time, though.
	 * Also, the connection can be suspended and resumed.
	 * <p>
	 * No further invocations should be attempted on this method after one
	 * has been already successful. Otherwise, an {@link AcsJIllegalStateEventEx}
	 * will be thrown. 
	 * <p>
	 * <b>If this method is not called, no event will ever be received</b>
	 * 
	 * @throws AcsJIllegalStateEventEx If the user calls this method on an object that
	 *   is already receiving events
	 * @throws AcsJCouldntPerformActionEx If any error happens while trying
	 *   to start receiving events
	 */
	public void startReceivingEvents() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx;

	/**
	 * Disconnects this subscriber from the Notification Channel, and releases all
	 * the resources associated with it. After this call, all registered handlers
	 * will stop receiving events, and this subscriber becomes unusable.
	 * <p>
	 * Calling this method over a subscriber object that has been already disconnected
	 * will throw an {@link AcsJIllegalStateEventEx}.
	 * <p>
	 * @throws AcsJIllegalStateEventEx If this method is called on an AcsEventSubscriber object
	 * that has been already disconnected
	 */
	public void disconnect() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx;

	/** 
	 * Used to temporarily halt receiving events of all types.
	 * <p>
	 * If the Subscriber has been connected already (method {@link #startReceivingEvents()}, 
	 * then after calling this method, incoming events will be buffered instead of being discarded; 
	 * unexpired events will be received later, after a call to {@link #resume()}.
	 * 
	 * <p>
//	 * This call has no effect if the Subscriber is not connected, or if it is
//	 * connected but already suspended.
	 *
	 * @throws AcsJIllegalStateEventEx if the subscriber is not connected to an NC.
	 */
	public void suspend() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx;

	/**
	 * Returns <code>true</code> if this subscriber has been suspended.
	 */
	public boolean isSuspended();


	/**
	 * Used to reenable the Subscriber after a call to the
	 * <code>suspend()</code> method. Queued events will be received after
	 * this call, see {@link #suspend()}.
	 * 
	 * This call has no effect if the Subscriber is not connected, or if it is
	 * connected and already processing events.
	 */
	public void resume() throws AcsJIllegalStateEventEx, AcsJCouldntPerformActionEx;

	/**
	 * User callback for received events.
	 * <p>
	 * This ACS-defined interface replaces the runtime search for the old
	 * "Consumer#receive(...)" method that was based on Java introspection.
	 */
	public static interface Callback<U> {
		
		/**
		 * Event delivery, from the framework to user code.
		 * @param eventData The event data, e.g. an IDL-defined struct.
		 * @param eventDescrip Event meta data.
		 */
		public void receive(U eventData, EventDescription eventDescrip);
		
		/**
		 * This method is needed for adding event-specific subscriptions 
		 * and for the type-safety of this API (based on java generics), 
		 * and should be implemented like
		 * <pre>
		 * public Class&lt;MyEvent&gt; getEventType() {
		 *     return MyEvent.class;
		 * }
		 * </pre>
		 */
		public Class<U> getEventType();
	}
	
	/**
	 * Generic event callback, for use with {@link AcsEventSubscriber#addGenericSubscription(GenericCallback)}.
	 */
	public static interface GenericCallback {
		
		/**
		 * TODO: Use T instead of Object ?
		 */
		public void receiveGeneric(Object event, EventDescription eventDescrip);
	}

}