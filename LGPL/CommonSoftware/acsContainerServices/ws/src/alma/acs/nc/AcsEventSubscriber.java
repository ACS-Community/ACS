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

import alma.acsnc.EventDescription;

/**
 * This interface provides an event subscriber, 
 * that can be implemented on top of Corba NC or DDS, in-memory for local testing, or on top of other technologies.
 * <p>
 * Discussion:
 * <ul>
 *   <li>One AcsEventSubscriber object may subscribe to different types of events,
 *       which for a Corba NC is normal, but for DDS requires tricks with topics and partitions.
 *   <li>Alternatively, we could make this class generic and fix a single event type for the entire object (interface),
 *       so that the java compiler would not allow to add a subscription for a different type.
 *   <li>TODO: The declared exceptions must be cleaned up. Rather no generic AcsJException, and no Corba-NC-specific exceptions.
 *       Also, determine which methods should expose IllegalStateException and which not
 *       ({@link #disconnect()}, {@link #resume()} and {@link #suspend()} depend on the current state of the subscriber).
 *   <li>TODO: We need to check if this API actually works for DDS.
 *   <li>An in-memory shortcutting (simulated channel/topic) could be useful, for testing without external processes running.
 * </ul>
 * @author jslopez
 */
public interface AcsEventSubscriber {

	/*===========================================*/
	/*   Subscription management methods         */
	/*===========================================*/
	/**
	 * Adds a handler that will receive events of a specific type. The even type
	 * is determined by the value returned by calling {@link Callback#getEventType()}
	 * over the <code>receiver</code> parameter.
	 * <p>
	 * Note that the same event type can only be subscribed to with one handler,
	 * which means that another handler added for the same type will replace the previous handler.
	 *
	 * @param receiver The callback to use when receiving events for the specified type.
	 *
	 * @throws CannotAddSubscriptionException If there is a problem and the receiver cannot
	 *  be added
	 */
	public void addSubscription(Callback<?> receiver) 
		throws CannotAddSubscriptionException;

	/**
	 * Removes the subscription for a specified event type, 
	 * so that the handler previously registered for that event type 
	 * will no longer receive events.
	 * 
	 * @param structClass the event type to be unsubscribed. If <code>null</code>,
	 *  then all subscriptions but the generic subscription are removed.
	 *
	 * @throws SubscriptionNotFoundException if the specified event type has not been
	 *  previously subscribed.
	 */
	public void removeSubscription(Class<?> structClass) 
		throws SubscriptionNotFoundException;

	/**
	 * Adds a generic handler for all types of events.
	 * <p> 
	 * It is possible to add a generic handler in addition to event type specific handlers
	 * (where the latter will get precedence). 
	 * Adding another generic handler will replace the previous generic handler.
	 *
	 * @param receiver The callback to use when receiving events
	 *
	 * @throws CannotAddSubscriptionException If there is a problem and the generic receiver cannot
	 *  be added
	 */
	public void addGenericSubscription(GenericCallback receiver)
		throws CannotAddSubscriptionException;
	
	/**
	 * Removes the generic event handler, so that it will no longer receive events.
	 * Event specific handlers may still receive events.
	 *
	 * @throws SubscriptionNotFoundException If a generic receiver has not been previously subscribed
	 * @throws CannotRemoveSubscriptionExeption If there is any problem while unsubscribing the generic receiver
	 */
	public void removeGenericSubscription()
		throws SubscriptionNotFoundException;

	/*===========================================*/
	/*   Lifecycle methods                       */
	/*===========================================*/
	/**
	 * This method must be called to actually start receiving events.
	 * Typically it is called after the subscriptions are set up.
	 * User may still add and remove subscriptions at any given time, though.
	 * Also, the connection can be suspended and resumed.
	 * <p>
	 * No further invocations should be attempted on this method after one
	 * has been already successful. Otherwise, an {@link IllegalStateException}
	 * will be thrown. 
	 * <p>
	 * <b>If this method is not called, no event will be received</b>
	 * 
	 * @throws CannotStartReceivingEventsException If any error happens while trying
	 *   to start receiving events
	 * @throws IllegalStateException If the user calls this method on an object that
	 *   is already receiving events
	 */
	public void startReceivingEvents() throws CannotStartReceivingEventsException, IllegalStateException;


	/**
	 * Disconnects this subscriber from the Notification Channel, and releases all
	 * the resources associated with it. After this call, all registered handlers
	 * will stop receiving events, and this subscriber becomes unusable.
	 * 
	 * Calling this method over a subscriber object that has been already disconnected
	 * will throw an {@link IllegalStateException}.
	 *
	 * @throws IllegalStateException If this method is called over an AcsEventSubscriber object
	 * that has been already disconnected
	 */
	public void disconnect() throws IllegalStateException;

	/** 
	 * Used to temporarily halt receiving events of all types.
	 * <p>
	 * If the Subscriber has been connected already (method {@link #startReceivingEvents()}, 
	 * then after calling this method, incoming events will be buffered instead of being discarded; 
	 * unexpired events will be received later, after a call to {@link #resume()}.
	 * 
	 * <p>
	 * This call has no effect if the Subscriber is not connected, or if it is
	 * connected but already suspended.
	 *
	 * @throws IllegalStateException if the subscriber is not connected to an NC.
	 */
	public void suspend() throws IllegalStateException;

	/**
	 * Used to reenable the Subscriber after a call to the
	 * <code>suspend()</code> method. Queued events will be received after
	 * this call, see {@link #suspend()}.
	 * 
	 * This call has no effect if the Subscriber is not connected, or if it is
	 * connected and already processing events.
	 */
	public void resume() throws IllegalStateException;

	/**
	 * This ACS-defined interface replaces the runtime search for the
	 * "receive(...)" method that works with Java introspection.
	 */
	public static interface Callback<T> {
		
		public void receive(T event, EventDescription eventDescrip);
		
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
		public Class<T> getEventType();
	}
	
	/**
	 * This ACS-defined interface replaces the runtime search for the
	 * "receive(...)" method that works with Java introspection.
	 */
	public static interface GenericCallback {
		public void receive(Object event, EventDescription eventDescrip);
	}

}